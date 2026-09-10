{-# LANGUAGE OverloadedStrings #-}

-- | The client-action protocol interpreter after ordinary route dispatch has
-- selected its request context.  It owns body bounds, origin/CSRF validation,
-- decoding, authorization, and handler invocation as one stable protocol
-- lifecycle; request timing and route selection remain in RequestExecution.
module HarchWeb.Server.ClientAction.Runtime
  ( clientActionResponse,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api.MediaType (apiUtf8ContentType, htmlMediaType, jsonContentType)
import HarchWeb.Api.Negotiation (ApiContentTypeNegotiationResult (..), selectContentTypeRepresentation)
import HarchWeb.ClientActionFailure (HarchClientFailure (..), failureReference)
import HarchWeb.Csrf (CsrfPagePreparationFailure (..), CsrfProtection (verifyCsrfToken), CsrfVerification (..), preparePageSecurity)
import HarchWeb.RequestId (RequestId)
import HarchWeb.Routing (RouteRequest (..))
import HarchWeb.Security (requestScheme)
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.RequestBody (RequestBodyReadFailure (..), readRequestBodyUpTo)
import HarchWeb.Server.Response
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

clientActionResponse :: Application route action context authorization -> RequestId -> Wai.Request -> Text -> Text -> context -> IO (Response route context)
clientActionResponse webApplication requestId request requestMethod requestPath routedRequestContext = do
  result <- runExceptT $ do
    let requestPolicyConfig = applicationRequestPolicy webApplication
        expectedOrigin =
          (\host -> requestScheme requestPolicyConfig request <> "://" <> host)
            <$> (lookup "Host" (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8')
    () <- liftClientActionEither (validateClientActionRequest expectedOrigin request)
    actionBody <- liftIO (readClientActionBody request)
    actionFields <- liftClientActionEither (actionBody >>= parseClientActionFields)
    csrfToken <- liftClientActionEither (validateClientActionCsrf request actionFields)
    let actionPayload =
          ClientActionPayload
            { clientActionMethod = requestMethod,
              clientActionPath = requestPath,
              clientActionFields = actionFields,
              clientActionCsrfToken = lookup "_harch_csrf" actionFields,
              clientActionIdempotencyKey = requestIdempotencyKey request,
              clientActionPayloadContext = routedRequestContext
            }
    case decodeClientAction webApplication actionPayload of
      UnrecognizedClientAction -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionNotFound))
      MethodNotAllowedClientAction allowedMethods -> pure (ClientActionBodyResponse (clientActionMethodNotAllowedResponse allowedMethods))
      MalformedClientAction _ -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionPayloadMalformed))
      InvalidClientActionDecoder -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionDecoderInvalid))
      DecodedClientAction action ->
        case clientActionRoute webApplication requestMethod requestPath routedRequestContext of
          Nothing -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionNotFound))
          Just actionRoute -> do
            let actionRouteRequest = RouteRequest actionRoute routedRequestContext
                actionRequest =
                  ClientActionRequest
                    { clientActionRouteRequest = actionRouteRequest,
                      clientAction = action,
                      clientActionRequestIdempotencyKey = requestIdempotencyKey request,
                      clientActionContext = routedRequestContext
                    }
            verification <- liftIO (verifyCsrfToken (csrfProtection webApplication) routedRequestContext csrfToken)
            case verification of
              CsrfRejected -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionCsrfRejected))
              CsrfVerificationUnavailable -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionCsrfUnavailable))
              CsrfVerified -> do
                maybeActionResult <- liftIO (handleClientAction webApplication actionRequest)
                liftIO (interpretActionResult actionRouteRequest maybeActionResult)
  pure (either (BodyResponse . clientActionProtocolErrorResponse requestId) id result)
  where
    attachFailureDestinations actionResponse =
      actionResponse
        { clientActionFailureDestinations =
            fmap
              failureDestinations
              (applicationClientActionFailureRoute webApplication)
        }
    failureDestinations failureRoute =
      ClientActionFailureDestinations
        { storageCleanupFailureDestination = failureRouteRequest StorageCleanupFailed,
          actionResponseProtocolFailureDestination = failureRouteRequest ActionResponseProtocolFailed,
          responseApplicationFailureDestination = failureRouteRequest ResponseApplicationFailed
        }
      where
        failureRouteRequest clientFailure =
          RouteRequest
            { requestRoute = failureRoute clientFailure (failureReference requestId),
              requestContext = routedRequestContext
            }

    interpretActionResult actionRouteRequest maybeActionResult =
      case maybeActionResult of
        Nothing -> pure (BodyResponse (clientActionProtocolErrorResponse requestId ClientActionNotFound))
        Just (ClientActionSucceeded actionResponse) ->
          pure (ClientActionBodyResponse (attachFailureDestinations actionResponse))
        Just (ClientActionFailedTerminally terminalFailure)
          | terminalHtmlRequested request -> terminalDocumentResponse actionRouteRequest terminalFailure
          | otherwise ->
              pure
                ( BodyResponse
                    ( appendTerminalDiagnostics
                        terminalFailure
                        (clientActionProtocolErrorResponse requestId ClientActionHandlerTerminalFailure)
                    )
                )

    terminalDocumentResponse actionRouteRequest terminalFailure = do
      preparedPageSecurity <- preparePageSecurity (csrfProtection webApplication) (csrfCookieFromRequest request) routedRequestContext
      pure $
        case preparedPageSecurity of
          Left CsrfPageProtectionUnavailable -> BodyResponse (clientActionProtocolErrorResponse requestId ClientActionCsrfUnavailable)
          Right pageSecurity ->
            PageResponseWithMetadata
              pageSecurity
              (terminalFailureResponseBody terminalFailure)
              (clientActionTerminalFailurePage terminalFailure (ClientActionFailurePresentation requestId actionRouteRequest))

terminalFailureResponseBody :: ClientActionTerminalFailure route context -> ResponseBody
terminalFailureResponseBody terminalFailure =
  ResponseBody
    { responseStatus = Http.internalServerError500,
      responseContentType = "text/html; charset=utf-8",
      responseBody = "",
      responseObservabilityAttributes = clientActionTerminalFailureObservabilityAttributes terminalFailure,
      responseLogEntries = clientActionTerminalFailureLogEntries terminalFailure,
      responseDatabaseOperations = []
    }

appendTerminalDiagnostics :: ClientActionTerminalFailure route context -> ResponseBody -> ResponseBody
appendTerminalDiagnostics terminalFailure responseBodyValue =
  responseBodyValue
    { responseObservabilityAttributes =
        responseObservabilityAttributes responseBodyValue
          <> clientActionTerminalFailureObservabilityAttributes terminalFailure,
      responseLogEntries =
        responseLogEntries responseBodyValue
          <> clientActionTerminalFailureLogEntries terminalFailure
    }

-- | Enhanced dispatch explicitly accepts an HTML fallback.  Normal API callers
-- that ask for JSON stay on the existing safe JSON rail.  The shared RFC 9110
-- selector owns quality, wildcard, and specific-exclusion behavior; an absent
-- header deliberately keeps the historical JSON default.
terminalHtmlRequested :: Wai.Request -> Bool
terminalHtmlRequested request =
  case lookup "Accept" (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8' of
    Nothing -> False
    Just acceptHeader ->
      case selectContentTypeRepresentation terminalRepresentations (Just acceptHeader) of
        SelectedContentTypeRepresentation selectedRepresentation -> selectedRepresentation == apiUtf8ContentType htmlMediaType
        NoAcceptableContentTypeRepresentation -> False
  where
    terminalRepresentations = apiUtf8ContentType htmlMediaType NonEmpty.:| [jsonContentType]

liftClientActionEither :: Either ClientActionProtocolError value -> ExceptT ClientActionProtocolError IO value
liftClientActionEither = either throwError pure

requestIdempotencyKey :: Wai.Request -> Maybe ClientActionIdempotencyKey
requestIdempotencyKey request =
  lookup "Idempotency-Key" (Wai.requestHeaders request)
    >>= either (const Nothing) Just . TextEncoding.decodeUtf8'

readClientActionBody :: Wai.Request -> IO (Either ClientActionProtocolError LazyByteString.ByteString)
readClientActionBody request = do
  result <- readRequestBodyUpTo maxClientActionBodyBytes request
  pure $
    case result of
      Left RequestBodyLimitExceeded -> Left ClientActionBodyTooLarge
      Right requestBody -> Right requestBody
