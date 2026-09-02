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
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Security (requestScheme)
import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.RequestBody (RequestBodyReadFailure (..), readRequestBodyUpTo)
import HarchWeb.Server.Response
import Network.Wai qualified as Wai

clientActionResponse :: Application route action context authorization -> Wai.Request -> Text -> Text -> context -> IO (Response route context)
clientActionResponse webApplication request requestMethod requestPath routedRequestContext = do
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
      UnrecognizedClientAction -> pure (BodyResponse (clientActionProtocolErrorResponse ClientActionNotFound))
      MethodNotAllowedClientAction allowedMethods -> pure (ClientActionBodyResponse (clientActionMethodNotAllowedResponse allowedMethods))
      MalformedClientAction _ -> pure (BodyResponse (clientActionProtocolErrorResponse ClientActionPayloadMalformed))
      InvalidClientActionDecoder -> pure (BodyResponse (clientActionProtocolErrorResponse ClientActionDecoderInvalid))
      DecodedClientAction action -> do
        let actionRequest =
              ClientActionRequest
                { clientAction = action,
                  clientActionRequestIdempotencyKey = requestIdempotencyKey request,
                  clientActionContext = routedRequestContext
                }
        authorized <- liftIO (authorizeClientActionCsrf webApplication actionRequest csrfToken)
        case authorized of
          False -> pure (BodyResponse (clientActionProtocolErrorResponse ClientActionCsrfRejected))
          True -> do
            maybeActionResponse <- liftIO (handleClientAction webApplication actionRequest)
            pure (maybe (BodyResponse (clientActionProtocolErrorResponse ClientActionNotFound)) ClientActionBodyResponse maybeActionResponse)
  pure (either (BodyResponse . clientActionProtocolErrorResponse) id result)

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
