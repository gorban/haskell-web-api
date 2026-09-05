{-# LANGUAGE OverloadedStrings #-}

-- | Response finalization and WAI rendering for typed applications.
module HarchWeb.Server.ResponseRendering
  ( applyResponseHeaders,
    internalRedirectResponse,
    redirectResponse,
    nonPageInternalRedirectResponse,
    nonPageInternalRedirectResponseWithHeaders,
    nonPageRedirectResponse,
    responseDiagnostics,
    responseKind,
    responsePageSecurity,
    responsePolicyHeaders,
    responseStatusCode,
    toWaiResponse,
    toWaiBodyResponse,
  )
where

import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Csrf (CsrfCookieDisposition (..), PageCsrf, PageSecurity, csrfCookieMaxAgeSeconds, csrfTokenText, pageCsrfCookieDisposition, pageCsrfCookieMaxAge, pageCsrfValue, pageSecurityCsrf, pageSecurityRuntimeNonce, samePageSecurity)
import HarchWeb.Document (Page)
import HarchWeb.Document qualified as Document
import HarchWeb.Markup (safeUrlText)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..), encodeRouteLocation)
import HarchWeb.Security (RequestPolicyConfig, requestPolicyResponseHeadersWithNonce)
import HarchWeb.Server.Application (Application (..))
import HarchWeb.Server.ClientAction (clientActionResponseBody)
import HarchWeb.Server.Response
import HarchWeb.Server.Sse (renderServerSentEvent)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

applyResponseHeaders :: Http.ResponseHeaders -> Wai.Response -> Wai.Response
applyResponseHeaders additionalHeaders =
  Wai.mapResponseHeaders (additionalHeaders <>)

responsePolicyHeaders :: RequestPolicyConfig -> Wai.Request -> Maybe Document.RuntimeNonce -> Http.ResponseHeaders
responsePolicyHeaders = requestPolicyResponseHeadersWithNonce

responsePageSecurity :: Response route context -> Maybe PageSecurity
responsePageSecurity response =
  case response of
    PageResponse pageSecurity _ -> Just pageSecurity
    PageResponseWithMetadata pageSecurity _ _ -> Just pageSecurity
    BodyResponse _ -> Nothing
    RedirectResponse _ _ -> Nothing
    InternalRedirectResponse _ _ -> Nothing
    InternalRedirectResponseWithHeaders {} -> Nothing
    ClientActionBodyResponse _ -> Nothing
    EventStreamResponse _ _ -> Nothing
    ProtocolResponseResult _ -> Nothing

redirectResponse :: Http.Status -> Text -> Response route context
redirectResponse status =
  nonPageResponse . nonPageRedirectResponse status

-- | Construct a redirect to an application-owned route. The root renderer is
-- the only point that turns it into a browser location.
internalRedirectResponse :: Http.Status -> RouteRequest route context -> Response route context
internalRedirectResponse status =
  nonPageResponse . nonPageInternalRedirectResponse status

-- | Construct a redirect that is valid from a protocol route or endpoint
-- guard. It deliberately inhabits 'NonPageResponse', so redirect challenges
-- retain the established response semantics without gaining page authority.
nonPageRedirectResponse :: Http.Status -> Text -> NonPageResponse route context
nonPageRedirectResponse status =
  NonPageRedirectResponse (emptyRedirectResponseBody status)

nonPageInternalRedirectResponse :: Http.Status -> RouteRequest route context -> NonPageResponse route context
nonPageInternalRedirectResponse status =
  NonPageInternalRedirectResponse (emptyRedirectResponseBody status)

-- | Redirect to an application route while retaining response headers needed
-- to establish a new grant. The renderer always derives the final
-- @Location@ from the typed route and discards a supplied @Location@ header.
nonPageInternalRedirectResponseWithHeaders :: Http.Status -> Http.ResponseHeaders -> RouteRequest route context -> NonPageResponse route context
nonPageInternalRedirectResponseWithHeaders status =
  NonPageInternalRedirectResponseWithHeaders (emptyRedirectResponseBody status)

emptyRedirectResponseBody :: Http.Status -> ResponseBody
emptyRedirectResponseBody status =
  ResponseBody
    { responseStatus = status,
      responseContentType = "text/plain; charset=utf-8",
      responseBody = "",
      responseObservabilityAttributes = [],
      responseLogEntries = [],
      responseDatabaseOperations = []
    }

responseDiagnostics :: Response route context -> ResponseDiagnostics
responseDiagnostics response =
  case response of
    PageResponse _ _ -> ResponseDiagnostics [] [] []
    PageResponseWithMetadata _ responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    BodyResponse responseBodyValue -> responseBodyDiagnostics responseBodyValue
    RedirectResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    InternalRedirectResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    InternalRedirectResponseWithHeaders responseBodyValue _ _ -> responseBodyDiagnostics responseBodyValue
    ClientActionBodyResponse actionResponse ->
      ResponseDiagnostics
        (clientActionObservabilityAttributes actionResponse)
        (clientActionLogEntries actionResponse)
        []
    EventStreamResponse responseBodyValue _ -> responseBodyDiagnostics responseBodyValue
    ProtocolResponseResult protocolResponse ->
      ResponseDiagnostics
        (protocolResponseObservabilityAttributes protocolResponse)
        (protocolResponseLogEntries protocolResponse)
        (protocolResponseDatabaseOperations protocolResponse)

responseBodyDiagnostics :: ResponseBody -> ResponseDiagnostics
responseBodyDiagnostics responseBodyValue =
  ResponseDiagnostics
    { diagnosticObservabilityAttributes = responseObservabilityAttributes responseBodyValue,
      diagnosticLogEntries = responseLogEntries responseBodyValue,
      diagnosticDatabaseOperations = responseDatabaseOperations responseBodyValue
    }

responseStatusCode :: (Eq route) => Application route action context authorization -> Response route context -> Int
responseStatusCode webApplication response =
  case response of
    PageResponse _ page -> Http.statusCode (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
    PageResponseWithMetadata _ responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    BodyResponse responseBodyValue -> Http.statusCode (responseStatus responseBodyValue)
    RedirectResponse responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    InternalRedirectResponse responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    InternalRedirectResponseWithHeaders responseBodyValue _ _ -> Http.statusCode (responseStatus responseBodyValue)
    ClientActionBodyResponse actionResponse -> Http.statusCode (clientActionStatus actionResponse)
    EventStreamResponse responseBodyValue _ -> Http.statusCode (responseStatus responseBodyValue)
    ProtocolResponseResult protocolResponse -> Http.statusCode (protocolResponseStatus protocolResponse)

responseKind :: Response route context -> Observability.ResponseKind
responseKind response =
  case response of
    PageResponse _ _ -> Observability.PageResponseKind
    PageResponseWithMetadata {} -> Observability.PageResponseKind
    BodyResponse _ -> Observability.BodyResponseKind
    RedirectResponse _ _ -> Observability.BodyResponseKind
    InternalRedirectResponse _ _ -> Observability.BodyResponseKind
    InternalRedirectResponseWithHeaders {} -> Observability.BodyResponseKind
    ClientActionBodyResponse _ -> Observability.BodyResponseKind
    EventStreamResponse _ _ -> Observability.BodyResponseKind
    ProtocolResponseResult _ -> Observability.BodyResponseKind

toWaiResponse ::
  (Eq route) =>
  Http.ResponseHeaders ->
  Maybe PageSecurity ->
  Application route action context authorization ->
  Response route context ->
  Wai.Response
toWaiResponse additionalHeaders maybePageSecurity webApplication response =
  case response of
    PageResponse pageSecurity page ->
      renderPageResponse
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        pageSecurity
        page
    PageResponseWithMetadata pageSecurity pageResponseBodyValue page ->
      renderPageResponse (responseStatus pageResponseBodyValue) pageSecurity page
    BodyResponse responseBodyValue -> toWaiBodyResponse additionalHeaders responseBodyValue
    RedirectResponse responseBodyValue location -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 location)]) responseBodyValue
    InternalRedirectResponse responseBodyValue routeRequest -> toWaiBodyResponse (additionalHeaders <> [(Http.hLocation, TextEncoding.encodeUtf8 (safeUrlText (encodeRouteLocation (renderRoute (routeCodec webApplication) routeRequest))))]) responseBodyValue
    InternalRedirectResponseWithHeaders responseBodyValue headers routeRequest ->
      toWaiBodyResponse
        (additionalHeaders <> filter ((/= Http.hLocation) . fst) headers <> [(Http.hLocation, TextEncoding.encodeUtf8 (safeUrlText (encodeRouteLocation (renderRoute (routeCodec webApplication) routeRequest))))])
        responseBodyValue
    ClientActionBodyResponse actionResponse -> toWaiBodyResponse (additionalHeaders <> clientActionHeaders actionResponse) (clientActionResponseBody (routeCodec webApplication) actionResponse)
    EventStreamResponse responseBodyValue eventSource -> toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource
    ProtocolResponseResult protocolResponse -> toWaiProtocolResponse additionalHeaders protocolResponse
  where
    renderPageResponse status pageSecurity page =
      case maybePageSecurity of
        Just renderedPageSecurity
          | samePageSecurity renderedPageSecurity pageSecurity ->
              Wai.responseLBS
                status
                (pageResponseHeaders additionalHeaders (pageSecurityCsrf pageSecurity))
                ( LazyByteString.fromStrict
                    ( TextEncoding.encodeUtf8
                        ( Document.renderDocumentWithNonceAndActionCsrf
                            (pageSecurityRuntimeNonce pageSecurity)
                            (Just (csrfTokenText (pageCsrfValue (pageSecurityCsrf pageSecurity))))
                            (pageShell webApplication page)
                        )
                    )
                )
        Nothing ->
          Wai.responseLBS
            Http.internalServerError500
            [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
            "A page response was missing its CSP nonce."
        Just _ ->
          Wai.responseLBS
            Http.internalServerError500
            [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
            "A page response security value did not match its renderer input."

pageResponseHeaders :: Http.ResponseHeaders -> PageCsrf -> Http.ResponseHeaders
pageResponseHeaders additionalHeaders pageCsrf =
  additionalHeaders
    <> [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
    <> case pageCsrfCookieDisposition pageCsrf of
      RetainCsrfCookie -> []
      SetCsrfCookie ->
        [ ("Set-Cookie", TextEncoding.encodeUtf8 ("__Host-harch-csrf=" <> csrfTokenText (pageCsrfValue pageCsrf) <> "; Path=/; Max-Age=" <> Text.pack (show (csrfCookieMaxAgeSeconds (pageCsrfCookieMaxAge pageCsrf))) <> "; Secure; HttpOnly; SameSite=Strict"))
        ]

toWaiBodyResponse :: Http.ResponseHeaders -> ResponseBody -> Wai.Response
toWaiBodyResponse additionalHeaders responseBodyValue =
  Wai.responseLBS
    (responseStatus responseBodyValue)
    (additionalHeaders <> [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))])
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

toWaiProtocolResponse :: Http.ResponseHeaders -> ProtocolResponse -> Wai.Response
toWaiProtocolResponse additionalHeaders protocolResponse =
  case protocolResponseBody protocolResponse of
    ProtocolResponseBytes bodyBytes ->
      Wai.responseLBS
        (protocolResponseStatus protocolResponse)
        (additionalHeaders <> protocolResponseHeaders protocolResponse)
        (LazyByteString.fromStrict bodyBytes)
    ProtocolResponseStream streamBody ->
      Wai.responseStream
        (protocolResponseStatus protocolResponse)
        (additionalHeaders <> protocolResponseHeaders protocolResponse)
        streamBody
    ProtocolResponseWai waiResponse ->
      applyResponseHeaders (additionalHeaders <> protocolResponseHeaders protocolResponse) waiResponse

toWaiEventStreamResponse :: Http.ResponseHeaders -> ResponseBody -> ServerSentEventSource -> Wai.Response
toWaiEventStreamResponse additionalHeaders responseBodyValue eventSource =
  Wai.responseStream
    (responseStatus responseBodyValue)
    ( additionalHeaders
        <> [ (Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue)),
             ("Cache-Control", "no-cache"),
             ("X-Accel-Buffering", "no")
           ]
    )
    streamEvents
  where
    streamEvents write flush = do
      maybeEvent <- nextServerSentEvent eventSource
      for_ maybeEvent $ \event -> do
        write (ByteStringBuilder.byteString (TextEncoding.encodeUtf8 (renderServerSentEvent event)))
        flush
        streamEvents write flush

isNotFoundPage :: (Eq route) => Application route action context authorization -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = Document.pageContext page
   in pageRequestContext `seq`
        Document.pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

htmlContentType :: Text
htmlContentType = "text/html; charset=utf-8"
