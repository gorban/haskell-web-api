{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module HarchWeb.Security
  ( CorsPolicyConfig (..),
    RequestByteLimit,
    RequestHeadLimitFailure (..),
    RequestHeadLimits (..),
    RequestHeaderCountLimit,
    RequestItemCountLimit,
    RequestContextField (..),
    RequestPolicyConfig (..),
    ResponseSecurityHeadersConfig (..),
    StrictTransportSecurityConfig (..),
    addRuntimeNonceToContentSecurityPolicy,
    applyRequestPathPrefix,
    corsPreflightResponse,
    defaultContentSecurityPolicy,
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    externalRequestPath,
    httpsRedirectResponse,
    requestContextObservabilityAttributes,
    requestByteLimit,
    requestByteLimitValue,
    mkRequestHeaderCountLimit,
    requestItemCountLimit,
    requestContextFields,
    requestHostWithoutPort,
    requestLogContextFields,
    requestPathPrefix,
    requestPolicyResponseHeaders,
    requestPolicyResponseHeadersWithNonce,
    prependRequestLogContext,
    requestRedirectLocation,
    requestScheme,
    requestTraceContext,
    responseSecurityHeaderValuesWithNonce,
    socketAddressText,
    stripRequestPathPrefix,
    unboundedRequestHeadLimits,
    validateRequestHead,
    waiRequestPath,
    waiRequestRouteTarget,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Char (isHexDigit)
import Data.Maybe (catMaybes, fromMaybe, isJust, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import HarchWeb.Document (RuntimeNonce (..))
import HarchWeb.Observability qualified as Observability
import HarchWeb.PathPrefix qualified as PathPrefix
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai

data StrictTransportSecurityConfig = StrictTransportSecurityConfig
  { strictTransportSecurityMaxAgeSeconds :: Int,
    strictTransportSecurityIncludeSubDomains :: Bool,
    strictTransportSecurityPreload :: Bool
  }
  deriving (Eq, Show)

data CorsPolicyConfig = CorsPolicyConfig
  { corsAllowedOrigins :: [Text],
    corsAllowedMethods :: [Text],
    corsAllowedHeaders :: [Text],
    corsMaxAgeSeconds :: Maybe Int
  }
  deriving (Eq, Show)

-- | A request-derived value retained in private traces and structured logs.
-- These fields may contain personal or high-cardinality data, so metric
-- projection is intentionally handled elsewhere.
data RequestContextField = RequestContextField
  { requestContextFieldName :: Text,
    requestContextFieldValue :: Text
  }
  deriving (Eq, Show)

defaultCorsPolicyConfig :: CorsPolicyConfig
defaultCorsPolicyConfig =
  CorsPolicyConfig
    { corsAllowedOrigins = [],
      corsAllowedMethods = ["GET", "HEAD", "OPTIONS"],
      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
      corsMaxAgeSeconds = Nothing
    }

data ResponseSecurityHeadersConfig = ResponseSecurityHeadersConfig
  { contentSecurityPolicy :: Maybe Text,
    contentTypeOptionsNoSniff :: Bool,
    xssProtection :: Maybe Text,
    referrerPolicy :: Maybe Text,
    permissionsPolicy :: Maybe Text,
    frameOptions :: Maybe Text
  }
  deriving (Eq, Show)

defaultContentSecurityPolicy :: Text
defaultContentSecurityPolicy =
  Text.intercalate
    "; "
    [ "default-src 'self'",
      "base-uri 'self'",
      "object-src 'none'",
      "frame-ancestors 'none'",
      "form-action 'self'",
      "script-src 'self'",
      "style-src 'self'",
      "img-src 'self' data:",
      "font-src 'self'",
      "connect-src 'self'"
    ]

defaultResponseSecurityHeadersConfig :: ResponseSecurityHeadersConfig
defaultResponseSecurityHeadersConfig =
  ResponseSecurityHeadersConfig
    { contentSecurityPolicy = Just defaultContentSecurityPolicy,
      contentTypeOptionsNoSniff = True,
      xssProtection = Just "1; mode=block",
      referrerPolicy = Just "strict-origin-when-cross-origin",
      permissionsPolicy = Just "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()",
      frameOptions = Just "DENY"
    }

data RequestPolicyConfig = RequestPolicyConfig
  { redirectHttpToHttps :: Bool,
    httpsRedirectPort :: Maybe Int,
    strictTransportSecurity :: Maybe StrictTransportSecurityConfig,
    trustForwardedHeaders :: Bool,
    requestHeadLimits :: RequestHeadLimits,
    corsPolicy :: CorsPolicyConfig,
    responseSecurityHeaders :: ResponseSecurityHeadersConfig
  }
  deriving (Eq, Show)

-- | A non-negative byte bound used for untrusted request metadata.  Construct it
-- with 'requestByteLimit' so a negative configuration cannot enter the
-- request boundary.
newtype RequestByteLimit = RequestByteLimit Int
  deriving (Eq, Show)

requestByteLimit :: Int -> Maybe RequestByteLimit
requestByteLimit byteCount
  | byteCount >= 0 = Just (RequestByteLimit byteCount)
  | otherwise = Nothing

requestByteLimitValue :: RequestByteLimit -> Int
requestByteLimitValue (RequestByteLimit byteCount) = byteCount

-- | A non-negative bound on the number of request header fields.
newtype RequestHeaderCountLimit = RequestHeaderCountLimit Int
  deriving (Eq, Show)

mkRequestHeaderCountLimit :: Int -> Maybe RequestHeaderCountLimit
mkRequestHeaderCountLimit headerCount
  | headerCount >= 0 = Just (RequestHeaderCountLimit headerCount)
  | otherwise = Nothing

-- | A non-negative bound on repeated untrusted request components such as
-- path segments or query fields.
newtype RequestItemCountLimit = RequestItemCountLimit Int
  deriving (Show)

instance Eq RequestItemCountLimit where
  RequestItemCountLimit left == RequestItemCountLimit right = left == right

requestItemCountLimit :: Int -> Maybe RequestItemCountLimit
requestItemCountLimit itemCount
  | itemCount >= 0 = Just (RequestItemCountLimit itemCount)
  | otherwise = Nothing

-- | Limits checked before request-derived text is parsed, logged, routed, or
-- handed to application middleware.  Every field is optional deliberately:
-- the framework currently preserves its established unbounded behaviour
-- until an application chooses a deployment-appropriate budget.
data RequestHeadLimits = RequestHeadLimits
  { requestTargetByteLimit :: Maybe RequestByteLimit,
    requestHeaderByteLimit :: Maybe RequestByteLimit,
    requestHeaderCountLimit :: Maybe RequestHeaderCountLimit,
    requestHeaderValueByteLimit :: Maybe RequestByteLimit,
    requestPathSegmentCountLimit :: Maybe RequestItemCountLimit,
    requestPathSegmentByteLimit :: Maybe RequestByteLimit,
    requestQueryFieldCountLimit :: Maybe RequestItemCountLimit,
    requestQueryFieldByteLimit :: Maybe RequestByteLimit
  }
  deriving (Eq, Show)

unboundedRequestHeadLimits :: RequestHeadLimits
unboundedRequestHeadLimits =
  RequestHeadLimits
    { requestTargetByteLimit = Nothing,
      requestHeaderByteLimit = Nothing,
      requestHeaderCountLimit = Nothing,
      requestHeaderValueByteLimit = Nothing,
      requestPathSegmentCountLimit = Nothing,
      requestPathSegmentByteLimit = Nothing,
      requestQueryFieldCountLimit = Nothing,
      requestQueryFieldByteLimit = Nothing
    }

-- | A stable, low-cardinality explanation for rejecting an inbound request
-- before application code owns it.  No constructor carries untrusted input,
-- keeping it safe to map to metrics or public protocol responses.
data RequestHeadLimitFailure
  = InvalidRequestTargetEncoding
  | RequestTargetTooLarge
  | TooManyRequestHeaders
  | RequestHeadersTooLarge
  | RequestHeaderValueTooLarge
  | TooManyPathSegments
  | RequestPathSegmentTooLarge
  | TooManyQueryFields
  | RequestQueryFieldTooLarge
  deriving (Eq, Show)

-- | Validate the raw request target and headers without consuming a body.
-- This runs before route parsing, middleware, and request observability so an
-- invalid UTF-8 target or configured head limit cannot become an exception or
-- an allocation amplifier in a downstream parser.
validateRequestHead :: RequestHeadLimits -> Wai.Request -> Either RequestHeadLimitFailure ()
validateRequestHead limits request
  | not (isUtf8 (Wai.rawPathInfo request) && isUtf8 (Wai.rawQueryString request)) = Left InvalidRequestTargetEncoding
  | exceedsByteLimit (requestTargetByteLimit limits) requestTargetBytes = Left RequestTargetTooLarge
  | exceedsHeaderCountLimit (requestHeaderCountLimit limits) (length requestHeaders) = Left TooManyRequestHeaders
  | exceedsByteLimit (requestHeaderByteLimit limits) requestHeadersBytes = Left RequestHeadersTooLarge
  | any (exceedsByteLimit (requestHeaderValueByteLimit limits) . ByteString.length . snd) requestHeaders = Left RequestHeaderValueTooLarge
  | exceedsItemCountLimit (requestPathSegmentCountLimit limits) (pathSegmentCount rawPath) = Left TooManyPathSegments
  | exceedsDelimitedFieldByteLimit (requestPathSegmentByteLimit limits) 47 rawPath = Left RequestPathSegmentTooLarge
  | exceedsItemCountLimit (requestQueryFieldCountLimit limits) (queryFieldCount rawQuery) = Left TooManyQueryFields
  | exceedsDelimitedFieldByteLimit (requestQueryFieldByteLimit limits) 38 (ByteString.drop 1 rawQuery) = Left RequestQueryFieldTooLarge
  | otherwise = Right ()
  where
    requestHeaders = Wai.requestHeaders request
    rawPath = Wai.rawPathInfo request
    rawQuery = Wai.rawQueryString request
    requestTargetBytes = ByteString.length rawPath + ByteString.length rawQuery
    requestHeadersBytes = sum [ByteString.length (CaseInsensitive.original name) + ByteString.length value | (name, value) <- requestHeaders]

isUtf8 :: ByteString.ByteString -> Bool
isUtf8 = either (const False) (const True) . TextEncoding.decodeUtf8'

exceedsByteLimit :: Maybe RequestByteLimit -> Int -> Bool
exceedsByteLimit maybeLimit byteCount =
  case maybeLimit of
    Nothing -> False
    Just (RequestByteLimit maximumBytes) -> byteCount > maximumBytes

exceedsHeaderCountLimit :: Maybe RequestHeaderCountLimit -> Int -> Bool
exceedsHeaderCountLimit maybeLimit headerCount =
  case maybeLimit of
    Nothing -> False
    Just (RequestHeaderCountLimit maximumHeaders) -> headerCount > maximumHeaders

exceedsItemCountLimit :: Maybe RequestItemCountLimit -> Int -> Bool
exceedsItemCountLimit maybeLimit itemCount =
  case maybeLimit of
    Nothing -> False
    Just (RequestItemCountLimit maximumItems) -> itemCount > maximumItems

pathSegmentCount :: ByteString.ByteString -> Int
pathSegmentCount = snd . ByteString.foldl' countSegment (True, 0)
  where
    countSegment (previousWasSeparator, segmentCount) byte
      | byte == 47 = (True, segmentCount)
      | previousWasSeparator = (False, segmentCount + 1)
      | otherwise = (False, segmentCount)

queryFieldCount :: ByteString.ByteString -> Int
queryFieldCount rawQuery =
  case ByteString.uncons rawQuery of
    Just (63, queryBytes)
      | not (ByteString.null queryBytes) -> ByteString.foldl' countSeparator 1 queryBytes
    _ -> 0
  where
    countSeparator fieldCount byte
      | byte == 38 = fieldCount + 1
      | otherwise = fieldCount

exceedsDelimitedFieldByteLimit :: Maybe RequestByteLimit -> Word8 -> ByteString.ByteString -> Bool
exceedsDelimitedFieldByteLimit maybeLimit delimiter =
  maybe (const False) (go 0 . requestByteLimitValue) maybeLimit
  where
    go fieldBytes maximumBytes bytes =
      case ByteString.uncons bytes of
        Nothing -> fieldBytes > maximumBytes
        Just (byte, remaining)
          | byte == delimiter -> fieldBytes > maximumBytes || go 0 maximumBytes remaining
          | otherwise -> go (fieldBytes + 1) maximumBytes remaining

httpsRedirectResponse :: ByteString.ByteString -> Wai.Response
httpsRedirectResponse redirectLocation =
  Wai.responseLBS
    Http.status308
    [ (Http.hLocation, redirectLocation),
      (Http.hContentType, "text/plain; charset=utf-8")
    ]
    "Redirecting to HTTPS"

waiRequestPath :: RequestPolicyConfig -> Wai.Request -> Text
waiRequestPath requestPolicyConfig request =
  stripRequestPathPrefix
    (requestPathPrefix requestPolicyConfig request)
    (rawRequestPath request)

requestRedirectLocation :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectLocation requestPolicyConfig request =
  if redirectHttpToHttps requestPolicyConfig
    && requestScheme requestPolicyConfig request == "http"
    && not (isAcmeHttp01ChallengeRequest requestPolicyConfig request)
    then
      fmap
        ( \redirectAuthority ->
            "https://"
              <> redirectAuthority
              <> requestRedirectPathAndQuery requestPolicyConfig request
        )
        (requestRedirectAuthority requestPolicyConfig request)
    else Nothing

requestRedirectAuthority :: RequestPolicyConfig -> Wai.Request -> Maybe ByteString.ByteString
requestRedirectAuthority requestPolicyConfig request =
  fmap
    (applyHttpsRedirectPort (httpsRedirectPort requestPolicyConfig))
    (lookup "Host" (Wai.requestHeaders request))

requestRedirectPathAndQuery :: RequestPolicyConfig -> Wai.Request -> ByteString.ByteString
requestRedirectPathAndQuery requestPolicyConfig request =
  TextEncoding.encodeUtf8 (externalRequestPath requestPolicyConfig request) <> Wai.rawQueryString request

applyHttpsRedirectPort :: Maybe Int -> ByteString.ByteString -> ByteString.ByteString
applyHttpsRedirectPort maybeRedirectPort hostHeader =
  let normalizedDefaultHostHeader =
        fromMaybe hostHeader (ByteStringChar8.stripSuffix ":80" hostHeader)
      hostOnly = ByteStringChar8.takeWhile (/= ':') normalizedDefaultHostHeader
   in case maybeRedirectPort of
        Nothing -> normalizedDefaultHostHeader
        Just 443 -> hostOnly
        Just redirectPort ->
          hostOnly <> ":" <> ByteStringChar8.pack (show redirectPort)

isAcmeHttp01ChallengeRequest :: RequestPolicyConfig -> Wai.Request -> Bool
isAcmeHttp01ChallengeRequest requestPolicyConfig request =
  Text.isPrefixOf "/.well-known/acme-challenge/" (waiRequestPath requestPolicyConfig request)

requestPolicyResponseHeaders :: RequestPolicyConfig -> Wai.Request -> Http.ResponseHeaders
requestPolicyResponseHeaders requestPolicyConfig request =
  requestPolicyResponseHeadersWithNonce requestPolicyConfig request Nothing

requestPolicyResponseHeadersWithNonce :: RequestPolicyConfig -> Wai.Request -> Maybe RuntimeNonce -> Http.ResponseHeaders
requestPolicyResponseHeadersWithNonce requestPolicyConfig request maybeRuntimeNonce =
  responseSecurityHeaderValuesWithNonce (responseSecurityHeaders requestPolicyConfig) maybeRuntimeNonce
    <> strictTransportSecurityHeaders requestPolicyConfig request
    <> corsPolicyHeaders (corsPolicy requestPolicyConfig) request

responseSecurityHeaderValuesWithNonce :: ResponseSecurityHeadersConfig -> Maybe RuntimeNonce -> Http.ResponseHeaders
responseSecurityHeaderValuesWithNonce responseSecurityHeadersConfig maybeRuntimeNonce =
  catMaybes
    [ ("Content-Security-Policy",) . TextEncoding.encodeUtf8
        <$> contentSecurityPolicyWithRuntimeNonce maybeRuntimeNonce (contentSecurityPolicy responseSecurityHeadersConfig),
      if contentTypeOptionsNoSniff responseSecurityHeadersConfig
        then Just ("X-Content-Type-Options", "nosniff")
        else Nothing,
      ("X-XSS-Protection",) . TextEncoding.encodeUtf8
        <$> xssProtection responseSecurityHeadersConfig,
      ("Referrer-Policy",) . TextEncoding.encodeUtf8
        <$> referrerPolicy responseSecurityHeadersConfig,
      ("Permissions-Policy",) . TextEncoding.encodeUtf8
        <$> permissionsPolicy responseSecurityHeadersConfig,
      ("X-Frame-Options",) . TextEncoding.encodeUtf8
        <$> frameOptions responseSecurityHeadersConfig
    ]

contentSecurityPolicyWithRuntimeNonce :: Maybe RuntimeNonce -> Maybe Text -> Maybe Text
contentSecurityPolicyWithRuntimeNonce maybeRuntimeNonce maybeContentSecurityPolicy =
  case (maybeRuntimeNonce, maybeContentSecurityPolicy) of
    (Just runtimeNonce, Just policy) -> Just (addRuntimeNonceToContentSecurityPolicy runtimeNonce policy)
    (_, policy) -> policy

addRuntimeNonceToContentSecurityPolicy :: RuntimeNonce -> Text -> Text
addRuntimeNonceToContentSecurityPolicy runtimeNonce policy =
  Text.intercalate
    "; "
    ( if any isScriptSourceDirective directives
        then map addNonceToDirective directives
        else directives <> ["script-src " <> nonceSource]
    )
  where
    nonceSource = "'nonce-" <> runtimeNonceValue runtimeNonce <> "'"
    directives = filter (not . Text.null) (map Text.strip (Text.splitOn ";" policy))
    isScriptSourceDirective directive =
      case Text.words directive of
        "script-src" : _ -> True
        _ -> False
    addNonceToDirective directive =
      case Text.words directive of
        "script-src" : sources ->
          Text.unwords
            ( "script-src"
                : if "'none'" `elem` sources
                  then [nonceSource]
                  else sources <> [nonceSource]
            )
        _ -> Text.strip directive

strictTransportSecurityHeaders :: RequestPolicyConfig -> Wai.Request -> Http.ResponseHeaders
strictTransportSecurityHeaders requestPolicyConfig request =
  case strictTransportSecurity requestPolicyConfig of
    Just config
      | requestScheme requestPolicyConfig request == "https" ->
          [ ( "Strict-Transport-Security",
              TextEncoding.encodeUtf8 (strictTransportSecurityHeaderValue config)
            )
          ]
    _ -> []

corsPolicyHeaders :: CorsPolicyConfig -> Wai.Request -> Http.ResponseHeaders
corsPolicyHeaders corsPolicyConfig request =
  case lookup "Origin" (Wai.requestHeaders request) of
    Just originHeader
      | originAllowed corsPolicyConfig originHeader ->
          [("Access-Control-Allow-Origin", originHeader), ("Vary", "Origin")]
            <> corsPreflightHeaders corsPolicyConfig request
    _ -> []

corsPreflightHeaders :: CorsPolicyConfig -> Wai.Request -> Http.ResponseHeaders
corsPreflightHeaders corsPolicyConfig request =
  if corsPreflightRequestAllowed corsPolicyConfig request
    then
      [("Access-Control-Allow-Methods", corsHeaderValue (corsAllowedMethods corsPolicyConfig))]
        <> [ ("Access-Control-Allow-Headers", corsHeaderValue (corsAllowedHeaders corsPolicyConfig))
           | not (null (corsAllowedHeaders corsPolicyConfig))
           ]
        <> [ ("Access-Control-Max-Age", ByteStringChar8.pack (show maxAgeSeconds))
           | Just maxAgeSeconds <- [corsMaxAgeSeconds corsPolicyConfig]
           ]
    else []

corsPreflightResponse :: RequestPolicyConfig -> Wai.Request -> Maybe Wai.Response
corsPreflightResponse requestPolicyConfig request =
  case lookup "Origin" (Wai.requestHeaders request) of
    Just originHeader
      | originAllowed (corsPolicy requestPolicyConfig) originHeader
          && corsPreflightRequestAllowed (corsPolicy requestPolicyConfig) request ->
          Just (Wai.responseLBS Http.status204 [] "")
    _ -> Nothing

isCorsPreflightRequest :: Wai.Request -> Bool
isCorsPreflightRequest request =
  Wai.requestMethod request == "OPTIONS"
    && isJust (lookup "Origin" (Wai.requestHeaders request))
    && isJust (lookup "Access-Control-Request-Method" (Wai.requestHeaders request))

corsPreflightRequestAllowed :: CorsPolicyConfig -> Wai.Request -> Bool
corsPreflightRequestAllowed corsPolicyConfig request =
  case lookup "Access-Control-Request-Method" (Wai.requestHeaders request) of
    Just requestedMethod ->
      isCorsPreflightRequest request
        && requestedMethodAllowed corsPolicyConfig requestedMethod
    Nothing -> False

originAllowed :: CorsPolicyConfig -> ByteString.ByteString -> Bool
originAllowed corsPolicyConfig originHeader =
  originHeader `elem` map TextEncoding.encodeUtf8 (corsAllowedOrigins corsPolicyConfig)

requestedMethodAllowed :: CorsPolicyConfig -> ByteString.ByteString -> Bool
requestedMethodAllowed corsPolicyConfig requestedMethod =
  requestedMethod `elem` map TextEncoding.encodeUtf8 (corsAllowedMethods corsPolicyConfig)

corsHeaderValue :: [Text] -> ByteString.ByteString
corsHeaderValue = TextEncoding.encodeUtf8 . Text.intercalate ", "

strictTransportSecurityHeaderValue :: StrictTransportSecurityConfig -> Text
strictTransportSecurityHeaderValue config =
  Text.intercalate
    "; "
    ( ["max-age=" <> Text.pack (show (strictTransportSecurityMaxAgeSeconds config))]
        ++ ["includeSubDomains" | strictTransportSecurityIncludeSubDomains config]
        ++ ["preload" | strictTransportSecurityPreload config]
    )

requestContextObservabilityAttributes :: RequestPolicyConfig -> Wai.Request -> [Observability.ObservabilityAttribute]
requestContextObservabilityAttributes requestPolicyConfig request =
  map
    requestContextFieldObservabilityAttribute
    (filter ((/= "url.scheme") . requestContextFieldName) (requestContextFields requestPolicyConfig request))

requestLogContextFields :: RequestPolicyConfig -> Wai.Request -> [Text]
requestLogContextFields requestPolicyConfig request =
  map requestContextFieldLogField (requestContextFields requestPolicyConfig request)

requestContextFields :: RequestPolicyConfig -> Wai.Request -> [RequestContextField]
requestContextFields requestPolicyConfig request =
  requiredField "client.address" (effectiveClientAddress requestPolicyConfig request)
    : requiredField "network.peer.address" (peerAddressText request)
    : optionalField "harch.client.address.source" (effectiveClientAddressSource requestPolicyConfig request)
    ++ optionalField "http.request.header.x_forwarded_for" (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-For" request)
    ++ optionalField "http.request.header.forwarded" (trustedRequestHeaderText requestPolicyConfig "Forwarded" request)
    ++ optionalField "http.request.header.x_forwarded_proto" (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Proto" request)
    ++ optionalField "http.request.header.x_forwarded_prefix" (trustedRequestHeaderText requestPolicyConfig "X-Forwarded-Prefix" request)
    ++ optionalField "user_agent.original" (requestHeaderText "User-Agent" request)
    ++ optionalField "http.request.header.referer" (sanitizedReferer request)
    ++ optionalField "http.request.header.x_requested_with" (requestHeaderText "X-Requested-With" request)
    ++ optionalField "harch.request.source" (requestSource request)
    ++ [requiredField "url.scheme" (requestScheme requestPolicyConfig request)]
  where
    requiredField = RequestContextField
    optionalField name = maybe [] (pure . RequestContextField name)

requestContextFieldObservabilityAttribute :: RequestContextField -> Observability.ObservabilityAttribute
requestContextFieldObservabilityAttribute field =
  textObservabilityAttribute (requestContextFieldName field) (requestContextFieldValue field)

requestContextFieldLogField :: RequestContextField -> Text
requestContextFieldLogField field =
  renderRequestLogField (requestContextFieldName field) (requestContextFieldValue field)

requestScheme :: RequestPolicyConfig -> Wai.Request -> Text
requestScheme requestPolicyConfig request =
  case fmap Text.toLower (trustedForwardedHeaderToken requestPolicyConfig "proto" request <|> trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-Proto" request) of
    Just "https" -> "https"
    Just "http" -> "http"
    _ -> if Wai.isSecure request then "https" else "http"

effectiveClientAddress :: RequestPolicyConfig -> Wai.Request -> Text
effectiveClientAddress requestPolicyConfig request =
  fromMaybe
    (peerAddressText request)
    (trustedForwardedHeaderToken requestPolicyConfig "for" request <|> trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-For" request)

effectiveClientAddressSource :: RequestPolicyConfig -> Wai.Request -> Maybe Text
effectiveClientAddressSource requestPolicyConfig request =
  case trustedForwardedHeaderToken requestPolicyConfig "for" request of
    Just _ -> Just "forwarded"
    Nothing -> case trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-For" request of
      Just _ -> Just "x-forwarded-for"
      Nothing -> Nothing

peerAddressText :: Wai.Request -> Text
peerAddressText = socketAddressText . Wai.remoteHost

socketAddressText :: Socket.SockAddr -> Text
socketAddressText socketAddress =
  case socketAddress of
    Socket.SockAddrInet _ hostAddress ->
      let (firstOctet, secondOctet, thirdOctet, fourthOctet) = Socket.hostAddressToTuple hostAddress
       in Text.intercalate "." (map (Text.pack . show) [firstOctet, secondOctet, thirdOctet, fourthOctet])
    _ -> Text.pack (show socketAddress)

requestHeaderToken :: Http.HeaderName -> Wai.Request -> Maybe Text
requestHeaderToken headerName request = requestHeaderText headerName request >>= firstCommaSeparatedValue

requestHeaderText :: Http.HeaderName -> Wai.Request -> Maybe Text
requestHeaderText headerName request =
  lookup headerName (Wai.requestHeaders request)
    >>= either (const Nothing) (Just . limitObservabilityHeaderValue . Text.strip) . TextEncoding.decodeUtf8'

trustedRequestHeaderText :: RequestPolicyConfig -> Http.HeaderName -> Wai.Request -> Maybe Text
trustedRequestHeaderText requestPolicyConfig headerName request =
  if trustForwardedHeaders requestPolicyConfig then requestHeaderText headerName request else Nothing

trustedRequestHeaderToken :: RequestPolicyConfig -> Http.HeaderName -> Wai.Request -> Maybe Text
trustedRequestHeaderToken requestPolicyConfig headerName request =
  if trustForwardedHeaders requestPolicyConfig then requestHeaderToken headerName request else Nothing

trustedForwardedHeaderToken :: RequestPolicyConfig -> Text -> Wai.Request -> Maybe Text
trustedForwardedHeaderToken requestPolicyConfig parameterName request =
  if trustForwardedHeaders requestPolicyConfig then forwardedHeaderToken parameterName request else Nothing

forwardedHeaderToken :: Text -> Wai.Request -> Maybe Text
forwardedHeaderToken parameterName request = requestHeaderText "Forwarded" request >>= forwardedParameterValue parameterName

forwardedParameterValue :: Text -> Text -> Maybe Text
forwardedParameterValue parameterName headerValue =
  case firstCommaSeparatedValue headerValue of
    Nothing -> Nothing
    Just forwardedElement ->
      listToMaybe
        [ cleanForwardedParameterValue parameterValue
        | parameter <- Text.splitOn ";" forwardedElement,
          let (parameterKey, parameterValueWithEquals) = Text.breakOn "=" (Text.strip parameter),
          Text.toLower (Text.strip parameterKey) == Text.toLower parameterName,
          Just parameterValue <- [Text.stripPrefix "=" parameterValueWithEquals],
          not (Text.null (cleanForwardedParameterValue parameterValue))
        ]

cleanForwardedParameterValue :: Text -> Text
cleanForwardedParameterValue = Text.strip . stripSurroundingQuotes . Text.strip

stripSurroundingQuotes :: Text -> Text
stripSurroundingQuotes value = fromMaybe value (Text.stripPrefix "\"" value >>= Text.stripSuffix "\"")

sanitizedReferer :: Wai.Request -> Maybe Text
sanitizedReferer request = sanitizeRefererValue <$> requestHeaderText "Referer" request

sanitizeRefererValue :: Text -> Text
sanitizeRefererValue = limitObservabilityHeaderValue . Text.takeWhile (\character -> character /= '?' && character /= '#')

requestSource :: Wai.Request -> Maybe Text
requestSource request =
  case fmap Text.toLower (requestHeaderText "X-Requested-With" request) of
    Just "tiny-navigation" -> Just "enhanced-navigation"
    Just "xmlhttprequest" -> Just "xml-http-request"
    Just _ -> Just "scripted-request"
    Nothing ->
      case (fmap Text.toLower (requestHeaderText "Accept" request), fmap Text.toLower (requestHeaderText "User-Agent" request)) of
        (Just acceptHeader, _) | "application/json" `Text.isInfixOf` acceptHeader -> Just "api-client"
        (_, Just userAgent) | "curl/" `Text.isPrefixOf` userAgent -> Just "manual-client"
        (_, Just _) -> Just "browser-or-client"
        _ -> Nothing

requestTraceContext :: Wai.Request -> Maybe Observability.RequestTraceContext
requestTraceContext request =
  parseTraceParentHeader =<< requestHeaderText "traceparent" request
  where
    parseTraceParentHeader traceParentHeader =
      case Text.splitOn "-" traceParentHeader of
        [version, traceId, parentSpanId, traceFlags]
          | isValidTraceParentVersion version
              && isValidTraceParentTraceId traceId
              && isValidTraceParentSpanId parentSpanId
              && isValidTraceParentFlags traceFlags ->
              Just
                Observability.RequestTraceContext
                  { Observability.traceContextTraceId = Text.toLower traceId,
                    Observability.traceContextParentSpanId = Text.toLower parentSpanId,
                    Observability.traceContextState = requestHeaderText "tracestate" request
                  }
        _ -> Nothing

isValidTraceParentVersion, isValidTraceParentTraceId, isValidTraceParentSpanId, isValidTraceParentFlags :: Text -> Bool
isValidTraceParentVersion version = Text.length version == 2 && Text.all isAsciiHexText version && Text.toLower version /= "ff"
isValidTraceParentTraceId traceId = Text.length traceId == 32 && Text.all isAsciiHexText traceId && traceId /= "00000000000000000000000000000000"
isValidTraceParentSpanId spanId = Text.length spanId == 16 && Text.all isAsciiHexText spanId && spanId /= "0000000000000000"
isValidTraceParentFlags traceFlags = Text.length traceFlags == 2 && Text.all isAsciiHexText traceFlags

isAsciiHexText :: Char -> Bool
isAsciiHexText character = isHexDigit character && fromEnum character < 128

limitObservabilityHeaderValue :: Text -> Text
limitObservabilityHeaderValue = Text.take 256

requestPathPrefix :: RequestPolicyConfig -> Wai.Request -> Text
requestPathPrefix requestPolicyConfig request =
  maybe Text.empty PathPrefix.normalizePathPrefix (trustedRequestHeaderToken requestPolicyConfig "X-Forwarded-Prefix" request)

rawRequestPath :: Wai.Request -> Text
rawRequestPath request
  | ByteString.null rawPath = "/"
  | otherwise = decodeUtf8OrEmpty rawPath
  where
    rawPath = Wai.rawPathInfo request

decodeUtf8OrEmpty :: ByteString.ByteString -> Text
decodeUtf8OrEmpty bytes =
  case TextEncoding.decodeUtf8' bytes of
    Left _ -> Text.empty
    Right decodedText -> decodedText

waiRequestRouteTarget :: RequestPolicyConfig -> Wai.Request -> Text
waiRequestRouteTarget requestPolicyConfig request = appendRawQueryString (waiRequestPath requestPolicyConfig request) (Wai.rawQueryString request)

appendRawQueryString :: Text -> ByteString.ByteString -> Text
appendRawQueryString path rawQueryString =
  if ByteString.null rawQueryString
    then path
    else either (const path) (path <>) (TextEncoding.decodeUtf8' rawQueryString)

externalRequestPath :: RequestPolicyConfig -> Wai.Request -> Text
externalRequestPath requestPolicyConfig request = applyRequestPathPrefix (requestPathPrefix requestPolicyConfig request) (waiRequestPath requestPolicyConfig request)

applyRequestPathPrefix :: Text -> Text -> Text
applyRequestPathPrefix = PathPrefix.applyPathPrefix

stripRequestPathPrefix :: Text -> Text -> Text
stripRequestPathPrefix = PathPrefix.stripPathPrefix

firstCommaSeparatedValue :: Text -> Maybe Text
firstCommaSeparatedValue value =
  case filter (not . Text.null) (map Text.strip (Text.splitOn "," value)) of
    [] -> Nothing
    firstValue : _ -> Just firstValue

requestHostWithoutPort :: Wai.Request -> Maybe Text
requestHostWithoutPort request = fmap (Text.takeWhile (/= ':')) (requestHeaderToken "Host" request)

textObservabilityAttribute :: Text -> Text -> Observability.ObservabilityAttribute
textObservabilityAttribute name value =
  Observability.ObservabilityAttribute
    { Observability.attributeName = name,
      Observability.attributeValue = Observability.TextAttribute value
    }

renderRequestLogField :: Text -> Text -> Text
renderRequestLogField fieldName fieldValue = fieldName <> "=" <> Text.pack (show fieldValue)

prependRequestLogContext :: [Text] -> Text -> Text
prependRequestLogContext fields logEntry = "[" <> Text.intercalate " " fields <> "] " <> logEntry
