{-# LANGUAGE OverloadedStrings #-}

-- | Opt-in request-resource limits checked before route parsing, middleware,
-- and request observability. This module owns only the request-budget
-- boundary; response security headers, request-context extraction, and
-- path/redirect handling stay in "HarchWeb.Security", which re-exports this
-- module's public interface as part of its own. Split out 2026-08-13 to
-- close the AL module-health export-count signal on "HarchWeb.Security" —
-- this cluster was already fully self-contained (only 'Data.ByteString'/
-- 'Network.Wai' dependencies, never touching 'HarchWeb.Security.RequestPolicyConfig'
-- or any other cluster there), so the split needed no re-export shim beyond
-- the facade module already re-exporting every sibling module it owns.
module HarchWeb.Security.RequestLimits
  ( RequestByteLimit,
    RequestConcurrencyLimit,
    RequestHeadLimitFailure (..),
    RequestHeadLimits (..),
    RequestHeaderCountLimit,
    RequestItemCountLimit,
    RequestTimeoutSeconds,
    RequestTransportLimits (..),
    mkRequestConcurrencyLimit,
    mkRequestHeaderCountLimit,
    requestByteLimit,
    requestByteLimitValue,
    requestConcurrencyLimitValue,
    requestItemCountLimit,
    requestTimeoutSeconds,
    requestTimeoutSecondsValue,
    unboundedRequestHeadLimits,
    validateRequestHead,
    warpDefaultRequestTransportLimits,
  )
where

import Data.ByteString qualified as ByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Maybe (isNothing)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import HarchWeb.Cookie (isCookieTokenByte)
import Network.Wai qualified as Wai

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

-- | A non-negative network-progress timeout in seconds. A value of zero has
-- Warp's documented meaning of disabling its timer; absence leaves Warp's
-- established default unchanged.
newtype RequestTimeoutSeconds = RequestTimeoutSeconds Int
  deriving (Eq, Show)

requestTimeoutSeconds :: Int -> Maybe RequestTimeoutSeconds
requestTimeoutSeconds seconds
  | seconds >= 0 = Just (RequestTimeoutSeconds seconds)
  | otherwise = Nothing

requestTimeoutSecondsValue :: RequestTimeoutSeconds -> Int
requestTimeoutSecondsValue (RequestTimeoutSeconds seconds) = seconds

-- | A positive bound on the number of requests the runtime admits at once
-- across every listener. Construct it with 'mkRequestConcurrencyLimit' so a
-- non-positive configuration — which would either admit nothing or carry no
-- meaning — cannot enter the request boundary. Absence preserves the
-- framework's established unbounded behaviour.
newtype RequestConcurrencyLimit = RequestConcurrencyLimit Int
  deriving (Show)

instance Eq RequestConcurrencyLimit where
  RequestConcurrencyLimit left == RequestConcurrencyLimit right = left == right

mkRequestConcurrencyLimit :: Int -> Maybe RequestConcurrencyLimit
mkRequestConcurrencyLimit admittedCount
  | admittedCount > 0 = Just (RequestConcurrencyLimit admittedCount)
  | otherwise = Nothing

requestConcurrencyLimitValue :: RequestConcurrencyLimit -> Int
requestConcurrencyLimitValue (RequestConcurrencyLimit admittedCount) = admittedCount

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
    -- | Optional bounds for syntactically valid request cookie pairs. These
    -- are deliberately separate from 'requestHeaderValueByteLimit': one raw
    -- @Cookie@ header can contain many independently application-visible
    -- pairs. The validator scans the raw bytes before API/action cookie
    -- decoding, counts only the valid pairs that decoder would retain, and
    -- does not build a list while doing so. See the EM decision record in
    -- @docs/design-guidance.md@.
    requestCookieCountLimit :: Maybe RequestItemCountLimit,
    requestCookieNameByteLimit :: Maybe RequestByteLimit,
    requestCookieValueByteLimit :: Maybe RequestByteLimit,
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
      requestCookieCountLimit = Nothing,
      requestCookieNameByteLimit = Nothing,
      requestCookieValueByteLimit = Nothing,
      requestPathSegmentCountLimit = Nothing,
      requestPathSegmentByteLimit = Nothing,
      requestQueryFieldCountLimit = Nothing,
      requestQueryFieldByteLimit = Nothing
    }

-- | Opt-in limits applied by the listener while network data arrives. 'Nothing'
-- deliberately preserves the installed Warp defaults, instead of introducing a
-- framework-wide production policy during an upgrade.
data RequestTransportLimits = RequestTransportLimits
  { requestNetworkTimeout :: Maybe RequestTimeoutSeconds,
    requestSlowlorisByteLimit :: Maybe RequestByteLimit
  }
  deriving (Eq, Show)

warpDefaultRequestTransportLimits :: RequestTransportLimits
warpDefaultRequestTransportLimits =
  RequestTransportLimits
    { requestNetworkTimeout = Nothing,
      requestSlowlorisByteLimit = Nothing
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
  | TooManyRequestCookies
  | RequestCookieNameTooLarge
  | RequestCookieValueTooLarge
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
  | Just cookieFailure <- validateRequestCookies limits requestHeaders = Left cookieFailure
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

-- | Validate only the bounded, syntactically valid cookie pairs that
-- 'HarchWeb.Api.Request.apiRequestDataFromWaiRequest' exposes to a caller.
-- Invalid fragments keep that decoder's established "ignore" behavior. No
-- cookie policy means no cookie scan, preserving the pre-EM compatibility
-- path entirely.
validateRequestCookies :: RequestHeadLimits -> [(CaseInsensitive.CI ByteString.ByteString, ByteString.ByteString)] -> Maybe RequestHeadLimitFailure
validateRequestCookies limits requestHeaders
  | noCookieLimits = Nothing
  | otherwise = goHeaders 0 requestHeaders
  where
    noCookieLimits =
      isNothing (requestCookieCountLimit limits)
        && isNothing (requestCookieNameByteLimit limits)
        && isNothing (requestCookieValueByteLimit limits)

    goHeaders _ [] = Nothing
    goHeaders cookieCount ((headerName, headerValue) : remainingHeaders)
      | CaseInsensitive.foldedCase headerName /= "cookie" = goHeaders cookieCount remainingHeaders
      | otherwise =
          case goCookieSegments cookieCount headerValue of
            Left cookieFailure -> Just cookieFailure
            Right nextCookieCount -> goHeaders nextCookieCount remainingHeaders

    goCookieSegments cookieCount cookieBytes =
      let (cookieSegment, remainingBytes) = ByteString.break (== 59) cookieBytes
       in case validateCookieSegment cookieCount cookieSegment of
            Left cookieFailure -> Left cookieFailure
            Right nextCookieCount ->
              case ByteString.uncons remainingBytes of
                Nothing -> Right nextCookieCount
                Just (_, nextCookieBytes) -> goCookieSegments nextCookieCount nextCookieBytes

    validateCookieSegment cookieCount rawCookie =
      case ByteString.break (== 61) (ByteString.dropWhile isCookieWhitespace rawCookie) of
        (cookieName, valueWithSeparator)
          | ByteString.null cookieName || ByteString.null valueWithSeparator || not (ByteString.all isCookieTokenByte cookieName) -> Right cookieCount
          | exceedsByteLimit (requestCookieNameByteLimit limits) (ByteString.length cookieName) -> Left RequestCookieNameTooLarge
          | exceedsByteLimit (requestCookieValueByteLimit limits) (ByteString.length (ByteString.drop 1 valueWithSeparator)) -> Left RequestCookieValueTooLarge
          | exceedsItemCountLimit (requestCookieCountLimit limits) (cookieCount + 1) -> Left TooManyRequestCookies
          | otherwise -> Right (cookieCount + 1)

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

isCookieWhitespace :: Word8 -> Bool
isCookieWhitespace byte = byte == 32 || byte == 9
