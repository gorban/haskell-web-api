{-# LANGUAGE OverloadedStrings #-}

-- | Framework-owned, opaque correlation identifiers for HTTP requests.
--
-- Decision record (AHI-5-RID, 2026-09-05): request correlation extends the
-- existing request-context ingress and response-finalization boundaries rather
-- than adding an application-local header convention. This module owns the
-- cryptographically random UUIDv4 representation, bounded validation, and an
-- adapter-neutral trusted-ingress contract. Applications authenticate a
-- service and grant its separate propagation capability, while this module
-- alone parses the standard header. It does not introduce account auditing,
-- an idempotency key, a trace hierarchy, mTLS, or a service-mesh implementation.
module HarchWeb.RequestId
  ( RequestId,
    RequestIdIngress,
    RequestIdIngressResult (..),
    RequestIdPropagationCapability,
    authenticatedServiceRequestIdIngress,
    freshRequestIdIngress,
    mkRequestId,
    newRequestId,
    requestIdHeader,
    requestIdPropagationCapability,
    requestIdText,
    resolveRequestIdIngress,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.Bits ((.&.), (.|.))
import Data.ByteString qualified as ByteString
import Data.ByteString.Base16 qualified as Base16
import Data.Char (isAscii, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word8)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | An application-selected ingress adapter. It may authorize the request as
-- a service identity, but it cannot supply an arbitrary request ID: this
-- module alone reads and validates the standard header after authorization.
newtype RequestIdIngress = RequestIdIngress
  { requestIdIngressTrustedService :: Wai.Request -> IO Bool
  }

-- | An explicit authorization marker for a service identity which may carry
-- correlation across a synchronous boundary. Keep it in server-side
-- authentication configuration; it is not accepted from an HTTP header.
data RequestIdPropagationCapability = RequestIdPropagationCapability

-- | The result of resolving request correlation at HTTP ingress. The rejected
-- case deliberately contains no input value, so diagnostics cannot retain
-- untrusted text.
data RequestIdIngressResult
  = FreshRequestId
  | InheritedRequestId
  | RejectedInheritedRequestId
  deriving (Eq, Show)

-- | The default public-web policy: every request begins a new correlation.
-- Public @X-Request-ID@ headers remain ordinary untrusted input.
freshRequestIdIngress :: RequestIdIngress
freshRequestIdIngress = RequestIdIngress (const (pure False))

-- | Build a pluggable adapter from an application's service authenticator and
-- separately granted propagation capability. Authentication alone does not
-- enable inheritance: the second function must return the capability for that
-- authenticated identity. The framework exposes no wire-level trust flag.
authenticatedServiceRequestIdIngress ::
  (Wai.Request -> IO (Maybe identity)) ->
  (identity -> Maybe RequestIdPropagationCapability) ->
  RequestIdIngress
authenticatedServiceRequestIdIngress authenticateService propagationCapability =
  RequestIdIngress $ \request -> do
    authenticatedIdentity <- authenticateService request
    pure $
      case authenticatedIdentity >>= propagationCapability of
        Nothing -> False
        Just RequestIdPropagationCapability -> True

-- | Assign this marker only in an application's authenticated-service policy,
-- never from an untrusted request value. It is deliberately separate from
-- ordinary service authentication.
requestIdPropagationCapability :: RequestIdPropagationCapability
requestIdPropagationCapability = RequestIdPropagationCapability

-- | A canonical lower-case UUIDv4.  Its constructor remains private so a
-- caller cannot use request correlation as an arbitrary text/header carrier.
newtype RequestId = RequestId Text
  deriving (Eq)

instance Show RequestId where
  showsPrec precedence requestId =
    showParen (precedence > 10) $
      showString "RequestId " . shows (requestIdText requestId)

requestIdText :: RequestId -> Text
requestIdText (RequestId value) = value

-- | The sole outbound representation for a trusted synchronous call. Harch
-- has no HTTP-client abstraction, so an application adds this pair after it
-- separately propagates its trace/span context.
requestIdHeader :: RequestId -> Http.Header
requestIdHeader requestId = ("X-Request-ID", TextEncoding.encodeUtf8 (requestIdText requestId))

-- | Accept only the 36-byte canonical UUIDv4 text form.  In particular,
-- upper-case, non-ASCII, non-v4 and non-RFC-4122-variant values cannot become
-- correlation identifiers.
mkRequestId :: Text -> Maybe RequestId
mkRequestId value
  | Text.length value /= 36 = Nothing
  | Text.index value 14 /= '4' = Nothing
  | Text.index value 19 `notElem` ['8', '9', 'a', 'b'] = Nothing
  | all isCanonicalCharacter (zip [0 :: Int ..] (Text.unpack value)) = Just (RequestId value)
  | otherwise = Nothing
  where
    isCanonicalCharacter (index, character)
      | index `elem` [8, 13, 18, 23] = character == '-'
      | otherwise = isAsciiDigit character || character `elem` ['a' .. 'f']

    isAsciiDigit character = isAscii character && isDigit character

-- | Generate an unpredictable canonical UUIDv4 from the operating system's
-- CSPRNG.  The version and RFC 4122 variant bits are set after entropy is
-- obtained, before its canonical text representation is exposed.
newRequestId :: IO RequestId
newRequestId = requestIdFromEntropy <$> getEntropy 16

-- | Resolve ingress correlation with a fresh value already available as the
-- safe fallback. Only a separately authorized service request can reuse an
-- inherited value. Multiple, oversized, malformed, non-UTF-8, and non-v4
-- inputs all take the same bounded rejection path and never escape as
-- diagnostic data.
resolveRequestIdIngress :: RequestIdIngress -> Wai.Request -> IO (RequestId, RequestIdIngressResult)
resolveRequestIdIngress requestIdIngress request = do
  freshRequestId <- newRequestId
  trustedService <- requestIdIngressTrustedService requestIdIngress request
  if not trustedService
    then pure (freshRequestId, FreshRequestId)
    else case inheritedRequestId request of
      Just inheritedRequestIdValue -> pure (inheritedRequestIdValue, InheritedRequestId)
      Nothing -> pure (freshRequestId, RejectedInheritedRequestId)

inheritedRequestId :: Wai.Request -> Maybe RequestId
inheritedRequestId request = do
  headerValue <- exactlyOneRequestIdHeader (Wai.requestHeaders request)
  if ByteString.length headerValue /= 36
    then Nothing
    else do
      headerText <- either (const Nothing) Just (TextEncoding.decodeUtf8' headerValue)
      mkRequestId headerText

exactlyOneRequestIdHeader :: Http.RequestHeaders -> Maybe ByteString.ByteString
exactlyOneRequestIdHeader requestHeaders =
  case filter ((== "X-Request-ID") . fst) requestHeaders of
    [headerValue] -> Just (snd headerValue)
    _ -> Nothing

requestIdFromEntropy :: ByteString.ByteString -> RequestId
requestIdFromEntropy entropy =
  RequestId (renderUuid (setUuidVersionAndVariant entropy))

setUuidVersionAndVariant :: ByteString.ByteString -> ByteString.ByteString
setUuidVersionAndVariant entropy =
  ByteString.pack (zipWith applyUuidBits [0 :: Int ..] (ByteString.unpack entropy))
  where
    applyUuidBits :: Int -> Word8 -> Word8
    applyUuidBits index octet =
      case index of
        6 -> (octet .&. 0x0f) .|. 0x40
        8 -> (octet .&. 0x3f) .|. 0x80
        _ -> octet

renderUuid :: ByteString.ByteString -> Text
renderUuid bytes =
  let hex = TextEncoding.decodeLatin1 (Base16.encode bytes)
   in Text.intercalate "-" [Text.take 8 hex, Text.take 4 (Text.drop 8 hex), Text.take 4 (Text.drop 12 hex), Text.take 4 (Text.drop 16 hex), Text.drop 20 hex]
