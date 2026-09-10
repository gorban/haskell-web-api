{-# LANGUAGE OverloadedStrings #-}

-- | The closed, browser-observable failures in Harch's enhanced client-action
-- lifecycle.
--
-- Applications never construct these failures from browser exception text.
-- A failure page can safely decode their stable tag and display its opaque
-- 'FailureReference', while detailed causes remain private diagnostics.
--
-- Decision record (AHI-4C, 2026-09-10): this extends the existing
-- 'RequestId' boundary with an opaque display/correlation reference rather
-- than introducing another UUID parser or an application-defined query-string
-- convention.  The closed failure sum is deliberately separate from a future
-- application failure ADT: browser-discovered failures cannot safely carry
-- application values back to the server.  This initial value layer does not
-- yet add a route, action-response transport, or browser replacement path.
module HarchWeb.ClientActionFailure
  ( FailureReference,
    HarchClientFailure (..),
    failureReference,
    failureReferenceRequestId,
    failureReferenceText,
    harchClientFailureCode,
    parseFailureReference,
    parseHarchClientFailure,
  )
where

import Data.Text (Text)
import HarchWeb.RequestId (RequestId, mkRequestId, requestIdText)

-- | Browser failures which the framework can classify without exposing an
-- exception, a storage key, a route, or session-related data.  The rendered
-- codes are a stable, low-cardinality wire vocabulary.
data HarchClientFailure
  = StorageCleanupFailed
  | ActionResponseProtocolFailed
  | ResponseApplicationFailed
  deriving (Eq, Show)

-- | A validated reference to the original action's framework-minted request
-- identifier.  It is display/correlation-only: possession never authorizes a
-- session, audit lookup, or error lookup.
newtype FailureReference = FailureReference RequestId
  deriving (Eq, Show)

-- | Convert the trusted original action request ID into a display-only
-- reference for a later public failure route.
failureReference :: RequestId -> FailureReference
failureReference = FailureReference

-- | Recover the original trusted action request ID for server-side response
-- construction.  Route decoders use 'parseFailureReference' instead.
failureReferenceRequestId :: FailureReference -> RequestId
failureReferenceRequestId (FailureReference requestId) = requestId

-- | Canonical text for a route parameter.  The only accepted input form is
-- the UUIDv4 representation already owned by 'HarchWeb.RequestId'.
failureReferenceText :: FailureReference -> Text
failureReferenceText = requestIdText . failureReferenceRequestId

-- | Stable, low-cardinality tag for a browser-discovered framework failure.
harchClientFailureCode :: HarchClientFailure -> Text
harchClientFailureCode clientFailure =
  case clientFailure of
    StorageCleanupFailed -> "storage-cleanup-failed"
    ActionResponseProtocolFailed -> "action-response-protocol-failed"
    ResponseApplicationFailed -> "response-application-failed"

-- | Decode only the framework's closed browser-failure vocabulary. Unknown
-- values remain untrusted route input and deliberately receive no fallback.
parseHarchClientFailure :: Text -> Maybe HarchClientFailure
parseHarchClientFailure failureCode =
  case failureCode of
    "storage-cleanup-failed" -> Just StorageCleanupFailed
    "action-response-protocol-failed" -> Just ActionResponseProtocolFailed
    "response-application-failed" -> Just ResponseApplicationFailed
    _ -> Nothing

-- | Validate an original-action reference received through a public failure
-- route. The private constructor prevents arbitrary application text becoming
-- a reference without the existing exact UUIDv4 validation.
parseFailureReference :: Text -> Maybe FailureReference
parseFailureReference = fmap FailureReference . mkRequestId
