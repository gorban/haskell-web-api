{-# LANGUAGE OverloadedStrings #-}

-- | The composed application's durable synchronizer-token CSRF adapter.
--
-- Harch still parses the one host-only cookie and submitted field in constant
-- work before invoking this adapter.  This module therefore owns only the
-- application-selected durable token record: it hashes a generated opaque
-- token before storage, resolves the current grant binding on every request,
-- and treats storage failure or capacity exhaustion as unavailable rather
-- than silently issuing an anonymous authority.  A PostgreSQL implementation
-- supplies 'SynchronizerTokenStore'; it is deliberately not a harch-web
-- dependency.
module App.Composed.CsrfSynchronizer
  ( SynchronizerTokenDigest,
    SynchronizerTokenStore (..),
    SynchronizerTokenStoreError (..),
    synchronizerTokenDigestText,
    synchronizerCsrfProtection,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Csrf
  ( CsrfBinding,
    CsrfBindingDigest,
    CsrfBindingResolution (..),
    CsrfCookieMaxAgeSeconds,
    CsrfIssuance (..),
    CsrfProtection (..),
    CsrfToken,
    CsrfVerification (..),
    csrfBindingDigest,
    csrfBindingFromCanonicalBytes,
    csrfTokenText,
    defaultCsrfCookieMaxAgeSeconds,
    generateCsrfToken,
    mkCsrfCookieMaxAgeSeconds,
  )
import HarchWeb.Time
  ( UnixTimeNanoseconds,
    addUnixTimeNanoseconds,
    unixTimeNanosecondsValue,
  )

-- | A SHA-256 token digest.  The raw token reaches only the page renderer and
-- the framework transport verifier; a durable adapter cannot accidentally
-- accept a record keyed by the bearer text.
newtype SynchronizerTokenDigest = SynchronizerTokenDigest (Digest SHA256)
  deriving (Eq)

instance Show SynchronizerTokenDigest where
  show _ = "SynchronizerTokenDigest <redacted>"

data SynchronizerTokenStoreError
  = SynchronizerTokenStoreUnavailable
  | SynchronizerTokenStoreCorrupt
  deriving (Eq, Show)

-- | Application-owned durable token capability.  The store may enforce its
-- per-binding capacity in 'saveSynchronizerToken'; @False@ means no bounded
-- slot is available, never that a token was saved.  Verification is always a
-- fresh durable read, so an explicit revocation takes effect immediately.
data SynchronizerTokenStore = SynchronizerTokenStore
  { saveSynchronizerToken :: SynchronizerTokenDigest -> CsrfBindingDigest -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> IO (Either SynchronizerTokenStoreError Bool),
    verifySynchronizerToken :: SynchronizerTokenDigest -> CsrfBindingDigest -> UnixTimeNanoseconds -> IO (Either SynchronizerTokenStoreError Bool),
    cleanupSynchronizerTokens :: UnixTimeNanoseconds -> IO (Either SynchronizerTokenStoreError ())
  }

-- | Build the composed override from a durable store and application-owned
-- current-grant resolver.  Anonymous records use the framework's one-hour
-- maximum; authenticated records are bounded by their earliest absolute
-- grant deadline.  The resolver is invoked on both issuance and verification
-- so changing either admission or account session binding rejects old tokens.
synchronizerCsrfProtection ::
  SynchronizerTokenStore ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  CsrfProtection context
synchronizerCsrfProtection store readClock resolveBinding =
  CsrfProtection
    { issueCsrfToken = issueSynchronizerToken store readClock resolveBinding,
      verifyCsrfToken = verifyDurableSynchronizerToken store readClock resolveBinding
    }

issueSynchronizerToken ::
  SynchronizerTokenStore ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  context ->
  IO CsrfIssuance
issueSynchronizerToken store readClock resolveBinding context = do
  bindingResolution <- resolveBinding context
  now <- readClock
  case bindingExpiry now bindingResolution of
    Nothing -> pure CsrfProtectionUnavailable
    Just (binding, expiresAt, cookieMaxAge) -> do
      cleanupResult <- cleanupSynchronizerTokens store now
      case cleanupResult of
        Left _ -> pure CsrfProtectionUnavailable
        Right () -> do
          token <- generateCsrfToken
          saved <- saveSynchronizerToken store (synchronizerTokenDigest token) (csrfBindingDigest binding) now expiresAt
          pure $
            case saved of
              Right True -> CsrfTokenIssued token cookieMaxAge
              Left _ -> CsrfProtectionUnavailable
              Right False -> CsrfProtectionUnavailable

verifyDurableSynchronizerToken ::
  SynchronizerTokenStore ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  context ->
  CsrfToken ->
  IO CsrfVerification
verifyDurableSynchronizerToken store readClock resolveBinding context token = do
  bindingResolution <- resolveBinding context
  now <- readClock
  case bindingResolution of
    CsrfBindingUnavailable -> pure CsrfVerificationUnavailable
    BoundCsrfBinding _ expiresAt
      | expiresAt <= now -> pure CsrfRejected
    _ ->
      case bindingExpiry now bindingResolution of
        Nothing -> pure CsrfVerificationUnavailable
        Just (binding, _, _) -> do
          verified <- verifySynchronizerToken store (synchronizerTokenDigest token) (csrfBindingDigest binding) now
          pure $
            case verified of
              Right True -> CsrfVerified
              Right False -> CsrfRejected
              Left _ -> CsrfVerificationUnavailable

bindingExpiry :: UnixTimeNanoseconds -> CsrfBindingResolution -> Maybe (CsrfBinding, UnixTimeNanoseconds, CsrfCookieMaxAgeSeconds)
bindingExpiry now resolution =
  case resolution of
    CsrfBindingUnavailable -> Nothing
    AnonymousCsrfBinding -> do
      expiresAt <- addUnixTimeNanoseconds now anonymousLifetimeNanoseconds
      pure (anonymousBinding, expiresAt, defaultCsrfCookieMaxAgeSeconds)
    BoundCsrfBinding binding expiresAt -> do
      cookieMaxAge <- cookieLifetime now expiresAt
      pure (binding, expiresAt, cookieMaxAge)
  where
    -- The anonymous marker is domain-separated and immediately digested by
    -- Harch; it carries neither a principal nor session data.
    anonymousBinding = csrfBindingFromCanonicalBytes "composed-domains.csrf.anonymous.v1"

cookieLifetime :: UnixTimeNanoseconds -> UnixTimeNanoseconds -> Maybe CsrfCookieMaxAgeSeconds
cookieLifetime now expiresAt = do
  let remainingNanoseconds = unixTimeNanosecondsValue expiresAt - unixTimeNanosecondsValue now
      remainingSeconds = remainingNanoseconds `div` nanosecondsPerSecond
  if unixTimeNanosecondsValue expiresAt <= unixTimeNanosecondsValue now
    then Nothing
    else mkCsrfCookieMaxAgeSeconds (max 1 remainingSeconds)

synchronizerTokenDigest :: CsrfToken -> SynchronizerTokenDigest
synchronizerTokenDigest token =
  SynchronizerTokenDigest
    (hash (TextEncoding.encodeUtf8 (csrfTokenText token)) :: Digest SHA256)

-- | Stable, non-secret rendering for a database key. It is the SHA-256
-- digest of a random CSRF token, never the token itself.
synchronizerTokenDigestText :: SynchronizerTokenDigest -> Text.Text
synchronizerTokenDigestText (SynchronizerTokenDigest digest) = Text.pack (show digest)

anonymousLifetimeNanoseconds, nanosecondsPerSecond :: Word64
anonymousLifetimeNanoseconds = 60 * 60 * nanosecondsPerSecond
nanosecondsPerSecond = 1000000000
