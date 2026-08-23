{-# LANGUAGE OverloadedStrings #-}

module WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
    MfaEnrollmentSessionStore (..),
    MfaEnrollmentSessionStoreError (..),
    issueAccountSession,
    issueMfaEnrollmentSession,
    mfaEnrollmentSessionCookiePolicy,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith)
import Data.Maybe (fromJust)
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionCookiePolicy (..),
    SessionId,
    defaultSessionCookiePolicy,
    generateCsrfToken,
    generateSessionId,
    mkSessionCookieName,
    sessionCookieMaxAgeSeconds,
    sessionCookieName,
  )
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds)

data AccountSessionStoreError
  = AccountSessionStoreUnavailable
  | AccountSessionStoreCorruptData
  deriving (Eq)

-- | Application-owned persistence for opaque account sessions. The framework
-- owns the opaque token and cookie semantics; this adapter owns the principal
-- mapping and revocation state.
data AccountSessionStore = AccountSessionStore
  { saveAccountSession :: OpaqueSession AccountId -> IO (Either AccountSessionStoreError Bool),
    loadAccountSession :: SessionId -> IO (Either AccountSessionStoreError (Maybe (OpaqueSession AccountId))),
    invalidateAccountSession :: SessionId -> UnixTimeNanoseconds -> IO (Either AccountSessionStoreError Bool)
  }

-- | Creates and persists a new opaque session. Its bearer and synchronizer
-- tokens are generated only after the caller has completed authentication.
issueAccountSession :: AccountSessionStore -> AccountId -> UnixTimeNanoseconds -> IO (Either AccountSessionStoreError (OpaqueSession AccountId))
issueAccountSession sessionStore accountId issuedAtNanoseconds =
  runExceptT $ do
    expiresAtNanoseconds <- fromMaybeError AccountSessionStoreCorruptData (boundedExpiration (sessionCookieMaxAgeSeconds defaultSessionCookiePolicy) issuedAtNanoseconds)
    opaqueSession <- liftIO (generateOpaqueSession accountId issuedAtNanoseconds expiresAtNanoseconds)
    saved <- liftSessionStore (saveAccountSession sessionStore opaqueSession)
    guardError AccountSessionStoreCorruptData saved
    pure opaqueSession

data MfaEnrollmentSessionStoreError
  = MfaEnrollmentSessionStoreUnavailable
  | MfaEnrollmentSessionStoreCorruptData
  deriving (Eq)

-- | A single-purpose session distinct from 'AccountSessionStore': it proves
-- only "this caller just verified an email or a password for this account,"
-- never general sign-in. Kept as its own store, table, and cookie
-- ('mfaEnrollmentSessionCookiePolicy') so a leaked or reused enrollment
-- token cannot be read back as an ordinary login session by
-- 'WebApi.Profile.loadProfile' or anything else keyed on
-- 'AccountSessionStore' — see the AM decision record in
-- @WebApi.AccountPages.Actions.Workflows@ for why reusing the login session
-- mechanism here would have silently weakened what "signed in" means.
data MfaEnrollmentSessionStore = MfaEnrollmentSessionStore
  { saveMfaEnrollmentSession :: OpaqueSession AccountId -> IO (Either MfaEnrollmentSessionStoreError Bool),
    loadMfaEnrollmentSession :: SessionId -> IO (Either MfaEnrollmentSessionStoreError (Maybe (OpaqueSession AccountId))),
    invalidateMfaEnrollmentSession :: SessionId -> UnixTimeNanoseconds -> IO (Either MfaEnrollmentSessionStoreError Bool)
  }

-- | A short-lived, single-purpose cookie policy distinct from
-- 'defaultSessionCookiePolicy': a 10-minute window is enough to finish TOTP
-- enrollment right after proving account ownership, without leaving a
-- long-lived bearer token that grants no other capability than enrollment.
mfaEnrollmentSessionCookiePolicy :: SessionCookiePolicy
mfaEnrollmentSessionCookiePolicy =
  SessionCookiePolicy
    { sessionCookieName = fromJust (mkSessionCookieName "__Host-harch-mfa-enrollment"),
      sessionCookieMaxAgeSeconds = 600
    }

-- | Creates and persists a new opaque MFA-enrollment session. Callers must
-- have already verified the account's email or password before calling this;
-- issuing this session is itself the authorization grant for enrollment.
issueMfaEnrollmentSession :: MfaEnrollmentSessionStore -> AccountId -> UnixTimeNanoseconds -> IO (Either MfaEnrollmentSessionStoreError (OpaqueSession AccountId))
issueMfaEnrollmentSession sessionStore accountId issuedAtNanoseconds =
  runExceptT $ do
    expiresAtNanoseconds <- fromMaybeError MfaEnrollmentSessionStoreCorruptData (boundedExpiration (sessionCookieMaxAgeSeconds mfaEnrollmentSessionCookiePolicy) issuedAtNanoseconds)
    opaqueSession <- liftIO (generateOpaqueSession accountId issuedAtNanoseconds expiresAtNanoseconds)
    saved <- liftEitherWith id (saveMfaEnrollmentSession sessionStore opaqueSession)
    guardError MfaEnrollmentSessionStoreCorruptData saved
    pure opaqueSession

liftSessionStore :: IO (Either AccountSessionStoreError value) -> ExceptT AccountSessionStoreError IO value
liftSessionStore = liftEitherWith id

generateOpaqueSession :: AccountId -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> IO (OpaqueSession AccountId)
generateOpaqueSession accountId issuedAtNanoseconds expiresAtNanoseconds = do
  newSessionId <- generateSessionId
  newCsrfToken <- generateCsrfToken
  pure
    OpaqueSession
      { sessionId = newSessionId,
        sessionPrincipal = accountId,
        sessionCsrfToken = newCsrfToken,
        sessionIssuedAtNanoseconds = issuedAtNanoseconds,
        sessionExpiresAtNanoseconds = expiresAtNanoseconds
      }

boundedExpiration :: Word64 -> UnixTimeNanoseconds -> Maybe UnixTimeNanoseconds
boundedExpiration sessionLifetimeSeconds issuedAtNanoseconds =
  let sessionLifetimeNanoseconds = sessionLifetimeSeconds * 1000000000
   in addUnixTimeNanoseconds issuedAtNanoseconds sessionLifetimeNanoseconds
