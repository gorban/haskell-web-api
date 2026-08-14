module WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
    issueAccountSession,
  )
where

import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (fromMaybeError, guardError, liftEitherWith)
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionId,
    defaultSessionCookiePolicy,
    generateCsrfToken,
    generateSessionId,
    sessionCookieMaxAgeSeconds,
  )

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
    invalidateAccountSession :: SessionId -> Word64 -> IO (Either AccountSessionStoreError Bool)
  }

-- | Creates and persists a new opaque session. Its bearer and synchronizer
-- tokens are generated only after the caller has completed authentication.
issueAccountSession :: AccountSessionStore -> AccountId -> Word64 -> IO (Either AccountSessionStoreError (OpaqueSession AccountId))
issueAccountSession sessionStore accountId issuedAtNanoseconds =
  runExceptT $ do
    expiresAtNanoseconds <- fromMaybeError AccountSessionStoreCorruptData (boundedExpiration issuedAtNanoseconds)
    opaqueSession <- liftIO (generateOpaqueSession accountId issuedAtNanoseconds expiresAtNanoseconds)
    saved <- liftSessionStore (saveAccountSession sessionStore opaqueSession)
    guardError AccountSessionStoreCorruptData saved
    pure opaqueSession

liftSessionStore :: IO (Either AccountSessionStoreError value) -> ExceptT AccountSessionStoreError IO value
liftSessionStore = liftEitherWith id

generateOpaqueSession :: AccountId -> Word64 -> Word64 -> IO (OpaqueSession AccountId)
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

boundedExpiration :: Word64 -> Maybe Word64
boundedExpiration issuedAtNanoseconds =
  let sessionLifetimeNanoseconds = sessionCookieMaxAgeSeconds defaultSessionCookiePolicy * 1000000000
      expiresAtNanoseconds = issuedAtNanoseconds + sessionLifetimeNanoseconds
   in if expiresAtNanoseconds < issuedAtNanoseconds
        then Nothing
        else Just expiresAtNanoseconds
