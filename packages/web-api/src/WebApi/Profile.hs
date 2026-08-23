{-# LANGUAGE OverloadedStrings #-}

module WebApi.Profile
  ( ProfileLoadError (..),
    ProfileState (..),
    loadProfile,
  )
where

import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Core.Control.Error (liftEitherWith)
import HarchWeb.Account (AccountId)
import HarchWeb.Session (OpaqueSession (..), SessionId)
import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStoreError (..),
  )
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )

-- | The authenticated state available to a profile page. A pending account has
-- a valid session but still needs email verification.
data ProfileState
  = ProfileUnauthenticated
  | ProfilePending AccountProfile
  | ProfileAuthenticated AccountProfile

-- | Operational failures stay distinct from an absent, expired, or revoked
-- session, all of which are ordinary unauthenticated outcomes.
data ProfileLoadError
  = ProfileSessionStoreError AccountSessionStoreError
  | ProfileAccountStoreError AccountStoreError

loadProfile :: AccountSessionStore -> AccountProfileStore -> UnixTimeNanoseconds -> Maybe SessionId -> IO (Either ProfileLoadError ProfileState)
loadProfile sessionStore profileStore nowNanoseconds maybeSessionId =
  runExceptT (maybe (pure ProfileUnauthenticated) loadProfileForSessionId maybeSessionId)
  where
    loadProfileForSessionId :: SessionId -> ExceptT ProfileLoadError IO ProfileState
    loadProfileForSessionId sessionIdValue = do
      maybeSession <- liftEitherWith ProfileSessionStoreError (loadAccountSession sessionStore sessionIdValue)
      maybe (pure ProfileUnauthenticated) loadActiveSession maybeSession

    loadActiveSession :: OpaqueSession AccountId -> ExceptT ProfileLoadError IO ProfileState
    loadActiveSession opaqueSession =
      if sessionExpiresAtNanoseconds opaqueSession <= nowNanoseconds
        then pure ProfileUnauthenticated
        else loadProfileForSession opaqueSession

    loadProfileForSession :: OpaqueSession AccountId -> ExceptT ProfileLoadError IO ProfileState
    loadProfileForSession opaqueSession = do
      maybeProfile <- liftEitherWith ProfileAccountStoreError (findAccountProfile profileStore (sessionPrincipal opaqueSession))
      maybe (pure ProfileUnauthenticated) (classifyProfile opaqueSession) maybeProfile

    classifyProfile :: OpaqueSession AccountId -> AccountProfile -> ExceptT ProfileLoadError IO ProfileState
    classifyProfile opaqueSession profile =
      if accountProfileId profile /= sessionPrincipal opaqueSession
        then throwError (ProfileAccountStoreError (AccountStoreCorruptData "account profile lookup returned a different account id"))
        else
          pure $
            if accountProfileEmailVerified profile
              then ProfileAuthenticated profile
              else ProfilePending profile
