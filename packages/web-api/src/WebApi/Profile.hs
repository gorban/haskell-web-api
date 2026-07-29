{-# LANGUAGE OverloadedStrings #-}

module WebApi.Profile
  ( ProfileLoadError (..),
    ProfileState (..),
    loadProfile,
  )
where

import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Session (OpaqueSession (..), SessionId)
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

loadProfile :: AccountSessionStore -> AccountProfileStore -> Word64 -> Maybe SessionId -> IO (Either ProfileLoadError ProfileState)
loadProfile sessionStore profileStore nowNanoseconds maybeSessionId =
  runExceptT (maybe (pure ProfileUnauthenticated) loadProfileForSessionId maybeSessionId)
  where
    loadProfileForSessionId :: SessionId -> ExceptT ProfileLoadError IO ProfileState
    loadProfileForSessionId sessionIdValue = do
      maybeSession <- withExceptT ProfileSessionStoreError (ExceptT (loadAccountSession sessionStore sessionIdValue))
      maybe (pure ProfileUnauthenticated) loadActiveSession maybeSession

    loadActiveSession :: OpaqueSession AccountId -> ExceptT ProfileLoadError IO ProfileState
    loadActiveSession opaqueSession
      | sessionExpiresAtNanoseconds opaqueSession <= nowNanoseconds = pure ProfileUnauthenticated
      | otherwise = loadProfileForSession opaqueSession

    loadProfileForSession :: OpaqueSession AccountId -> ExceptT ProfileLoadError IO ProfileState
    loadProfileForSession opaqueSession = do
      maybeProfile <- withExceptT ProfileAccountStoreError (ExceptT (findAccountProfile profileStore (sessionPrincipal opaqueSession)))
      maybe (pure ProfileUnauthenticated) (classifyProfile opaqueSession) maybeProfile

    classifyProfile :: OpaqueSession AccountId -> AccountProfile -> ExceptT ProfileLoadError IO ProfileState
    classifyProfile opaqueSession profile
      | accountProfileId profile /= sessionPrincipal opaqueSession =
          throwError (ProfileAccountStoreError (AccountStoreCorruptData "account profile lookup returned a different account id"))
      | accountProfileEmailVerified profile = pure (ProfileAuthenticated profile)
      | otherwise = pure (ProfilePending profile)
