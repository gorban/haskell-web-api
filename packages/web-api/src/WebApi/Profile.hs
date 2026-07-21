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
  runExceptT $
    case maybeSessionId of
      Nothing -> pure ProfileUnauthenticated
      Just sessionIdValue -> do
        maybeSession <- withExceptT ProfileSessionStoreError (ExceptT (loadAccountSession sessionStore sessionIdValue))
        case maybeSession of
          Nothing -> pure ProfileUnauthenticated
          Just opaqueSession ->
            if sessionExpiresAtNanoseconds opaqueSession <= nowNanoseconds
              then pure ProfileUnauthenticated
              else loadProfileForSession opaqueSession
  where
    loadProfileForSession :: OpaqueSession AccountId -> ExceptT ProfileLoadError IO ProfileState
    loadProfileForSession opaqueSession = do
      maybeProfile <- withExceptT ProfileAccountStoreError (ExceptT (findAccountProfile profileStore (sessionPrincipal opaqueSession)))
      case maybeProfile of
        Nothing -> pure ProfileUnauthenticated
        Just profile ->
          if accountProfileId profile /= sessionPrincipal opaqueSession
            then throwError (ProfileAccountStoreError (AccountStoreCorruptData "account profile lookup returned a different account id"))
            else
              if accountProfileEmailVerified profile
                then pure (ProfileAuthenticated profile)
                else pure (ProfilePending profile)
