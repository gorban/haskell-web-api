{-# LANGUAGE OverloadedStrings #-}

module WebApi.Profile
  ( ProfileLoadError (..),
    ProfileState (..),
    loadProfileForPrincipal,
  )
where

import Control.Monad.Except (runExceptT, throwError)
import Core.Control.Error (liftEitherWith)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStoreError (..),
  )
import WebApi.AccountPrincipal (AccountPrincipal, accountPrincipalAccountId)

-- | The authenticated state available to a profile page. A pending account has
-- a valid session but still needs email verification.
data ProfileState
  = ProfileUnauthenticated
  | ProfilePending AccountProfile
  | ProfileAuthenticated AccountProfile

-- | An unavailable account-profile store remains distinct from a principal
-- with no profile, which is an ordinary unauthenticated outcome.
newtype ProfileLoadError
  = ProfileAccountStoreError AccountStoreError

-- | Authentication already resolved and compared the durable session before
-- this page/action workflow starts. The profile layer therefore consumes only
-- the attached principal instead of parsing or re-looking-up a bearer cookie.
loadProfileForPrincipal :: AccountProfileStore -> Maybe AccountPrincipal -> IO (Either ProfileLoadError ProfileState)
loadProfileForPrincipal profileStore maybePrincipal =
  runExceptT $
    case maybePrincipal of
      Nothing -> pure ProfileUnauthenticated
      Just principal -> do
        maybeProfile <- liftEitherWith ProfileAccountStoreError (findAccountProfile profileStore (accountPrincipalAccountId principal))
        maybe (pure ProfileUnauthenticated) (classifyProfile principal) maybeProfile
  where
    classifyProfile principal profile =
      if accountProfileId profile /= accountPrincipalAccountId principal
        then throwError (ProfileAccountStoreError (AccountStoreCorruptData "account profile lookup returned a different account id"))
        else
          pure $
            if accountProfileEmailVerified profile
              then ProfileAuthenticated profile
              else ProfilePending profile
