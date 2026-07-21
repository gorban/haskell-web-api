{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.ProfileSpec (spec) where

import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import Test.Hspec
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStoreError (..),
  )
import WebApi.Profile (ProfileLoadError (..), ProfileState (..), loadProfile)
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )

spec :: Spec
spec =
  describe "profile session resolution" $ do
    it "distinguishes absent, expired, pending, and authenticated sessions" $ do
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Right (Just pendingProfile))) 150 Nothing) isUnauthenticated
      assertProfileResult (loadProfile (sessionStore (Right Nothing)) (profileStore (Right (Just pendingProfile))) 150 (Just testSessionId)) isUnauthenticated
      assertProfileResult (loadProfile (sessionStore (Right (Just expiredSession))) (profileStore (Right (Just pendingProfile))) 150 (Just testSessionId)) isUnauthenticated
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Right Nothing)) 150 (Just testSessionId)) isUnauthenticated
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Right (Just pendingProfile))) 150 (Just testSessionId)) (isPendingProfile pendingProfile)
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Right (Just verifiedProfile))) 150 (Just testSessionId)) (isAuthenticatedProfile verifiedProfile)

    it "keeps session and profile persistence failures on the error rail" $ do
      assertProfileResult (loadProfile (sessionStore (Left AccountSessionStoreUnavailable)) (profileStore (Right (Just verifiedProfile))) 150 (Just testSessionId)) isUnavailableSessionFailure
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Left (AccountStoreUnavailable "database unavailable"))) 150 (Just testSessionId)) (isAccountFailure (AccountStoreUnavailable "database unavailable"))
      assertProfileResult (loadProfile (sessionStore (Right (Just activeSession))) (profileStore (Right (Just mismatchedProfile))) 150 (Just testSessionId)) (isAccountFailure (AccountStoreCorruptData "account profile lookup returned a different account id"))

assertProfileResult :: IO (Either ProfileLoadError ProfileState) -> (Either ProfileLoadError ProfileState -> Bool) -> Expectation
assertProfileResult action matches = do
  result <- action
  if matches result
    then pure ()
    else expectationFailure "unexpected profile-resolution result"

isUnauthenticated :: Either ProfileLoadError ProfileState -> Bool
isUnauthenticated result =
  case result of
    Right ProfileUnauthenticated -> True
    _ -> False

isPendingProfile :: AccountProfile -> Either ProfileLoadError ProfileState -> Bool
isPendingProfile expectedProfile result =
  case result of
    Right (ProfilePending actualProfile) -> sameProfile actualProfile expectedProfile
    _ -> False

isAuthenticatedProfile :: AccountProfile -> Either ProfileLoadError ProfileState -> Bool
isAuthenticatedProfile expectedProfile result =
  case result of
    Right (ProfileAuthenticated actualProfile) -> sameProfile actualProfile expectedProfile
    _ -> False

isUnavailableSessionFailure :: Either ProfileLoadError ProfileState -> Bool
isUnavailableSessionFailure result =
  case result of
    Left (ProfileSessionStoreError AccountSessionStoreUnavailable) -> True
    _ -> False

isAccountFailure :: AccountStoreError -> Either ProfileLoadError ProfileState -> Bool
isAccountFailure expectedError result =
  case result of
    Left (ProfileAccountStoreError actualError) -> sameAccountStoreError actualError expectedError
    _ -> False

sameProfile :: AccountProfile -> AccountProfile -> Bool
sameProfile leftProfile rightProfile =
  accountProfileId leftProfile == accountProfileId rightProfile
    && accountProfileEmail leftProfile == accountProfileEmail rightProfile
    && accountProfileEmailVerified leftProfile == accountProfileEmailVerified rightProfile

sameAccountStoreError :: AccountStoreError -> AccountStoreError -> Bool
sameAccountStoreError leftError rightError =
  case (leftError, rightError) of
    (AccountStoreUnavailable leftDetail, AccountStoreUnavailable rightDetail) -> leftDetail == rightDetail
    (AccountStoreCorruptData leftDetail, AccountStoreCorruptData rightDetail) -> leftDetail == rightDetail
    _ -> False

sessionStore :: Either AccountSessionStoreError (Maybe (OpaqueSession AccountId)) -> AccountSessionStore
sessionStore result =
  AccountSessionStore
    { saveAccountSession = \_ -> pure (Right True),
      loadAccountSession = \sessionIdValue -> sessionIdValue `seq` pure result,
      invalidateAccountSession = \_ -> pure (Right False)
    }

profileStore :: Either AccountStoreError (Maybe AccountProfile) -> AccountProfileStore
profileStore result = AccountProfileStore (\accountIdValue -> accountIdValue `seq` pure result)

activeSession :: OpaqueSession AccountId
activeSession = opaqueSession 200

expiredSession :: OpaqueSession AccountId
expiredSession = opaqueSession 150

opaqueSession :: Word64 -> OpaqueSession AccountId
opaqueSession expiresAtNanoseconds =
  case mkCsrfToken "abcdefghijklmnopqrstuvwxyz0123456789-_" of
    Just csrfToken ->
      OpaqueSession
        { sessionId = testSessionId,
          sessionPrincipal = accountId,
          sessionCsrfToken = csrfToken,
          sessionIssuedAtNanoseconds = 100,
          sessionExpiresAtNanoseconds = expiresAtNanoseconds
        }
    Nothing -> error "expected a valid CSRF token"

pendingProfile :: AccountProfile
pendingProfile = AccountProfile accountId emailAddress False

verifiedProfile :: AccountProfile
verifiedProfile = AccountProfile accountId emailAddress True

mismatchedProfile :: AccountProfile
mismatchedProfile = AccountProfile otherAccountId emailAddress True

accountId :: AccountId
accountId = requiredAccountId "account_01"

otherAccountId :: AccountId
otherAccountId = requiredAccountId "account_02"

emailAddress :: EmailAddress
emailAddress =
  case mkEmailAddress "person@example.test" of
    Just value -> value
    Nothing -> error "expected a valid email address"

testSessionId :: SessionId
testSessionId =
  case mkSessionId "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_" of
    Just value -> value
    Nothing -> error "expected a valid session id"

requiredAccountId :: Text -> AccountId
requiredAccountId value =
  case mkAccountId value of
    Just account -> account
    Nothing -> error "expected a valid account id"
