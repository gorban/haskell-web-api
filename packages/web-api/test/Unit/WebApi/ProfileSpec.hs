{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.ProfileSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStoreError (..),
  )
import WebApi.App (unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (defaultAppConfig)
import WebApi.Database (defaultPageRepository)
import WebApi.Profile (ProfileLoadError (..), ProfileState (..), loadProfile)
import WebApi.Response (selectResponseWithDatabaseAndAccountWorkflow)
import WebApi.Route (AppRoute (..), defaultRequestContext)
import WebApi.Route qualified
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

    it "renders signed-out, pending, authenticated, and unavailable profiles as SSR pages" $ do
      signedOutResponse <- profileResponse (workflowFor (sessionStore (Right Nothing)) (profileStore (Right (Just verifiedProfile)))) defaultRequestContext
      pendingResponse <- profileResponse (workflowFor (sessionStore (Right (Just activeSession))) (profileStore (Right (Just pendingProfile)))) sessionRequestContext
      authenticatedResponse <- profileResponse (workflowFor (sessionStore (Right (Just activeSession))) (profileStore (Right (Just verifiedProfile)))) sessionRequestContext
      spanishPendingResponse <- profileResponse (workflowFor (sessionStore (Right (Just activeSession))) (profileStore (Right (Just pendingProfile)))) spanishSessionRequestContext
      spanishAuthenticatedResponse <- profileResponse (workflowFor (sessionStore (Right (Just activeSession))) (profileStore (Right (Just verifiedProfile)))) spanishSessionRequestContext
      unavailableResponse <- profileResponse (workflowFor (sessionStore (Left AccountSessionStoreUnavailable)) (profileStore (Right (Just verifiedProfile)))) sessionRequestContext
      spanishUnavailableResponse <- profileResponse (workflowFor (sessionStore (Left AccountSessionStoreUnavailable)) (profileStore (Right (Just verifiedProfile)))) spanishSessionRequestContext
      accountUnavailableResponse <- profileResponse (workflowFor (sessionStore (Right (Just activeSession))) (profileStore (Left (AccountStoreUnavailable "database unavailable")))) sessionRequestContext
      secondPageResponse <-
        selectResponseWithDatabaseAndAccountWorkflow
          defaultAppConfig
          defaultPageRepository
          unavailableAccountWorkflow
          (HarchWeb.RouteRequest SecondRoute defaultRequestContext)
      expectAll
        ( (responsePageBody signedOutResponse `shouldSatisfy` containsAll ["Sign in to view and manage your profile.", "href=\"/login\"", "href=\"/register\""])
            :| [ responsePageBody pendingResponse `shouldSatisfy` containsAll ["Verify your email address before continuing.", "data-profile-username=\"true\">person_01", "data-profile-display-name=\"true\">Person Example", "data-profile-email=\"true\">person@example.test", "id=\"profile-region\"", "data-harch-control", "value=\"resend-verification\"", "Resend verification email", "href=\"/logout\""],
                 responsePageBody authenticatedResponse `shouldSatisfy` containsAll ["You are signed in.", "data-profile-username=\"true\">person_01", "data-profile-display-name=\"true\">Person Example", "data-profile-email=\"true\">person@example.test", "href=\"/logout\""],
                 responsePageBody spanishPendingResponse `shouldSatisfy` containsAll ["Verifica tu dirección de correo antes de continuar.", "href=\"/es/logout\""],
                 responsePageBody spanishAuthenticatedResponse `shouldSatisfy` containsAll ["Has iniciado sesión.", "href=\"/es/logout\""],
                 responsePageBody unavailableResponse `shouldSatisfy` containsAll ["Your profile is temporarily unavailable.", "href=\"/login\""],
                 responsePageBody spanishUnavailableResponse `shouldSatisfy` containsAll ["Tu perfil no está disponible temporalmente.", "href=\"/es/login\""],
                 responseStatus unavailableResponse `shouldBe` 500,
                 responsePageTitle authenticatedResponse `shouldBe` "web-api: Profile",
                 responsePageTitle unavailableResponse `shouldBe` "web-api: Profile",
                 responsePageBody unavailableResponse `shouldSatisfy` (not . Text.isInfixOf "AccountSessionStoreUnavailable"),
                 responseDiagnosticAttributes unavailableResponse `shouldBe` profileFailureAttributes "AccountSessionStoreError",
                 responseDiagnosticLogs unavailableResponse `shouldBe` ["Profile loading failed: AccountSessionStoreError"],
                 HarchWeb.diagnosticDatabaseOperations (HarchWeb.responseDiagnostics unavailableResponse) `shouldBe` [],
                 responseDiagnosticAttributes accountUnavailableResponse `shouldBe` profileFailureAttributes "AccountStoreError",
                 responseDiagnosticLogs accountUnavailableResponse `shouldBe` ["Profile loading failed: AccountStoreError"],
                 responsePageBody secondPageResponse `shouldSatisfy` Text.isInfixOf "data-page=\"second\""
               ]
        )

assertProfileResult :: IO (Either ProfileLoadError ProfileState) -> (Either ProfileLoadError ProfileState -> Bool) -> Expectation
assertProfileResult action matches = do
  result <- action
  if matches result
    then pure ()
    else expectationFailure "unexpected profile-resolution result"

profileResponse :: AccountWorkflow -> WebApi.Route.AppRequestContext -> IO (HarchWeb.Response AppRoute WebApi.Route.AppRequestContext)
profileResponse workflow requestContext =
  selectResponseWithDatabaseAndAccountWorkflow
    defaultAppConfig
    defaultPageRepository
    workflow
    (HarchWeb.RouteRequest ProfileRoute requestContext)

workflowFor :: AccountSessionStore -> AccountProfileStore -> AccountWorkflow
workflowFor sessionStoreValue profileStoreValue =
  unavailableAccountWorkflow
    { accountWorkflowClock = pure 150,
      accountWorkflowSessionStore = sessionStoreValue,
      accountWorkflowProfileStore = profileStoreValue
    }

sessionRequestContext :: WebApi.Route.AppRequestContext
sessionRequestContext = defaultRequestContext {WebApi.Route.requestSessionId = Just testSessionId}

spanishSessionRequestContext :: WebApi.Route.AppRequestContext
spanishSessionRequestContext = sessionRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}

responsePageBody :: HarchWeb.Response AppRoute WebApi.Route.AppRequestContext -> Text
responsePageBody response =
  case response of
    HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page)
    HarchWeb.PageResponseWithMetadata _ page -> HarchWeb.renderHtml (HarchWeb.pageBody page)
    _ -> ""

responseStatus :: HarchWeb.Response AppRoute WebApi.Route.AppRequestContext -> Int
responseStatus response =
  case response of
    HarchWeb.PageResponse _ -> 200
    HarchWeb.PageResponseWithMetadata metadata _ -> Http.statusCode (HarchWeb.responseStatus metadata)
    _ -> 0

responsePageTitle :: HarchWeb.Response AppRoute WebApi.Route.AppRequestContext -> Text
responsePageTitle response =
  case response of
    HarchWeb.PageResponse page -> HarchWeb.pageTitle page
    HarchWeb.PageResponseWithMetadata _ page -> HarchWeb.pageTitle page
    _ -> ""

responseDiagnosticAttributes :: HarchWeb.Response AppRoute WebApi.Route.AppRequestContext -> [Observability.ObservabilityAttribute]
responseDiagnosticAttributes = HarchWeb.diagnosticObservabilityAttributes . HarchWeb.responseDiagnostics

responseDiagnosticLogs :: HarchWeb.Response AppRoute WebApi.Route.AppRequestContext -> [Text]
responseDiagnosticLogs = HarchWeb.diagnosticLogEntries . HarchWeb.responseDiagnostics

profileFailureAttributes :: Text -> [Observability.ObservabilityAttribute]
profileFailureAttributes errorType =
  [ Observability.ObservabilityAttribute "error.type" (Observability.TextAttribute errorType),
    Observability.ObservabilityAttribute "app.failure.code" (Observability.TextAttribute "profile.load"),
    Observability.ObservabilityAttribute "app.route" (Observability.TextAttribute "/profile"),
    Observability.ObservabilityAttribute "app.surface" (Observability.TextAttribute "page")
  ]

containsAll :: [Text] -> Text -> Bool
containsAll expectedFragments actualBody = all (`Text.isInfixOf` actualBody) expectedFragments

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
      invalidateAccountSession = \_ _ -> pure (Right False)
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
pendingProfile = AccountProfile accountId emailAddress (Username.mkUsername "person_01") (Just "Person Example") False

verifiedProfile :: AccountProfile
verifiedProfile = AccountProfile accountId emailAddress (Username.mkUsername "person_01") (Just "Person Example") True

mismatchedProfile :: AccountProfile
mismatchedProfile = AccountProfile otherAccountId emailAddress Nothing Nothing True

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
