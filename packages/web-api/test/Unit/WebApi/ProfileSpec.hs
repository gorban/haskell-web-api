{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Database (DatabaseOperation)
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session (SessionId, mkSessionId)
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import TestCore.Wai (waiRequest)
import Unit.WebApi.TestSupport (pureApplication, testRequestId)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStoreError (..))
import WebApi.AccountPrincipal (mkAccountPrincipal)
import WebApi.App (unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (defaultAppConfig)
import WebApi.Database (defaultPageRepository)
import WebApi.Profile (ProfileLoadError (..), ProfileState (..), loadProfileForPrincipal)
import WebApi.Response (selectResponseWithDatabaseAndAccountWorkflow)
import WebApi.Route (AppRoute (..), defaultRequestContext)
import WebApi.Route qualified

spec =
  describe "profile principal resolution" $ do
    it "distinguishes no principal, no profile, pending, and authenticated accounts" $ do
      assertProfileResult (loadProfileForPrincipal (profileStore (Right (Just pendingProfile))) Nothing) isUnauthenticated
      assertProfileResult (loadProfileForPrincipal (profileStore (Right Nothing)) (WebApi.Route.requestAccountPrincipal sessionRequestContext)) isUnauthenticated
      assertProfileResult (loadProfileForPrincipal (profileStore (Right (Just pendingProfile))) (WebApi.Route.requestAccountPrincipal sessionRequestContext)) (isPendingProfile pendingProfile)
      assertProfileResult (loadProfileForPrincipal (profileStore (Right (Just verifiedProfile))) (WebApi.Route.requestAccountPrincipal sessionRequestContext)) (isAuthenticatedProfile verifiedProfile)

    it "keeps account persistence failures on the error rail" $ do
      assertProfileResult (loadProfileForPrincipal (profileStore (Left (AccountStoreUnavailable "database unavailable"))) (WebApi.Route.requestAccountPrincipal sessionRequestContext)) (isAccountFailure (AccountStoreUnavailable "database unavailable"))
      assertProfileResult (loadProfileForPrincipal (profileStore (Right (Just mismatchedProfile))) (WebApi.Route.requestAccountPrincipal sessionRequestContext)) (isAccountFailure (AccountStoreCorruptData "account profile lookup returned a different account id"))

    it "keeps a replayed raw legacy session cookie out of request context" $ do
      let replayedCookieValue = Text.replicate 43 "a"
          rawReplayRequest =
            (waiRequest ["profile"])
              { Wai.requestHeaders =
                  [("Cookie", TextEncoding.encodeUtf8 ("__Host-harch-session=" <> replayedCookieValue))]
              }
      let replayContext = HarchWeb.requestContextFromRequest pureApplication rawReplayRequest testRequestId defaultRequestContext
      replayContext
        `shouldBe` defaultRequestContext
          { WebApi.Route.requestCorrelationId = Just testRequestId,
            WebApi.Route.requestClientAddress = HarchWeb.requestClientAddress (HarchWeb.applicationRequestPolicy pureApplication) rawReplayRequest,
            WebApi.Route.requestAccountPrincipal = Nothing
          }
      assertProfileResult
        (loadProfileForPrincipal (profileStore (Right (Just verifiedProfile))) (WebApi.Route.requestAccountPrincipal replayContext))
        isUnauthenticated

    it "renders signed-out, pending, authenticated, and unavailable profiles as SSR pages" $ do
      signedOutResponse <- profileResponse (workflowFor (profileStore (Right (Just verifiedProfile)))) defaultRequestContext
      pendingResponse <- profileResponse (workflowFor (profileStore (Right (Just pendingProfile)))) sessionRequestContext
      authenticatedResponse <- profileResponse (workflowFor (profileStore (Right (Just verifiedProfile)))) sessionRequestContext
      spanishPendingResponse <- profileResponse (workflowFor (profileStore (Right (Just pendingProfile)))) spanishSessionRequestContext
      spanishAuthenticatedResponse <- profileResponse (workflowFor (profileStore (Right (Just verifiedProfile)))) spanishSessionRequestContext
      unavailableResponse <- profileResponse (workflowFor (profileStore (Left (AccountStoreUnavailable "database unavailable")))) sessionRequestContext
      spanishUnavailableResponse <- profileResponse (workflowFor (profileStore (Left (AccountStoreUnavailable "database unavailable")))) spanishSessionRequestContext
      secondPageResponse <-
        selectResponseWithDatabaseAndAccountWorkflow
          defaultAppConfig
          defaultPageRepository
          unavailableAccountWorkflow
          (HarchWeb.RouteRequest SecondRoute defaultRequestContext)
      expectAll
        ( (responsePageBody signedOutResponse `shouldSatisfy` containsAll ["Sign in to view and manage your profile.", "href=\"/login\"", "href=\"/register\""])
            :| [ responsePageBody pendingResponse `shouldSatisfy` containsAll ["Verify your email address before continuing.", "data-profile-username=\"true\" class=\"harch-profile-identity-value\">person_01", "data-profile-display-name=\"true\" class=\"harch-profile-identity-value\">Person Example", "data-profile-email=\"true\">person@example.test", "id=\"profile-region\"", "data-harch-control", "value=\"resend-verification\"", "Resend verification email", "href=\"/logout\""],
                 responsePageBody authenticatedResponse `shouldSatisfy` containsAll ["You are signed in.", "data-profile-username=\"true\" class=\"harch-profile-identity-value\">person_01", "data-profile-display-name=\"true\" class=\"harch-profile-identity-value\">Person Example", "data-profile-email=\"true\" class=\"harch-profile-identity-value\">person@example.test", "href=\"/logout\""],
                 responsePageBody spanishPendingResponse `shouldSatisfy` containsAll ["Verifica tu dirección de correo antes de continuar.", "href=\"/es/logout\""],
                 responsePageBody spanishAuthenticatedResponse `shouldSatisfy` containsAll ["Has iniciado sesión.", "href=\"/es/logout\""],
                 responsePageBody unavailableResponse `shouldSatisfy` containsAll ["Your profile is temporarily unavailable.", "href=\"/login\""],
                 responsePageBody spanishUnavailableResponse `shouldSatisfy` containsAll ["Tu perfil no está disponible temporalmente.", "href=\"/es/login\""],
                 responseStatus unavailableResponse `shouldBe` 500,
                 responsePageTitle authenticatedResponse `shouldBe` "web-api: Profile",
                 responsePageTitle unavailableResponse `shouldBe` "web-api: Profile",
                 responsePageBody unavailableResponse `shouldSatisfy` (not . Text.isInfixOf "AccountSessionStoreUnavailable"),
                 responseDiagnosticAttributes unavailableResponse `shouldBe` profileFailureAttributes "AccountStoreError",
                 responseDiagnosticLogs unavailableResponse `shouldBe` ["Profile loading failed: AccountStoreError"],
                 responseDiagnosticDatabaseOperations unavailableResponse `shouldBe` [],
                 responsePageBody secondPageResponse `shouldSatisfy` Text.isInfixOf "data-page=\"second\""
               ]
        )

assertProfileResult :: IO (Either ProfileLoadError ProfileState) -> (Either ProfileLoadError ProfileState -> Bool) -> Expectation
assertProfileResult action matches = do
  result <- action
  if matches result
    then pure ()
    else expectationFailure "unexpected profile-resolution result"

profileResponse :: AccountWorkflow -> WebApi.Route.AppRequestContext -> IO (HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext)
profileResponse workflow requestContext =
  selectResponseWithDatabaseAndAccountWorkflow
    defaultAppConfig
    defaultPageRepository
    workflow
    (HarchWeb.RouteRequest ProfileRoute requestContext)

workflowFor :: AccountProfileStore -> AccountWorkflow
workflowFor profileStoreValue =
  unavailableAccountWorkflow
    { accountWorkflowProfileStore = profileStoreValue
    }

sessionRequestContext :: WebApi.Route.AppRequestContext
sessionRequestContext = defaultRequestContext {WebApi.Route.requestAccountPrincipal = Just (mkAccountPrincipal accountId testSessionId 200)}

spanishSessionRequestContext :: WebApi.Route.AppRequestContext
spanishSessionRequestContext = sessionRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}

responsePageBody :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> Text
responsePageBody response =
  case response of
    HarchWeb.RenderedPage page -> HarchWeb.renderHtml (HarchWeb.pageBody page)
    HarchWeb.RenderedPageWithMetadata _ page -> HarchWeb.renderHtml (HarchWeb.pageBody page)

responseStatus :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> Int
responseStatus response =
  case response of
    HarchWeb.RenderedPage _ -> 200
    HarchWeb.RenderedPageWithMetadata metadata _ -> Http.statusCode (HarchWeb.responseStatus metadata)

responsePageTitle :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> Text
responsePageTitle response =
  case response of
    HarchWeb.RenderedPage page -> HarchWeb.pageTitle page
    HarchWeb.RenderedPageWithMetadata _ page -> HarchWeb.pageTitle page

responseDiagnosticAttributes :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> [Observability.ObservabilityAttribute]
responseDiagnosticAttributes pageResult =
  case pageResult of
    HarchWeb.RenderedPage _ -> []
    HarchWeb.RenderedPageWithMetadata metadata _ -> HarchWeb.responseObservabilityAttributes metadata

responseDiagnosticLogs :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> [Text]
responseDiagnosticLogs pageResult =
  case pageResult of
    HarchWeb.RenderedPage _ -> []
    HarchWeb.RenderedPageWithMetadata metadata _ -> HarchWeb.responseLogEntries metadata

responseDiagnosticDatabaseOperations :: HarchWeb.PageResult AppRoute WebApi.Route.AppRequestContext -> [DatabaseOperation]
responseDiagnosticDatabaseOperations pageResult =
  case pageResult of
    HarchWeb.RenderedPage _ -> []
    HarchWeb.RenderedPageWithMetadata metadata _ -> HarchWeb.responseDatabaseOperations metadata

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

profileStore :: Either AccountStoreError (Maybe AccountProfile) -> AccountProfileStore
profileStore result = AccountProfileStore (\accountIdValue -> accountIdValue `seq` pure result)

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
