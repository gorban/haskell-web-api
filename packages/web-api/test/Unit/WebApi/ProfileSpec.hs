{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.ProfileSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account (AccountId, emailVerificationTokenText, mkAccountId, storedVerificationTokenDigest)
import HarchWeb.Action qualified as Action
import HarchWeb.Email (EmailAddress, EmailDelivery (..), mkEmailAddress)
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStore (..),
    AccountStoreError (..),
  )
import WebApi.AccountPages (AccountActionTarget (..), PendingProfileForm (..), accountActions, handleAccountAction, renderPendingProfileRegion)
import WebApi.App (unavailableAccountWorkflow)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config (defaultAppConfig)
import WebApi.Database (defaultPageRepository)
import WebApi.Page (AppPageModel (..), CallToAction (..), ProfilePageModel (..))
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
                 responseDiagnosticAttributes accountUnavailableResponse `shouldBe` profileFailureAttributes "AccountStoreError",
                 responseDiagnosticLogs accountUnavailableResponse `shouldBe` ["Profile loading failed: AccountStoreError"],
                 responsePageBody secondPageResponse `shouldSatisfy` Text.isInfixOf "data-page=\"second\""
               ]
        )

    it "resends pending-profile verification through a localized client-action patch" $ do
      let actionRequest requestContext fields =
            fromMaybe
              (error "expected a recognized profile action fixture")
              ( do
                  action <-
                    case Action.decodeAction
                      accountActions
                      HarchWeb.ClientActionPayload
                        { HarchWeb.clientActionMethod = "POST",
                          HarchWeb.clientActionPath = profileActionPath requestContext,
                          HarchWeb.clientActionFields = fields,
                          HarchWeb.clientActionCsrfToken = Nothing,
                          HarchWeb.clientActionIdempotencyKey = Nothing,
                          HarchWeb.clientActionPayloadContext = requestContext
                        } of
                      HarchWeb.DecodedClientAction decodedAction -> Just decodedAction
                      _ -> Nothing
                  pure
                    HarchWeb.ClientActionRequest
                      { HarchWeb.clientAction = action,
                        HarchWeb.clientActionRequestIdempotencyKey = Nothing,
                        HarchWeb.clientActionContext = requestContext
                      }
              )
          profileActionPath requestContext =
            case WebApi.Route.requestLocale requestContext of
              WebApi.Route.English -> "/profile"
              WebApi.Route.Spanish -> "/es/profile"
          store replacementResult =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \verification -> storedVerificationTokenDigest verification `seq` pure replacementResult,
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          workflow replacementResult emailDelivery sessionResult loadedProfile now =
            unavailableAccountWorkflow
              { accountWorkflowStore = store replacementResult,
                accountWorkflowEmailDelivery = emailDelivery,
                accountWorkflowClock = pure now,
                accountWorkflowSessionStore = sessionStore sessionResult,
                accountWorkflowProfileStore = profileStore loadedProfile,
                accountWorkflowVerificationUrl = \requestContext token ->
                  case WebApi.Route.requestLocale requestContext of
                    WebApi.Route.English -> "https://account.example.test/verify?token=" <> emailVerificationTokenText token
                    WebApi.Route.Spanish -> "https://account.example.test/es/verify?token=" <> emailVerificationTokenText token
              }
          delivery = EmailDelivery (\message -> Text.length (Email.emailMessageBody message) `seq` pure ())
          pendingWorkflow = workflow (Right True) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150
          expect workflowValue request expectedStatus expectedText = do
            actionResult <- handleAccountAction workflowValue request
            case actionResult of
              Nothing -> expectationFailure "expected a profile client-action response"
              Just response ->
                expectAll
                  ( (Http.statusCode (HarchWeb.clientActionStatus response) `shouldBe` expectedStatus)
                      :| [ HarchWeb.clientActionPatches response `shouldSatisfy` any ((== "profile-region") . HarchWeb.regionPatchId),
                           HarchWeb.clientActionPatches response `shouldSatisfy` any (Text.isInfixOf expectedText . HarchWeb.regionPatchHtml),
                           length (show response) `shouldSatisfy` (> 0)
                         ]
                  )
      expect pendingWorkflow (actionRequest sessionRequestContext [("intent", "resend-verification")]) 202 "Check your inbox"
      expect pendingWorkflow (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 202 "Revisa tu bandeja"
      expect pendingWorkflow (actionRequest sessionRequestContext []) 422 "Choose a profile action"
      expect pendingWorkflow (actionRequest defaultRequestContext [("intent", "resend-verification")]) 403 "Sign in before"
      expect pendingWorkflow (actionRequest spanishSessionRequestContext []) 422 "Elige una accion de perfil"
      expect pendingWorkflow (actionRequest (defaultRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}) [("intent", "resend-verification")]) 403 "Inicia sesion antes"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Right (Just verifiedProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 409 "already verified"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Right (Just verifiedProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 409 "Tu direccion de correo ya esta verificada"
      expect (workflow (Right False) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 409 "profile state changed"
      expect (workflow (Right False) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 409 "El estado de tu perfil ha cambiado"
      expect (workflow (Left (AccountStoreUnavailable "database unavailable")) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Left (AccountStoreUnavailable "database unavailable")) delivery (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) (EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))) (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 502 "could not send"
      expect (workflow (Right True) (EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))) (Right (Just activeSession)) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 502 "No pudimos enviar"
      expect (workflow (Right True) delivery (Left AccountSessionStoreUnavailable) (Right (Just pendingProfile)) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Left AccountSessionStoreUnavailable) (Right (Just pendingProfile)) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Left (AccountStoreUnavailable "database unavailable")) 150) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Right (Just activeSession)) (Left (AccountStoreUnavailable "database unavailable")) 150) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"
      expect (workflow (Right True) delivery (Right (Just (opaqueSession maxBound))) (Right (Just pendingProfile)) (maxBound - 1)) (actionRequest sessionRequestContext [("intent", "resend-verification")]) 503 "temporarily unavailable"
      expect (workflow (Right True) delivery (Right (Just (opaqueSession maxBound))) (Right (Just pendingProfile)) (maxBound - 1)) (actionRequest spanishSessionRequestContext [("intent", "resend-verification")]) 503 "Tu perfil no esta disponible"

    it "keeps every profile page model comparable and printable" $ do
      let signInAction = CallToAction "Sign in" LoginRoute "/login"
          registrationAction = CallToAction "Create account" RegistrationRoute "/register"
          signOutAction = CallToAction "Sign out" LogoutRoute "/logout"
          signedOutModel = SignedOutProfilePage "Profile" "Sign in to view and manage your profile." signInAction registrationAction
          pendingModel = PendingProfilePage "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
          pendingModelWithIdentity = PendingProfilePage "Profile" "Verify your email address before continuing." "person@example.test" (Just "pending-person") (Just "Pending Person") UpdateProfileTarget "Resend verification email" signOutAction
          authenticatedModel = AuthenticatedProfilePage "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
          authenticatedModelWithIdentity = AuthenticatedProfilePage "Profile" "You are signed in." "person@example.test" (Just "authenticated-person") (Just "Authenticated Person") signOutAction
          unavailableModel = UnavailableProfilePage "Profile" "Your profile is temporarily unavailable." signInAction
          models =
            [ (signedOutModel, "SignedOutProfilePage"),
              (pendingModel, "PendingProfilePage"),
              (authenticatedModel, "AuthenticatedProfilePage"),
              (unavailableModel, "UnavailableProfilePage")
            ]
      mapM_ (assertProfilePageModel . fst) models
      mapM_ assertProfilePageModelShow models
      expectAll
        ( (ProfilePage signedOutModel == ProfilePage pendingModel `shouldBe` False)
            :| [ equalValues pendingModel pendingModelWithIdentity
                   `shouldBe` True,
                 equalValues authenticatedModel authenticatedModelWithIdentity
                   `shouldBe` True,
                 notEqualValues pendingModel authenticatedModel
                   `shouldBe` True,
                 equalValues
                   (PendingProfileForm "person@example.test" Nothing False "Resend verification email")
                   (PendingProfileForm "person@example.test" Nothing False "Resend verification email")
                   `shouldBe` True,
                 equalValues
                   (PendingProfileForm "person@example.test" Nothing False "Resend verification email")
                   (PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email")
                   `shouldBe` False,
                 equalValues
                   (PendingProfileForm "person@example.test" Nothing False "Resend verification email")
                   (PendingProfileForm "person@example.test" Nothing True "Resend verification email")
                   `shouldBe` False,
                 equalValues
                   (PendingProfileForm "person@example.test" Nothing False "Resend verification email")
                   (PendingProfileForm "person@example.test" Nothing False "Send again")
                   `shouldBe` False,
                 (PendingProfileForm "person@example.test" Nothing False "Resend verification email" /= PendingProfileForm "person@example.test" Nothing False "Send again")
                   `shouldBe` True,
                 renderPendingProfileRegion defaultRequestContext UpdateProfileTarget (PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email")
                   `shouldSatisfy` (not . Text.isInfixOf "data-message-error=\"true\"")
               ]
        )

assertProfileResult :: IO (Either ProfileLoadError ProfileState) -> (Either ProfileLoadError ProfileState -> Bool) -> Expectation
assertProfileResult action matches = do
  result <- action
  if matches result
    then pure ()
    else expectationFailure "unexpected profile-resolution result"

equalValues :: (Eq value) => value -> value -> Bool
equalValues = (==)
{-# NOINLINE equalValues #-}

notEqualValues :: (Eq value) => value -> value -> Bool
notEqualValues = (/=)
{-# NOINLINE notEqualValues #-}

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

assertProfilePageModel :: ProfilePageModel -> Expectation
assertProfilePageModel profilePageModel =
  ProfilePage profilePageModel == ProfilePage profilePageModel `shouldBe` True

assertProfilePageModelShow :: (ProfilePageModel, Text) -> Expectation
assertProfilePageModelShow (profilePageModel, expectedPrefix) =
  Text.pack (show (ProfilePage profilePageModel)) `shouldSatisfy` Text.isPrefixOf expectedPrefix

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
