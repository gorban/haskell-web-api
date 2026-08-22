{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.AccountPagesSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account (AccountId, emailVerificationTokenText, mkAccountId, storedVerificationTokenDigest)
import HarchWeb.Action qualified as Action
import HarchWeb.Email (EmailAddress, EmailDelivery (..), mkEmailAddress)
import HarchWeb.Email qualified as Email
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
import WebApi.Route (AppRequestContext, defaultRequestContext)
import WebApi.Route qualified
import WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )

spec :: Spec
spec =
  describe "WebApi.AccountPages" $ do
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

    it "keeps PendingProfileForm comparable and its rendered region free of a false error flag" $
      expectAll
        ( ( ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                == PendingProfileForm "person@example.test" Nothing False "Resend verification email"
            )
              `shouldBe` True
          )
            :| [ ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                     /= PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email"
                 )
                   `shouldBe` True,
                 ( PendingProfileForm "person@example.test" Nothing False "Resend verification email"
                     /= PendingProfileForm "person@example.test" Nothing True "Resend verification email"
                 )
                   `shouldBe` True,
                 (PendingProfileForm "person@example.test" Nothing False "Resend verification email" /= PendingProfileForm "person@example.test" Nothing False "Send again")
                   `shouldBe` True,
                 renderPendingProfileRegion defaultRequestContext UpdateProfileTarget (PendingProfileForm "person@example.test" (Just "Updated") False "Resend verification email")
                   `shouldSatisfy` (not . Text.isInfixOf "data-message-error=\"true\"")
               ]
        )

sessionStore :: Either AccountSessionStoreError (Maybe (OpaqueSession AccountId)) -> AccountSessionStore
sessionStore result =
  AccountSessionStore
    { saveAccountSession = \_ -> pure (Right True),
      loadAccountSession = \sessionIdValue -> sessionIdValue `seq` pure result,
      invalidateAccountSession = \_ _ -> pure (Right False)
    }

profileStore :: Either AccountStoreError (Maybe AccountProfile) -> AccountProfileStore
profileStore result = AccountProfileStore (\accountIdValue -> accountIdValue `seq` pure result)

sessionRequestContext :: AppRequestContext
sessionRequestContext = defaultRequestContext {WebApi.Route.requestSessionId = Just testSessionId}

spanishSessionRequestContext :: AppRequestContext
spanishSessionRequestContext = sessionRequestContext {WebApi.Route.requestLocale = WebApi.Route.Spanish, WebApi.Route.requestLocaleIsExplicit = True}

activeSession :: OpaqueSession AccountId
activeSession = opaqueSession 200

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

accountId :: AccountId
accountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

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
