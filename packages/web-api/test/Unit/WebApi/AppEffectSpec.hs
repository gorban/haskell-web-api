{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Text (Text)
import WebApi.App (unavailableAccountWorkflow)
import WebApi.AppEffect qualified as AppEffect

spec = do
  describe "WebApi.AppEffect" $ do
    it "renders every closed failure code to its stable telemetry value" $
      map
        AppEffect.renderFailureCode
        [ AppEffect.RegistrationDeliveryFailure,
          AppEffect.RegistrationStoreFailure,
          AppEffect.RegistrationPasswordHashFailure,
          AppEffect.RegistrationPasswordWorkBudgetFailure,
          AppEffect.RegistrationClockFailure,
          AppEffect.VerificationStoreFailure,
          AppEffect.MfaEnrollmentSessionFailure,
          AppEffect.MfaEnrollmentStartFailure,
          AppEffect.MfaEnrollmentConfirmFailure,
          AppEffect.LoginCredentialStoreFailure,
          AppEffect.LoginMfaStoreFailure,
          AppEffect.LoginAttemptStoreFailure,
          AppEffect.LoginPasswordWorkBudgetFailure,
          AppEffect.LoginCorruptEnrollmentFailure,
          AppEffect.LoginSessionFailure,
          AppEffect.LogoutSessionFailure,
          AppEffect.ProfileLoadFailure,
          AppEffect.ProfileResendDeliveryFailure,
          AppEffect.ProfileResendStoreFailure,
          AppEffect.ProfileResendClockFailure
        ]
        `shouldBe` [ "account.registration.delivery",
                     "account.registration.store",
                     "account.registration.password-hash",
                     "account.registration.password-work-budget",
                     "account.registration.clock",
                     "account.verification.store",
                     "account.mfa.enrollment-session",
                     "account.mfa.start",
                     "account.mfa.confirm",
                     "account.login.credential-store",
                     "account.login.mfa-store",
                     "account.login.attempt-store",
                     "account.login.password-work-budget",
                     "account.login.corrupt-enrollment",
                     "account.login.session",
                     "account.logout.session",
                     "account.profile.load",
                     "account.profile.resend.delivery",
                     "account.profile.resend.store",
                     "account.profile.resend.clock"
                   ]

    it "keeps failure codes comparable and inspectable" $ do
      let registrationFailure = AppEffect.RegistrationStoreFailure
          loginFailure = AppEffect.LoginSessionFailure
      registrationFailure `shouldNotBe` loginFailure
      show registrationFailure `shouldBe` "RegistrationStoreFailure"
      show [registrationFailure] `shouldBe` "[RegistrationStoreFailure]"

    it "composes application services and typed failures through one boundary" $ do
      let services = AppEffect.AppServices unavailableAccountWorkflow
          successfulAction :: AppEffect.AppM Text Int
          successfulAction = pure 42
          failureDiagnostics = AppEffect.FailureDiagnostics AppEffect.RegistrationStoreFailure "SampleFailure" ["private detail"]
          failure = AppEffect.AppFailure "safe failure" failureDiagnostics
          failingAction :: AppEffect.AppM Text ()
          failingAction = AppEffect.throwAppFailure failure
      AppEffect.runAppM services successfulAction
        >>= \case
          Right 42 -> pure ()
          _ -> expectationFailure "expected the applicative application action to return 42"
      AppEffect.runAppM services failingAction
        >>= \case
          Left actualFailure -> do
            AppEffect.appFailurePublic actualFailure `shouldBe` "safe failure"
            AppEffect.failureCode (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` AppEffect.RegistrationStoreFailure
            AppEffect.failureType (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` "SampleFailure"
            AppEffect.failureLogEntries (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` ["private detail"]
          Right () -> expectationFailure "expected a typed application failure"
