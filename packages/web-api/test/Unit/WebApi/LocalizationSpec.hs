{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Monad (forM_)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Localization
import WebApi.Route (AppLocale (..))

spec =
  describe "WebApi.Localization" $ do
    it "covers every closed application key in both shipped locales" $
      forM_ [English, Spanish] $ \requestedLocale ->
        forM_ [minBound .. maxBound] $ \messageKey ->
          localizedMessage requestedLocale messageKey `shouldSatisfy` (not . Text.null)

    it "uses the catalog instead of preserving positional strings at action call sites" $
      localizedMessage Spanish VerificationDeliveryFailed `shouldBe` "No pudimos enviar el correo de verificacion. Intentalo de nuevo en breve."

    it "keeps closed catalog values inspectable and raw locale misses safe" $ do
      fromEnum AddAuthenticatorSecret `shouldBe` 0
      toEnum (fromEnum SecondPageLoadFailed) `shouldBe` SecondPageLoadFailed
      succ AddAuthenticatorSecret `shouldBe` AuthenticatorEnrolled
      pred AuthenticatorEnrolled `shouldBe` AddAuthenticatorSecret
      take 2 (enumFrom AddAuthenticatorSecret)
        `shouldBe` [AddAuthenticatorSecret, AuthenticatorEnrolled]
      take 3 (enumFromThen AddAuthenticatorSecret AuthenticatorEnrolled)
        `shouldBe` [AddAuthenticatorSecret, AuthenticatorEnrolled, AuthenticatorEnrollmentUnavailable]
      enumFromThenTo AddAuthenticatorSecret AuthenticatorEnrolled ChooseProfileAction
        `shouldBe` [ AddAuthenticatorSecret,
                     AuthenticatorEnrolled,
                     AuthenticatorEnrollmentUnavailable,
                     CheckVerificationInbox,
                     ChooseProfileAction
                   ]
      AddAuthenticatorSecret `shouldNotBe` SecondPageLoadFailed
      show AddAuthenticatorSecret `shouldBe` "AddAuthenticatorSecret"
      showsPrec 11 AddAuthenticatorSecret "" `shouldBe` "AddAuthenticatorSecret"
      showList [AddAuthenticatorSecret] "" `shouldBe` "[AddAuthenticatorSecret]"
      localizedMessageForLocale (HarchWeb.locale "fr") AddAuthenticatorSecret `shouldBe` "Message unavailable."
