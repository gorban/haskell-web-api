{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.AccountSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import HarchWeb.Account
import HarchWeb.Email (mkEmailAddress)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec = do
  describe "AccountId" $ do
    it "accepts stable application-owned identifiers and rejects unsafe values" $ do
      accountIdText <$> mkAccountId "account_01-verified" `shouldBe` Just "account_01-verified"
      map mkAccountId ["", "account id", "account/id"] `shouldBe` [Nothing, Nothing, Nothing]

    it "generates opaque URL-safe identifiers" $ do
      accountId <- generateAccountId
      expectAll
        ( (Text.length (accountIdText accountId) `shouldBe` 22)
            :| [mkAccountId (accountIdText accountId) `shouldBe` Just accountId]
        )

  describe "EmailVerificationToken" $ do
    it "accepts URL-safe bearer tokens without exposing a constructor" $ do
      emailVerificationTokenText <$> mkEmailVerificationToken validToken `shouldBe` Just validToken
      all (isNothing . mkEmailVerificationToken) ["short", Text.replicate 32 "!", Text.replicate 32 "a" <> "\n"] `shouldBe` True

    it "generates a URL-safe 256-bit bearer token" $ do
      token <- generateEmailVerificationToken
      expectAll
        ( (Text.length (emailVerificationTokenText token) `shouldBe` 43)
            :| [emailVerificationTokenText <$> mkEmailVerificationToken (emailVerificationTokenText token) `shouldBe` Just (emailVerificationTokenText token)]
        )

    it "stores a one-way digest and validates its account, email, and expiry" $ do
      let accountId = required (mkAccountId "account_01")
          emailAddress = required (mkEmailAddress "person@example.test")
          token = required (mkEmailVerificationToken validToken)
          otherToken = required (mkEmailVerificationToken (Text.replicate 43 "b"))
          stored = mkStoredEmailVerification accountId emailAddress 500 token
      expectAll
        ( (emailVerificationTokenDigest token `shouldNotBe` emailVerificationTokenDigest otherToken)
            :| [ validateEmailVerificationToken 499 token stored `shouldBe` EmailVerificationAccepted accountId emailAddress,
                 validateEmailVerificationToken 499 otherToken stored `shouldBe` EmailVerificationRejected,
                 validateEmailVerificationToken 500 token stored `shouldBe` EmailVerificationExpired
               ]
        )

    it "covers the persistence-facing value representations without rendering bearer tokens" $ do
      let accountId = required (mkAccountId "account_01")
          emailAddress = required (mkEmailAddress "person@example.test")
          token = required (mkEmailVerificationToken validToken)
          digest = emailVerificationTokenDigest token
          stored = mkStoredEmailVerification accountId emailAddress 500 token
          accepted = EmailVerificationAccepted accountId emailAddress
      expectAll
        ( (emailVerificationTokenDigestText digest `shouldBe` "ZtNPunH49FD35FWYhT5Tv8I7vRKQJ8uxMaL0_9eHjNA")
            :| [ accountId /= required (mkAccountId "account_02") `shouldBe` True,
                 stored /= mkStoredEmailVerification accountId emailAddress 501 token `shouldBe` True,
                 accepted /= EmailVerificationRejected `shouldBe` True,
                 show accountId `shouldBe` "AccountId \"account_01\"",
                 show digest `shouldBe` "EmailVerificationTokenDigest \"ZtNPunH49FD35FWYhT5Tv8I7vRKQJ8uxMaL0_9eHjNA\"",
                 show stored `shouldBe` "StoredEmailVerification {storedVerificationAccountId = AccountId \"account_01\", storedVerificationEmail = EmailAddress \"person@example.test\", storedVerificationTokenDigest = EmailVerificationTokenDigest \"ZtNPunH49FD35FWYhT5Tv8I7vRKQJ8uxMaL0_9eHjNA\", storedVerificationExpiresAtNanoseconds = 500}",
                 show accepted `shouldBe` "EmailVerificationAccepted (AccountId \"account_01\") (EmailAddress \"person@example.test\")",
                 show [accountId] `shouldBe` "[AccountId \"account_01\"]",
                 show [digest] `shouldBe` "[EmailVerificationTokenDigest \"ZtNPunH49FD35FWYhT5Tv8I7vRKQJ8uxMaL0_9eHjNA\"]",
                 show [stored] `shouldBe` "[StoredEmailVerification {storedVerificationAccountId = AccountId \"account_01\", storedVerificationEmail = EmailAddress \"person@example.test\", storedVerificationTokenDigest = EmailVerificationTokenDigest \"ZtNPunH49FD35FWYhT5Tv8I7vRKQJ8uxMaL0_9eHjNA\", storedVerificationExpiresAtNanoseconds = 500}]",
                 show [accepted] `shouldBe` "[EmailVerificationAccepted (AccountId \"account_01\") (EmailAddress \"person@example.test\")]"
               ]
        )

validToken :: Text.Text
validToken = Text.replicate 43 "a"

required :: Maybe value -> value
required = fromMaybe (error "Expected a valid account test value")
