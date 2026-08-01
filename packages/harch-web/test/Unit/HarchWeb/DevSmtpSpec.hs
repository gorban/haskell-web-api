{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.DevSmtpSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Data.ByteString qualified as ByteString
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Data.Word (Word16)
import HarchWeb.DevSmtp
import HarchWeb.Email
import Test.Hspec

spec :: Spec
spec =
  describe "DevSmtpServer" $ do
    it "captures loopback SMTP mail by recipient and consumes the recipient index" $
      bracket startDevSmtpServer stopDevSmtpServer $ \server -> do
        let sender = required (mkEmailAddress "noreply@example.test")
            recipient = required (mkEmailAddress "ada@example.test")
            message = required (mkEmailMessage (EmailMessageInput recipient "Welcome" "First line\n.second line"))
            config = required (mkSmtpConfig (smtpConfigInput "127.0.0.1" (devSmtpPort server) "account.example.test" sender Nothing))
        (null <$> devSmtpReceivedEmails server) `shouldReturn` True
        deliverSmtpEmail config message
        delivered <- awaitEmail server "ada@example.test"
        devSmtpEnvelopeSender delivered `shouldBe` "noreply@example.test"
        devSmtpRecipients delivered `shouldBe` ["ada@example.test"]
        devSmtpRawMessage delivered `shouldSatisfy` ByteString.isInfixOf "Subject: Welcome"
        devSmtpRawMessage delivered `shouldSatisfy` ByteString.isInfixOf ".second line"
        (isNothing <$> takeLatestDevSmtpEmailTo server "ADA@EXAMPLE.TEST") `shouldReturn` True
        (null <$> devSmtpReceivedEmails server) `shouldReturn` True

    it "accepts development SMTP AUTH PLAIN" $
      bracket startDevSmtpServer stopDevSmtpServer $ \server -> do
        let sender = required (mkEmailAddress "noreply@example.test")
            recipient = required (mkEmailAddress "ada@example.test")
            message = required (mkEmailMessage (EmailMessageInput recipient "Authenticated" "Body"))
            config = required (mkSmtpConfig (smtpConfigInput "127.0.0.1" (devSmtpPort server) "account.example.test" sender (Just (smtpAuthentication (smtpLoginUsername "local-user") (smtpLoginPassword "local-password")))))
        deliverSmtpEmail config message
        delivered <- awaitEmail server "ada@example.test"
        devSmtpEnvelopeSender delivered `shouldBe` "noreply@example.test"
        devSmtpRecipients delivered `shouldBe` ["ada@example.test"]
        devSmtpRawMessage delivered `shouldSatisfy` ByteString.isInfixOf "Subject: Authenticated"

required :: Maybe value -> value
required = fromMaybe (error "Expected valid development SMTP fixture")

smtpConfigInput :: Text.Text -> Word16 -> Text.Text -> EmailAddress -> Maybe SmtpAuthentication -> SmtpConfigInput
smtpConfigInput host port heloName sender authentication =
  SmtpConfigInput
    { smtpInputHost = smtpServerHost host,
      smtpInputPort = port,
      smtpInputHeloName = smtpServerHeloName heloName,
      smtpInputEnvelopeSender = sender,
      smtpInputAuthentication = authentication
    }

awaitEmail :: DevSmtpServer -> String -> IO DevSmtpEmail
awaitEmail server recipient = go (100 :: Int)
  where
    go remaining = do
      found <- takeLatestDevSmtpEmailTo server (fromString recipient)
      case found of
        Just email -> pure email
        Nothing
          | remaining > 0 -> threadDelay 10000 >> go (remaining - 1)
          | otherwise -> expectationFailure "Timed out waiting for development SMTP mail" >> error "unreachable"

fromString :: String -> Text.Text
fromString = Text.pack
