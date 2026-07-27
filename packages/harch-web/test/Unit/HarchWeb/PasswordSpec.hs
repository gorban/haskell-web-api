{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.PasswordSpec (spec) where

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import HarchWeb.Password
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

testPolicy :: PasswordHashingPolicy
testPolicy = required "test policy" (mkPasswordHashingPolicy 1 8 1)

samplePassword :: Password
samplePassword = mkPassword "correct horse battery staple"

sampleSalt :: ByteString.ByteString
sampleSalt = "0123456789abcdef"

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

spec :: Spec
spec = do
  describe "PasswordHashingPolicy" $ do
    it "uses an Argon2id production policy and rejects invalid resource settings" $ do
      expectAll
        ( (passwordHashIterations defaultPasswordHashingPolicy `shouldBe` 3)
            :| [ passwordHashMemoryKibibytes defaultPasswordHashingPolicy `shouldBe` 65536,
                 passwordHashParallelism defaultPasswordHashingPolicy `shouldBe` 1,
                 show defaultPasswordHashingPolicy
                   `shouldBe` "PasswordHashingPolicy {passwordHashIterations = 3, passwordHashMemoryKibibytes = 65536, passwordHashParallelism = 1}",
                 show [defaultPasswordHashingPolicy]
                   `shouldBe` "[PasswordHashingPolicy {passwordHashIterations = 3, passwordHashMemoryKibibytes = 65536, passwordHashParallelism = 1}]",
                 mkPasswordHashingPolicy 0 8 1 `shouldBe` Nothing,
                 mkPasswordHashingPolicy 1 8 0 `shouldBe` Nothing,
                 mkPasswordHashingPolicy 1 15 2 `shouldBe` Nothing,
                 mkPasswordHashingPolicy 1 16 2 `shouldBe` Just (PasswordHashingPolicy 1 16 2),
                 defaultPasswordHashingPolicy /= testPolicy `shouldBe` True
               ]
        )

  describe "Argon2id password hashes" $ do
    it "encodes policy and salt, then verifies only the original password" $ do
      let passwordHash = required "password hash" (hashPasswordWithSalt testPolicy sampleSalt samplePassword)
      expectAll
        ( ( passwordHashText passwordHash
              `shouldBe` "$argon2id$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE"
          )
            :| [ passwordHashText (required "read password hash" (readPasswordHash (passwordHashText passwordHash)))
                   `shouldBe` passwordHashText passwordHash,
                 verifyPassword samplePassword passwordHash `shouldBe` True,
                 verifyPassword (mkPassword "wrong password") passwordHash `shouldBe` False
               ]
        )

    it "rejects malformed, unsupported, and truncated stored hashes" $ do
      expectAll
        ( (isNothing (hashPasswordWithSalt testPolicy "short" samplePassword) `shouldBe` True)
            :| [ isNothing (readPasswordHash "$argon2id$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$short") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=19$m=8,t=1,p=1$@@@$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2Rl$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2i$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$2kTgJk6VdlkTLGO2AH5eQCP8qZAQH4CZAPyfIsyPXdo") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=16$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$2kTgJk6VdlkTLGO2AH5eQCP8qZAQH4CZAPyfIsyPXdo") `shouldBe` True,
                 verifyPassword samplePassword (PasswordHash "not-a-password-hash") `shouldBe` False
               ]
        )

    it "draws a fresh salt through the IO hashing boundary" $ do
      firstHash <- hashPassword testPolicy samplePassword
      secondHash <- hashPassword testPolicy samplePassword
      expectAll
        ( (maybe False (verifyPassword samplePassword) firstHash `shouldBe` True)
            :| [ maybe False (verifyPassword samplePassword) secondHash `shouldBe` True,
                 fmap passwordHashText firstHash /= fmap passwordHashText secondHash `shouldBe` True
               ]
        )
