{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (AsyncException (ThreadKilled), throwIO, try)
import Control.Monad (replicateM_)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Word (Word32)
import HarchWeb.Password
import System.Timeout (timeout)

testPolicy :: PasswordHashingPolicy
testPolicy = required "test policy" (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 1))

samplePassword :: Password
samplePassword = mkPassword "correct horse battery staple"

sampleSalt :: ByteString.ByteString
sampleSalt = "0123456789abcdef"

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

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
                 mkPasswordHashingPolicy (argon2Iterations 0) (argon2MemoryKib 8) (argon2Parallelism 1) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 8) (argon2Parallelism 0) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 15) (argon2Parallelism 2) `shouldBe` Nothing,
                 fmap policyCosts (mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 16) (argon2Parallelism 2)) `shouldBe` Just (1, 16, 2),
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 127) (argon2Parallelism 16) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 11) (argon2MemoryKib 8) (argon2Parallelism 1) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 262145) (argon2Parallelism 1) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 128) (argon2Parallelism 17) `shouldBe` Nothing,
                 mkPasswordHashingPolicy (argon2Iterations 1) (argon2MemoryKib 0) (argon2Parallelism 536870912) `shouldBe` Nothing,
                 defaultPasswordHashingPolicy /= testPolicy `shouldBe` True
               ]
        )

  describe "PasswordWorkBudget" $ do
    it "rejects an empty capacity and preserves a validated capacity" $ do
      isNothing (mkPasswordWorkBudget 0) `shouldBe` True
      fmap passwordWorkBudgetKibibytes (mkPasswordWorkBudget 8) `shouldBe` Just 8
      passwordWorkBudgetKibibytes defaultPasswordWorkBudget `shouldBe` 524288

    it "fails overload immediately, then releases capacity after normal and asynchronous exits" $ do
      gate <- newPasswordWorkGate (required "password-work budget" (mkPasswordWorkBudget 8))
      started <- newEmptyMVar
      release <- newEmptyMVar
      completed <- newEmptyMVar
      _ <-
        forkIO $ do
          result <- withPasswordWork gate 8 (putMVar started () >> takeMVar release)
          putMVar completed result
      takeMVar started
      withPasswordWork gate 1 (pure ()) `shouldReturn` Nothing
      putMVar release ()
      timeout 1000000 (takeMVar completed) `shouldReturn` Just (Just ())
      failed <- try (withPasswordWork gate 8 (throwIO ThreadKilled)) :: IO (Either AsyncException (Maybe ()))
      failed `shouldBe` Left ThreadKilled
      withPasswordWork gate 8 (pure ()) `shouldReturn` Just ()

    it "caps CPU concurrency even when each valid hash has a small memory cost" $ do
      gate <- newPasswordWorkGate (required "password-work budget" (mkPasswordWorkBudget 1024))
      started <- newEmptyMVar
      release <- newEmptyMVar
      completed <- newEmptyMVar
      replicateM_ 8 $ do
        _ <- forkIO (withPasswordWork gate 1 (putMVar started () >> takeMVar release) >>= putMVar completed)
        pure ()
      replicateM_ 8 (takeMVar started)
      withPasswordWork gate 1 (pure ()) `shouldReturn` Nothing
      replicateM_ 8 (putMVar release ())
      replicateM_ 8 (timeout 1000000 (takeMVar completed) >>= (`shouldBe` Just (Just ())))
      withPasswordWork gate 1 (pure ()) `shouldReturn` Just ()

  describe "Argon2id password hashes" $ do
    it "renders the fixed login timing-defense hash" $ do
      let passwordHash = required "timing-defense hash" (hashPasswordWithSalt defaultPasswordHashingPolicy "0000000000000000" (mkPassword "login-existence-oracle-defense"))
      passwordHashText passwordHash `shouldBe` "$argon2id$v=19$m=65536,t=3,p=1$MDAwMDAwMDAwMDAwMDAwMA$nTQzDQsyrnF98d3p5wV9nHhxGtnnTCDElTqAkW2qVkk"

    it "encodes policy and salt, then verifies only the original password" $ do
      let passwordHash = required "password hash" (hashPasswordWithSalt testPolicy sampleSalt samplePassword)
      expectAll
        ( ( passwordHashText passwordHash
              `shouldBe` "$argon2id$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE"
          )
            :| [ passwordHashText (required "read password hash" (readPasswordHash (passwordHashText passwordHash)))
                   `shouldBe` passwordHashText passwordHash,
                 passwordHashWorkKibibytes passwordHash `shouldBe` Just 8,
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
                 isNothing (readPasswordHash "$argon2id$v=19$m=262145,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=19$m=8,t=11,p=1$MDEyMzQ1Njc4OWFiY2RlZg$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=19$m=128,t=1,p=17$MDEyMzQ1Njc4OWFiY2RlZg$XdTJKPvVpEdCNu922dgwQXj-XfOITS04PpG3cDMn5sE") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2i$v=19$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$2kTgJk6VdlkTLGO2AH5eQCP8qZAQH4CZAPyfIsyPXdo") `shouldBe` True,
                 isNothing (readPasswordHash "$argon2id$v=16$m=8,t=1,p=1$MDEyMzQ1Njc4OWFiY2RlZg$2kTgJk6VdlkTLGO2AH5eQCP8qZAQH4CZAPyfIsyPXdo") `shouldBe` True,
                 passwordHashWorkKibibytes (PasswordHash "not-a-password-hash") `shouldBe` Nothing,
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

policyCosts :: PasswordHashingPolicy -> (Word32, Word32, Word32)
policyCosts policy =
  ( passwordHashIterations policy,
    passwordHashMemoryKibibytes policy,
    passwordHashParallelism policy
  )
