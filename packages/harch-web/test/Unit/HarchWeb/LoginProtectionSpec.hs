{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.LoginProtectionSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.LoginProtection
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

policy :: LoginProtectionPolicy
policy = LoginProtectionPolicy 2 100 50

spec :: Spec
spec = do
  describe "LoginProtectionPolicy" $ do
    it "has a secure default and exposes stable diagnostics" $ do
      expectAll
        ( (defaultLoginProtectionPolicy `shouldBe` LoginProtectionPolicy 5 900000000000 900000000000)
            :| [ defaultLoginProtectionPolicy /= policy `shouldBe` True,
                 show defaultLoginProtectionPolicy `shouldBe` "LoginProtectionPolicy {loginProtectionMaximumFailures = 5, loginProtectionWindowNanoseconds = 900000000000, loginProtectionLockoutNanoseconds = 900000000000}",
                 show [defaultLoginProtectionPolicy] `shouldContain` "LoginProtectionPolicy"
               ]
        )

  describe "evaluateLoginAttempt" $ do
    it "permits requests below the failure threshold and ignores successful or expired attempts" $ do
      expectAll
        ( (evaluateLoginAttempt policy 100 [] `shouldBe` LoginPermitted)
            :| [ evaluateLoginAttempt policy 100 [LoginAttempt 99 False] `shouldBe` LoginPermitted,
                 evaluateLoginAttempt policy 100 [LoginAttempt 99 True, LoginAttempt 0 False] `shouldBe` LoginPermitted,
                 evaluateLoginAttempt policy 100 [LoginAttempt 101 False] `shouldBe` LoginPermitted,
                 LoginAttempt 1 False /= LoginAttempt 1 True `shouldBe` True,
                 show (LoginAttempt 1 False) `shouldBe` "LoginAttempt {loginAttemptAtNanoseconds = 1, loginAttemptSucceeded = False}",
                 show [LoginAttempt 1 False] `shouldContain` "LoginAttempt"
               ]
        )

    it "throttles at the failure threshold until the newest relevant failure expires" $ do
      expectAll
        ( (evaluateLoginAttempt policy 100 [LoginAttempt 60 False, LoginAttempt 90 False] `shouldBe` LoginThrottledUntil 140)
            :| [ evaluateLoginAttempt policy 141 [LoginAttempt 60 False, LoginAttempt 90 False] `shouldBe` LoginPermitted,
                 evaluateLoginAttempt policy 161 [LoginAttempt 60 False, LoginAttempt 90 False] `shouldBe` LoginPermitted,
                 LoginPermitted /= LoginThrottledUntil 140 `shouldBe` True,
                 show (LoginThrottledUntil 140) `shouldBe` "LoginThrottledUntil 140",
                 show [LoginPermitted, LoginThrottledUntil 140] `shouldBe` "[LoginPermitted,LoginThrottledUntil 140]"
               ]
        )

    it "computes the same lockout regardless of whether the caller passes attempts oldest-first or newest-first" $ do
      expectAll
        ( (evaluateLoginAttempt policy 100 [LoginAttempt 90 False, LoginAttempt 60 False] `shouldBe` LoginThrottledUntil 140)
            :| [ evaluateLoginAttempt policy 100 [LoginAttempt 60 False, LoginAttempt 90 False] `shouldBe` LoginThrottledUntil 140,
                 evaluateLoginAttempt policy 100 [LoginAttempt 90 False, LoginAttempt 20 False, LoginAttempt 60 False] `shouldBe` LoginThrottledUntil 140
               ]
        )

  describe "AuthenticationAuditSink" $ do
    it "leaves audit delivery application-owned" $ do
      events <- newIORef []
      let sink =
            AuthenticationAuditSink
              ( \event -> do
                  recordedEvents <- readIORef events
                  writeIORef events (event : recordedEvents)
              )
      recordAuthenticationAuditEvent sink (AuthenticationFailed "account@example.test")
      recordAuthenticationAuditEvent sink (AuthenticationThrottled "account@example.test" 140)
      recordAuthenticationAuditEvent sink (AuthenticationSucceeded "account@example.test")
      recordedEvents <- readIORef events
      recordedEvents
        `shouldBe` [AuthenticationSucceeded "account@example.test", AuthenticationThrottled "account@example.test" 140, AuthenticationFailed "account@example.test"]
      AuthenticationSucceeded "account@example.test" /= AuthenticationFailed "account@example.test" `shouldBe` True
      show (AuthenticationThrottled "account@example.test" 140) `shouldBe` "AuthenticationThrottled \"account@example.test\" 140"
      show [AuthenticationSucceeded "account@example.test"] `shouldBe` "[AuthenticationSucceeded \"account@example.test\"]"
