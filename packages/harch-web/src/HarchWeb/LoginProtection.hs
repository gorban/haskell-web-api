module HarchWeb.LoginProtection
  ( AuthenticationAuditEvent (..),
    AuthenticationAuditSink (..),
    LoginAttempt (..),
    LoginProtectionPolicy (..),
    LoginProtectionResult (..),
    defaultLoginProtectionPolicy,
    evaluateLoginAttempt,
  )
where

import Data.Text (Text)
import Data.Word (Word64)

data LoginAttempt = LoginAttempt
  { loginAttemptAtNanoseconds :: Word64,
    loginAttemptSucceeded :: Bool
  }
  deriving (Eq, Show)

data LoginProtectionPolicy = LoginProtectionPolicy
  { loginProtectionMaximumFailures :: Word64,
    loginProtectionWindowNanoseconds :: Word64,
    loginProtectionLockoutNanoseconds :: Word64
  }
  deriving (Eq, Show)

defaultLoginProtectionPolicy :: LoginProtectionPolicy
defaultLoginProtectionPolicy = LoginProtectionPolicy 5 900000000000 900000000000

data LoginProtectionResult
  = LoginPermitted
  | LoginThrottledUntil Word64
  deriving (Eq, Show)

data AuthenticationAuditEvent
  = AuthenticationSucceeded Text
  | AuthenticationFailed Text
  | AuthenticationThrottled Text Word64
  deriving (Eq, Show)

newtype AuthenticationAuditSink = AuthenticationAuditSink
  { recordAuthenticationAuditEvent :: AuthenticationAuditEvent -> IO ()
  }

evaluateLoginAttempt :: LoginProtectionPolicy -> Word64 -> [LoginAttempt] -> LoginProtectionResult
evaluateLoginAttempt policy now attempts =
  case reverse [loginAttemptAtNanoseconds attempt | attempt <- attempts, not (loginAttemptSucceeded attempt), now >= loginAttemptAtNanoseconds attempt, now - loginAttemptAtNanoseconds attempt < loginProtectionWindowNanoseconds policy] of
    [] -> LoginPermitted
    newestFailure : olderFailures ->
      if fromIntegral (length olderFailures + 1) < loginProtectionMaximumFailures policy
        then LoginPermitted
        else
          let lockoutEndsAt = newestFailure + loginProtectionLockoutNanoseconds policy
           in if now < lockoutEndsAt
                then LoginThrottledUntil lockoutEndsAt
                else LoginPermitted
