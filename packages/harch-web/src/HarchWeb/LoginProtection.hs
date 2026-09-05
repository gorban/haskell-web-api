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
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds, unixTimeNanosecondsValue)

data LoginAttempt = LoginAttempt
  { loginAttemptAtNanoseconds :: UnixTimeNanoseconds,
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
  | LoginThrottledUntil UnixTimeNanoseconds
  deriving (Eq, Show)

data AuthenticationAuditEvent
  = AuthenticationSucceeded Text
  | AuthenticationFailed Text
  | AuthenticationThrottled Text UnixTimeNanoseconds
  deriving (Eq, Show)

newtype AuthenticationAuditSink = AuthenticationAuditSink
  { recordAuthenticationAuditEvent :: AuthenticationAuditEvent -> IO ()
  }

-- | Deliberately does not assume any particular ordering of 'attempts': an
-- earlier version relied on the caller passing them oldest-first (undocumented
-- except in the test suite) and used 'reverse' to find the newest failure,
-- so a caller that instead queried its store newest-first (an @ORDER BY …
-- DESC@, the natural way to write "recent attempts") would silently compute
-- a lockout expiry from the oldest failure instead of the newest, expiring
-- the lockout too early. Computing the newest timestamp with 'maximum'
-- instead makes the result correct for any input order.
evaluateLoginAttempt :: LoginProtectionPolicy -> UnixTimeNanoseconds -> [LoginAttempt] -> LoginProtectionResult
evaluateLoginAttempt policy now attempts =
  case recentFailureTimestamps of
    [] -> LoginPermitted
    _ ->
      if fromIntegral (length recentFailureTimestamps) < loginProtectionMaximumFailures policy
        then LoginPermitted
        else case addUnixTimeNanoseconds (maximum recentFailureTimestamps) (loginProtectionLockoutNanoseconds policy) of
          Nothing -> LoginThrottledUntil (maximum recentFailureTimestamps)
          Just lockoutEndsAt ->
            if now < lockoutEndsAt
              then LoginThrottledUntil lockoutEndsAt
              else LoginPermitted
  where
    recentFailureTimestamps =
      [ loginAttemptAtNanoseconds attempt
      | attempt <- attempts,
        not (loginAttemptSucceeded attempt),
        now >= loginAttemptAtNanoseconds attempt,
        unixTimeNanosecondsValue now - unixTimeNanosecondsValue (loginAttemptAtNanoseconds attempt) < loginProtectionWindowNanoseconds policy
      ]
