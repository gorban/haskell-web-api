module WebApi.Login.Attempt
  ( runAdmittedLoginAttempt,
  )
where

import HarchWeb.Authentication.Attempt qualified as Attempt
import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.Login.Types
  ( LoginAttemptAdmission (..),
    LoginAttemptBudgets,
    LoginAttemptStore (..),
    LoginAttemptStoreError,
    LoginThrottleContext (..),
  )

-- | A reservation is a durable provisional failure until it is settled or
-- cancelled. The outer mask makes ownership handoffs atomic, while restored
-- database and password/MFA work remains interruptible. If compensation is
-- itself interrupted or the process exits, the store's retention policy owns
-- recovery; this function does not claim crash-proof cleanup.
runAdmittedLoginAttempt ::
  LoginThrottleContext ->
  LoginAttemptBudgets ->
  (UnixTimeNanoseconds -> result) ->
  (LoginAttemptStoreError -> result) ->
  IO (result, Maybe Bool) ->
  IO result
runAdmittedLoginAttempt throttle =
  Attempt.runAdmittedAttempt attemptStore
  where
    store = loginThrottleStore throttle
    now = loginThrottleNow throttle
    attemptStore =
      Attempt.AttemptReservationStore
        { Attempt.reserveAttempt = \attemptBudgets ->
            fmap (fmap mapAdmission) (reserveLoginAttempt store attemptBudgets now),
          Attempt.settleAttempt = settleLoginAttempt store,
          Attempt.cancelAttempt = cancelLoginAttempt store
        }
    mapAdmission admission =
      case admission of
        LoginAttemptReserved reservation -> Attempt.AttemptReserved reservation
        LoginAttemptThrottled lockoutEndsAt -> Attempt.AttemptThrottled lockoutEndsAt
