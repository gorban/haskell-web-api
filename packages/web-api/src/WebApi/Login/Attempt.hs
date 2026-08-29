module WebApi.Login.Attempt
  ( runAdmittedLoginAttempt,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (void)
import Data.Text (Text)
import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.Login.Types
  ( LoginAttemptAdmission (..),
    LoginAttemptReservation,
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
  Text ->
  (UnixTimeNanoseconds -> result) ->
  (LoginAttemptStoreError -> result) ->
  IO (result, Maybe Bool) ->
  IO result
runAdmittedLoginAttempt throttle key throttled storeFailure work =
  mask $ \restore -> do
    admissionResult <- restore (reserveLoginAttempt store key policy now)
    case admissionResult of
      Left storeError -> pure (storeFailure storeError)
      Right (LoginAttemptThrottled lockoutEndsAt) -> pure (throttled lockoutEndsAt)
      Right (LoginAttemptReserved reservation) -> do
        (result, settlement) <- restore work `onException` discardReservation reservation
        case settlement of
          Nothing -> cancelOrFail reservation result
          Just succeeded -> settleOrFail restore reservation succeeded result
  where
    store = loginThrottleStore throttle
    policy = loginThrottlePolicy throttle
    now = loginThrottleNow throttle
    discardReservation :: LoginAttemptReservation -> IO ()
    discardReservation reservation = void (cancelLoginAttempt store reservation)
    cancelOrFail reservation result = do
      cancelResult <- cancelLoginAttempt store reservation
      pure (either storeFailure (const result) cancelResult)
    settleOrFail restore reservation succeeded result = do
      settleResult <- restore (settleLoginAttempt store reservation succeeded) `onException` discardReservation reservation
      case settleResult of
        Right () -> pure result
        Left storeError -> do
          -- Cancellation only deletes still-unsettled rows, so this recovers
          -- the ordinary failed write without undoing a committed settlement.
          discardReservation reservation
          pure (storeFailure storeError)
