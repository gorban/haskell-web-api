-- | Storage-neutral, cancellation-safe lifecycle for a grouped authentication
-- attempt reservation.
--
-- An application owns the typed attempt scopes, durable adapter, retention,
-- and public result mapping. Harch owns only the hand-off protocol common to
-- any authentication proof: reserve before interruptible work, settle a known
-- outcome, cancel an indeterminate outcome, and cancel again if settlement
-- fails. This extends the existing authentication boundary rather than making
-- a framework store or assuming account identifiers.
module HarchWeb.Authentication.Attempt
  ( AttemptAdmission (..),
    AttemptReservationStore (..),
    runAdmittedAttempt,
  )
where

import Control.Exception (mask, onException)
import Control.Monad (void)

-- | The only successful outcomes of asking an application store to admit a
-- grouped attempt. The application supplies the lockout-time representation;
-- the lifecycle never renders or logs it.
data AttemptAdmission reservation lockout
  = AttemptReserved reservation
  | AttemptThrottled lockout

-- | An application-selected durable reservation capability. A reservation
-- represents every scoped budget in one atomic group, so a caller cannot
-- accidentally settle only its principal or peer component.
data AttemptReservationStore budget reservation lockout storeError = AttemptReservationStore
  { reserveAttempt :: budget -> IO (Either storeError (AttemptAdmission reservation lockout)),
    settleAttempt :: reservation -> Bool -> IO (Either storeError ()),
    cancelAttempt :: reservation -> IO (Either storeError ())
  }

-- | Run one admitted proof attempt with cancellation-safe ownership transfer.
-- The application supplies its typed result constructors, allowing expected
-- throttling and store failure to remain ordinary outcomes rather than
-- exceptions. Work remains interruptible; an asynchronous exception cancels
-- its reservation best-effort, leaving process-loss recovery to the store's
-- explicit retention policy.
runAdmittedAttempt ::
  AttemptReservationStore budget reservation lockout storeError ->
  budget ->
  (lockout -> result) ->
  (storeError -> result) ->
  IO (result, Maybe Bool) ->
  IO result
runAdmittedAttempt store budget throttled storeFailure work =
  mask $ \restore -> do
    admissionResult <- restore (reserveAttempt store budget)
    case admissionResult of
      Left storeError -> pure (storeFailure storeError)
      Right (AttemptThrottled lockout) -> pure (throttled lockout)
      Right (AttemptReserved reservation) -> do
        (result, settlement) <- restore work `onException` discardReservation reservation
        case settlement of
          Nothing -> cancelOrFail reservation result
          Just succeeded -> settleOrFail restore reservation succeeded result
  where
    discardReservation reservation = void (cancelAttempt store reservation)
    cancelOrFail reservation result = do
      cancellationResult <- cancelAttempt store reservation
      pure (either storeFailure (const result) cancellationResult)
    settleOrFail restore reservation succeeded result = do
      settlementResult <- restore (settleAttempt store reservation succeeded) `onException` discardReservation reservation
      case settlementResult of
        Right () -> pure result
        Left storeError -> do
          discardReservation reservation
          pure (storeFailure storeError)
