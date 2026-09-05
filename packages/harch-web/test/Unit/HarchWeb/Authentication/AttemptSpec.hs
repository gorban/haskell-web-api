module Unit.HarchWeb.Authentication.AttemptSpec (spec) where

import Control.Exception (SomeException, try)
import Data.IORef
import HarchWeb.Authentication.Attempt
import Test.Hspec

data TestAdmissionError
  = ReserveFailed
  | SettleFailed
  | CancelFailed
  deriving (Eq, Show)

data TestLockout = TestLockout
  deriving (Eq, Show)

newtype TestReservation = TestReservation Int
  deriving (Eq, Show)

newtype TestBudget = TestBudget Int
  deriving (Eq, Show)

testBudget :: TestBudget
testBudget = TestBudget 3

spec :: Spec
spec = describe "HarchWeb.Authentication.Attempt" $ do
  it "maps store rejection and throttling without running proof work" $ do
    rejected <- runAdmittedAttempt rejectedStore testBudget throttlingResult storeFailureResult (error "proof work must not run")
    throttled <- runAdmittedAttempt throttledStore testBudget throttlingResult storeFailureResult (error "proof work must not run")
    rejected `shouldBe` "unavailable"
    throttled `shouldBe` "throttled"

  it "settles determinate work, cancels indeterminate work, and interprets store failures" $ do
    settled <- newIORef []
    cancelled <- newIORef []
    let successfulStore = recordingStore settled cancelled (Right ()) (Right ())
        failedSettlementStore = recordingStore settled cancelled (Left SettleFailed) (Right ())
        failedCancellationStore = recordingStore settled cancelled (Right ()) (Left CancelFailed)
    accepted <- runAdmittedAttempt successfulStore testBudget throttlingResult storeFailureResult (pure ("accepted", Just True))
    indeterminate <- runAdmittedAttempt successfulStore testBudget throttlingResult storeFailureResult (pure ("indeterminate", Nothing))
    settlementFailure <- runAdmittedAttempt failedSettlementStore testBudget throttlingResult storeFailureResult (pure ("accepted", Just False))
    cancellationFailure <- runAdmittedAttempt failedCancellationStore testBudget throttlingResult storeFailureResult (pure ("indeterminate", Nothing))
    accepted `shouldBe` "accepted"
    indeterminate `shouldBe` "indeterminate"
    settlementFailure `shouldBe` "unavailable"
    cancellationFailure `shouldBe` "unavailable"
    readIORef settled `shouldReturn` [(TestReservation 1, True), (TestReservation 1, False)]
    readIORef cancelled `shouldReturn` [TestReservation 1, TestReservation 1, TestReservation 1]

  it "cancels a reserved attempt when interruptible proof work throws" $ do
    cancelled <- newIORef []
    let store = recordingStore (error "settlement must not run") cancelled (Right ()) (Right ())
    result <- try (runAdmittedAttempt store testBudget throttlingResult storeFailureResult (ioError (userError "interrupted"))) :: IO (Either SomeException String)
    result `shouldSatisfy` either (const True) (const False)
    readIORef cancelled `shouldReturn` [TestReservation 1]

  it "cancels a reservation when interruptible settlement throws" $ do
    cancelled <- newIORef []
    let store =
          AttemptReservationStore
            { reserveAttempt = checkedReserve (pure (Right (AttemptReserved (TestReservation 1)))),
              settleAttempt = \_ _ -> ioError (userError "settlement interrupted"),
              cancelAttempt = \reservation -> modifyIORef' cancelled (<> [reservation]) >> pure (Right ())
            }
    result <- try (runAdmittedAttempt store testBudget throttlingResult storeFailureResult (pure ("accepted", Just True))) :: IO (Either SomeException String)
    result `shouldSatisfy` either (const True) (const False)
    readIORef cancelled `shouldReturn` [TestReservation 1]

rejectedStore :: AttemptReservationStore TestBudget TestReservation TestLockout TestAdmissionError
rejectedStore =
  AttemptReservationStore
    { reserveAttempt = checkedReserve (pure (Left ReserveFailed)),
      settleAttempt = \_ _ -> pure (Right ()),
      cancelAttempt = \_ -> pure (Right ())
    }

throttledStore :: AttemptReservationStore TestBudget TestReservation TestLockout TestAdmissionError
throttledStore =
  AttemptReservationStore
    { reserveAttempt = checkedReserve (pure (Right (AttemptThrottled TestLockout))),
      settleAttempt = \_ _ -> pure (Right ()),
      cancelAttempt = \_ -> pure (Right ())
    }

recordingStore :: IORef [(TestReservation, Bool)] -> IORef [TestReservation] -> Either TestAdmissionError () -> Either TestAdmissionError () -> AttemptReservationStore TestBudget TestReservation TestLockout TestAdmissionError
recordingStore settled cancelled settlementResult cancellationResult =
  AttemptReservationStore
    { reserveAttempt = checkedReserve (pure (Right (AttemptReserved (TestReservation 1)))),
      settleAttempt = \reservation succeeded -> do
        modifyIORef' settled (<> [(reservation, succeeded)])
        pure settlementResult,
      cancelAttempt = \reservation -> do
        modifyIORef' cancelled (<> [reservation])
        pure cancellationResult
    }

checkedReserve :: IO value -> TestBudget -> IO value
checkedReserve result budget = do
  budget `shouldBe` testBudget
  result

throttlingResult :: TestLockout -> String
throttlingResult TestLockout = "throttled"

storeFailureResult :: TestAdmissionError -> String
storeFailureResult storeError =
  case storeError of
    ReserveFailed -> "unavailable"
    SettleFailed -> "unavailable"
    CancelFailed -> "unavailable"
