{-# LANGUAGE OverloadedStrings #-}

module Unit.App.Composed.CsrfSynchronizerSpec (spec) where

import App.Composed
import Data.IORef
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import HarchWeb.Csrf
  ( CsrfBindingResolution (..),
    CsrfIssuance (..),
    CsrfVerification (..),
    csrfBindingDigest,
    csrfBindingFromCanonicalBytes,
    csrfCookieMaxAgeSeconds,
    issueCsrfToken,
    verifyCsrfToken,
  )
import HarchWeb.Time (unixTimeNanoseconds)
import Postgres.DatabaseChange (DatabaseChange (..), DatabaseChangeError (DatabaseChangeQueryReturnedNoRows), DatabaseChangeExecutor (..), DatabaseChangeResult (DatabaseChangeCommandSucceeded))
import Test.Hspec

spec :: Spec
spec = describe "Unit.App.Composed.CsrfSynchronizer" $ do
  it "stores only a digest after deterministic cleanup and rechecks durable state" $ do
    cleanups <- newIORef (0 :: Int)
    saves <- newIORef (0 :: Int)
    verifications <- newIORef (0 :: Int)
    let store =
          SynchronizerTokenStore
            { saveSynchronizerToken = \_ _ _ _ -> modifyIORef' saves (+ 1) >> pure (Right True),
              verifySynchronizerToken = \_ _ _ -> modifyIORef' verifications (+ 1) >> pure (Right True),
              cleanupSynchronizerTokens = \_ -> modifyIORef' cleanups (+ 1) >> pure (Right ())
            }
        protection = synchronizerCsrfProtection store (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))
    issuance <- issueCsrfToken protection ()
    case issuance of
      CsrfTokenIssued token maxAge -> do
        csrfCookieMaxAgeSeconds maxAge `shouldBe` 3600
        verifyCsrfToken protection () token `shouldReturn` CsrfVerified
        verifyCsrfToken protection () token `shouldReturn` CsrfVerified
      CsrfProtectionUnavailable -> expectationFailure "expected bounded anonymous synchronizer token"
    readIORef cleanups `shouldReturn` 1
    readIORef saves `shouldReturn` 1
    readIORef verifications `shouldReturn` 2

  it "uses the active binding and its earliest deadline, and rejects expired state" $ do
    let binding = csrfBindingFromCanonicalBytes "admission-session-v1"
        activeStore =
          SynchronizerTokenStore
            { saveSynchronizerToken = \_ receivedBinding _ _ -> pure (Right (receivedBinding == csrfBindingDigest binding)),
              verifySynchronizerToken = \_ receivedBinding _ -> pure (Right (receivedBinding == csrfBindingDigest binding)),
              cleanupSynchronizerTokens = \_ -> pure (Right ())
            }
        activeProtection = synchronizerCsrfProtection activeStore (pure (unixTimeNanoseconds 100)) (const (pure (BoundCsrfBinding binding (unixTimeNanoseconds 5100000000))))
        expiredProtection = synchronizerCsrfProtection activeStore (pure (unixTimeNanoseconds 100)) (const (pure (BoundCsrfBinding binding (unixTimeNanoseconds 100))))
    issuance <- issueCsrfToken activeProtection ()
    case issuance of
      CsrfTokenIssued token maxAge -> do
        csrfCookieMaxAgeSeconds maxAge `shouldBe` 5
        verifyCsrfToken activeProtection () token `shouldReturn` CsrfVerified
        verifyCsrfToken expiredProtection () token `shouldReturn` CsrfRejected
      CsrfProtectionUnavailable -> expectationFailure "expected active bound token"

  it "fails closed on capacity, storage, or binding-resolution failure" $ do
    let capacityStore = testStore (Right False) (Right False) (Right ())
        unavailableStore = testStore (Left SynchronizerTokenStoreUnavailable) (Left SynchronizerTokenStoreUnavailable) (Left SynchronizerTokenStoreUnavailable)
        unavailableBinding = synchronizerCsrfProtection capacityStore (pure (unixTimeNanoseconds 100)) (const (pure CsrfBindingUnavailable))
        capacityProtection = synchronizerCsrfProtection capacityStore (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))
        unavailableProtection = synchronizerCsrfProtection unavailableStore (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))
    issueCsrfToken unavailableBinding () `shouldReturn` CsrfProtectionUnavailable
    issueCsrfToken capacityProtection () `shouldReturn` CsrfProtectionUnavailable
    issueCsrfToken unavailableProtection () `shouldReturn` CsrfProtectionUnavailable
    tokenIssuance <- issueCsrfToken (synchronizerCsrfProtection (testStore (Right True) (Left SynchronizerTokenStoreCorrupt) (Right ())) (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))) ()
    case tokenIssuance of
      CsrfTokenIssued token _ -> do
        verifyCsrfToken (synchronizerCsrfProtection unavailableStore (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))) () token `shouldReturn` CsrfVerificationUnavailable
        verifyCsrfToken (synchronizerCsrfProtection capacityStore (pure (unixTimeNanoseconds 100)) (const (pure CsrfBindingUnavailable))) () token `shouldReturn` CsrfVerificationUnavailable
      CsrfProtectionUnavailable -> expectationFailure "expected token for verification failure proof"
    issueCsrfToken (synchronizerCsrfProtection capacityStore (pure (unixTimeNanoseconds (maxBound :: Word64))) (const (pure AnonymousCsrfBinding))) () `shouldReturn` CsrfProtectionUnavailable

  it "keeps the admission and synchronizer schema in the composed application ledger" $ do
    let allSql = concatMap (NonEmpty.toList . databaseChangeStatements) composedDatabaseChanges
        executor = DatabaseChangeExecutor (const (pure (Right (Just DatabaseChangeCommandSucceeded))))
    allSql `shouldSatisfy` any (Text.isInfixOf "admission_credentials")
    runComposedDatabaseChangesWithExecutor executor `shouldReturn` Left DatabaseChangeQueryReturnedNoRows

  it "fails closed when the composed migration connection cannot be opened" $ do
    migrationResult <- runComposedDatabaseChanges (ComposedDatabaseConnectionString "host=127.0.0.1 port=1 connect_timeout=1")
    case migrationResult of
      Left _ -> pure ()
      Right () -> expectationFailure "an unreachable migration connection must not be reported as applied"

  it "uses parameterized PostgreSQL storage for cleanup, bounded issuance, and fresh verification" $ do
    calls <- newIORef ([] :: [(Text.Text, [Text.Text])])
    let runner _ sql parameters = do
          modifyIORef' calls (<> [(sql, parameters)])
          pure (Right [["true"] | "INSERT" `Text.isInfixOf` sql || "SELECT CASE" `Text.isInfixOf` sql])
        store = buildPostgresSynchronizerTokenStoreWithRunner defaultSynchronizerStoragePolicy runner ()
        protection = synchronizerCsrfProtection store (pure (unixTimeNanoseconds 100)) (const (pure AnonymousCsrfBinding))
    issuance <- issueCsrfToken protection ()
    case issuance of
      CsrfTokenIssued token _ -> verifyCsrfToken protection () token `shouldReturn` CsrfVerified
      CsrfProtectionUnavailable -> expectationFailure "expected parameterized synchronizer issuance"
    cleanupSynchronizerTokens store (unixTimeNanoseconds 200) `shouldReturn` Right ()
    recordedCalls <- readIORef calls
    map (length . snd) recordedCalls `shouldBe` [1, 5, 3, 1]
    case recordedCalls of
      [(_, cleanupParameters), (_, saveParameters), (_, verificationParameters), (cleanupSql, finalCleanupParameters)] -> do
        cleanupParameters `shouldBe` ["100"]
        drop 2 saveParameters `shouldBe` ["100", "3600000000100", "16"]
        drop 2 verificationParameters `shouldBe` ["100"]
        cleanupSql `shouldSatisfy` Text.isInfixOf "DELETE FROM composed.csrf_synchronizer_tokens"
        finalCleanupParameters `shouldBe` ["200"]
        all (Text.all (/= ' ')) (take 2 saveParameters <> take 2 verificationParameters) `shouldBe` True
      _ -> expectationFailure "expected cleanup, save, and verification queries"

  it "maps every durable-adapter result explicitly without storing bearer text" $ do
    let binding = csrfBindingFromCanonicalBytes "admission-session-v1"
        resultStore result =
          buildPostgresSynchronizerTokenStoreWithRunner
            defaultSynchronizerStoragePolicy
            (\_ _ _ -> pure result)
            ()
        resolver = const (pure (BoundCsrfBinding binding (unixTimeNanoseconds 5000000000)))
        activeProtection store = synchronizerCsrfProtection store (pure (unixTimeNanoseconds 100)) resolver
    issued <- issueCsrfToken (activeProtection (resultStore (Right [["true"]]))) ()
    token <-
      case issued of
        CsrfTokenIssued value _ -> pure value
        CsrfProtectionUnavailable -> expectationFailure "expected durable token" >> fail "unreachable"
    let captureDigest =
          SynchronizerTokenStore
            { saveSynchronizerToken = \digest _ _ _ -> do
                show digest `shouldBe` "SynchronizerTokenDigest <redacted>"
                pure (Right True),
              verifySynchronizerToken = \_ _ _ -> pure (Right False),
              cleanupSynchronizerTokens = \_ -> pure (Right ())
            }
    capturedIssuance <- issueCsrfToken (activeProtection captureDigest) ()
    case capturedIssuance of
      CsrfTokenIssued _ _ -> pure ()
      CsrfProtectionUnavailable -> expectationFailure "expected captured durable token"
    verifyCsrfToken (activeProtection captureDigest) () token `shouldReturn` CsrfRejected
    verifyCsrfToken (activeProtection (resultStore (Left "database unavailable"))) () token `shouldReturn` CsrfVerificationUnavailable
    verifyCsrfToken (activeProtection (resultStore (Right [["wrong"]]))) () token `shouldReturn` CsrfVerificationUnavailable
    issueCsrfToken (activeProtection (resultStore (Right [["false"]]))) () `shouldReturn` CsrfProtectionUnavailable
    issueCsrfToken (activeProtection (resultStore (Right [["wrong"]]))) () `shouldReturn` CsrfProtectionUnavailable
    issueCsrfToken (activeProtection (resultStore (Left "database unavailable"))) () `shouldReturn` CsrfProtectionUnavailable
    let expiredProtection = synchronizerCsrfProtection captureDigest (pure (unixTimeNanoseconds 100)) (const (pure (BoundCsrfBinding binding (unixTimeNanoseconds 100))))
    issueCsrfToken expiredProtection () `shouldReturn` CsrfProtectionUnavailable
    mkSynchronizerStoragePolicy 0 `shouldBe` Nothing
    mkSynchronizerStoragePolicy 1 `shouldBe` Just (requiredPolicy "synchronizer policy" (mkSynchronizerStoragePolicy 1))
  where
    requiredPolicy label = fromMaybe (error ("expected " <> label))

testStore :: Either SynchronizerTokenStoreError Bool -> Either SynchronizerTokenStoreError Bool -> Either SynchronizerTokenStoreError () -> SynchronizerTokenStore
testStore saveResult verificationResult cleanupResult =
  SynchronizerTokenStore
    { saveSynchronizerToken = \_ _ _ _ -> pure saveResult,
      verifySynchronizerToken = \_ _ _ -> pure verificationResult,
      cleanupSynchronizerTokens = \_ -> pure cleanupResult
    }
