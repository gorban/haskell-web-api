{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString qualified as ByteString
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Postgres.DatabaseChange

spec = do
  describe "Postgres.DatabaseChange" $ do
    it "validates bounded IDs and ledger identifiers before SQL construction" $ do
      expectAll
        ( (mkDatabaseChangeId "initial-schema" `shouldBe` Right (requiredId "initial-schema"))
            :| [ mkDatabaseChangeId "a_b.c-2" `shouldBe` Right (requiredId "a_b.c-2"),
                 mkDatabaseChangeId (Text.replicate 200 "a") `shouldBe` Right (requiredId (Text.replicate 200 "a")),
                 mkDatabaseChangeId "" `shouldBe` Left DatabaseChangeInvalidId,
                 mkDatabaseChangeId (Text.replicate 201 "a") `shouldBe` Left DatabaseChangeInvalidId,
                 mkDatabaseChangeId "spaces are not stable IDs" `shouldBe` Left DatabaseChangeInvalidId,
                 mkDatabaseChangeLedger "web_api" "database_changes" (Just "schema_migrations") 12
                   `shouldBe` Right (requiredLedgerWithLock "web_api" "database_changes" (Just "schema_migrations") 12),
                 mkDatabaseChangeLedger "_web_api" "database_changes" Nothing 12
                   `shouldBe` Right (requiredLedgerWithLock "_web_api" "database_changes" Nothing 12),
                 mkDatabaseChangeLedger "" "database_changes" Nothing 12 `shouldBe` Left DatabaseChangeInvalidLedger,
                 mkDatabaseChangeLedger "web-api" "database_changes" Nothing 12 `shouldBe` Left DatabaseChangeInvalidLedger,
                 mkDatabaseChangeLedger "web_api" (Text.replicate 41 "a") Nothing 12 `shouldBe` Left DatabaseChangeInvalidLedger
               ]
        )

    it "keeps public values comparable and diagnostic without exposing SQL payloads" $ do
      let changeId = requiredId "first"
          databaseChange = change "first" ["SELECT 1;"]
          executorError = DatabaseChangeExecutorError
          rows = DatabaseChangeRows [[encode "first"]]
          failure = DatabaseChangeDigestMismatch changeId
          allFailures =
            [ DatabaseChangeInvalidId,
              DatabaseChangeInvalidLedger,
              DatabaseChangeDuplicateId changeId,
              DatabaseChangeExecutionFailed,
              DatabaseChangeCommandReturnedRows,
              DatabaseChangeQueryReturnedNoRows,
              DatabaseChangeMalformedLedgerRow,
              DatabaseChangeUnknownRecordedId changeId,
              failure,
              DatabaseChangeOutOfOrder changeId
            ]
      expectAll
        ( (compare changeId (requiredId "second") `shouldBe` LT)
            :| [ changeId < requiredId "second" `shouldBe` True,
                 changeId <= changeId `shouldBe` True,
                 requiredId "second" > changeId `shouldBe` True,
                 requiredId "second" >= requiredId "second" `shouldBe` True,
                 min changeId (requiredId "second") `shouldBe` changeId,
                 max changeId (requiredId "second") `shouldBe` requiredId "second",
                 databaseChange /= change "second" ["SELECT 1;"] `shouldBe` True,
                 testLedger /= requiredLedger "other_schema" "database_changes" (Just "schema_migrations") `shouldBe` True,
                 hasDerivedContract [executorError] `shouldBe` True,
                 rows /= DatabaseChangeCommandSucceeded `shouldBe` True,
                 failure /= DatabaseChangeInvalidId `shouldBe` True,
                 show changeId `shouldBe` "DatabaseChangeId \"first\"",
                 showsPrec 11 changeId "" `shouldBe` "(DatabaseChangeId \"first\")",
                 show [changeId] `shouldBe` "[DatabaseChangeId \"first\"]",
                 show databaseChange `shouldBe` "DatabaseChange {databaseChangeId = DatabaseChangeId \"first\", databaseChangeStatements = \"SELECT 1;\" :| []}",
                 showsPrec 11 databaseChange "" `shouldContain` "(DatabaseChange {",
                 show [databaseChange] `shouldContain` "[DatabaseChange {",
                 show testLedger `shouldSatisfy` (Text.isInfixOf "DatabaseChangeLedger" . Text.pack),
                 showsPrec 11 testLedger "" `shouldContain` "(DatabaseChangeLedger",
                 show [testLedger] `shouldContain` "[DatabaseChangeLedger",
                 show executorError `shouldBe` "DatabaseChangeExecutorError",
                 showsPrec 11 executorError "" `shouldBe` "DatabaseChangeExecutorError",
                 show [executorError] `shouldBe` "[DatabaseChangeExecutorError]",
                 show rows `shouldSatisfy` (Text.isInfixOf "DatabaseChangeRows" . Text.pack),
                 showsPrec 11 rows "" `shouldContain` "(DatabaseChangeRows",
                 show [rows] `shouldContain` "[DatabaseChangeRows",
                 show failure `shouldBe` "DatabaseChangeDigestMismatch (DatabaseChangeId \"first\")",
                 showsPrec 11 failure "" `shouldBe` "(DatabaseChangeDigestMismatch (DatabaseChangeId \"first\"))",
                 all (\value -> value == value) allFailures `shouldBe` True,
                 show allFailures
                   `shouldBe` "[DatabaseChangeInvalidId,DatabaseChangeInvalidLedger,DatabaseChangeDuplicateId (DatabaseChangeId \"first\"),DatabaseChangeExecutionFailed,DatabaseChangeCommandReturnedRows,DatabaseChangeQueryReturnedNoRows,DatabaseChangeMalformedLedgerRow,DatabaseChangeUnknownRecordedId (DatabaseChangeId \"first\"),DatabaseChangeDigestMismatch (DatabaseChangeId \"first\"),DatabaseChangeOutOfOrder (DatabaseChangeId \"first\")]"
               ]
        )

    it "quotes mixed-case ledger names consistently during legacy cutover" $ do
      recordedSqlReference <- newIORef []
      let ledger = requiredLedger "AuditSchema" "DatabaseChanges" (Just "SchemaMigrations")
      runDatabaseChangesWithExecutor (recordingExecutor recordedSqlReference [] Nothing) ledger [] [] `shouldReturn` Right ()
      readIORef recordedSqlReference
        >>= (`shouldSatisfy` any (Text.isInfixOf "to_regclass('\"AuditSchema\".\"SchemaMigrations\"')"))

    it "hashes the ordered exact UTF-8 statements rather than normalized SQL" $ do
      databaseChangeDigest (change "first" ["SELECT 1;", "SELECT 2;"])
        `shouldNotBe` databaseChangeDigest (change "first" ["SELECT 2;", "SELECT 1;"])
      databaseChangeDigest (change "first" ["SELECT 1;"])
        `shouldNotBe` databaseChangeDigest (change "first" [" SELECT 1;"])
      databaseChangeDigest (change "first" ["SELECT 1;"])
        `shouldBe` databaseChangeDigest (change "second" ["SELECT 1;"])

    it "applies a contiguous plan, records immutable digests, and runs deployment reconciliation in the same transaction" $ do
      recordedSqlReference <- newIORef []
      let changes = [change "first" ["CREATE TABLE example (id INTEGER);"], change "second" ["INSERT INTO example VALUES (1);"]]
          executor = recordingExecutor recordedSqlReference [] Nothing
      runDatabaseChangesWithExecutor executor testLedger changes ["GRANT SELECT ON example TO runtime;"] `shouldReturn` Right ()
      recordedSql <- readIORef recordedSqlReference
      take 2 recordedSql `shouldBe` ["BEGIN;", "SELECT pg_advisory_xact_lock(782476311);"]
      all (`elem` recordedSql) ["CREATE TABLE example (id INTEGER);", "INSERT INTO example VALUES (1);", "GRANT SELECT ON example TO runtime;", "COMMIT;"] `shouldBe` True
      recordedSql `shouldSatisfy` any (Text.isInfixOf "INSERT INTO \"web_api\".\"database_changes\" (change_id, change_order, sql_digest) VALUES ('first', 1")
      recordedSql `shouldSatisfy` any (Text.isInfixOf "'second', 2")

    it "accepts an unchanged contiguous ledger without reapplying statements" $ do
      recordedSqlReference <- newIORef []
      let first = change "first" ["SELECT 1;"]
          second = change "second" ["SELECT 2;"]
          changes = [first, second]
          rows = [ledgerRow first 1, ledgerRow second 2]
      runDatabaseChangesWithExecutor (recordingExecutor recordedSqlReference rows Nothing) testLedger changes [] `shouldReturn` Right ()
      readIORef recordedSqlReference >>= \recordedSql -> do
        recordedSql `shouldNotContain` ["SELECT 1;", "SELECT 2;"]
        recordedSql `shouldNotContain` ["ROLLBACK;"]
        last recordedSql `shouldBe` "COMMIT;"

    it "validates constructed records before BEGIN and keeps a no-legacy ledger on the normal path" $ do
      invalidPlanSqlReference <- newIORef []
      let invalidChange = changeWithId (DatabaseChangeId "not a stable id") ["SELECT 1;"]
      runDatabaseChangesWithExecutor (recordingExecutor invalidPlanSqlReference [] Nothing) testLedger [invalidChange] []
        `shouldReturn` Left DatabaseChangeInvalidId
      readIORef invalidPlanSqlReference `shouldReturn` []
      duplicatePlanSqlReference <- newIORef []
      let duplicate = change "duplicate" ["SELECT 1;"]
      runDatabaseChangesWithExecutor (recordingExecutor duplicatePlanSqlReference [] Nothing) testLedger [duplicate, duplicate] []
        `shouldReturn` Left (DatabaseChangeDuplicateId (requiredId "duplicate"))
      readIORef duplicatePlanSqlReference `shouldReturn` []
      invalidLedgerSqlReference <- newIORef []
      let invalidLedger = testLedger {databaseChangeLedgerTable = "not-a-table"}
      runDatabaseChangesWithExecutor (recordingExecutor invalidLedgerSqlReference [] Nothing) invalidLedger [] []
        `shouldReturn` Left DatabaseChangeInvalidLedger
      readIORef invalidLedgerSqlReference `shouldReturn` []
      noLegacySqlReference <- newIORef []
      runDatabaseChangesWithExecutor (recordingExecutor noLegacySqlReference [] Nothing) (requiredLedger "web_api" "database_changes" Nothing) [] []
        `shouldReturn` Right ()
      readIORef noLegacySqlReference >>= (`shouldNotSatisfy` any (Text.isInfixOf "to_regclass"))

    it "rejects unknown, modified, missing, and out-of-order ledger records without running application SQL" $ do
      let changes = [change "first" ["SELECT 1;"], change "second" ["SELECT 2;"]]
          second = changes !! 1
      assertRejected changes [[encode "other", encode "1", encode "digest"]] (DatabaseChangeUnknownRecordedId (requiredId "other"))
      assertRejected changes [[encode "first", encode "1", encode "wrong"]] (DatabaseChangeDigestMismatch (requiredId "first"))
      assertRejected changes [ledgerRow second 2] (DatabaseChangeOutOfOrder (requiredId "first"))
      assertRejected changes [ledgerRow second 1] (DatabaseChangeOutOfOrder (requiredId "second"))

    it "rejects malformed ledger rows before application SQL" $ do
      let changes = [change "first" ["SELECT 1;"]]
      mapM_
        (\rows -> assertRejected changes rows DatabaseChangeMalformedLedgerRow)
        [ [[]],
          [[Nothing, encode "1", encode "digest"]],
          [[encode "first", encode "0", encode "digest"]],
          [[encode "first", encode "not-an-order", encode "digest"]],
          [[Just (ByteString.pack [255]), encode "1", encode "digest"]]
        ]
      assertRejected changes [[encode "not a stable id", encode "1", encode "digest"]] DatabaseChangeInvalidId

    it "rolls back every failure after BEGIN while preserving a failed BEGIN as a non-transaction" $ do
      let oneChange = [change "first" ["SELECT 1;"]]
      assertFailure oneChange "BEGIN;" DatabaseChangeCommandReturnedRows ["BEGIN;"]
      assertFailure oneChange "SELECT pg_advisory_xact_lock(782476311);" DatabaseChangeQueryReturnedNoRows ["BEGIN;", "SELECT pg_advisory_xact_lock(782476311);", "ROLLBACK;"]
      assertFailure oneChange "SELECT 1;" DatabaseChangeExecutionFailed []

    it "distinguishes command and row protocol shapes at every repository-owned boundary" $ do
      commandQuerySqlReference <- newIORef []
      runDatabaseChangesWithExecutor (commandForLedgerQueryExecutor commandQuerySqlReference) testLedger [] []
        `shouldReturn` Left DatabaseChangeQueryReturnedNoRows
      readIORef commandQuerySqlReference >>= \recordedSql -> do
        recordedSql `shouldSatisfy` any (Text.isPrefixOf "SELECT change_id, change_order::TEXT, sql_digest FROM ")
        last recordedSql `shouldBe` "ROLLBACK;"
      rowsCommandSqlReference <- newIORef []
      runDatabaseChangesWithExecutor (rowsForSchemaCommandExecutor rowsCommandSqlReference) testLedger [] []
        `shouldReturn` Left DatabaseChangeCommandReturnedRows
      readIORef rowsCommandSqlReference `shouldReturn` ["BEGIN;", "SELECT pg_advisory_xact_lock(782476311);", "CREATE SCHEMA IF NOT EXISTS \"web_api\";", "ROLLBACK;"]

    it "maps adapter failures and unavailable direct libpq connections to the safe execution error" $ do
      let adapterFailure = DatabaseChangeExecutor (const (pure (Left DatabaseChangeExecutorError)))
      runDatabaseChangesWithExecutor adapterFailure testLedger [] [] `shouldReturn` Left DatabaseChangeExecutionFailed
      runDatabaseChanges (DatabaseChangeConnectionString "host=127.0.0.1 port=1 connect_timeout=1") testLedger [] [] `shouldReturn` Left DatabaseChangeExecutionFailed

    it "adapts real PostgreSQL command and row results inside one owned transaction" $ do
      let ledger = requiredLedger "database_change_adapter_test" "changes" Nothing
          connection = DatabaseChangeConnectionString "host=127.0.0.1 port=5432 dbname=web_api_dev user=web_api_owner password=web_api_owner"
          changes = [change "adapter-path" ["CREATE TABLE database_change_adapter_test.example (id INTEGER);"]]
      runDatabaseChanges connection ledger changes ["DROP SCHEMA database_change_adapter_test CASCADE;"] `shouldReturn` Right ()
      runDatabaseChanges connection ledger [] ["DO $$ BEGIN RAISE EXCEPTION 'expected adapter failure'; END $$;"] `shouldReturn` Left DatabaseChangeExecutionFailed

testLedger :: DatabaseChangeLedger
testLedger = requiredLedger "web_api" "database_changes" (Just "schema_migrations")

requiredLedger :: Text.Text -> Text.Text -> Maybe Text.Text -> DatabaseChangeLedger
requiredLedger schemaName tableName legacyTable = requiredLedgerWithLock schemaName tableName legacyTable 782476311

requiredLedgerWithLock :: Text.Text -> Text.Text -> Maybe Text.Text -> Integer -> DatabaseChangeLedger
requiredLedgerWithLock schemaName tableName legacyTable lockId =
  case mkDatabaseChangeLedger schemaName tableName legacyTable lockId of
    Right ledger -> ledger
    Left failure -> error (show failure)

change :: Text.Text -> [Text.Text] -> DatabaseChange
change changeId = changeWithId (requiredId changeId)

changeWithId :: DatabaseChangeId -> [Text.Text] -> DatabaseChange
changeWithId changeId statements =
  DatabaseChange
    { databaseChangeId = changeId,
      databaseChangeStatements = requiredStatements statements
    }

requiredId :: Text.Text -> DatabaseChangeId
requiredId value =
  case mkDatabaseChangeId value of
    Right changeId -> changeId
    Left failure -> error (show failure)

requiredStatements :: [Text.Text] -> NonEmpty Text.Text
requiredStatements statements =
  case statements of
    first : rest -> first :| rest
    [] -> error "Expected a non-empty database change"

ledgerRow :: DatabaseChange -> Integer -> [Maybe ByteString.ByteString]
ledgerRow databaseChange order =
  [ encode (databaseChangeIdText (databaseChangeId databaseChange)),
    encode (Text.pack (show order)),
    encode (databaseChangeDigest databaseChange)
  ]

encode :: Text.Text -> Maybe ByteString.ByteString
encode = Just . TextEncoding.encodeUtf8

assertRejected :: [DatabaseChange] -> [[Maybe ByteString.ByteString]] -> DatabaseChangeError -> Expectation
assertRejected changes rows expected = do
  recordedSqlReference <- newIORef []
  runDatabaseChangesWithExecutor (recordingExecutor recordedSqlReference rows Nothing) testLedger changes [] `shouldReturn` Left expected
  readIORef recordedSqlReference >>= \recordedSql -> do
    recordedSql `shouldContain` ["ROLLBACK;"]
    recordedSql `shouldNotContain` ["SELECT 1;", "SELECT 2;"]

assertFailure :: [DatabaseChange] -> Text.Text -> DatabaseChangeError -> [Text.Text] -> Expectation
assertFailure changes failingSql expected exactSql = do
  recordedSqlReference <- newIORef []
  result <- runDatabaseChangesWithExecutor (recordingExecutor recordedSqlReference [] (Just failingSql)) testLedger changes []
  result `shouldBe` Left expected
  recordedSql <- readIORef recordedSqlReference
  if null exactSql then recordedSql `shouldContain` ["ROLLBACK;"] else recordedSql `shouldBe` exactSql

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show item) + length (showList [item] "") | item <- values] > 0

commandForLedgerQueryExecutor :: IORef [Text.Text] -> DatabaseChangeExecutor
commandForLedgerQueryExecutor recordedSqlReference =
  DatabaseChangeExecutor $ \sql -> do
    modifyIORef' recordedSqlReference (<> [sql])
    pure $
      if "SELECT change_id, change_order::TEXT, sql_digest FROM " `Text.isPrefixOf` sql
        then Right (Just DatabaseChangeCommandSucceeded)
        else
          if Text.isPrefixOf "SELECT pg_advisory_xact_lock" sql
            then Right (Just (DatabaseChangeRows []))
            else Right (Just DatabaseChangeCommandSucceeded)

rowsForSchemaCommandExecutor :: IORef [Text.Text] -> DatabaseChangeExecutor
rowsForSchemaCommandExecutor recordedSqlReference =
  DatabaseChangeExecutor $ \sql -> do
    modifyIORef' recordedSqlReference (<> [sql])
    pure $
      if sql == "CREATE SCHEMA IF NOT EXISTS \"web_api\";"
        || Text.isPrefixOf "SELECT pg_advisory_xact_lock" sql
        || "SELECT change_id, change_order::TEXT, sql_digest FROM " `Text.isPrefixOf` sql
        then Right (Just (DatabaseChangeRows []))
        else Right (Just DatabaseChangeCommandSucceeded)

recordingExecutor :: IORef [Text.Text] -> [[Maybe ByteString.ByteString]] -> Maybe Text.Text -> DatabaseChangeExecutor
recordingExecutor recordedSqlReference ledgerRows maybeFailingSql =
  DatabaseChangeExecutor $ \sql -> do
    modifyIORef' recordedSqlReference (<> [sql])
    pure $
      if "SELECT change_id, change_order::TEXT, sql_digest FROM " `Text.isPrefixOf` sql
        then Right (Just (DatabaseChangeRows ledgerRows))
        else
          if Just sql == maybeFailingSql
            then
              if sql == "BEGIN;"
                then Right (Just (DatabaseChangeRows []))
                else
                  if Text.isPrefixOf "SELECT pg_advisory_xact_lock" sql
                    then Right (Just DatabaseChangeCommandSucceeded)
                    else Right Nothing
            else
              if Text.isPrefixOf "SELECT pg_advisory_xact_lock" sql
                then Right (Just (DatabaseChangeRows []))
                else Right (Just DatabaseChangeCommandSucceeded)
