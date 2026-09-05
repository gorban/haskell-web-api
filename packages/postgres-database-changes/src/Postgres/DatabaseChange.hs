{-# LANGUAGE OverloadedStrings #-}

-- | PostgreSQL-specific, connection-scoped application of immutable ordered
-- database changes.  This module deliberately does not import an application
-- configuration type: applications own their connection credentials, schema,
-- ledger location, changes, and deployment reconciliation SQL.  The runner
-- owns the one privileged transaction, advisory lock, legacy-ledger cutover,
-- digest verification, and rollback protocol.
--
-- A ledger is a contiguous prefix of the supplied change plan.  The digest is
-- over a length-delimited UTF-8 sequence, not normalized SQL text, so editing
-- any historical statement is a startup failure rather than a silent rerun.
-- See the AHI-4C database-change decision in @docs/design-guidance.md@.
module Postgres.DatabaseChange
  ( DatabaseChange (..),
    DatabaseChangeConnectionString (..),
    DatabaseChangeError (..),
    DatabaseChangeExecutor (..),
    DatabaseChangeExecutorError (..),
    DatabaseChangeId (..),
    DatabaseChangeLedger (..),
    DatabaseChangeResult (..),
    databaseChangeDigest,
    databaseChangeIdText,
    mkDatabaseChangeId,
    mkDatabaseChangeLedger,
    runDatabaseChanges,
    runDatabaseChangesWithExecutor,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, void, when)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Core.Control.Error (liftEitherWith)
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAlpha, isAlphaNum)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Text.Read (readMaybe)

-- | A stable application-owned database change identifier.  IDs are values,
-- not executable SQL, but bounded printable identifiers keep diagnostics and
-- the ledger resource bounded.
newtype DatabaseChangeId = DatabaseChangeId Text
  deriving (Eq, Ord, Show)

-- | One immutable DDL and/or DML change.  The non-empty statement sequence is
-- included in its digest in order; no whitespace normalization is performed.
data DatabaseChange = DatabaseChange
  { databaseChangeId :: DatabaseChangeId,
    databaseChangeStatements :: NonEmpty Text
  }
  deriving (Eq, Show)

-- | An application-owned schema/table pair with an optional predecessor table
-- in the same schema.  The predecessor is renamed and upgraded atomically on
-- the first run, preserving its IDs and @applied_at@ values.
data DatabaseChangeLedger = DatabaseChangeLedger
  { databaseChangeLedgerSchema :: Text,
    databaseChangeLedgerTable :: Text,
    databaseChangeLegacyTable :: Maybe Text,
    databaseChangeLedgerLockId :: Integer
  }
  deriving (Eq, Show)

-- | A libpq connection string supplied by the application rather than an
-- application-specific configuration dependency.
newtype DatabaseChangeConnectionString = DatabaseChangeConnectionString ByteString.ByteString

-- | A connection-scoped SQL protocol.  Tests can substitute this executor and
-- prove every transaction decision without recreating libpq's own suite.
newtype DatabaseChangeExecutor = DatabaseChangeExecutor
  { executeDatabaseChangeSql :: Text -> IO (Either DatabaseChangeExecutorError (Maybe DatabaseChangeResult))
  }

-- | The adapter's safe failure classification. It deliberately carries no
-- server message or SQL text: the runner has one execution-failure rail, and
-- retaining a detail that no caller may interpret would only create a route
-- for application-owned DML values to escape.
data DatabaseChangeExecutorError = DatabaseChangeExecutorError
  deriving (Eq, Show)

-- | The only successful result shapes accepted from the executor.
data DatabaseChangeResult
  = DatabaseChangeCommandSucceeded
  | DatabaseChangeRows [[Maybe ByteString.ByteString]]
  deriving (Eq, Show)

-- | Failures on the database-change rail.  They are structured so callers can
-- log a stable code without echoing SQL, credentials, or database payloads.
data DatabaseChangeError
  = DatabaseChangeInvalidId
  | DatabaseChangeInvalidLedger
  | DatabaseChangeDuplicateId DatabaseChangeId
  | DatabaseChangeExecutionFailed
  | DatabaseChangeCommandReturnedRows
  | DatabaseChangeQueryReturnedNoRows
  | DatabaseChangeMalformedLedgerRow
  | DatabaseChangeUnknownRecordedId DatabaseChangeId
  | DatabaseChangeDigestMismatch DatabaseChangeId
  | DatabaseChangeOutOfOrder DatabaseChangeId
  deriving (Eq, Show)

mkDatabaseChangeId :: Text -> Either DatabaseChangeError DatabaseChangeId
mkDatabaseChangeId value
  | Text.null value || Text.length value > 200 = Left DatabaseChangeInvalidId
  | Text.all isPrintableIdCharacter value = Right (DatabaseChangeId value)
  | otherwise = Left DatabaseChangeInvalidId
  where
    isPrintableIdCharacter character = isAlphaNum character || character `elem` ("-_." :: String)

databaseChangeIdText :: DatabaseChangeId -> Text
databaseChangeIdText (DatabaseChangeId value) = value

-- | Construct a ledger location only from ordinary PostgreSQL identifiers.
-- SQL identifiers are never constructed from unvalidated deployment text.
mkDatabaseChangeLedger :: Text -> Text -> Maybe Text -> Integer -> Either DatabaseChangeError DatabaseChangeLedger
mkDatabaseChangeLedger schemaName tableName maybeLegacyTable lockId
  | validLedgerLocation schemaName tableName maybeLegacyTable =
      Right
        DatabaseChangeLedger
          { databaseChangeLedgerSchema = schemaName,
            databaseChangeLedgerTable = tableName,
            databaseChangeLegacyTable = maybeLegacyTable,
            databaseChangeLedgerLockId = lockId
          }
  | otherwise = Left DatabaseChangeInvalidLedger

validLedgerLocation :: Text -> Text -> Maybe Text -> Bool
validLedgerLocation schemaName tableName maybeLegacyTable =
  all validIdentifier (schemaName : tableName : maybe [] pure maybeLegacyTable)

validIdentifier :: Text -> Bool
validIdentifier value =
  case Text.uncons value of
    Nothing -> False
    Just (firstCharacter, rest) ->
      Text.length value <= 40
        && (isAlpha firstCharacter || firstCharacter == '_')
        && Text.all (\character -> isAlphaNum character || character == '_') rest

-- | Run a plan through a libpq connection which is opened and closed exactly
-- once for the whole transaction.
runDatabaseChanges :: DatabaseChangeConnectionString -> DatabaseChangeLedger -> [DatabaseChange] -> [Text] -> IO (Either DatabaseChangeError ())
runDatabaseChanges (DatabaseChangeConnectionString connectionString) ledger changes finalStatements =
  bracket
    (LibPQ.connectdb connectionString)
    LibPQ.finish
    (\connection -> runDatabaseChangesWithExecutor (libpqExecutor connection) ledger changes finalStatements)

-- | Apply the immutable plan and explicit deployment reconciliation sequence
-- in one locked transaction.  The final statements are not ledger entries:
-- they represent deployment-owned role/password reconciliation whose mutable
-- configuration must not be confused with application security data.
runDatabaseChangesWithExecutor :: DatabaseChangeExecutor -> DatabaseChangeLedger -> [DatabaseChange] -> [Text] -> IO (Either DatabaseChangeError ())
runDatabaseChangesWithExecutor executor ledger changes finalStatements =
  case (,) <$> validateLedger ledger <*> validateChangePlan changes of
    Left failure -> pure (Left failure)
    Right (validatedLedger, validatedChanges) -> do
      begun <- runExceptT (executeCommand executor "BEGIN;")
      case begun of
        Left failure -> pure (Left failure)
        Right () -> do
          result <- runExceptT $ do
            executeQuery executor (advisoryLockSql validatedLedger)
            prepareLedger executor validatedLedger validatedChanges
            recordedChanges <- loadRecordedChanges executor validatedLedger
            validatedRecordedChanges <- validateRecordedChanges validatedChanges recordedChanges
            constrainLedger executor validatedLedger
            applyPendingChanges executor validatedLedger validatedChanges validatedRecordedChanges
            mapM_ (executeCommand executor) finalStatements
            executeCommand executor "COMMIT;"
          case result of
            Right () -> pure (Right ())
            Left failure -> rollbackAfterFailure executor failure

rollbackAfterFailure :: DatabaseChangeExecutor -> DatabaseChangeError -> IO (Either DatabaseChangeError ())
rollbackAfterFailure executor failure = do
  void (runExceptT (executeCommand executor "ROLLBACK;"))
  pure (Left failure)

validateChangePlan :: [DatabaseChange] -> Either DatabaseChangeError [DatabaseChange]
validateChangePlan = go []
  where
    go _ [] = Right []
    go seen (change : rest)
      | Left failure <- mkDatabaseChangeId (databaseChangeIdText (databaseChangeId change)) = Left failure
      | databaseChangeId change `elem` seen = Left (DatabaseChangeDuplicateId (databaseChangeId change))
      | otherwise = (change :) <$> go (databaseChangeId change : seen) rest

validateLedger :: DatabaseChangeLedger -> Either DatabaseChangeError DatabaseChangeLedger
validateLedger ledger =
  if validLedgerLocation
    (databaseChangeLedgerSchema ledger)
    (databaseChangeLedgerTable ledger)
    (databaseChangeLegacyTable ledger)
    then Right ledger
    else Left DatabaseChangeInvalidLedger

prepareLedger :: DatabaseChangeExecutor -> DatabaseChangeLedger -> [DatabaseChange] -> ExceptT DatabaseChangeError IO ()
prepareLedger executor ledger changes = do
  executeCommand executor ("CREATE SCHEMA IF NOT EXISTS " <> quotedIdentifier (databaseChangeLedgerSchema ledger) <> ";")
  mapM_ (executeCommand executor . cutoverRenameSql ledger) (databaseChangeLegacyTable ledger)
  executeCommand executor (cutoverColumnSql ledger)
  executeCommand executor (createLedgerSql ledger)
  -- A predecessor table can preserve @applied_at@ as a non-null column without
  -- preserving the original default.  Keep its historical values intact while
  -- making the next recorded change receive the same transaction timestamp as
  -- a fresh ledger.
  executeCommand executor ("ALTER TABLE " <> qualifiedLedgerName ledger <> " ALTER COLUMN applied_at SET DEFAULT CURRENT_TIMESTAMP;")
  executeCommand executor ("ALTER TABLE " <> qualifiedLedgerName ledger <> " ADD COLUMN IF NOT EXISTS change_order BIGINT;")
  executeCommand executor ("ALTER TABLE " <> qualifiedLedgerName ledger <> " ADD COLUMN IF NOT EXISTS sql_digest TEXT;")
  forM_ (zip [1 :: Integer ..] changes) $ \(position, change) ->
    executeCommand executor (backfillLegacyDigestSql ledger position change)

constrainLedger :: DatabaseChangeExecutor -> DatabaseChangeLedger -> ExceptT DatabaseChangeError IO ()
constrainLedger executor ledger = do
  executeCommand executor ("ALTER TABLE " <> qualifiedLedgerName ledger <> " ALTER COLUMN change_order SET NOT NULL;")
  executeCommand executor ("ALTER TABLE " <> qualifiedLedgerName ledger <> " ALTER COLUMN sql_digest SET NOT NULL;")
  executeCommand executor ("CREATE UNIQUE INDEX IF NOT EXISTS " <> quotedIdentifier (databaseChangeLedgerTable ledger <> "_change_order_unique") <> " ON " <> qualifiedLedgerName ledger <> " (change_order);")

data RecordedChange = RecordedChange
  { recordedChangeId :: DatabaseChangeId,
    recordedChangeOrder :: Integer,
    recordedChangeDigest :: Text
  }

loadRecordedChanges :: DatabaseChangeExecutor -> DatabaseChangeLedger -> ExceptT DatabaseChangeError IO [RecordedChange]
loadRecordedChanges executor ledger = do
  rows <- executeRows executor ("SELECT change_id, change_order::TEXT, sql_digest FROM " <> qualifiedLedgerName ledger <> " ORDER BY change_order ASC, change_id ASC;")
  case traverse decodeRecordedChange rows of
    Left failure -> throwError failure
    Right recordedChanges -> pure recordedChanges

decodeRecordedChange :: [Maybe ByteString.ByteString] -> Either DatabaseChangeError RecordedChange
decodeRecordedChange row =
  case row of
    [Just rawId, Just rawOrder, Just rawDigest] -> do
      changeId <- decodeUtf8 rawId >>= mkDatabaseChangeId
      orderText <- decodeUtf8 rawOrder
      digest <- decodeUtf8 rawDigest
      case readMaybe (Text.unpack orderText) of
        Just order | order > 0 -> Right (RecordedChange changeId order digest)
        _ -> Left DatabaseChangeMalformedLedgerRow
    _ -> Left DatabaseChangeMalformedLedgerRow

decodeUtf8 :: ByteString.ByteString -> Either DatabaseChangeError Text
decodeUtf8 = either (const (Left DatabaseChangeMalformedLedgerRow)) Right . TextEncoding.decodeUtf8'

validateRecordedChanges :: [DatabaseChange] -> [RecordedChange] -> ExceptT DatabaseChangeError IO [RecordedChange]
validateRecordedChanges changes recordedChanges =
  case traverse validateOne recordedChanges of
    Left failure -> throwError failure
    Right validatedRecordedChanges ->
      case firstMissingChange changes validatedRecordedChanges of
        Nothing -> pure validatedRecordedChanges
        Just missingChange -> do
          when
            (any (\recorded -> recordedChangeOrder recorded > expectedOrder missingChange) validatedRecordedChanges)
            (throwError (DatabaseChangeOutOfOrder (databaseChangeId missingChange)))
          pure validatedRecordedChanges
  where
    validateOne recorded =
      case find (\change -> databaseChangeId change == recordedChangeId recorded) changes of
        Nothing -> Left (DatabaseChangeUnknownRecordedId (recordedChangeId recorded))
        Just change
          | recordedChangeDigest recorded /= databaseChangeDigest change -> Left (DatabaseChangeDigestMismatch (databaseChangeId change))
          | recordedChangeOrder recorded /= expectedOrder change -> Left (DatabaseChangeOutOfOrder (databaseChangeId change))
          | otherwise -> Right recorded
    expectedOrder change =
      toInteger (length (takeWhile (\candidate -> databaseChangeId candidate /= databaseChangeId change) changes) + 1)

firstMissingChange :: [DatabaseChange] -> [RecordedChange] -> Maybe DatabaseChange
firstMissingChange changes recordedChanges =
  find (\change -> isNothing (find (\recorded -> recordedChangeId recorded == databaseChangeId change) recordedChanges)) changes

applyPendingChanges :: DatabaseChangeExecutor -> DatabaseChangeLedger -> [DatabaseChange] -> [RecordedChange] -> ExceptT DatabaseChangeError IO ()
applyPendingChanges executor ledger changes recordedChanges =
  forM_ (zip [1 :: Integer ..] changes) $ \(position, change) ->
    when (isNothing (find (\recorded -> recordedChangeId recorded == databaseChangeId change) recordedChanges)) $ do
      mapM_ (executeCommand executor) (NonEmpty.toList (databaseChangeStatements change))
      executeCommand executor (insertRecordedChangeSql ledger position change)

databaseChangeDigest :: DatabaseChange -> Text
databaseChangeDigest change =
  Text.pack (show (hash (canonicalStatementBytes (databaseChangeStatements change)) :: Digest SHA256))

canonicalStatementBytes :: NonEmpty Text -> ByteString.ByteString
canonicalStatementBytes statements =
  LazyByteString.toStrict
    (LazyByteString.fromStrict "postgres-database-changes-v1:\n" <> foldMap encodeStatement statements)
  where
    encodeStatement statement =
      let bytes = TextEncoding.encodeUtf8 statement
       in LazyByteString.fromStrict (ByteString.Char8.pack (show (ByteString.length bytes)))
            <> LazyByteString.fromStrict ":"
            <> LazyByteString.fromStrict bytes

libpqExecutor :: LibPQ.Connection -> DatabaseChangeExecutor
libpqExecutor connection = DatabaseChangeExecutor (runLibpqSql connection)

runLibpqSql :: LibPQ.Connection -> Text -> IO (Either DatabaseChangeExecutorError (Maybe DatabaseChangeResult))
runLibpqSql connection sql =
  LibPQ.exec connection (TextEncoding.encodeUtf8 sql)
    >>= maybe libpqExecutionFailure runLibpqResult

-- | Adapt only the two successful libpq protocol statuses that the immutable
-- change runner can own.  The lookup is deliberately closed: notices, copy
-- protocols, and every server failure remain the same redacted executor
-- failure rather than acquiring accidental migration semantics.
runLibpqResult :: LibPQ.Result -> IO (Either DatabaseChangeExecutorError (Maybe DatabaseChangeResult))
runLibpqResult result = do
  status <- LibPQ.resultStatus result
  fromMaybe
    libpqExecutionFailure
    ( lookup
        status
        [ (LibPQ.CommandOk, pure (Right (Just DatabaseChangeCommandSucceeded))),
          (LibPQ.TuplesOk, Right . Just . DatabaseChangeRows <$> readRows result)
        ]
    )

libpqExecutionFailure :: IO (Either DatabaseChangeExecutorError (Maybe DatabaseChangeResult))
libpqExecutionFailure = pure (Left DatabaseChangeExecutorError)

readRows :: LibPQ.Result -> IO [[Maybe ByteString.ByteString]]
readRows result = do
  rowCount <- LibPQ.ntuples result
  columnCount <- LibPQ.nfields result
  traverse (\rowIndex -> traverse (LibPQ.getvalue result rowIndex) [0 .. columnCount - 1]) [0 .. rowCount - 1]

executeCommand :: DatabaseChangeExecutor -> Text -> ExceptT DatabaseChangeError IO ()
executeCommand executor sql = do
  result <- executeSql executor sql
  case result of
    DatabaseChangeCommandSucceeded -> pure ()
    DatabaseChangeRows _ -> throwError DatabaseChangeCommandReturnedRows

executeQuery :: DatabaseChangeExecutor -> Text -> ExceptT DatabaseChangeError IO ()
executeQuery executor sql = do
  void (executeRows executor sql)

executeRows :: DatabaseChangeExecutor -> Text -> ExceptT DatabaseChangeError IO [[Maybe ByteString.ByteString]]
executeRows executor sql = do
  result <- executeSql executor sql
  case result of
    DatabaseChangeCommandSucceeded -> throwError DatabaseChangeQueryReturnedNoRows
    DatabaseChangeRows rows -> pure rows

executeSql :: DatabaseChangeExecutor -> Text -> ExceptT DatabaseChangeError IO DatabaseChangeResult
executeSql executor sql = do
  result <- liftExecutorResult (executeDatabaseChangeSql executor sql)
  case result of
    Nothing -> throwError DatabaseChangeExecutionFailed
    Just value -> pure value

liftExecutorResult :: IO (Either DatabaseChangeExecutorError value) -> ExceptT DatabaseChangeError IO value
liftExecutorResult = liftEitherWith databaseChangeExecutorFailure

databaseChangeExecutorFailure :: DatabaseChangeExecutorError -> DatabaseChangeError
databaseChangeExecutorFailure DatabaseChangeExecutorError = DatabaseChangeExecutionFailed

advisoryLockSql :: DatabaseChangeLedger -> Text
advisoryLockSql ledger = "SELECT pg_advisory_xact_lock(" <> Text.pack (show (databaseChangeLedgerLockId ledger)) <> ");"

cutoverRenameSql :: DatabaseChangeLedger -> Text -> Text
cutoverRenameSql ledger legacyTable =
  "DO $$ BEGIN IF to_regclass("
    <> sqlLiteral (qualifiedName (databaseChangeLedgerSchema ledger) legacyTable)
    <> ") IS NOT NULL AND to_regclass("
    <> sqlLiteral (qualifiedLedgerName ledger)
    <> ") IS NULL THEN ALTER TABLE "
    <> qualifiedName (databaseChangeLedgerSchema ledger) legacyTable
    <> " RENAME TO "
    <> quotedIdentifier (databaseChangeLedgerTable ledger)
    <> "; END IF; END $$;"

cutoverColumnSql :: DatabaseChangeLedger -> Text
cutoverColumnSql ledger =
  "DO $$ BEGIN IF EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = "
    <> sqlLiteral (databaseChangeLedgerSchema ledger)
    <> " AND table_name = "
    <> sqlLiteral (databaseChangeLedgerTable ledger)
    <> " AND column_name = 'version') AND NOT EXISTS (SELECT 1 FROM information_schema.columns WHERE table_schema = "
    <> sqlLiteral (databaseChangeLedgerSchema ledger)
    <> " AND table_name = "
    <> sqlLiteral (databaseChangeLedgerTable ledger)
    <> " AND column_name = 'change_id') THEN ALTER TABLE "
    <> qualifiedLedgerName ledger
    <> " RENAME COLUMN version TO change_id; END IF; END $$;"

createLedgerSql :: DatabaseChangeLedger -> Text
createLedgerSql ledger =
  "CREATE TABLE IF NOT EXISTS "
    <> qualifiedLedgerName ledger
    <> " (change_id TEXT PRIMARY KEY, change_order BIGINT NOT NULL, sql_digest TEXT NOT NULL, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);"

backfillLegacyDigestSql :: DatabaseChangeLedger -> Integer -> DatabaseChange -> Text
backfillLegacyDigestSql ledger position change =
  "UPDATE "
    <> qualifiedLedgerName ledger
    <> " SET change_order = "
    <> Text.pack (show position)
    <> ", sql_digest = "
    <> sqlLiteral (databaseChangeDigest change)
    <> " WHERE change_id = "
    <> sqlLiteral (databaseChangeIdText (databaseChangeId change))
    <> " AND change_order IS NULL AND sql_digest IS NULL;"

insertRecordedChangeSql :: DatabaseChangeLedger -> Integer -> DatabaseChange -> Text
insertRecordedChangeSql ledger position change =
  "INSERT INTO "
    <> qualifiedLedgerName ledger
    <> " (change_id, change_order, sql_digest) VALUES ("
    <> sqlLiteral (databaseChangeIdText (databaseChangeId change))
    <> ", "
    <> Text.pack (show position)
    <> ", "
    <> sqlLiteral (databaseChangeDigest change)
    <> ");"

qualifiedLedgerName :: DatabaseChangeLedger -> Text
qualifiedLedgerName ledger = qualifiedName (databaseChangeLedgerSchema ledger) (databaseChangeLedgerTable ledger)

qualifiedName :: Text -> Text -> Text
qualifiedName schemaName tableName = quotedIdentifier schemaName <> "." <> quotedIdentifier tableName

quotedIdentifier :: Text -> Text
quotedIdentifier value = "\"" <> value <> "\""

sqlLiteral :: Text -> Text
sqlLiteral value = "'" <> Text.replace "'" "''" value <> "'"
