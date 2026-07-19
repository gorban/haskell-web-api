{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres
  ( PostgresCommand (..),
    PostgresCommandResult (..),
    PostgresRunnerError (..),
    buildPostgresDatabaseEffect,
    buildPostgresDatabaseEffectWithRunner,
    buildRuntimePostgresAccountStore,
    buildRuntimePostgresAccountStoreWithRunner,
    buildRuntimePostgresDatabaseEffect,
    buildRuntimePostgresDatabaseEffectWithRunner,
    decodeRuntimeQueryValue,
    renderRuntimeConnectionErrorMessage,
    renderRuntimeResultErrorMessage,
    migrationStatementsFor,
    runRuntimeRowsQuery,
    runRuntimeParameterizedRowsQuery,
    runRuntimeScalarQuery,
    runPostgresMigrations,
    runPostgresMigrationsForRuntime,
    runPostgresMigrationsWithRunner,
    runPostgresMigrationsWithRunnerForRuntime,
    runPostgresSeed,
    runPostgresSeedWithRunner,
    seedStatements,
  )
where

import Control.Exception (bracket, evaluate)
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Account
  ( StoredEmailVerification (..),
    accountIdText,
    emailVerificationTokenDigestText,
    mkAccountId,
    storedVerificationAccountId,
    storedVerificationEmail,
    storedVerificationExpiresAtNanoseconds,
    storedVerificationTokenDigest,
  )
import HarchWeb.Email (emailAddressText, mkEmailAddress)
import HarchWeb.Password (passwordHashText)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (env, proc, readCreateProcessWithExitCode)
import Text.Read (readMaybe)
import WebApi.Account
  ( AccountStore (..),
    AccountStoreError (..),
    PendingAccount (..),
  )
import WebApi.Config (DatabaseConfig (..))
import WebApi.Database
  ( DatabaseEffect (..),
    DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    HomePageData (..),
    SecondPageData (..),
  )
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
  )

data PostgresCommand = PostgresCommand
  { postgresExecutable :: FilePath,
    postgresArguments :: [String],
    postgresEnvironment :: [(String, String)]
  }
  deriving (Eq, Show)

data PostgresCommandResult = PostgresCommandResult
  { postgresExitCode :: ExitCode,
    postgresStdout :: Text,
    postgresStderr :: Text
  }
  deriving (Eq, Show)

data PostgresRunnerError
  = PostgresCommandFailed PostgresCommand PostgresCommandResult
  | UnexpectedQueryRows Text [Text]
  deriving (Eq, Show)

buildPostgresDatabaseEffect :: DatabaseConfig -> DatabaseEffect
buildPostgresDatabaseEffect =
  buildPostgresDatabaseEffectWithRunner runPostgresCommand

buildPostgresDatabaseEffectWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> DatabaseEffect
buildPostgresDatabaseEffectWithRunner runCommand databaseConfig =
  DatabaseEffect
    { loadHomePageData = fmap databaseResultValue . loadPostgresHomePageData,
      loadHomePageDataWithObservability =
        loadPostgresHomePageData,
      loadSecondPageData = fmap databaseResultValue . loadPostgresSecondPageData,
      loadSecondPageDataWithObservability =
        loadPostgresSecondPageData
    }
  where
    loadPostgresHomePageData requestContext = do
      (summaryResult, operation) <-
        timedDatabaseOperation homeSummaryOperation $
          fmap
            (fmap HomePageData)
            (runRequiredScalarQuery runCommand databaseConfig (homeSummaryQuery (requestLocale requestContext)) HomePageDataError)
      pure
        DatabaseResult
          { databaseResultValue = summaryResult,
            databaseResultOperations = [operation]
          }
    loadPostgresSecondPageData requestContext = do
      (summaryResult, summaryOperation) <-
        timedDatabaseOperation secondSummaryOperation $
          runRequiredScalarQuery runCommand databaseConfig (secondSummaryQuery (requestLocale requestContext)) SecondPageDataError
      case summaryResult of
        Left databaseError ->
          pure
            DatabaseResult
              { databaseResultValue = Left databaseError,
                databaseResultOperations = [summaryOperation]
              }
        Right secondSummary -> do
          (highlightsResult, highlightsOperation) <-
            timedDatabaseOperation secondHighlightsOperation $
              runOptionalRowsQuery runCommand databaseConfig (secondHighlightsQuery (requestLocale requestContext)) SecondPageDataError
          pure
            DatabaseResult
              { databaseResultValue =
                  fmap
                    ( \highlights ->
                        SecondPageData
                          { secondPageDataSummary = secondSummary,
                            secondPageDataHighlights = highlights
                          }
                    )
                    highlightsResult,
                databaseResultOperations = [summaryOperation, highlightsOperation]
              }

buildRuntimePostgresDatabaseEffect :: DatabaseConfig -> DatabaseEffect
buildRuntimePostgresDatabaseEffect =
  buildRuntimePostgresDatabaseEffectWithRunner runRuntimeScalarQuery runRuntimeRowsQuery

buildRuntimePostgresAccountStore :: DatabaseConfig -> AccountStore
buildRuntimePostgresAccountStore !databaseConfig =
  buildRuntimePostgresAccountStoreWithRunner runRuntimeParameterizedRowsQuery databaseConfig

buildRuntimePostgresAccountStoreWithRunner ::
  (DatabaseConfig -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  DatabaseConfig ->
  AccountStore
buildRuntimePostgresAccountStoreWithRunner runQuery databaseConfig =
  AccountStore
    { createPendingAccount = createAccount,
      findEmailVerification = findVerification,
      consumeEmailVerification = consumeVerification
    }
  where
    createAccount pendingAccount = do
      queryResult <-
        runQuery
          databaseConfig
          createPendingAccountQuery
          [ accountIdText (pendingAccountId pendingAccount),
            emailAddressText (pendingAccountEmail pendingAccount),
            passwordHashText (pendingAccountPasswordHash pendingAccount),
            emailVerificationTokenDigestText (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)),
            Text.pack (show (storedVerificationExpiresAtNanoseconds (pendingAccountVerification pendingAccount))),
            Text.pack (show (pendingAccountCreatedAtNanoseconds pendingAccount))
          ]
      pure $
        case queryResult of
          Left queryError -> Left (AccountStoreUnavailable queryError)
          Right [] -> Right False
          Right [[createdAccountId]]
            | createdAccountId == accountIdText (pendingAccountId pendingAccount) -> Right True
          Right rows -> Left (AccountStoreCorruptData ("unexpected pending-account result: " <> Text.pack (show rows)))

    findVerification tokenDigest = do
      queryResult <- runQuery databaseConfig findEmailVerificationQuery [emailVerificationTokenDigestText tokenDigest]
      pure $
        case queryResult of
          Left queryError -> Left (AccountStoreUnavailable queryError)
          Right [] -> Right Nothing
          Right [[accountIdValue, emailAddressValue, expiresAtValue]] -> do
            accountId <- maybe (Left (AccountStoreCorruptData "email verification has an invalid account id")) Right (mkAccountId accountIdValue)
            emailAddress <- maybe (Left (AccountStoreCorruptData "email verification has an invalid email address")) Right (mkEmailAddress emailAddressValue)
            expiresAt <- maybe (Left (AccountStoreCorruptData "email verification has an invalid expiry")) Right (readMaybe (Text.unpack expiresAtValue))
            Right
              ( Just
                  StoredEmailVerification
                    { storedVerificationAccountId = accountId,
                      storedVerificationEmail = emailAddress,
                      storedVerificationTokenDigest = tokenDigest,
                      storedVerificationExpiresAtNanoseconds = expiresAt
                    }
              )
          Right rows -> Left (AccountStoreCorruptData ("unexpected email-verification result: " <> Text.pack (show rows)))

    consumeVerification tokenDigest now = do
      queryResult <-
        runQuery
          databaseConfig
          consumeEmailVerificationQuery
          [emailVerificationTokenDigestText tokenDigest, Text.pack (show now)]
      pure $
        case queryResult of
          Left queryError -> Left (AccountStoreUnavailable queryError)
          Right [] -> Right Nothing
          Right [[accountIdValue]] ->
            maybe
              (Left (AccountStoreCorruptData "email verification was consumed for an invalid account id"))
              (Right . Just)
              (mkAccountId accountIdValue)
          Right rows -> Left (AccountStoreCorruptData ("unexpected email-verification consumption result: " <> Text.pack (show rows)))

buildRuntimePostgresDatabaseEffectWithRunner ::
  (DatabaseConfig -> Text -> IO (Either Text Text)) ->
  (DatabaseConfig -> Text -> IO (Either Text [Text])) ->
  DatabaseConfig ->
  DatabaseEffect
buildRuntimePostgresDatabaseEffectWithRunner runScalarQuery runRowsQuery databaseConfig =
  DatabaseEffect
    { loadHomePageData = fmap databaseResultValue . loadRuntimeHomePageData,
      loadHomePageDataWithObservability = loadRuntimeHomePageData,
      loadSecondPageData = fmap databaseResultValue . loadRuntimeSecondPageData,
      loadSecondPageDataWithObservability = loadRuntimeSecondPageData
    }
  where
    loadRuntimeHomePageData requestContext = do
      (summaryResult, operation) <-
        timedDatabaseOperation homeSummaryOperation $
          fmap
            (fmap HomePageData)
            (runScalarQuery databaseConfig (homeSummaryQuery (requestLocale requestContext)))
      pure
        DatabaseResult
          { databaseResultValue = first HomePageDataError summaryResult,
            databaseResultOperations = [operation]
          }
    loadRuntimeSecondPageData requestContext = do
      (summaryResult, summaryOperation) <-
        timedDatabaseOperation secondSummaryOperation $
          runScalarQuery databaseConfig (secondSummaryQuery (requestLocale requestContext))
      case first SecondPageDataError summaryResult of
        Left databaseError ->
          pure
            DatabaseResult
              { databaseResultValue = Left databaseError,
                databaseResultOperations = [summaryOperation]
              }
        Right secondSummary -> do
          (highlightsResult, highlightsOperation) <-
            timedDatabaseOperation secondHighlightsOperation $
              runRowsQuery databaseConfig (secondHighlightsQuery (requestLocale requestContext))
          pure
            DatabaseResult
              { databaseResultValue =
                  fmap
                    ( \highlights ->
                        SecondPageData
                          { secondPageDataSummary = secondSummary,
                            secondPageDataHighlights = highlights
                          }
                    )
                    (first SecondPageDataError highlightsResult),
                databaseResultOperations = [summaryOperation, highlightsOperation]
              }

timedDatabaseOperation :: DatabaseOperation -> IO a -> IO (a, DatabaseOperation)
timedDatabaseOperation databaseOperation action = do
  _ <- evaluate (databaseOperationStartedAtNanoseconds databaseOperation)
  _ <- evaluate (databaseOperationEndedAtNanoseconds databaseOperation)
  startedAt <- getMonotonicTimeNSec
  result <- action
  endedAt <- getMonotonicTimeNSec
  pure
    ( result,
      databaseOperation
        { databaseOperationStartedAtNanoseconds = Just startedAt,
          databaseOperationEndedAtNanoseconds = Just endedAt
        }
    )

runPostgresMigrations :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrations =
  runPostgresMigrationsWithRunner runPostgresCommand

runPostgresMigrationsWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithRunner runCommand databaseConfig =
  runPostgresMigrationsWithRunnerForRuntime runCommand databaseConfig databaseConfig

runPostgresMigrationsForRuntime :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsForRuntime =
  runPostgresMigrationsWithRunnerForRuntime runPostgresCommand

runPostgresMigrationsWithRunnerForRuntime :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithRunnerForRuntime runCommand migrationDatabaseConfig runtimeDatabaseConfig =
  runStatements runCommand migrationDatabaseConfig (migrationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig)

runPostgresSeed :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeed =
  runPostgresSeedWithRunner runPostgresCommand

runPostgresSeedWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeedWithRunner runCommand databaseConfig =
  runStatements runCommand databaseConfig seedStatements

appSchemaName :: Text
appSchemaName = "web_api"

migrationStatementsFor :: DatabaseConfig -> DatabaseConfig -> [Text]
migrationStatementsFor migrationDatabaseConfig runtimeDatabaseConfig =
  baseSchemaStatements
    <> privilegeStatements
  where
    baseSchemaStatements =
      [ "CREATE SCHEMA IF NOT EXISTS " <> appSchemaName <> ";",
        "ALTER SCHEMA " <> appSchemaName <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER DATABASE " <> sqlIdentifier (databaseName migrationDatabaseConfig) <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_content" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "page_highlights" <> " (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "accounts" <> " (account_id TEXT PRIMARY KEY, email_normalized TEXT NOT NULL UNIQUE, password_hash TEXT NOT NULL, email_verified_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "email_verifications" <> " (token_digest TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_totp" <> " (account_id TEXT PRIMARY KEY REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, encrypted_secret BYTEA NOT NULL, confirmed_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
        "CREATE TABLE IF NOT EXISTS " <> qualifiedTableName "account_recovery_codes" <> " (account_id TEXT NOT NULL REFERENCES " <> qualifiedTableName "accounts" <> " (account_id) ON DELETE CASCADE, code_hash TEXT NOT NULL UNIQUE, created_at_nanoseconds BIGINT NOT NULL, used_at_nanoseconds BIGINT, PRIMARY KEY (account_id, code_hash));",
        "ALTER TABLE " <> qualifiedTableName "page_content" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER TABLE " <> qualifiedTableName "page_highlights" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER TABLE " <> qualifiedTableName "accounts" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER TABLE " <> qualifiedTableName "email_verifications" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER TABLE " <> qualifiedTableName "account_totp" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";",
        "ALTER TABLE " <> qualifiedTableName "account_recovery_codes" <> " OWNER TO " <> sqlIdentifier (databaseUser migrationDatabaseConfig) <> ";"
      ]

    privilegeStatements =
      if databaseUser migrationDatabaseConfig == databaseUser runtimeDatabaseConfig
        then []
        else
          [ ensureRuntimeRoleStatement runtimeDatabaseConfig,
            "REVOKE ALL ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " FROM PUBLIC;",
            "REVOKE ALL ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " FROM " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT CONNECT ON DATABASE " <> sqlIdentifier (databaseName runtimeDatabaseConfig) <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "REVOKE ALL ON SCHEMA public FROM PUBLIC;",
            "REVOKE ALL ON SCHEMA public FROM " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "REVOKE ALL ON SCHEMA " <> appSchemaName <> " FROM PUBLIC;",
            "GRANT USAGE ON SCHEMA " <> appSchemaName <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "page_content" <> " FROM PUBLIC;",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "page_highlights" <> " FROM PUBLIC;",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "accounts" <> " FROM PUBLIC;",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "email_verifications" <> " FROM PUBLIC;",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "account_totp" <> " FROM PUBLIC;",
            "REVOKE ALL ON TABLE " <> qualifiedTableName "account_recovery_codes" <> " FROM PUBLIC;",
            "GRANT SELECT ON TABLE " <> qualifiedTableName "page_content" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT SELECT ON TABLE " <> qualifiedTableName "page_highlights" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName "accounts" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName "email_verifications" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName "account_totp" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";",
            "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE " <> qualifiedTableName "account_recovery_codes" <> " TO " <> sqlIdentifier (databaseUser runtimeDatabaseConfig) <> ";"
          ]

seedStatements :: [Text]
seedStatements =
  [ "DELETE FROM " <> qualifiedTableName "page_highlights" <> ";",
    "DELETE FROM " <> qualifiedTableName "page_content" <> ";",
    "INSERT INTO " <> qualifiedTableName "page_content" <> " (route_slug, locale, summary) VALUES ('home', 'en', 'Server-rendered home page with stubbed content.'), ('home', 'es', 'Inicio renderizado en el servidor con datos de desarrollo preconfigurados.'), ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'es', 'Second page content with stubbed data ready for future loaders.');"
  ]

runRequiredScalarQuery :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> Text -> (Text -> DatabaseError) -> IO (Either DatabaseError Text)
runRequiredScalarQuery runCommand databaseConfig sql toDatabaseError =
  fmap
    ( \commandResult ->
        case normalizeQueryResult sql commandResult of
          Left runnerError -> Left (toDatabaseError (renderRunnerError runnerError))
          Right rows ->
            case parseRequiredScalarRows rows of
              Left runnerError -> Left (toDatabaseError (renderRunnerError runnerError))
              Right value -> Right value
    )
    (runCommand (queryCommand databaseConfig sql))

runOptionalRowsQuery :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> Text -> (Text -> DatabaseError) -> IO (Either DatabaseError [Text])
runOptionalRowsQuery runCommand databaseConfig sql toDatabaseError =
  fmap
    ( either (Left . toDatabaseError . renderRunnerError) Right
        . normalizeQueryResult sql
    )
    (runCommand (queryCommand databaseConfig sql))

runStatements :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> [Text] -> IO (Either PostgresRunnerError ())
runStatements runCommand databaseConfig =
  go
  where
    go remainingStatements =
      case remainingStatements of
        [] -> pure (Right ())
        statement : rest -> do
          commandResult <- runCommand (mutationCommand databaseConfig statement)
          case commandSucceeded commandResult of
            True -> go rest
            False -> pure (Left (PostgresCommandFailed (mutationCommand databaseConfig statement) commandResult))

runPostgresCommand :: PostgresCommand -> IO PostgresCommandResult
runPostgresCommand command = do
  inheritedEnvironment <- getEnvironment
  let createProcess =
        (proc (postgresExecutable command) (postgresArguments command))
          { env =
              Just (mergeEnvironment inheritedEnvironment (postgresEnvironment command))
          }
  (exitCode, stdoutText, stderrText) <- readCreateProcessWithExitCode createProcess ""
  pure
    PostgresCommandResult
      { postgresExitCode = exitCode,
        postgresStdout = Text.pack stdoutText,
        postgresStderr = Text.pack stderrText
      }

runRuntimeScalarQuery :: DatabaseConfig -> Text -> IO (Either Text Text)
runRuntimeScalarQuery databaseConfig sql =
  fmap runtimeScalarRowsResult (runRuntimeRowsQuery databaseConfig sql)

runtimeScalarRowsResult :: Either Text [Text] -> Either Text Text
runtimeScalarRowsResult rowsResult =
  case rowsResult of
    Left runtimeError -> Left runtimeError
    Right rows ->
      case parseRequiredScalarRows rows of
        Left runnerError -> Left (renderRunnerError runnerError)
        Right value -> Right value

runRuntimeRowsQuery :: DatabaseConfig -> Text -> IO (Either Text [Text])
runRuntimeRowsQuery databaseConfig sql =
  bracket
    (LibPQ.connectdb (runtimeConnectionString databaseConfig))
    LibPQ.finish
    (runRuntimeQueryRows sql)

runRuntimeParameterizedRowsQuery :: DatabaseConfig -> Text -> [Text] -> IO (Either Text [[Text]])
runRuntimeParameterizedRowsQuery databaseConfig sql parameters =
  bracket
    (LibPQ.connectdb (runtimeConnectionString databaseConfig))
    LibPQ.finish
    (runRuntimeParameterizedQueryRows sql parameters)

runRuntimeQueryRows :: Text -> LibPQ.Connection -> IO (Either Text [Text])
runRuntimeQueryRows sql connection = do
  maybeResult <- LibPQ.exec connection (TextEncoding.encodeUtf8 sql)
  case maybeResult of
    Nothing ->
      fmap Left (renderRuntimeConnectionError connection)
    Just result -> do
      resultStatus <- LibPQ.resultStatus result
      case resultStatus of
        LibPQ.TuplesOk ->
          readRuntimeQueryRows result
        _ ->
          fmap Left (renderRuntimeResultError result)

runRuntimeParameterizedQueryRows :: Text -> [Text] -> LibPQ.Connection -> IO (Either Text [[Text]])
runRuntimeParameterizedQueryRows sql parameters connection = do
  maybeResult <-
    LibPQ.execParams
      connection
      (TextEncoding.encodeUtf8 sql)
      (fmap parameterValue parameters)
      LibPQ.Text
  case maybeResult of
    Nothing -> fmap Left (renderRuntimeConnectionError connection)
    Just result -> do
      resultStatus <- LibPQ.resultStatus result
      case resultStatus of
        LibPQ.TuplesOk -> readRuntimeQueryTable result
        _ -> fmap Left (renderRuntimeResultError result)

parameterValue :: Text -> Maybe (LibPQ.Oid, ByteString.ByteString, LibPQ.Format)
parameterValue value =
  Just (LibPQ.Oid 0, TextEncoding.encodeUtf8 value, LibPQ.Text)

readRuntimeQueryRows :: LibPQ.Result -> IO (Either Text [Text])
readRuntimeQueryRows result = do
  rowCount <- LibPQ.ntuples result
  values <-
    traverse
      (readRuntimeQueryValue result)
      [0 .. rowCount - 1]
  pure (sequence values)

readRuntimeQueryTable :: LibPQ.Result -> IO (Either Text [[Text]])
readRuntimeQueryTable result = do
  rowCount <- LibPQ.ntuples result
  columnCount <- LibPQ.nfields result
  rows <-
    traverse
      ( \rowIndex ->
          traverse
            (readRuntimeQueryColumnValue result rowIndex)
            [0 .. columnCount - 1]
      )
      [0 .. rowCount - 1]
  pure (mapM sequence rows)

readRuntimeQueryValue :: LibPQ.Result -> LibPQ.Row -> IO (Either Text Text)
readRuntimeQueryValue result rowIndex =
  readRuntimeQueryColumnValue result rowIndex 0

readRuntimeQueryColumnValue :: LibPQ.Result -> LibPQ.Row -> LibPQ.Column -> IO (Either Text Text)
readRuntimeQueryColumnValue result rowIndex columnIndex =
  fmap decodeRuntimeQueryValue (LibPQ.getvalue result rowIndex columnIndex)

decodeRuntimeQueryValue :: Maybe ByteString.ByteString -> Either Text Text
decodeRuntimeQueryValue maybeValue =
  case maybeValue of
    Nothing -> Left "unexpected NULL column value"
    Just value -> Right (TextEncoding.decodeUtf8With lenientDecode value)

runtimeConnectionString :: DatabaseConfig -> ByteString.ByteString
runtimeConnectionString databaseConfig =
  TextEncoding.encodeUtf8 $
    Text.unwords
      [ "host=" <> libpqConnectionValue (databaseHost databaseConfig),
        "port=" <> Text.pack (show (databasePort databaseConfig)),
        "dbname=" <> libpqConnectionValue (databaseName databaseConfig),
        "user=" <> libpqConnectionValue (databaseUser databaseConfig),
        "password=" <> libpqConnectionValue (databasePassword databaseConfig)
      ]

libpqConnectionValue :: Text -> Text
libpqConnectionValue value =
  "'" <> Text.replace "\\" "\\\\" (Text.replace "'" "\\'" value) <> "'"

renderRuntimeConnectionError :: LibPQ.Connection -> IO Text
renderRuntimeConnectionError connection = do
  maybeMessage <- LibPQ.errorMessage connection
  pure (renderRuntimeConnectionErrorMessage maybeMessage)

renderRuntimeResultError :: LibPQ.Result -> IO Text
renderRuntimeResultError result = do
  maybeMessage <- LibPQ.resultErrorMessage result
  pure (renderRuntimeResultErrorMessage maybeMessage)

renderRuntimeConnectionErrorMessage :: Maybe ByteString.ByteString -> Text
renderRuntimeConnectionErrorMessage maybeMessage =
  case maybeMessage of
    Nothing -> "libpq connection failed"
    Just message -> Text.strip (TextEncoding.decodeUtf8With lenientDecode message)

renderRuntimeResultErrorMessage :: Maybe ByteString.ByteString -> Text
renderRuntimeResultErrorMessage maybeMessage =
  case maybeMessage of
    Nothing -> "libpq query failed"
    Just message -> Text.strip (TextEncoding.decodeUtf8With lenientDecode message)

queryCommand :: DatabaseConfig -> Text -> PostgresCommand
queryCommand databaseConfig sql =
  PostgresCommand
    { postgresExecutable = "psql",
      postgresArguments =
        commonPsqlArguments databaseConfig
          <> [ "--tuples-only",
               "--no-align",
               "--quiet",
               "--command",
               Text.unpack sql
             ],
      postgresEnvironment = passwordEnvironment databaseConfig
    }

mutationCommand :: DatabaseConfig -> Text -> PostgresCommand
mutationCommand databaseConfig sql =
  PostgresCommand
    { postgresExecutable = "psql",
      postgresArguments =
        commonPsqlArguments databaseConfig
          <> [ "--command",
               Text.unpack sql
             ],
      postgresEnvironment = passwordEnvironment databaseConfig
    }

commonPsqlArguments :: DatabaseConfig -> [String]
commonPsqlArguments databaseConfig =
  [ "--host",
    Text.unpack (databaseHost databaseConfig),
    "--port",
    show (databasePort databaseConfig),
    "--dbname",
    Text.unpack (databaseName databaseConfig),
    "--username",
    Text.unpack (databaseUser databaseConfig),
    "--no-password",
    "--set",
    "ON_ERROR_STOP=1"
  ]

passwordEnvironment :: DatabaseConfig -> [(String, String)]
passwordEnvironment databaseConfig =
  [("PGPASSWORD", Text.unpack (databasePassword databaseConfig))]

homeSummaryQuery :: AppLocale -> Text
homeSummaryQuery locale =
  Text.concat
    [ "SELECT summary FROM ",
      qualifiedTableName "page_content",
      " WHERE route_slug = 'home' AND locale = '",
      renderLocaleCode locale,
      "';"
    ]

secondSummaryQuery :: AppLocale -> Text
secondSummaryQuery locale =
  Text.concat
    [ "SELECT summary FROM ",
      qualifiedTableName "page_content",
      " WHERE route_slug = 'second' AND locale = '",
      renderLocaleCode locale,
      "';"
    ]

secondHighlightsQuery :: AppLocale -> Text
secondHighlightsQuery locale =
  Text.concat
    [ "SELECT highlight FROM ",
      qualifiedTableName "page_highlights",
      " WHERE route_slug = 'second' AND locale = '",
      renderLocaleCode locale,
      "' ORDER BY position ASC;"
    ]

createPendingAccountQuery :: Text
createPendingAccountQuery =
  "WITH inserted_account AS (INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ($1, $2, $3, $6) ON CONFLICT (email_normalized) DO NOTHING RETURNING account_id) INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds) SELECT $4, account_id, $2, $5 FROM inserted_account RETURNING account_id;"

findEmailVerificationQuery :: Text
findEmailVerificationQuery =
  "SELECT account_id, email_normalized, expires_at_nanoseconds FROM web_api.email_verifications WHERE token_digest = $1;"

consumeEmailVerificationQuery :: Text
consumeEmailVerificationQuery =
  "WITH consumed_verification AS (DELETE FROM web_api.email_verifications WHERE token_digest = $1 AND expires_at_nanoseconds > $2 RETURNING account_id) UPDATE web_api.accounts SET email_verified_at_nanoseconds = $2 WHERE account_id IN (SELECT account_id FROM consumed_verification) RETURNING account_id;"

homeSummaryOperation :: DatabaseOperation
homeSummaryOperation =
  DatabaseOperation
    { databaseOperationName = "load-home-page-summary",
      databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

secondSummaryOperation :: DatabaseOperation
secondSummaryOperation =
  DatabaseOperation
    { databaseOperationName = "load-second-page-summary",
      databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

secondHighlightsOperation :: DatabaseOperation
secondHighlightsOperation =
  DatabaseOperation
    { databaseOperationName = "load-second-page-highlights",
      databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

qualifiedTableName :: Text -> Text
qualifiedTableName tableName =
  appSchemaName <> "." <> tableName

ensureRuntimeRoleStatement :: DatabaseConfig -> Text
ensureRuntimeRoleStatement runtimeDatabaseConfig =
  "DO $$ BEGIN IF EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = "
    <> sqlLiteral (databaseUser runtimeDatabaseConfig)
    <> ") THEN EXECUTE "
    <> sqlLiteral alterRoleCommand
    <> "; ELSE EXECUTE "
    <> sqlLiteral createRoleCommand
    <> "; END IF; END $$;"
  where
    createRoleCommand =
      "CREATE ROLE "
        <> sqlIdentifier (databaseUser runtimeDatabaseConfig)
        <> " WITH LOGIN PASSWORD "
        <> sqlLiteral (databasePassword runtimeDatabaseConfig)
        <> " NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT"

    alterRoleCommand =
      "ALTER ROLE "
        <> sqlIdentifier (databaseUser runtimeDatabaseConfig)
        <> " WITH LOGIN PASSWORD "
        <> sqlLiteral (databasePassword runtimeDatabaseConfig)
        <> " NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT"

sqlIdentifier :: Text -> Text
sqlIdentifier value =
  "\"" <> Text.replace "\"" "\"\"" value <> "\""

sqlLiteral :: Text -> Text
sqlLiteral value =
  "'" <> Text.replace "'" "''" value <> "'"

normalizeQueryResult :: Text -> PostgresCommandResult -> Either PostgresRunnerError [Text]
normalizeQueryResult sql commandResult =
  case commandSucceeded commandResult of
    True -> Right (queryRows commandResult)
    False -> Left (PostgresCommandFailed (PostgresCommand {postgresExecutable = "psql", postgresArguments = [Text.unpack sql], postgresEnvironment = []}) commandResult)

parseRequiredScalarRows :: [Text] -> Either PostgresRunnerError Text
parseRequiredScalarRows rows =
  case rows of
    [value] -> Right value
    _ -> Left (UnexpectedQueryRows "expected exactly one row" rows)

queryRows :: PostgresCommandResult -> [Text]
queryRows commandResult =
  filter (not . Text.null) (map Text.strip (Text.lines (postgresStdout commandResult)))

commandSucceeded :: PostgresCommandResult -> Bool
commandSucceeded commandResult =
  case postgresExitCode commandResult of
    ExitSuccess -> True
    ExitFailure _ -> False

mergeEnvironment :: [(String, String)] -> [(String, String)] -> [(String, String)]
mergeEnvironment inheritedEnvironment additionalEnvironment =
  additionalEnvironment
    <> filter (\(key, _) -> key `notElem` map fst additionalEnvironment) inheritedEnvironment

renderLocaleCode :: AppLocale -> Text
renderLocaleCode locale =
  case locale of
    English -> "en"
    Spanish -> "es"

renderRunnerError :: PostgresRunnerError -> Text
renderRunnerError runnerError =
  case runnerError of
    PostgresCommandFailed command commandResult ->
      let commandSummary =
            Text.pack
              (unwords (postgresExecutable command : postgresArguments command))
          environmentSummary = Text.pack (show (postgresEnvironment command))
          stderrText = Text.strip (postgresStderr commandResult)
       in commandSummary `seq`
            environmentSummary `seq`
              if Text.null stderrText
                then "psql command failed"
                else stderrText
    UnexpectedQueryRows message rows ->
      Text.concat
        [ message,
          ": ",
          Text.intercalate ", " rows
        ]
