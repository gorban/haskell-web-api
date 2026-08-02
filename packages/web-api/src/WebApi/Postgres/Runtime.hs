{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.Runtime
  ( PostgresCommand (..),
    PostgresCommandResult (..),
    PostgresRunnerError (..),
    buildPostgresPageRepository,
    buildPostgresPageRepositoryWithRunner,
    buildRuntimePostgresPageRepository,
    buildRuntimePostgresPageRepositoryWithRunner,
    decodeRuntimeQueryValue,
    renderRuntimeConnectionErrorMessage,
    renderRuntimeResultErrorMessage,
    runRuntimeRowsQuery,
    runRuntimeParameterizedRowsQuery,
    runRuntimeScalarQuery,
    runPostgresCommand,
    runStatements,
  )
where

import Control.Exception (bracket)
import Data.Bifunctor (first)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error (lenientDecode)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (env, proc, readCreateProcessWithExitCode)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Database
  ( PageRepository,
  )
import WebApi.Postgres.PageRepository qualified as PageRepository
import WebApi.Postgres.QueryRunner (PageQueryRunner (..))

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

buildPostgresPageRepository :: DatabaseConfig -> PageRepository
buildPostgresPageRepository =
  buildPostgresPageRepositoryWithRunner runPostgresCommand

buildPostgresPageRepositoryWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> PageRepository
buildPostgresPageRepositoryWithRunner runCommand databaseConfig =
  PageRepository.pageRepository
    PageQueryRunner
      { runRequiredTextQuery =
          fmap (first renderRunnerError)
            . runRequiredScalarCommand runCommand databaseConfig,
        runTextRowsQuery =
          fmap (first renderRunnerError)
            . runRowsCommand runCommand databaseConfig
      }

buildRuntimePostgresPageRepository :: DatabaseConfig -> PageRepository
buildRuntimePostgresPageRepository =
  buildRuntimePostgresPageRepositoryWithRunner runRuntimeScalarQuery runRuntimeRowsQuery

buildRuntimePostgresPageRepositoryWithRunner ::
  (DatabaseConfig -> Text -> IO (Either Text Text)) ->
  (DatabaseConfig -> Text -> IO (Either Text [Text])) ->
  DatabaseConfig ->
  PageRepository
buildRuntimePostgresPageRepositoryWithRunner runScalarQuery runRowsQuery databaseConfig =
  PageRepository.pageRepository
    PageQueryRunner
      { runRequiredTextQuery = runScalarQuery databaseConfig,
        runTextRowsQuery = runRowsQuery databaseConfig
      }

runRequiredScalarCommand :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> Text -> IO (Either PostgresRunnerError Text)
runRequiredScalarCommand runCommand databaseConfig sql =
  fmap
    ( \commandResult ->
        case normalizeQueryResult sql commandResult of
          Left runnerError -> Left runnerError
          Right rows ->
            case parseRequiredScalarRows rows of
              Left runnerError -> Left runnerError
              Right value -> Right value
    )
    (runCommand (queryCommand databaseConfig sql))

runRowsCommand :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> Text -> IO (Either PostgresRunnerError [Text])
runRowsCommand runCommand databaseConfig sql =
  fmap
    (normalizeQueryResult sql)
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
runRuntimeQueryRows sql =
  runRuntimeQuery
    (\connection -> LibPQ.exec connection (TextEncoding.encodeUtf8 sql))
    readRuntimeQueryRows

runRuntimeParameterizedQueryRows :: Text -> [Text] -> LibPQ.Connection -> IO (Either Text [[Text]])
runRuntimeParameterizedQueryRows sql parameters =
  runRuntimeQuery
    ( \connection ->
        LibPQ.execParams
          connection
          (TextEncoding.encodeUtf8 sql)
          (fmap parameterValue parameters)
          LibPQ.Text
    )
    readRuntimeQueryTable

runRuntimeQuery :: (LibPQ.Connection -> IO (Maybe LibPQ.Result)) -> (LibPQ.Result -> IO (Either Text value)) -> LibPQ.Connection -> IO (Either Text value)
runRuntimeQuery runQuery readRows connection = do
  maybeResult <- runQuery connection
  maybe (Left <$> renderRuntimeConnectionError connection) readResult maybeResult
  where
    readResult result = do
      resultStatus <- LibPQ.resultStatus result
      if resultStatus == LibPQ.TuplesOk
        then readRows result
        else Left <$> renderRuntimeResultError result

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
