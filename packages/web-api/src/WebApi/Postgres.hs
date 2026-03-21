{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres
  ( PostgresCommand (..),
    PostgresCommandResult (..),
    PostgresRunnerError (..),
    buildPostgresDatabaseEffect,
    buildPostgresDatabaseEffectWithRunner,
    migrationStatements,
    runPostgresMigrations,
    runPostgresMigrationsWithRunner,
    runPostgresSeed,
    runPostgresSeedWithRunner,
    seedStatements,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.Process (env, proc, readCreateProcessWithExitCode)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Database
  ( DatabaseEffect (..),
    DatabaseError (..),
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
    { loadHomePageData =
        \requestContext ->
          fmap
            (fmap HomePageData)
            (runRequiredScalarQuery runCommand databaseConfig (homeSummaryQuery (requestLocale requestContext)) HomePageDataError),
      loadSecondPageData =
        \requestContext -> do
          summaryResult <- runRequiredScalarQuery runCommand databaseConfig (secondSummaryQuery (requestLocale requestContext)) SecondPageDataError
          highlightsResult <- runOptionalRowsQuery runCommand databaseConfig (secondHighlightsQuery (requestLocale requestContext)) SecondPageDataError
          pure $
            SecondPageData
              <$> summaryResult
              <*> highlightsResult
    }

runPostgresMigrations :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrations =
  runPostgresMigrationsWithRunner runPostgresCommand

runPostgresMigrationsWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresMigrationsWithRunner runCommand databaseConfig =
  runStatements runCommand databaseConfig migrationStatements

runPostgresSeed :: DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeed =
  runPostgresSeedWithRunner runPostgresCommand

runPostgresSeedWithRunner :: (PostgresCommand -> IO PostgresCommandResult) -> DatabaseConfig -> IO (Either PostgresRunnerError ())
runPostgresSeedWithRunner runCommand databaseConfig =
  runStatements runCommand databaseConfig seedStatements

migrationStatements :: [Text]
migrationStatements =
  [ "CREATE TABLE IF NOT EXISTS page_content (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));",
    "CREATE TABLE IF NOT EXISTS page_highlights (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));"
  ]

seedStatements :: [Text]
seedStatements =
  [ "DELETE FROM page_highlights;",
    "DELETE FROM page_content;",
    "INSERT INTO page_content (route_slug, locale, summary) VALUES ('home', 'en', 'Server-rendered home page with stubbed content.'), ('home', 'fr', 'Accueil cote serveur avec des donnees de developpement preconfigurees.'), ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'fr', 'Second page content with stubbed data ready for future loaders.');"
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
    [ "SELECT summary FROM page_content WHERE route_slug = 'home' AND locale = '",
      renderLocaleCode locale,
      "';"
    ]

secondSummaryQuery :: AppLocale -> Text
secondSummaryQuery locale =
  Text.concat
    [ "SELECT summary FROM page_content WHERE route_slug = 'second' AND locale = '",
      renderLocaleCode locale,
      "';"
    ]

secondHighlightsQuery :: AppLocale -> Text
secondHighlightsQuery locale =
  Text.concat
    [ "SELECT highlight FROM page_highlights WHERE route_slug = 'second' AND locale = '",
      renderLocaleCode locale,
      "' ORDER BY position ASC;"
    ]

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
    French -> "fr"

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
