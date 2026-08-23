{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# SPEC #-}

import Control.Exception (IOException, displayException, try)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, withContainerizedPsqlOnPath)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Config (DatabaseConfig (..))
import WebApi.DatabaseSetup (DatabaseSetupCommand (..), DatabaseSetupError (..), loadDatabaseSetupConfig, parseDatabaseSetupCommand, parseDatabaseSetupConfig, renderDatabaseSetupError, runDatabaseSetupArgs, runDatabaseSetupArgsWith, runDatabaseSetupCommand, runDatabaseSetupCommandWith)
import WebApi.Postgres.Testing (PostgresCommand (..), PostgresCommandResult (..), PostgresRunnerError (..), seedStatements)

spec = do
  describe "parseDatabaseSetupCommand" $ do
    it "accepts migrate, seed, and migrate-and-seed" $ do
      parseDatabaseSetupCommand ["migrate"] `shouldBe` Right MigrateDatabase
      parseDatabaseSetupCommand ["seed"] `shouldBe` Right SeedDatabase
      parseDatabaseSetupCommand ["migrate-and-seed"] `shouldBe` Right MigrateAndSeedDatabase

    it "rejects unsupported command lines with explicit guidance" $ do
      parseDatabaseSetupCommand ["deploy"]
        `shouldBe` Left (InvalidDatabaseSetupCommand ["deploy"])
      renderDatabaseSetupError (InvalidDatabaseSetupCommand ["deploy"])
        `shouldBe` "Unsupported database setup command: deploy\nExpected one of: migrate, seed, migrate-and-seed"

    it "keeps command and error values stable" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          configSetupError = DatabaseSetupConfigLoadError loadError
          runtimeConfigSetupError = DatabaseSetupRuntimeConfigLoadError runtimeLoadError
          migrationSetupError = DatabaseSetupMigrationError (UnexpectedQueryRows "expected exactly one row" ["first", "second"])
          seedSetupError = DatabaseSetupSeedError (UnexpectedQueryRows "expected exactly one row" ["seed"])
      MigrateDatabase `shouldBe` MigrateDatabase
      MigrateDatabase `shouldNotBe` SeedDatabase
      show MigrateDatabase `shouldBe` "MigrateDatabase"
      show SeedDatabase `shouldBe` "SeedDatabase"
      show MigrateAndSeedDatabase `shouldBe` "MigrateAndSeedDatabase"
      show [MigrateDatabase, SeedDatabase, MigrateAndSeedDatabase]
        `shouldBe` "[MigrateDatabase,SeedDatabase,MigrateAndSeedDatabase]"
      configSetupError `shouldBe` configSetupError
      configSetupError `shouldNotBe` migrationSetupError
      runtimeConfigSetupError `shouldBe` runtimeConfigSetupError
      runtimeConfigSetupError `shouldNotBe` configSetupError
      seedSetupError `shouldBe` seedSetupError
      show configSetupError
        `shouldBe` "DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")"
      show runtimeConfigSetupError
        `shouldBe` "DatabaseSetupRuntimeConfigLoadError (MissingConfigValue \"DATABASE_PASSWORD\")"
      show migrationSetupError
        `shouldBe` "DatabaseSetupMigrationError (UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"])"
      show seedSetupError
        `shouldBe` "DatabaseSetupSeedError (UnexpectedQueryRows \"expected exactly one row\" [\"seed\"])"
      show [configSetupError]
        `shouldBe` "[DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")]"

    it "renders load, migration, and seed failures explicitly" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          migrationRunnerError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
          nativeMigrationRunnerError = PostgresMigrationFailed "PostgreSQL migration command failed"
          seedRunnerError = UnexpectedQueryRows "expected exactly one row" ["seed"]
      renderDatabaseSetupError (DatabaseSetupConfigLoadError loadError)
        `shouldBe` "Failed to load database setup config: InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\""
      renderDatabaseSetupError (DatabaseSetupRuntimeConfigLoadError runtimeLoadError)
        `shouldBe` "Failed to load runtime database config: MissingConfigValue \"DATABASE_PASSWORD\""
      renderDatabaseSetupError (DatabaseSetupMigrationError migrationRunnerError)
        `shouldBe` "Failed to apply database migrations: expected exactly one row: first, second"
      renderDatabaseSetupError (DatabaseSetupMigrationError nativeMigrationRunnerError)
        `shouldBe` "Failed to apply database migrations: PostgreSQL migration command failed"
      renderDatabaseSetupError (DatabaseSetupSeedError seedRunnerError)
        `shouldBe` "Failed to apply database seed data: expected exactly one row: seed"
      let failedCommandRunnerError =
            PostgresCommandFailed
              PostgresCommand
                { postgresExecutable = "psql",
                  postgresArguments = ["--command", "CREATE ROLE web_api_runtime PASSWORD 'super-secret'"],
                  postgresEnvironment = [("PGPASSWORD", "super-secret")]
                }
              PostgresCommandResult
                { postgresExitCode = ExitFailure 1,
                  postgresStdout = Text.empty,
                  postgresStderr = "psql: error: connection refused"
                }
      renderDatabaseSetupError (DatabaseSetupMigrationError failedCommandRunnerError)
        `shouldBe` "Failed to apply database migrations: psql: error: connection refused"
      renderDatabaseSetupError (DatabaseSetupMigrationError failedCommandRunnerError) `shouldNotContain` "super-secret"

  describe "parseDatabaseSetupConfig" $ do
    it "reads owner-level migration credentials from dedicated environment variables" $
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Right
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret",
              databaseConnectTimeoutSeconds = 10,
              databasePoolCapacity = requiredDatabasePoolCapacity 1
            }

    it "fails missing or invalid migration environment values explicitly" $ do
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PASSWORD")
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadDatabaseSetupConfig" $
    it "reads dedicated migration credentials from the process environment" $
      withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1") $
        withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432") $
          withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev") $
            withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner") $
              withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret") $
                loadDatabaseSetupConfig
                  `shouldReturn` Right
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret",
                        databaseConnectTimeoutSeconds = 10,
                        databasePoolCapacity = requiredDatabasePoolCapacity 1
                      }

  describe "runDatabaseSetupCommand" $ do
    it "uses the default migration environment loader and psql seed runner for single-command setup"
      $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
      $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
      $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
      $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
      $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
      $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
      $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
      $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
      $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
      $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
      $ withFakePsqlScript
        (fmap (,Text.empty) seedStatements)
      $ \argsLogPath -> do
        runDatabaseSetupCommand SeedDatabase `shouldReturn` Right ()
        let renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            (fmap (renderMutationLogEntry setupMigrationPostgresTestConfig) seedStatements)

    it "runs the default native migration and seed setup paths against PostgreSQL" $
      withDefaultDatabaseSetupEnvironment $
        withContainerizedPsqlOnPath $ do
          ensureDefaultPostgresAvailable
          runDatabaseSetupCommand MigrateDatabase `shouldReturn` Right ()
          withSystemTempFile "database-setup-default-migrate.txt" $ \migrateOutputPath migrateOutputHandle -> do
            runDatabaseSetupArgs migrateOutputHandle ["migrate"]
            hClose migrateOutputHandle
            readFile migrateOutputPath `shouldReturn` "Applied database migrations.\n"
          withSystemTempFile "database-setup-default-migrate-and-seed.txt" $ \setupOutputPath setupOutputHandle -> do
            runDatabaseSetupArgs setupOutputHandle ["migrate-and-seed"]
            hClose setupOutputHandle
            readFile setupOutputPath `shouldReturn` "Applied database migrations and seed data.\n"

  describe "runDatabaseSetupCommandWith" $ do
    it "returns configuration load errors before running any commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          unexpectedRuntimeLoader =
            modifyIORef' recordedStepsReference (<> ["runtime-loader"])
              >> pure (Right postgresTestConfig)
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["runner"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Left loadError))
        unexpectedRuntimeLoader
        unexpectedMigrationRunner
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "returns runtime configuration load errors before running database commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = MissingConfigValue "DATABASE_PASSWORD"
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Right ())
          unexpectedSeedRunner _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Left loadError))
        unexpectedMigrationRunner
        unexpectedSeedRunner
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupRuntimeConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "runs migrations and seed data in order with the loaded database config" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let recordMigrationStep migrationDatabaseConfig runtimeDatabaseConfig =
            modifyIORef'
              recordedStepsReference
              (<> ["migrate:" <> databaseUser migrationDatabaseConfig <> "->" <> databaseUser runtimeDatabaseConfig <> ":" <> databaseName runtimeDatabaseConfig])
              >> pure (Right ())
          recordSeedStep databaseRuntimeConfig =
            modifyIORef' recordedStepsReference (<> ["seed:" <> databaseUser databaseRuntimeConfig <> ":" <> databaseName databaseRuntimeConfig])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        recordMigrationStep
        recordSeedStep
        MigrateAndSeedDatabase
        `shouldReturn` Right ()
      readIORef recordedStepsReference
        `shouldReturn` ["migrate:web_api_owner->web_api_app:web_api_prod", "seed:web_api_owner:web_api_prod"]

    it "maps single-command migration failures explicitly" $ do
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Left migrationError))
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)

    it "maps single-command seed failures explicitly" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        SeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

    it "stops after the first migration failure and preserves the runner error" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
          failingMigrations _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Left migrationError)
          unexpectedSeed _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        failingMigrations
        unexpectedSeed
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)
      readIORef recordedStepsReference `shouldReturn` ["migrate"]

    it "maps migrate-and-seed seed failures explicitly after successful migrations" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

  describe "runDatabaseSetupArgsWith" $ do
    it "prints a success message for completed setup commands" $
      withSystemTempFile "database-setup-stdout.txt" $ \outputPath outputHandle -> do
        runDatabaseSetupArgsWith
          (pure (Right migrationPostgresTestConfig))
          (pure (Right postgresTestConfig))
          (\_ _ -> pure (Right ()))
          (\_ -> pure (Right ()))
          outputHandle
          ["seed"]
        hClose outputHandle
        readFile outputPath `shouldReturn` "Applied database seed data.\n"

    it "throws an explicit user error for unsupported command lines" $
      withSystemTempFile "database-setup-invalid-stdout.txt" $ \_ outputHandle -> do
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Right migrationPostgresTestConfig))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["deploy"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Unsupported database setup command: deploy"
          Right () ->
            expectationFailure "expected invalid database setup command to raise an exception"

    it "throws an explicit user error when setup returns a failure" $
      withSystemTempFile "database-setup-error-stdout.txt" $ \_ outputHandle -> do
        let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Left loadError))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["migrate"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Failed to load database setup config"
          Right () ->
            expectationFailure "expected database setup failure to raise an exception"

  describe "runDatabaseSetupArgs"
    $ it "uses the default migration environment loader and psql seed runner for seed output"
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
    $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
    $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
    $ withFakePsqlScript
      (fmap (,Text.empty) seedStatements)
    $ \_ ->
      withSystemTempFile "database-setup-args-seed.txt" $ \seedOutputPath seedOutputHandle -> do
        runDatabaseSetupArgs seedOutputHandle ["seed"]
        hClose seedOutputHandle
        readFile seedOutputPath `shouldReturn` "Applied database seed data.\n"

withDefaultDatabaseSetupEnvironment :: IO a -> IO a
withDefaultDatabaseSetupEnvironment =
  withDatabaseConfigEnvironment "WEB_API_MIGRATION_DATABASE" defaultMigrationPostgresConfig
    . withDatabaseConfigEnvironment "DATABASE" defaultRealPostgresConfig

withDatabaseConfigEnvironment :: String -> DatabaseConfig -> IO a -> IO a
withDatabaseConfigEnvironment prefix databaseConfig =
  withTemporaryEnvironment (prefix <> "_HOST") (Just (Text.unpack (databaseHost databaseConfig)))
    . withTemporaryEnvironment (prefix <> "_PORT") (Just (show (databasePort databaseConfig)))
    . withTemporaryEnvironment (prefix <> "_NAME") (Just (Text.unpack (databaseName databaseConfig)))
    . withTemporaryEnvironment (prefix <> "_USER") (Just (Text.unpack (databaseUser databaseConfig)))
    . withTemporaryEnvironment (prefix <> "_PASSWORD") (Just (Text.unpack (databasePassword databaseConfig)))
