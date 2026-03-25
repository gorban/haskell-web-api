{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (finally)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (StdStream (UseHandle), callProcess, createProcess, cwd, env, proc, std_out, waitForProcess)
import WebApi.Config (AppEnvironmentConfig (..), DatabaseConfig (..), defaultAppEnvironmentConfig)
import WebApi.Database (DatabaseEffect (..), HomePageData (..), SecondPageData (..))
import WebApi.Postgres (buildPostgresDatabaseEffect)
import WebApi.Route (AppLocale (French), AppRequestContext (..), defaultRequestContext)

spec = do
  describe "main" $
    it "exits successfully through the stub HarchWeb server" $ do
      exitCode <- withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
        (_, _, _, processHandle) <- createProcess ((proc "haskell-web-api" []) {std_out = UseHandle outputHandle})
        result <- waitForProcess processHandle
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
        pure result
      exitCode `shouldBe` ExitSuccess

  describe "database integration" $
    it "runs migrate-and-seed and loads seeded page data against real PostgreSQL" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        inheritedEnvironment <- getEnvironment
        exitCode <-
          withSystemTempDirectory "haskell-web-api-db" $ \workingDirectory ->
            withSystemTempFile "haskell-web-api-db-stdout.txt" $ \outputPath outputHandle -> do
              (_, _, _, processHandle) <-
                createProcess
                  ( (proc "haskell-web-api-db" ["migrate-and-seed"])
                      { cwd = Just workingDirectory,
                        env = Just (databaseSetupEnvironment inheritedEnvironment),
                        std_out = UseHandle outputHandle
                      }
                  )
              result <- waitForProcess processHandle
              hClose outputHandle
              readFile outputPath `shouldReturn` "Applied database migrations and seed data.\n"
              pure result
        exitCode `shouldBe` ExitSuccess

        let postgresEffect = buildPostgresDatabaseEffect defaultDatabaseConfig
            frenchRequestContext = defaultRequestContext {requestLocale = French}
        loadHomePageData postgresEffect defaultRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Server-rendered home page with stubbed content."
              }
        loadSecondPageData postgresEffect defaultRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                secondPageDataHighlights = []
              }
        loadHomePageData postgresEffect frenchRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
              }
        loadSecondPageData postgresEffect frenchRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                secondPageDataHighlights = []
              }
  where
    defaultDatabaseConfig = databaseConfig defaultAppEnvironmentConfig

    lookupValue key entries =
      fromMaybe "" (lookup key entries)

    databaseSetupEnvironment inheritedEnvironment =
      [ ("WEB_API_MIGRATION_DATABASE_HOST", Text.unpack (databaseHost defaultDatabaseConfig)),
        ("WEB_API_MIGRATION_DATABASE_PORT", show (databasePort defaultDatabaseConfig)),
        ("WEB_API_MIGRATION_DATABASE_NAME", Text.unpack (databaseName defaultDatabaseConfig)),
        ("WEB_API_MIGRATION_DATABASE_USER", Text.unpack (databaseUser defaultDatabaseConfig)),
        ("WEB_API_MIGRATION_DATABASE_PASSWORD", Text.unpack (databasePassword defaultDatabaseConfig)),
        ("PATH", lookupValue "PATH" inheritedEnvironment)
      ]
        <> filter
          ( \(key, _) ->
              key
                `notElem` [ "PATH",
                            "WEB_API_MIGRATION_DATABASE_HOST",
                            "WEB_API_MIGRATION_DATABASE_PORT",
                            "WEB_API_MIGRATION_DATABASE_NAME",
                            "WEB_API_MIGRATION_DATABASE_USER",
                            "WEB_API_MIGRATION_DATABASE_PASSWORD"
                          ]
          )
          inheritedEnvironment

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  previousValue <- lookupEnv key
  case maybeValue of
    Just value -> setEnv key value
    Nothing -> unsetEnv key
  let restore =
        case previousValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  action `finally` restore

withContainerizedPsqlOnPath :: IO a -> IO a
withContainerizedPsqlOnPath action =
  withSystemTempDirectory "containerized-psql" $ \binDirectory -> do
    originalPath <- fromMaybe "" <$> lookupEnv "PATH"
    let scriptPath = binDirectory <> "/psql"
    writeFile
      scriptPath
      ( unlines
          [ "#!/usr/bin/env bash",
            "set -euo pipefail",
            "if command -v podman >/dev/null 2>&1; then",
            "  exec podman exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\"",
            "fi",
            "exec docker exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\""
          ]
      )
    callProcess "chmod" ["+x", scriptPath]
    withTemporaryEnvironment "PATH" (Just (binDirectory <> ":" <> originalPath)) action

ensureDefaultPostgresAvailable :: IO ()
ensureDefaultPostgresAvailable =
  callProcess
    "bash"
    [ "-eu",
      "-c",
      unlines
        [ "if command -v podman >/dev/null 2>&1; then",
          "  runtime=podman",
          "elif command -v docker >/dev/null 2>&1; then",
          "  runtime=docker",
          "else",
          "  echo 'No supported container runtime found' >&2",
          "  exit 1",
          "fi",
          "\"$runtime\" start web-api-postgres >/dev/null 2>&1 || \\",
          "  \"$runtime\" run --name web-api-postgres -e POSTGRES_USER=web_api -e POSTGRES_PASSWORD=web_api -e POSTGRES_DB=web_api_dev -p 127.0.0.1:5432:5432 -d docker.io/library/postgres:17 >/dev/null",
          "for _ in $(seq 1 30); do",
          "  if \"$runtime\" exec web-api-postgres pg_isready --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api >/dev/null 2>&1; then",
          "    exit 0",
          "  fi",
          "  sleep 1",
          "done",
          "echo 'PostgreSQL did not become ready in time' >&2",
          "exit 1"
        ]
    ]
