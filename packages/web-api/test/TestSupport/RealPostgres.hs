{-# LANGUAGE OverloadedStrings #-}

module TestSupport.RealPostgres
  ( databaseSetupEnvironment,
    defaultMigrationPostgresConfig,
    defaultPostgresContainerImage,
    defaultRealPostgresConfig,
    ensureDefaultPostgresAvailable,
    supportedPostgresMajorVersions,
    withContainerizedPsqlOnPath,
  )
where

import Control.Exception (finally)
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import WebApi.Config (AppEnvironmentConfig (..), DatabaseConfig (..), defaultAppEnvironmentConfig)

defaultPostgresContainerImage :: String
defaultPostgresContainerImage = "docker.io/library/postgres:17"

supportedPostgresMajorVersions :: [Int]
supportedPostgresMajorVersions = [17]

defaultRealPostgresConfig :: DatabaseConfig
defaultRealPostgresConfig =
  databaseConfig defaultAppEnvironmentConfig

defaultMigrationPostgresConfig :: DatabaseConfig
defaultMigrationPostgresConfig =
  defaultRealPostgresConfig
    { databaseUser = "web_api_owner",
      databasePassword = "web_api_owner"
    }

databaseSetupEnvironment :: [(String, String)] -> [(String, String)]
databaseSetupEnvironment inheritedEnvironment =
  [ ("DATABASE_HOST", Text.unpack (databaseHost defaultRealPostgresConfig)),
    ("DATABASE_PORT", show (databasePort defaultRealPostgresConfig)),
    ("DATABASE_NAME", Text.unpack (databaseName defaultRealPostgresConfig)),
    ("DATABASE_USER", Text.unpack (databaseUser defaultRealPostgresConfig)),
    ("DATABASE_PASSWORD", Text.unpack (databasePassword defaultRealPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_HOST", Text.unpack (databaseHost defaultMigrationPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_PORT", show (databasePort defaultMigrationPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_NAME", Text.unpack (databaseName defaultMigrationPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_USER", Text.unpack (databaseUser defaultMigrationPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_PASSWORD", Text.unpack (databasePassword defaultMigrationPostgresConfig)),
    ("PATH", lookupValue "PATH" inheritedEnvironment)
  ]
    <> filter
      ( \(key, _) ->
          key
            `notElem` [ "PATH",
                        "DATABASE_HOST",
                        "DATABASE_PORT",
                        "DATABASE_NAME",
                        "DATABASE_USER",
                        "DATABASE_PASSWORD",
                        "WEB_API_MIGRATION_DATABASE_HOST",
                        "WEB_API_MIGRATION_DATABASE_PORT",
                        "WEB_API_MIGRATION_DATABASE_NAME",
                        "WEB_API_MIGRATION_DATABASE_USER",
                        "WEB_API_MIGRATION_DATABASE_PASSWORD"
                      ]
      )
      inheritedEnvironment
  where
    lookupValue key entries =
      fromMaybe "" (lookup key entries)

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
          "container_is_running() {",
          "  [ \"$($runtime inspect --format '{{.State.Running}}' web-api-postgres 2>/dev/null || true)\" = \"true\" ]",
          "}",
          "start_container() {",
          "  if container_is_running; then",
          "    return 0",
          "  fi",
          "  \"$runtime\" start web-api-postgres >/dev/null 2>&1 && return 0",
          "  \"$runtime\" run --name web-api-postgres -e POSTGRES_USER=web_api -e POSTGRES_PASSWORD=web_api -e POSTGRES_DB=web_api_dev -p 127.0.0.1:5432:5432 -d " <> defaultPostgresContainerImage <> " >/dev/null",
          "}",
          "wait_until_ready() {",
          "  local ready=false",
          "  for _ in $(seq 1 30); do",
          "    if \"$runtime\" exec web-api-postgres pg_isready --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api >/dev/null 2>&1; then",
          "      ready=true",
          "      break",
          "    fi",
          "    sleep 1",
          "  done",
          "  if [ \"$ready\" != true ]; then",
          "    echo 'PostgreSQL did not become ready in time' >&2",
          "    exit 1",
          "  fi",
          "}",
          "owner_is_superuser() {",
          "  [ \"$(PGPASSWORD=web_api_owner \"$runtime\" exec -e PGPASSWORD=web_api_owner web-api-postgres psql --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api_owner --no-password --tuples-only --no-align --quiet --set ON_ERROR_STOP=1 --command 'SELECT CASE WHEN rolsuper THEN $$t$$ ELSE $$f$$ END FROM pg_catalog.pg_roles WHERE rolname = current_user;' 2>/dev/null || true)\" = \"t\" ]",
          "}",
          "ensure_owner_superuser_via_runtime() {",
          "  PGPASSWORD=web_api \"$runtime\" exec -e PGPASSWORD=web_api web-api-postgres psql --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api --no-password --set ON_ERROR_STOP=1 --command \"ALTER ROLE web_api_owner WITH LOGIN SUPERUSER PASSWORD 'web_api_owner';\" >/dev/null 2>&1 || \\",
          "    PGPASSWORD=web_api \"$runtime\" exec -e PGPASSWORD=web_api web-api-postgres psql --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api --no-password --set ON_ERROR_STOP=1 --command \"CREATE ROLE web_api_owner WITH LOGIN SUPERUSER PASSWORD 'web_api_owner';\" >/dev/null 2>&1",
          "}",
          "start_container",
          "wait_until_ready",
          "if owner_is_superuser; then",
          "  exit 0",
          "fi",
          "if ensure_owner_superuser_via_runtime; then",
          "  exit 0",
          "fi",
          "\"$runtime\" rm -f web-api-postgres >/dev/null",
          "start_container",
          "wait_until_ready",
          "ensure_owner_superuser_via_runtime",
          "if owner_is_superuser; then",
          "  exit 0",
          "fi",
          "echo 'Failed to provision web_api_owner as a superuser for tests' >&2",
          "exit 1"
        ]
    ]

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
