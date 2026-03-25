{-# LANGUAGE OverloadedStrings #-}

module TestSupport.RealPostgres
  ( databaseSetupEnvironment,
    defaultRealPostgresConfig,
    ensureDefaultPostgresAvailable,
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

defaultRealPostgresConfig :: DatabaseConfig
defaultRealPostgresConfig =
  databaseConfig defaultAppEnvironmentConfig

databaseSetupEnvironment :: [(String, String)] -> [(String, String)]
databaseSetupEnvironment inheritedEnvironment =
  [ ("WEB_API_MIGRATION_DATABASE_HOST", Text.unpack (databaseHost defaultRealPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_PORT", show (databasePort defaultRealPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_NAME", Text.unpack (databaseName defaultRealPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_USER", Text.unpack (databaseUser defaultRealPostgresConfig)),
    ("WEB_API_MIGRATION_DATABASE_PASSWORD", Text.unpack (databasePassword defaultRealPostgresConfig)),
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
