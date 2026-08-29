{-# LANGUAGE OverloadedStrings #-}

module TestSupport.RealPostgres
  ( databaseSetupEnvironment,
    containerizedPsqlScriptContents,
    defaultMigrationPostgresConfig,
    defaultPostgresContainerImage,
    defaultRealPostgresConfig,
    ensureDefaultPostgresAvailable,
    ensureDefaultPostgresAvailableScript,
    supportedPostgresMajorVersions,
    withContainerizedPsqlOnPath,
    withPostgresTlsFixtures,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, bracket_, finally, onException, try)
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import Network.Socket qualified as Socket
import System.Directory (findExecutable)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeFileName)
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import WebApi.Config (AppEnvironmentConfig (..), DatabaseConfig (..), DatabaseSslMode (DatabaseSslVerifyFull), DatabaseTransportSecurity (DatabaseTransportSsl), defaultAppEnvironmentConfig)

defaultPostgresContainerImage :: String
defaultPostgresContainerImage = "docker.io/library/postgres:17"

supportedPostgresMajorVersions :: [Int]
supportedPostgresMajorVersions = [17]

realHostPsqlPathVariable :: String
realHostPsqlPathVariable = "WEB_API_REAL_PSQL_PATH"

databaseEndpointReachabilityScriptLines :: [String]
databaseEndpointReachabilityScriptLines =
  [ "database_endpoint_is_reachable() {",
    "  bash -c '</dev/tcp/127.0.0.1/5432' >/dev/null 2>&1",
    "}"
  ]

hostPsqlSelectionScriptLines :: [String]
hostPsqlSelectionScriptLines =
  [ "host_psql_path=\"${" <> realHostPsqlPathVariable <> ":-}\"",
    "host_psql_is_available() {",
    "  [ -n \"$host_psql_path\" ] && [ -x \"$host_psql_path\" ]",
    "}",
    "owner_is_superuser_via_host_psql() {",
    "  [ \"$(PGPASSWORD=web_api_owner \"$host_psql_path\" --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api_owner --no-password --tuples-only --no-align --quiet --set ON_ERROR_STOP=1 --command 'SELECT CASE WHEN rolsuper THEN $$t$$ ELSE $$f$$ END FROM pg_catalog.pg_roles WHERE rolname = current_user;' 2>/dev/null || true)\" = \"t\" ]",
    "}",
    "ensure_owner_superuser_via_host_psql() {",
    "  PGPASSWORD=web_api \"$host_psql_path\" --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api --no-password --set ON_ERROR_STOP=1 --command \"ALTER ROLE web_api_owner WITH LOGIN SUPERUSER PASSWORD 'web_api_owner';\" >/dev/null 2>&1 || \\",
    "    PGPASSWORD=web_api \"$host_psql_path\" --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api --no-password --set ON_ERROR_STOP=1 --command \"CREATE ROLE web_api_owner WITH LOGIN SUPERUSER PASSWORD 'web_api_owner';\" >/dev/null 2>&1",
    "}"
  ]

containerRuntimeSelectionScriptLines :: [String]
containerRuntimeSelectionScriptLines =
  [ "runtime_with_running_container() {",
    "  for candidate in docker podman; do",
    "    if command -v \"$candidate\" >/dev/null 2>&1 && [ \"$(\"$candidate\" inspect --format '{{.State.Running}}' web-api-postgres 2>/dev/null || true)\" = \"true\" ]; then",
    "      printf '%s\\n' \"$candidate\"",
    "      return 0",
    "    fi",
    "  done",
    "  return 1",
    "}",
    "runtime_with_existing_container() {",
    "  for candidate in docker podman; do",
    "    if command -v \"$candidate\" >/dev/null 2>&1 && \"$candidate\" inspect web-api-postgres >/dev/null 2>&1; then",
    "      printf '%s\\n' \"$candidate\"",
    "      return 0",
    "    fi",
    "  done",
    "  return 1",
    "}",
    "if runtime=$(runtime_with_running_container); then",
    "  :",
    "elif runtime=$(runtime_with_existing_container); then",
    "  :",
    "elif command -v podman >/dev/null 2>&1; then",
    "  runtime=podman",
    "elif command -v docker >/dev/null 2>&1; then",
    "  runtime=docker",
    "else",
    "  echo 'No supported container runtime found' >&2",
    "  exit 1",
    "fi"
  ]

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

containerizedPsqlScriptContents :: String
containerizedPsqlScriptContents =
  unlines $
    [ "#!/usr/bin/env bash",
      "set -euo pipefail"
    ]
      <> databaseEndpointReachabilityScriptLines
      <> [ "host_psql_path=\"${" <> realHostPsqlPathVariable <> ":-}\"",
           "if [ -n \"$host_psql_path\" ] && [ -x \"$host_psql_path\" ] && database_endpoint_is_reachable; then",
           "  exec \"$host_psql_path\" \"$@\"",
           "fi"
         ]
      <> containerRuntimeSelectionScriptLines
      <> ["exec \"$runtime\" exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\""]

withContainerizedPsqlOnPath :: IO a -> IO a
withContainerizedPsqlOnPath action =
  withSystemTempDirectory "containerized-psql" $ \binDirectory -> do
    originalPath <- fromMaybe "" <$> lookupEnv "PATH"
    realHostPsqlPath <- findExecutable "psql"
    let scriptPath = binDirectory <> "/psql"
    writeFile scriptPath containerizedPsqlScriptContents
    callProcess "chmod" ["+x", scriptPath]
    withTemporaryEnvironment realHostPsqlPathVariable realHostPsqlPath $
      withTemporaryEnvironment "PATH" (Just (binDirectory <> ":" <> originalPath)) action

ensureDefaultPostgresAvailableScript :: String
ensureDefaultPostgresAvailableScript =
  unlines $
    databaseEndpointReachabilityScriptLines
      <> hostPsqlSelectionScriptLines
      <> [ "if database_endpoint_is_reachable && host_psql_is_available; then",
           "  if owner_is_superuser_via_host_psql; then",
           "    exit 0",
           "  fi",
           "  if ensure_owner_superuser_via_host_psql; then",
           "    exit 0",
           "  fi",
           "fi",
           "if database_endpoint_is_reachable && ! host_psql_is_available; then",
           "  exit 0",
           "fi"
         ]
      <> containerRuntimeSelectionScriptLines
      <> [ "container_is_running() {",
           "  [ \"$($runtime inspect --format '{{.State.Running}}' web-api-postgres 2>/dev/null || true)\" = \"true\" ]",
           "}",
           "start_container() {",
           "  if container_is_running; then",
           "    return 0",
           "  fi",
           "  \"$runtime\" start web-api-postgres >/dev/null 2>&1 && return 0",
           "  \"$runtime\" run --name web-api-postgres -e POSTGRES_USER=web_api_owner -e POSTGRES_PASSWORD=web_api_owner -e POSTGRES_DB=web_api_dev -p 127.0.0.1:5432:5432 -d " <> defaultPostgresContainerImage <> " >/dev/null",
           "}",
           "wait_until_ready() {",
           "  local ready=false",
           "  for _ in $(seq 1 30); do",
           "    if \"$runtime\" exec web-api-postgres pg_isready --host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api_owner >/dev/null 2>&1; then",
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

ensureDefaultPostgresAvailable :: IO ()
ensureDefaultPostgresAvailable =
  callProcess
    "bash"
    [ "-eu",
      "-c",
      ensureDefaultPostgresAvailableScript
    ]

-- | Starts isolated PostgreSQL 17 listeners with and without TLS. The TLS
-- listener's self-signed certificate is deliberately trusted only by the
-- returned verified configuration, while the other returned configurations
-- prove bad-CA, hostname-mismatch, and TLS-disabled failures through libpq.
withPostgresTlsFixtures :: (DatabaseConfig -> DatabaseConfig -> DatabaseConfig -> DatabaseConfig -> IO value) -> IO value
withPostgresTlsFixtures action = do
  containerRuntime <- requireContainerRuntime
  withSystemTempDirectory "web-api-postgres-tls" $ \certificateDirectory -> do
    callProcess "chmod" ["755", certificateDirectory]
    let fixtureName = "web-api-postgres-tls-" <> takeFileName certificateDirectory
        tlsContainerName = fixtureName <> "-tls"
        plainContainerName = fixtureName <> "-plain"
        certificateMount = certificateDirectory <> ":/tls:Z"
    withUnusedLoopbackPort $ \tlsPort ->
      withUnusedLoopbackPort $ \plainPort -> do
        callProcess
          containerRuntime
          [ "run",
            "--rm",
            "--volume",
            certificateMount,
            "--entrypoint",
            "sh",
            defaultPostgresContainerImage,
            "-c",
            "openssl req -new -x509 -nodes -subj '/CN=127.0.0.1' -addext 'subjectAltName=IP:127.0.0.1' -keyout /tls/server.key -out /tls/root.crt -days 1 && cp /tls/root.crt /tls/server.crt && openssl req -new -x509 -nodes -subj '/CN=untrusted.example' -keyout /tls/untrusted.key -out /tls/untrusted.crt -days 1 && chmod 600 /tls/server.key && chown 999:999 /tls/server.key /tls/server.crt /tls/root.crt /tls/untrusted.crt"
          ]
        let startTls =
              callProcess
                containerRuntime
                [ "run",
                  "--rm",
                  "--detach",
                  "--name",
                  tlsContainerName,
                  "--env",
                  "POSTGRES_USER=web_api_runtime",
                  "--env",
                  "POSTGRES_PASSWORD=web_api",
                  "--env",
                  "POSTGRES_DB=web_api_tls",
                  "--publish",
                  "127.0.0.1:" <> show tlsPort <> ":5432",
                  "--volume",
                  certificateDirectory <> ":/tls:ro,Z",
                  defaultPostgresContainerImage,
                  "postgres",
                  "-c",
                  "ssl=on",
                  "-c",
                  "ssl_cert_file=/tls/server.crt",
                  "-c",
                  "ssl_key_file=/tls/server.key"
                ]
            startPlain =
              callProcess
                containerRuntime
                [ "run",
                  "--rm",
                  "--detach",
                  "--name",
                  plainContainerName,
                  "--env",
                  "POSTGRES_USER=web_api_runtime",
                  "--env",
                  "POSTGRES_PASSWORD=web_api",
                  "--env",
                  "POSTGRES_DB=web_api_tls",
                  "--publish",
                  "127.0.0.1:" <> show plainPort <> ":5432",
                  defaultPostgresContainerImage
                ]
            stopContainer containerName = do
              _ <- try (callProcess containerRuntime ["rm", "--force", containerName]) :: IO (Either IOError ())
              pure ()
            fixtureConfig port rootCertificate =
              defaultRealPostgresConfig
                { databasePort = port,
                  databaseName = "web_api_tls",
                  databaseUser = "web_api_runtime",
                  databasePassword = "web_api",
                  databaseTransportSecurity = DatabaseTransportSsl DatabaseSslVerifyFull (Just (Text.pack rootCertificate))
                }
            verifiedConfig = fixtureConfig tlsPort (certificateDirectory <> "/root.crt")
            untrustedCaConfig = fixtureConfig tlsPort (certificateDirectory <> "/untrusted.crt")
            hostnameMismatchConfig = verifiedConfig {databaseHost = "localhost"}
            tlsDisabledConfig = fixtureConfig plainPort (certificateDirectory <> "/root.crt")
        bracket_
          ( (startTls >> startPlain >> waitForPostgres containerRuntime tlsContainerName >> waitForPostgres containerRuntime plainContainerName)
              `onException` (stopContainer plainContainerName >> stopContainer tlsContainerName)
          )
          (stopContainer plainContainerName >> stopContainer tlsContainerName)
          (action verifiedConfig untrustedCaConfig hostnameMismatchConfig tlsDisabledConfig)

requireContainerRuntime :: IO FilePath
requireContainerRuntime = do
  podman <- findExecutable "podman"
  docker <- findExecutable "docker"
  case podman <|> docker of
    Just executable -> pure executable
    Nothing -> ioError (userError "A podman or docker runtime is required for PostgreSQL TLS integration tests")

waitForPostgres :: FilePath -> String -> IO ()
waitForPostgres containerRuntime containerName = go (30 :: Int)
  where
    go attempts = do
      ready <- try (callProcess containerRuntime ["exec", containerName, "pg_isready", "--username=web_api_runtime", "--dbname=web_api_tls"]) :: IO (Either IOError ())
      case ready of
        Right () -> pure ()
        Left _
          | attempts > 0 -> threadDelay 1000000 >> go (attempts - 1)
          | otherwise -> ioError (userError "PostgreSQL TLS fixture did not become ready")

withUnusedLoopbackPort :: (Int -> IO value) -> IO value
withUnusedLoopbackPort action = do
  port <-
    bracket
      (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol)
      Socket.close
      ( \listeningSocket -> do
          Socket.bind listeningSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
          socketAddress <- Socket.getSocketName listeningSocket
          case socketAddress of
            Socket.SockAddrInet availablePort _ -> pure (fromIntegral availablePort)
            _ -> ioError (userError "Expected an IPv4 loopback socket")
      )
  action port

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
