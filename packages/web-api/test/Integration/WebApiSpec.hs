{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (threadDelay)
import Control.Exception (finally)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, socket, tupleToHostAddress)
import Network.Socket qualified as NetworkSocket
import Network.Socket.ByteString qualified as SocketByteString
import Numeric (readHex)
import System.Directory (doesFileExist)
import System.Environment (getEnvironment, getExecutablePath, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath (takeDirectory, (</>))
import System.IO (hClose)
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (ProcessHandle, StdStream (UseHandle), createProcess, cwd, env, getProcessExitCode, proc, readCreateProcessWithExitCode, readProcessWithExitCode, std_out, terminateProcess, waitForProcess)
import TestSupport.AccountJwt (withTestAccountJwtFixture)
import TestSupport.RealPostgres (databaseSetupEnvironment, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, supportedPostgresMajorVersions, withContainerizedPsqlOnPath)
import WebApi.Config (DatabaseConfig (..))
import WebApi.Database (DatabaseError (..), DatabaseResult (..), PageRepository (..), SecondPageData (..))
import WebApi.Postgres (buildPostgresPageRepository, buildRuntimePostgresPageRepository, newPostgresPool)
import WebApi.Route (AppLocale (Spanish), AppRequestContext (..), defaultRequestContext)

loadSecondPageValueForRequest :: PageRepository -> AppRequestContext -> IO (Either DatabaseError SecondPageData)
loadSecondPageValueForRequest pageRepository requestContext =
  databaseResultValue <$> loadSecondPage pageRepository (requestLocale requestContext)

spec = do
  describe "main" $ do
    it "stays running while idle, serves real HTTP traffic, and only stops when terminated" $ withTestAccountJwtFixture $ \_ jwtConfigLines -> do
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempDirectory "haskell-web-api-run" $ \workingDirectory -> do
          writeFile
            (workingDirectory <> "/.env")
            ( "LISTENER_0_PORT="
                <> show unusedPort
                <> "\nDATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\nCSRF_SIGNING_ACTIVE_KEY_ID=development-v1\nCSRF_SIGNING_VERIFICATION_KEYS=development-v1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
                <> unlines jwtConfigLines
            )
          webApiExecutable <- testBuildToolPath "haskell-web-api"
          withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
            (_, _, _, processHandle) <-
              createProcess
                ( (proc webApiExecutable [])
                    { cwd = Just workingDirectory,
                      std_out = UseHandle outputHandle
                    }
                )
            (responseText, runningExitCode) <-
              ( do
                  threadDelay 1500000
                  idleExitCode <- getProcessExitCode processHandle
                  readyResponse <- waitForProcessResponse processHandle unusedPort "/api/status"
                  stillRunningExitCode <- getProcessExitCode processHandle
                  idleExitCode `shouldBe` Nothing
                  pure (readyResponse, stillRunningExitCode)
              )
                `finally` do
                  terminateProcess processHandle
                  _ <- waitForProcess processHandle
                  hClose outputHandle
            output <- readFile outputPath
            expectAll
              ( (responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}")
                  :| [ runningExitCode `shouldBe` Nothing,
                       output
                         `shouldBe` unlines
                           [ "Loaded config file: ./.env",
                             "Config file missing: ./.env.local",
                             "Parsed listener config: http://127.0.0.1:" <> show unusedPort,
                             "HTTP Server listening at http://127.0.0.1:" <> show unusedPort
                           ]
                     ]
              )

    it "defaults plain HTTP traffic to HTTPS redirects when both HTTP and manual TLS listeners are configured" $ withTestAccountJwtFixture $ \_ jwtConfigLines ->
      withUnusedLoopbackPort $ \httpPort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempDirectory "haskell-web-api-https-redirect" $ \workingDirectory -> do
              writeFile
                (workingDirectory <> "/.env")
                ( unlines
                    ( [ "LISTENER_0_HOST=127.0.0.1",
                        "LISTENER_0_PORT=" <> show httpPort,
                        "LISTENER_0_SCHEME=http",
                        "LISTENER_1_HOST=127.0.0.1",
                        "LISTENER_1_PORT=" <> show httpsPort,
                        "LISTENER_1_SCHEME=https",
                        "LISTENER_1_TLS_SOURCE=manual",
                        "LISTENER_1_TLS_CERTIFICATE_FILE=" <> certificatePath,
                        "LISTENER_1_TLS_PRIVATE_KEY_FILE=" <> privateKeyPath,
                        "DATABASE_PASSWORD=web_api",
                        "SMTP_PASSWORD=password",
                        "TOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                        "CSRF_SIGNING_ACTIVE_KEY_ID=development-v1",
                        "CSRF_SIGNING_VERIFICATION_KEYS=development-v1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                      ]
                        <> jwtConfigLines
                    )
                )
              webApiExecutable <- testBuildToolPath "haskell-web-api"
              withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc webApiExecutable [])
                        { cwd = Just workingDirectory,
                          std_out = UseHandle outputHandle
                        }
                    )
                (redirectHeaders, httpsResponseText, runningExitCode) <-
                  ( do
                      readyRedirectHeaders <- waitForProcessHttpHeaders processHandle httpPort "/api/status"
                      readyHttpsResponse <- waitForProcessTrustedHttpsResponse processHandle certificatePath httpsPort "/api/status"
                      stillRunningExitCode <- getProcessExitCode processHandle
                      pure (readyRedirectHeaders, readyHttpsResponse, stillRunningExitCode)
                  )
                    `finally` do
                      terminateProcess processHandle
                      _ <- waitForProcess processHandle
                      hClose outputHandle
                output <- readFile outputPath
                expectAll
                  ( (redirectHeaders `shouldContain` "308 Permanent Redirect")
                      :| [ redirectHeaders `shouldContain` ("Location: https://127.0.0.1:" <> show httpsPort <> "/api/status"),
                           httpsResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}",
                           runningExitCode `shouldBe` Nothing,
                           output
                             `shouldBe` unlines
                               [ "Loaded config file: ./.env",
                                 "Config file missing: ./.env.local",
                                 "Parsed listener config: http://127.0.0.1:" <> show httpPort,
                                 "Parsed listener config: https://127.0.0.1:" <> show httpsPort,
                                 "HTTP Server listening at http://127.0.0.1:" <> show httpPort,
                                 "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                               ]
                         ]
                  )

    it "lets REDIRECT_HTTP_TO_HTTPS=false keep both HTTP and HTTPS listeners serving traffic" $ withTestAccountJwtFixture $ \_ jwtConfigLines ->
      withUnusedLoopbackPort $ \httpPort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempDirectory "haskell-web-api-dual-listener" $ \workingDirectory -> do
              writeFile
                (workingDirectory <> "/.env")
                ( unlines
                    ( [ "LISTENER_0_HOST=127.0.0.1",
                        "LISTENER_0_PORT=" <> show httpPort,
                        "LISTENER_0_SCHEME=http",
                        "LISTENER_1_HOST=127.0.0.1",
                        "LISTENER_1_PORT=" <> show httpsPort,
                        "LISTENER_1_SCHEME=https",
                        "LISTENER_1_TLS_SOURCE=manual",
                        "LISTENER_1_TLS_CERTIFICATE_FILE=" <> certificatePath,
                        "LISTENER_1_TLS_PRIVATE_KEY_FILE=" <> privateKeyPath,
                        "REDIRECT_HTTP_TO_HTTPS=false",
                        "DATABASE_PASSWORD=web_api",
                        "SMTP_PASSWORD=password",
                        "TOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA",
                        "CSRF_SIGNING_ACTIVE_KEY_ID=development-v1",
                        "CSRF_SIGNING_VERIFICATION_KEYS=development-v1:AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
                      ]
                        <> jwtConfigLines
                    )
                )
              webApiExecutable <- testBuildToolPath "haskell-web-api"
              withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc webApiExecutable [])
                        { cwd = Just workingDirectory,
                          std_out = UseHandle outputHandle
                        }
                    )
                (httpResponseText, httpsResponseText, runningExitCode) <-
                  ( do
                      readyHttpResponse <- waitForProcessResponse processHandle httpPort "/api/status"
                      readyHttpsResponse <- waitForProcessTrustedHttpsResponse processHandle certificatePath httpsPort "/api/status"
                      stillRunningExitCode <- getProcessExitCode processHandle
                      pure (readyHttpResponse, readyHttpsResponse, stillRunningExitCode)
                  )
                    `finally` do
                      terminateProcess processHandle
                      _ <- waitForProcess processHandle
                      hClose outputHandle
                output <- readFile outputPath
                expectAll
                  ( (httpResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}")
                      :| [ httpsResponseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}",
                           runningExitCode `shouldBe` Nothing,
                           output
                             `shouldBe` unlines
                               [ "Loaded config file: ./.env",
                                 "Config file missing: ./.env.local",
                                 "Parsed listener config: http://127.0.0.1:" <> show httpPort,
                                 "Parsed listener config: https://127.0.0.1:" <> show httpsPort,
                                 "HTTP Server listening at http://127.0.0.1:" <> show httpPort,
                                 "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                               ]
                         ]
                  )

  describe "database integration" $ do
    it
      "runs migrate-and-seed, verifies the supported PostgreSQL major version, loads seeded page data, and enforces runtime-role privileges against real PostgreSQL"
      ( withContainerizedPsqlOnPath $ do
          ensureDefaultPostgresAvailable
          inheritedEnvironment <- getEnvironment
          exitCode <-
            withSystemTempDirectory "haskell-web-api-db" $ \workingDirectory ->
              withSystemTempFile "haskell-web-api-db-stdout.txt" $ \outputPath outputHandle -> do
                databaseSetupExecutable <- testBuildToolPath "haskell-web-api-db"
                (_, _, _, processHandle) <-
                  createProcess
                    ( (proc databaseSetupExecutable ["migrate-and-seed"])
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

          supportedVersionResult <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_owner", "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", "SELECT current_setting('server_version_num')::integer / 10000;"])
                  { env = Just (("PGPASSWORD", "web_api_owner") : inheritedEnvironment)
                  }
              )
              ""
          supportedVersionResult
            `shouldSatisfy` (`elem` fmap (\majorVersion -> (ExitSuccess, show majorVersion <> "\n", "")) supportedPostgresMajorVersions)

          let postgresEffect = buildPostgresPageRepository defaultRealPostgresConfig
              spanishRequestContext = defaultRequestContext {requestLocale = Spanish}
          loadSecondPageValueForRequest postgresEffect defaultRequestContext
            `shouldReturn` Right
              SecondPageData
                { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                  secondPageDataHighlights = []
                }
          loadSecondPageValueForRequest postgresEffect spanishRequestContext
            `shouldReturn` Right
              SecondPageData
                { secondPageDataSummary = "Contenido de la segunda pagina con datos de ejemplo listos para futuros cargadores.",
                  secondPageDataHighlights = []
                }

          withTemporaryEnvironment "PATH" (Just "") $ do
            runtimePool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
            let runtimePostgresEffect = buildRuntimePostgresPageRepository runtimePool
            loadSecondPageValueForRequest runtimePostgresEffect spanishRequestContext
              `shouldReturn` Right
                SecondPageData
                  { secondPageDataSummary = "Contenido de la segunda pagina con datos de ejemplo listos para futuros cargadores.",
                    secondPageDataHighlights = []
                  }

          allowedSelect <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          allowedSelect `shouldBe` (ExitSuccess, "Second page content with stubbed data ready for future loaders.\n", "")

          forbiddenInsert <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "INSERT INTO web_api.page_content (route_slug, locale, summary) VALUES ('forbidden', 'en', 'nope');"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenInsert `shouldNotBe` ExitSuccess
          thd3 forbiddenInsert `shouldContain` "permission denied"

          forbiddenSchemaChange <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "DO $$ BEGIN EXECUTE format('CREATE TABLE web_api.forbidden_runtime_table_%s (id INTEGER);', pg_backend_pid()); END $$;"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenSchemaChange `shouldNotBe` ExitSuccess
          thd3 forbiddenSchemaChange `shouldContain` "permission denied"

          forbiddenRoleCreate <-
            readCreateProcessWithExitCode
              ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--command", "CREATE ROLE forbidden_runtime_role LOGIN PASSWORD 'forbidden_runtime_role';"])
                  { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                  }
              )
              ""
          fst3 forbiddenRoleCreate `shouldNotBe` ExitSuccess
          thd3 forbiddenRoleCreate `shouldContain` "permission denied"
      )

    it
      "installs controlled account-audit append, RLS scope reads, and deterministic partition maintenance on real PostgreSQL"
      ( withContainerizedPsqlOnPath $ do
          ensureDefaultPostgresAvailable
          inheritedEnvironment <- getEnvironment
          databaseSetupExecutable <- testBuildToolPath "haskell-web-api-db"
          withSystemTempDirectory "haskell-web-api-audit-db" $ \workingDirectory -> do
            let runMigrate = do
                  (_, _, _, processHandle) <-
                    createProcess
                      ( (proc databaseSetupExecutable ["migrate"])
                          { cwd = Just workingDirectory,
                            env = Just (databaseSetupEnvironment inheritedEnvironment)
                          }
                      )
                  waitForProcess processHandle
            runMigrate `shouldReturn` ExitSuccess
            -- The second installation must update the same named pg_cron jobs,
            -- rather than creating another active maintenance schedule.
            runMigrate `shouldReturn` ExitSuccess

          scheduledJobs <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT jobname || '|' || database || '|' || username || '|' || active::TEXT || '|' || schedule || '|' || command FROM cron.job WHERE jobname IN ('account-audit-maintenance', 'web-api-cron-run-details-retention') ORDER BY jobname;"
          scheduledJobs
            `shouldBe` ( ExitSuccess,
                         "account-audit-maintenance|web_api_dev|web_api_audit_scheduler|true|0 3 * * *|SELECT account_audit.maintain_activity_partitions();\nweb-api-cron-run-details-retention|web_api_dev|web_api_audit_scheduler|true|41 3 * * *|DELETE FROM cron.job_run_details WHERE username = current_user AND end_time IS NOT NULL AND end_time < statement_timestamp() - interval '30 days';\n",
                         ""
                       )

          schedulerMaintenance <-
            runPsql
              inheritedEnvironment
              "web_api_audit_scheduler"
              "web_api_audit_scheduler"
              "SELECT account_audit.maintain_activity_partitions();"
          case schedulerMaintenance of
            (ExitSuccess, _, "") -> pure ()
            _ -> expectationFailure "expected the scheduler to invoke only the safe no-argument maintenance wrapper"

          runPsql
            inheritedEnvironment
            "web_api_audit_scheduler"
            "web_api_audit_scheduler"
            "DELETE FROM cron.job_run_details WHERE username = current_user AND end_time IS NOT NULL AND end_time < statement_timestamp() - interval '30 days';"
            `shouldReturn` (ExitSuccess, "", "")

          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "DO $$ DECLARE registry_row RECORD; BEGIN FOR registry_row IN SELECT partition_name FROM account_audit.partition_registry LOOP EXECUTE format('DROP TABLE account_audit.%I', registry_row.partition_name); END LOOP; DELETE FROM account_audit.partition_registry; PERFORM account_audit.maintain_activity_partitions_at('2026-09-15 12:00:00+00', interval '12 months'); END $$;"
            `shouldReturn` (ExitSuccess, "", "")

          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "ALTER ROLE web_api_audit_reader LOGIN PASSWORD 'audit-reader'; ALTER ROLE web_api_audit_scheduler LOGIN PASSWORD 'audit-scheduler'; DELETE FROM account_audit.reader_scope_grant WHERE reader_role_name = 'web_api_audit_reader'; INSERT INTO account_audit.reader_scope_grant (reader_role_name, audit_scope_id) VALUES ('web_api_audit_reader', 'default');"
            `shouldReturn` (ExitSuccess, "", "")

          initialPartitions <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT partition_name || '|' || lower_bound || '|' || upper_bound FROM account_audit.partition_registry ORDER BY lower_bound;"
          initialPartitions
            `shouldBe` ( ExitSuccess,
                         "activity_2026_09|2026-09-01 00:00:00+00|2026-10-01 00:00:00+00\nactivity_2026_10|2026-10-01 00:00:00+00|2026-11-01 00:00:00+00\n",
                         ""
                       )

          runtimeAppend <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT activity_id::TEXT || '|' || utilization_percent::TEXT FROM account_audit.append_activity('account_audit_test', '550e8400-e29b-41d4-a716-446655440000', 'account-session-issued', 1::SMALLINT, 'password', NULL, NULL, NULL, NULL);"
          case runtimeAppend of
            (ExitSuccess, resultText, "") -> resultText `shouldContain` "|0\n"
            _ -> expectationFailure "expected the controlled audit append to return one committed ID"

          -- AHI-5's first AuditRequired mutation is deliberately a separate
          -- controlled operation, not a best-effort append after the old
          -- session insert.  The runtime role sees one function result only;
          -- the security-definer function owns the session insert and appends
          -- the closed audit event in that same statement transaction.
          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "DELETE FROM web_api.accounts WHERE account_id = 'account_audit_atomic_test'; INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ('account_audit_atomic_test', 'account-audit-atomic@example.test', 'test-hash', 1);"
            `shouldReturn` (ExitSuccess, "", "")
          atomicSessionIssue <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT session_id FROM account_audit.issue_account_session_with_activity('account-audit-atomic-session', 'account_audit_atomic_test', 100, 200, 'account_audit_atomic_test', '550e8400-e29b-41d4-a716-446655440001', 'account-session-issued', 1::SMALLINT, 'password', NULL, NULL, NULL, NULL);"
          atomicSessionIssue `shouldBe` (ExitSuccess, "account-audit-atomic-session\n", "")
          committedAtomicRows <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT (SELECT count(*)::TEXT FROM web_api.account_sessions WHERE session_id = 'account-audit-atomic-session') || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = 'account_audit_atomic_test' AND event_code = 'account-session-issued');"
          committedAtomicRows `shouldBe` (ExitSuccess, "1|1\n", "")

          -- The invalid event reaches append_activity only after the function
          -- has attempted its session insert. PostgreSQL must roll that insert
          -- back with the rejected append; we test our transaction contract,
          -- rather than any scheduler behavior.
          rejectedAtomicSessionIssue <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT session_id FROM account_audit.issue_account_session_with_activity('account-audit-rejected-session', 'account_audit_atomic_test', 101, 201, 'account_audit_atomic_test', '550e8400-e29b-41d4-a716-446655440002', 'not-an-account-audit-event', 1::SMALLINT, 'password', NULL, NULL, NULL, NULL);"
          fst3 rejectedAtomicSessionIssue `shouldNotBe` ExitSuccess
          thd3 rejectedAtomicSessionIssue `shouldContain` "account audit append received invalid typed fields"
          rolledBackAtomicSession <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT count(*)::TEXT FROM web_api.account_sessions WHERE session_id = 'account-audit-rejected-session';"
          rolledBackAtomicSession `shouldBe` (ExitSuccess, "0\n", "")

          -- A registration email was already accepted by SMTP before this
          -- controlled operation.  Its durable delivery settlement is still
          -- AuditRequired: the state change and closed event commit together,
          -- and an invalid event leaves the claim available for a later
          -- registration retry rather than asserting delivery without proof.
          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "DELETE FROM web_api.accounts WHERE account_id = 'account_audit_delivery_test'; INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ('account_audit_delivery_test', 'account-audit-delivery@example.test', 'test-hash', 1); INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES ('account-audit-delivery-digest', 'account_audit_delivery_test', 'account-audit-delivery@example.test', 200, 'claimed', 1);"
            `shouldReturn` (ExitSuccess, "", "")
          atomicDeliverySettlement <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT account_id FROM account_audit.complete_pending_registration_delivery_with_activity('account_audit_delivery_test', 'account-audit-delivery-digest', 'account_audit_delivery_test', '550e8400-e29b-41d4-a716-446655440003', 'pending-registration-delivered', 1::SMALLINT, 'created', NULL, NULL, NULL, NULL);"
          atomicDeliverySettlement `shouldBe` (ExitSuccess, "account_audit_delivery_test\n", "")
          committedAtomicDelivery <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT (SELECT delivery_state FROM web_api.email_verifications WHERE account_id = 'account_audit_delivery_test') || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = 'account_audit_delivery_test' AND event_code = 'pending-registration-delivered');"
          committedAtomicDelivery `shouldBe` (ExitSuccess, "delivered|1\n", "")
          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "UPDATE web_api.email_verifications SET delivery_state = 'claimed', delivery_claimed_at_nanoseconds = 1 WHERE account_id = 'account_audit_delivery_test';"
            `shouldReturn` (ExitSuccess, "", "")
          rejectedAtomicDelivery <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT account_id FROM account_audit.complete_pending_registration_delivery_with_activity('account_audit_delivery_test', 'account-audit-delivery-digest', 'account_audit_delivery_test', '550e8400-e29b-41d4-a716-446655440004', 'not-an-account-audit-event', 1::SMALLINT, 'created', NULL, NULL, NULL, NULL);"
          fst3 rejectedAtomicDelivery `shouldNotBe` ExitSuccess
          thd3 rejectedAtomicDelivery `shouldContain` "account audit append received invalid typed fields"
          rolledBackAtomicDelivery <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT (SELECT delivery_state FROM web_api.email_verifications WHERE account_id = 'account_audit_delivery_test') || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = 'account_audit_delivery_test' AND request_id = '550e8400-e29b-41d4-a716-446655440004');"
          rolledBackAtomicDelivery `shouldBe` (ExitSuccess, "claimed|0\n", "")

          -- Verification resend uses the generic claim-promotion lifecycle
          -- inside a separate audit-owner operation. An invalid closed event
          -- must roll back its candidate promotion, rolling delivery record,
          -- and audit row together, preserving the currently delivered token
          -- and retryable candidate claim.
          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "DELETE FROM web_api.accounts WHERE account_id = 'account_audit_resend_test'; INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ('account_audit_resend_test', 'account-audit-resend@example.test', 'test-hash', 1); INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES ('account-audit-resend-old-digest', 'account_audit_resend_test', 'account-audit-resend@example.test', 200, 'delivered', NULL); INSERT INTO web_api.verification_resend_claims (account_id, token_digest, email_normalized, expires_at_nanoseconds, claimed_at_nanoseconds) VALUES ('account_audit_resend_test', 'account-audit-resend-candidate-digest', 'account-audit-resend@example.test', 300, 1);"
            `shouldReturn` (ExitSuccess, "", "")
          atomicResendSettlement <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT outcome || '|' || value FROM account_audit.complete_verification_resend_with_activity('account_audit_resend_test', 'account-audit-resend-candidate-digest', 100, 'account_audit_resend_test', '550e8400-e29b-41d4-a716-446655440005', 'verification-resend-delivered', 1::SMALLINT, NULL, NULL, NULL, NULL, NULL);"
          atomicResendSettlement `shouldBe` (ExitSuccess, "settled|account_audit_resend_test\n", "")
          committedAtomicResend <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT (SELECT token_digest FROM web_api.email_verifications WHERE account_id = 'account_audit_resend_test') || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_claims WHERE account_id = 'account_audit_resend_test') || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_deliveries WHERE account_id = 'account_audit_resend_test') || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = 'account_audit_resend_test' AND event_code = 'verification-resend-delivered');"
          committedAtomicResend `shouldBe` (ExitSuccess, "account-audit-resend-candidate-digest|0|1|1\n", "")
          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "DELETE FROM web_api.accounts WHERE account_id = 'account_audit_resend_rollback_test'; INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds) VALUES ('account_audit_resend_rollback_test', 'account-audit-resend-rollback@example.test', 'test-hash', 1); INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds, delivery_state, delivery_claimed_at_nanoseconds) VALUES ('account-audit-resend-rollback-old-digest', 'account_audit_resend_rollback_test', 'account-audit-resend-rollback@example.test', 200, 'delivered', NULL); INSERT INTO web_api.verification_resend_claims (account_id, token_digest, email_normalized, expires_at_nanoseconds, claimed_at_nanoseconds) VALUES ('account_audit_resend_rollback_test', 'account-audit-resend-rollback-candidate-digest', 'account-audit-resend-rollback@example.test', 300, 1);"
            `shouldReturn` (ExitSuccess, "", "")
          rejectedAtomicResend <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT outcome || '|' || value FROM account_audit.complete_verification_resend_with_activity('account_audit_resend_rollback_test', 'account-audit-resend-rollback-candidate-digest', 100, 'account_audit_resend_rollback_test', '550e8400-e29b-41d4-a716-446655440006', 'not-an-account-audit-event', 1::SMALLINT, NULL, NULL, NULL, NULL, NULL);"
          fst3 rejectedAtomicResend `shouldNotBe` ExitSuccess
          thd3 rejectedAtomicResend `shouldContain` "account audit append received invalid typed fields"
          rolledBackAtomicResend <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT (SELECT token_digest FROM web_api.email_verifications WHERE account_id = 'account_audit_resend_rollback_test') || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_claims WHERE account_id = 'account_audit_resend_rollback_test') || '|' || (SELECT count(*)::TEXT FROM web_api.verification_resend_deliveries WHERE account_id = 'account_audit_resend_rollback_test') || '|' || (SELECT count(*)::TEXT FROM account_audit.activity WHERE account_id = 'account_audit_resend_rollback_test' AND request_id = '550e8400-e29b-41d4-a716-446655440006');"
          rolledBackAtomicResend `shouldBe` (ExitSuccess, "account-audit-resend-rollback-old-digest|1|0|0\n", "")

          directRuntimeRead <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "SELECT account_id FROM account_audit.activity;"
          fst3 directRuntimeRead `shouldNotBe` ExitSuccess
          thd3 directRuntimeRead `shouldContain` "permission denied"

          directRuntimeInsert <-
            runPsql
              inheritedEnvironment
              "web_api_runtime"
              "web_api"
              "INSERT INTO account_audit.activity (occurred_at, audit_scope_id, account_id, request_id, event_code, payload_version, payload_detail) VALUES (statement_timestamp(), 'default', 'forbidden', '550e8400-e29b-41d4-a716-446655440000', 'account-session-issued', 1, 'password');"
          fst3 directRuntimeInsert `shouldNotBe` ExitSuccess
          thd3 directRuntimeInsert `shouldContain` "permission denied"

          runPsql
            inheritedEnvironment
            "web_api_owner"
            "web_api_owner"
            "INSERT INTO account_audit.activity (occurred_at, audit_scope_id, account_id, request_id, event_code, payload_version, payload_detail) VALUES (statement_timestamp(), 'other-scope', 'hidden', '550e8400-e29b-41d4-a716-446655440000', 'account-session-issued', 1, 'password');"
            `shouldReturn` (ExitSuccess, "", "")
          scopedReaderRows <-
            runPsql
              inheritedEnvironment
              "web_api_audit_reader"
              "audit-reader"
              "SELECT audit_scope_id || '|' || account_id FROM account_audit.activity WHERE request_id = '550e8400-e29b-41d4-a716-446655440000' ORDER BY audit_scope_id;"
          scopedReaderRows `shouldBe` (ExitSuccess, "default|account_audit_test\n", "")

          schedulerExplicitTime <-
            runPsql
              inheritedEnvironment
              "web_api_audit_scheduler"
              "audit-scheduler"
              "SELECT * FROM account_audit.maintain_activity_partitions_at('2026-09-15 12:00:00+00', interval '12 months');"
          fst3 schedulerExplicitTime `shouldNotBe` ExitSuccess
          thd3 schedulerExplicitTime `shouldContain` "permission denied"

          maintenanceNoOp <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT created_partition_count::TEXT || '|' || dropped_partition_count::TEXT FROM account_audit.maintain_activity_partitions_at('2026-09-15 12:00:00+00', interval '12 months');"
          maintenanceNoOp `shouldBe` (ExitSuccess, "0|0\n", "")
          createdOneExpiredPair <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT * FROM account_audit.maintain_activity_partitions_at('2025-08-15 12:00:00+00', interval '12 months');"
          case createdOneExpiredPair of
            (ExitSuccess, _, "") -> pure ()
            _ -> expectationFailure "expected deterministic maintenance to prepare the first expired pair"
          maintenanceOneDrop <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT created_partition_count::TEXT || '|' || dropped_partition_count::TEXT FROM account_audit.maintain_activity_partitions_at('2026-09-15 12:00:00+00', interval '12 months');"
          maintenanceOneDrop `shouldBe` (ExitSuccess, "0|1\n", "")
          createdTwoExpiredPairs <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT * FROM account_audit.maintain_activity_partitions_at('2025-07-15 12:00:00+00', interval '12 months');"
          case createdTwoExpiredPairs of
            (ExitSuccess, _, "") -> pure ()
            _ -> expectationFailure "expected deterministic maintenance to prepare the second expired pair"
          maintenanceTwoDrops <-
            runPsql
              inheritedEnvironment
              "web_api_owner"
              "web_api_owner"
              "SELECT created_partition_count::TEXT || '|' || dropped_partition_count::TEXT FROM account_audit.maintain_activity_partitions_at('2026-09-15 12:00:00+00', interval '12 months');"
          maintenanceTwoDrops `shouldBe` (ExitSuccess, "0|2\n", "")
      )

    it "maps runtime PostgreSQL connection failures into database errors without shelling out to psql" $
      withUnusedLoopbackPort $ \unusedPort ->
        withTemporaryEnvironment "PATH" (Just "") $ do
          let unreachableDatabaseConfig = defaultRealPostgresConfig {databasePort = unusedPort}
          unreachablePool <- newPostgresPool (databasePoolCapacity unreachableDatabaseConfig) unreachableDatabaseConfig
          let runtimePostgresEffect = buildRuntimePostgresPageRepository unreachablePool
          loadSecondPageValueForRequest runtimePostgresEffect defaultRequestContext
            >>= \case
              Left (SecondPageDataError errorMessage) -> do
                expectAll
                  ( (errorMessage `shouldSatisfy` (not . Text.null))
                      :| [errorMessage `shouldSatisfy` (not . Text.isInfixOf "posix_spawnp")]
                  )
              Right secondPageData ->
                expectationFailure ("expected runtime connection failure, got " <> show secondPageData)
  where
    fst3 (firstValue, _, _) = firstValue
    thd3 (_, _, thirdValue) = thirdValue

    runPsql inheritedEnvironment username password sql =
      readCreateProcessWithExitCode
        ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", username, "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", sql])
            { env = Just (("PGPASSWORD", password) : inheritedEnvironment)
            }
        )
        ""

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action (fromIntegral port)
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"

waitForProcessResponse :: ProcessHandle -> Int -> Text.Text -> IO Text.Text
waitForProcessResponse processHandle port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTP requests"
    (readLoopbackHttpResponse port path)

waitForProcessTrustedHttpsResponse :: ProcessHandle -> FilePath -> Int -> Text.Text -> IO Text.Text
waitForProcessTrustedHttpsResponse processHandle certificatePath port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTPS requests"
    (readTrustedLoopbackHttpsResponse certificatePath port path)

waitForProcessHttpHeaders :: ProcessHandle -> Int -> Text.Text -> IO String
waitForProcessHttpHeaders processHandle port path =
  waitForProcessReadiness
    processHandle
    "expected haskell-web-api to accept loopback HTTP requests"
    (readLoopbackHttpResponseHeaders port path)

waitForProcessReadiness :: ProcessHandle -> String -> IO response -> IO response
waitForProcessReadiness processHandle failureMessage readResponse =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      exitCode <- getProcessExitCode processHandle
      case exitCode of
        Just completedExitCode ->
          expectationFailure ("expected haskell-web-api to keep running, but it exited early with " <> show completedExitCode)
            >> readResponse
        Nothing -> do
          responseResult <- tryIOError readResponse
          case responseResult of
            Right responseValue -> pure responseValue
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure failureMessage
                    >> readResponse

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  originalValue <- lookupEnv key
  let restoreEnvironment =
        case originalValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
      setTemporaryEnvironment =
        case maybeValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  setTemporaryEnvironment
  action `finally` restoreEnvironment

readLoopbackHttpResponse :: Int -> Text.Text -> IO Text.Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpResponseHeaders :: Int -> Text.Text -> IO String
readLoopbackHttpResponseHeaders port path = do
  let url = "http://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--noproxy", "*", "--dump-header", "-", "--output", "/dev/null", url]
      ""
  case exitCode of
    ExitSuccess -> pure stdoutText
    _ -> ioError (userError stderrText)

readTrustedLoopbackHttpsResponse :: FilePath -> Int -> Text.Text -> IO Text.Text
readTrustedLoopbackHttpsResponse certificatePath port path = do
  let url = "https://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--fail", "--noproxy", "*", "--cacert", certificatePath, url]
      ""
  case exitCode of
    ExitSuccess -> pure (Text.pack stdoutText)
    _ -> ioError (userError stderrText)

readLoopbackHttpResponseBytes :: Int -> Text.Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port path = do
  clientSocket <- socket AF_INET Stream defaultProtocol
  NetworkSocket.connect clientSocket (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))
  SocketByteString.sendAll clientSocket (buildHttpRequest path)
  responseBytes <- readAllSocketChunks clientSocket
  close clientSocket
  pure (extractHttpBody responseBytes)

buildHttpRequest :: Text.Text -> ByteString.ByteString
buildHttpRequest path =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"

readAllSocketChunks :: NetworkSocket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (headers, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
      responseBody = ByteString.drop 4 withSeparator
   in if ByteStringChar8.isInfixOf "Transfer-Encoding: chunked" headers
        then decodeChunkedBody responseBody
        else responseBody

decodeChunkedBody :: ByteString.ByteString -> ByteString.ByteString
decodeChunkedBody chunkedBytes =
  case ByteStringChar8.breakSubstring "\r\n" chunkedBytes of
    (chunkSizeHex, withSizeSeparator)
      | ByteString.null withSizeSeparator ->
          chunkedBytes
      | otherwise ->
          case readHex (ByteStringChar8.unpack chunkSizeHex) of
            [(chunkSize, "")]
              | chunkSize == (0 :: Int) ->
                  ByteString.empty
              | otherwise ->
                  let chunkPayload = ByteString.drop 2 withSizeSeparator
                      (chunk, withChunkSuffix) = ByteString.splitAt chunkSize chunkPayload
                   in chunk <> decodeChunkedBody (ByteString.drop 2 withChunkSuffix)
            _ ->
              chunkedBytes

-- | Cabal's build-tool path is available during compilation but some test
-- runners do not preserve it in the child process environment.  Resolve the
-- sibling executable built for this package and retain the normal PATH name
-- for installed-suite runs.
testBuildToolPath :: FilePath -> IO FilePath
testBuildToolPath executableName = do
  testExecutable <- getExecutablePath
  let buildDirectory = takeDirectory (takeDirectory testExecutable)
      siblingExecutable = buildDirectory </> executableName </> executableName
  exists <- doesFileExist siblingExecutable
  pure (if exists then siblingExecutable else executableName)

withManualTlsFiles :: (FilePath -> FilePath -> IO a) -> IO a
withManualTlsFiles action =
  withSystemTempDirectory "web-api-integration-tls" $ \tempDirectory -> do
    let certificatePath = tempDirectory </> "cert.pem"
        privateKeyPath = tempDirectory </> "key.pem"
    writeFile certificatePath manualTlsCertificatePem
    writeFile privateKeyPath manualTlsPrivateKeyPem
    action certificatePath privateKeyPath

manualTlsCertificatePem :: String
manualTlsCertificatePem =
  unlines
    [ "-----BEGIN CERTIFICATE-----",
      "MIICMzCCAdmgAwIBAgIUAliSVDIFHNHzI1q+e3P+1Ah1kbkwCgYIKoZIzj0EAwIw",
      "QDEXMBUGA1UECgwOdHJ1c3RtZSB2MS4yLjExJTAjBgNVBAsMHFRlc3RpbmcgQ0Eg",
      "I2JZeVBlbjVhVnQ0MHlLaXAwIBcNMDAwMTAxMDAwMDAwWhgPMzAwMDAxMDEwMDAw",
      "MDBaMEIxFzAVBgNVBAoMDnRydXN0bWUgdjEuMi4xMScwJQYDVQQLDB5UZXN0aW5n",
      "IGNlcnQgI3JHR1p2N1VLMVQyd1hjeG8wWTATBgcqhkjOPQIBBggqhkjOPQMBBwNC",
      "AARK6NEQhfcGYBt2TRWkrktWpYdmCvYo76sciH70kYBcihzjqaKEw5dD/KbdJjmU",
      "v4pqTQEMnb8hVwKMfSYqOmqwo4GsMIGpMB0GA1UdDgQWBBR8NRVz81tKH8nCWLNI",
      "Pn7zdlXakTAMBgNVHRMBAf8EAjAAMB8GA1UdIwQYMBaAFCVFUSwlXOOm5JvKD5o1",
      "fvsmUu2bMB0GA1UdEQEB/wQTMBGHBH8AAAGCCWxvY2FsaG9zdDAOBgNVHQ8BAf8E",
      "BAMCBaAwKgYDVR0lAQH/BCAwHgYIKwYBBQUHAwIGCCsGAQUFBwMBBggrBgEFBQcD",
      "AzAKBggqhkjOPQQDAgNIADBFAiEAujBETz7z5tWMOpwL/NQFEX9LcbcuHA3+T2oa",
      "6z0Y87gCIDvX/o0KT31LKZM9LklDE11u1S63AYjY0948jEd4Jnrx",
      "-----END CERTIFICATE-----"
    ]

manualTlsPrivateKeyPem :: String
manualTlsPrivateKeyPem =
  unlines
    [ "-----BEGIN EC PRIVATE KEY-----",
      "MHcCAQEEIJ9itNr2Vm4XTUo74d26GQWuZNdRfEjN6cZqWK418T5LoAoGCCqGSM49",
      "AwEHoUQDQgAESujREIX3BmAbdk0VpK5LVqWHZgr2KO+rHIh+9JGAXIoc46mihMOX",
      "Q/ym3SY5lL+Kak0BDJ2/IVcCjH0mKjpqsA==",
      "-----END EC PRIVATE KEY-----"
    ]
