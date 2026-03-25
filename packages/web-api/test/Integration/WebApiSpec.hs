{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (StdStream (UseHandle), createProcess, cwd, env, proc, readCreateProcessWithExitCode, std_out, waitForProcess)
import TestSupport.RealPostgres (databaseSetupEnvironment, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, withContainerizedPsqlOnPath)
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
    it "runs migrate-and-seed, loads seeded page data, and enforces runtime-role privileges against real PostgreSQL" $
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

        let postgresEffect = buildPostgresDatabaseEffect defaultRealPostgresConfig
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

        allowedSelect <-
          readCreateProcessWithExitCode
            ( (proc "psql" ["--host", "127.0.0.1", "--port", "5432", "--dbname", "web_api_dev", "--username", "web_api_runtime", "--no-password", "--set", "ON_ERROR_STOP=1", "--tuples-only", "--no-align", "--quiet", "--command", "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';"])
                { env = Just (("PGPASSWORD", "web_api") : inheritedEnvironment)
                }
            )
            ""
        allowedSelect `shouldBe` (ExitSuccess, "Server-rendered home page with stubbed content.\n", "")

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
  where
    fst3 (firstValue, _, _) = firstValue
    thd3 (_, _, thirdValue) = thirdValue
