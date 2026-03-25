{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (StdStream (UseHandle), callProcess, createProcess, cwd, env, proc, std_out, waitForProcess)
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
