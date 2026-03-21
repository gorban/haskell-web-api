{-# SPEC #-}

import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (StdStream (UseHandle), callProcess, createProcess, cwd, env, proc, std_out, waitForProcess)

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

  describe "database setup executable" $
    it "runs migrations and seed data with dedicated migration credentials" $ do
      inheritedEnvironment <- getEnvironment
      exitCode <-
        withSystemTempDirectory "fake-psql" $ \binDirectory -> do
          let scriptPath = binDirectory <> "/psql"
              argsLogPath = binDirectory <> "/psql-args.log"
              processEnvironment =
                ("PATH", binDirectory <> ":" <> lookupValue "PATH" inheritedEnvironment)
                  : ("PSQL_ARGS_LOG", argsLogPath)
                  : ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1")
                  : ("WEB_API_MIGRATION_DATABASE_PORT", "5432")
                  : ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev")
                  : ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
                  : ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
                  : filter (\(key, _) -> key /= "PATH" && key /= "PSQL_ARGS_LOG") inheritedEnvironment
          writeFile
            scriptPath
            ( unlines
                [ "#!/usr/bin/env bash",
                  "set -euo pipefail",
                  "printf '%s\\n' \"$*\" >> \"$PSQL_ARGS_LOG\""
                ]
            )
          callProcess "chmod" ["+x", scriptPath]
          withSystemTempDirectory "haskell-web-api-db" $ \workingDirectory ->
            withSystemTempFile "haskell-web-api-db-stdout.txt" $ \outputPath outputHandle -> do
              (_, _, _, processHandle) <-
                createProcess
                  ( (proc "haskell-web-api-db" ["migrate-and-seed"])
                      { cwd = Just workingDirectory,
                        env = Just processEnvironment,
                        std_out = UseHandle outputHandle
                      }
                  )
              result <- waitForProcess processHandle
              hClose outputHandle
              readFile outputPath `shouldReturn` "Applied database migrations and seed data.\n"
              argsLog <- readFile argsLogPath
              argsLog `shouldContain` "--host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api_owner --no-password --set ON_ERROR_STOP=1 --command CREATE TABLE IF NOT EXISTS page_content"
              argsLog `shouldContain` "--host 127.0.0.1 --port 5432 --dbname web_api_dev --username web_api_owner --no-password --set ON_ERROR_STOP=1 --command INSERT INTO page_content"
              pure result
      exitCode `shouldBe` ExitSuccess
  where
    lookupValue key entries =
      fromMaybe "" (lookup key entries)
