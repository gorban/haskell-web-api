{-# SPEC #-}

import System.Exit (ExitCode (ExitSuccess))
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import System.Process (StdStream (UseHandle), createProcess, proc, std_out, waitForProcess)

spec = describe "main" $
  it "exits successfully through the stub HarchWeb server" $ do
    exitCode <- withSystemTempFile "haskell-web-api-stdout.txt" $ \outputPath outputHandle -> do
      (_, _, _, processHandle) <- createProcess ((proc "haskell-web-api" []) {std_out = UseHandle outputHandle})
      result <- waitForProcess processHandle
      hClose outputHandle
      readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
      pure result
    exitCode `shouldBe` ExitSuccess
