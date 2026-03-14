{-# SPEC #-}

import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

spec = describe "main" $
  it "exits successfully through the stub HarchWeb server" $ do
    (exitCode, stdout, _) <- readProcessWithExitCode "haskell-web-api" [] ""
    exitCode `shouldBe` ExitSuccess
    stdout `shouldContain'` "HTTP Server listening at http://localhost:5001"
