{-# SPEC #-}

import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)

spec = describe "main" $
  it "prints \"Web API Module\"" $ do
    (exitCode, stdout, _) <- readProcessWithExitCode "haskell-web-api" [] ""
    exitCode `shouldBe` ExitSuccess
    stdout `shouldContain'` "Web API Module"
