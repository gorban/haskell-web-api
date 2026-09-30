module Unit.Core.Setup.TestSupport (withEmptyPath) where

import Control.Exception (finally)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.IO.Temp (withSystemTempDirectory)

-- | Run an action with 'PATH' set to an otherwise empty temporary directory.
-- This makes real executable lookup fail deterministically without retaining
-- an unused ability to install scripts in that directory.
withEmptyPath :: IO value -> IO value
withEmptyPath action =
  withSystemTempDirectory "empty-path" $ \temporaryDirectory -> do
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" temporaryDirectory
    action
      `finally` maybe
        (unsetEnv "PATH")
        (setEnv "PATH")
        originalPath
