module TestLib.Temp (withTempFile) where

import Control.Exception (bracket)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)

withTempFile :: String -> [String] -> String -> ((FilePath, FilePath) -> IO a) -> IO a
withTempFile directoryTemplate subdirectories filename =
  bracket acquire release
  where
    acquire = do
      let currentDirectory = "."
      tempRoot <- createTempDirectory currentDirectory directoryTemplate
      let targetDirectory = foldl (</>) tempRoot subdirectories
      createDirectoryIfMissing True targetDirectory
      let tempFile = targetDirectory </> filename
      pure (tempRoot, tempFile)
    release (tempRoot, _) = removeDirectoryRecursive tempRoot
