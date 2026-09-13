module Core.System.Temp (withTempFile) where

import Control.Exception (bracket)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)

-- | Create a temporary file in a temporary directory, run an action, then clean up.
--
-- Creates a temp directory using the given template, optionally creates subdirectories,
-- and provides both the temp root and the full file path to the action.
-- The entire temp directory is removed after the action completes.
--
-- Example:
--
-- @
-- withTempFile "test" ["sub", "dir"] "file.txt" $ \(tempRoot, filePath) -> do
--   writeFile filePath "content"
--   -- filePath is something like "./test12345/sub/dir/file.txt"
-- @
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
