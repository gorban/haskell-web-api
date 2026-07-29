import Control.Monad (when)
import Distribution.Simple (defaultMainWithHooks, preConf, simpleUserHooks)
import System.Directory (copyFile, doesFileExist)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preConf = \args flags -> do
          copyLicenseFromRoot
          preConf simpleUserHooks args flags
      }

copyLicenseFromRoot :: IO ()
copyLicenseFromRoot = do
  let source = "../../LICENSE"
      destination = "LICENSE"
  sourceExists <- doesFileExist source
  destinationExists <- doesFileExist destination
  when (sourceExists && not destinationExists) $ do
    copyFile source destination
    putStrLn "Setup: Copied LICENSE from repository root"
