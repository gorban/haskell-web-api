import Control.Monad (when)
import Distribution.Simple (defaultMainWithHooks, simpleUserHooks, preConf)
import System.Directory (copyFile, doesFileExist)

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf = \args flags -> do
      copyLicenseFromRoot
      preConf simpleUserHooks args flags
  }

copyLicenseFromRoot :: IO ()
copyLicenseFromRoot = do
  let src = "../../LICENSE"
      dest = "LICENSE"
  srcExists <- doesFileExist src
  destExists <- doesFileExist dest
  when (srcExists && not destExists) $ do
    copyFile src dest
    putStrLn "Setup: Copied LICENSE from repository root"
