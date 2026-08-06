module Main (main) where

import App.App (buildApplication, buildNativeUploadMiddleware, twoPageServerConfig)
import HarchWeb qualified
import System.IO (stdout)

main :: IO ()
main = do
  nativeUploadMiddleware <- buildNativeUploadMiddleware
  HarchWeb.runServerWithWaiMiddleware nativeUploadMiddleware stdout twoPageServerConfig buildApplication
