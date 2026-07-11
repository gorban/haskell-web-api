module Main (main) where

import App.App (buildApplication, twoPageServerConfig)
import qualified HarchWeb
import System.IO (stdout)

main :: IO ()
main = HarchWeb.runServer stdout twoPageServerConfig buildApplication
