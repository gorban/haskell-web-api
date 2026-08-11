module Main (main) where

import App.App (buildApplication, twoPageServerConfig)
import HarchWeb qualified
import System.IO (stdout)

main :: IO ()
main = HarchWeb.runServer stdout twoPageServerConfig buildApplication
