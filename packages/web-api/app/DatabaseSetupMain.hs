module Main (main) where

import System.Environment (getArgs)
import System.IO (stdout)
import WebApi (runDatabaseSetupArgs)

main :: IO ()
main = getArgs >>= runDatabaseSetupArgs stdout
