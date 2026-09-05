module Main (main) where

import System.IO (stdout)
import WebApi (run)

main :: IO ()
main = run stdout
