module Main (main) where

import Core.Prelude (handleError)
import System.Environment (getArgs)
import System.Exit (die)
import TestCore.SpecPreprocessor (run)

main :: IO ()
main = do
  args <- getArgs
  run args `handleError` die
