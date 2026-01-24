module Main (main) where

import Control.Monad.Except (runExceptT)
import System.Environment (getArgs)
import System.Exit (die)
import TestLib.SpecPreprocessor (run)

main :: IO ()
main = do
  args <- getArgs
  result <- runExceptT (run args)
  either die pure result
