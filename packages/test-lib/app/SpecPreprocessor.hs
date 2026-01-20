module Main (main) where

import           System.Environment (getArgs)

import           TestLib.SpecPreprocessor (run)

main :: IO ()
main = getArgs >>= run
