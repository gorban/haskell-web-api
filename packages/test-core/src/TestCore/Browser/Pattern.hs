{-# LANGUAGE TemplateHaskell #-}

-- | Pattern assertions extend the existing observed assertion block by generating
-- a 'matches' callback with 'shouldMatch'. Keeping this adapter separate from
-- the scenario interpreter preserves its batching, retry, and failure semantics.
module TestCore.Browser.Pattern
  ( matchesPattern,
  )
where

import Control.Monad (void)
import Language.Haskell.TH (Exp, Pat, Q, newName, varE, varP)
import Test.Hspec.Expectations.Match (shouldMatch)
import TestCore.Browser.Scenario (matches)

infix 1 `matchesPattern`

-- | Add a quoted observation and pattern to an aggregate browser snapshot.
-- Pattern bindings are discarded: dependent checks belong outside the block.
--
-- >>> $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {hardNavigationCount = 0}|])
matchesPattern :: Q Exp -> Q Pat -> Q Exp
matchesPattern observation patternQuote = do
  value <- newName "observedValue"
  [|matches $observation (\ $(varP value) -> void $(shouldMatch (varE value) patternQuote))|]
