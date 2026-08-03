-- | A Haskell-authored, real-browser scenario API. The Node process is only a
-- thin Playwright adapter; scenario control flow and assertions stay here.
module TestCore.Browser
  ( module TestCore.Browser.Config,
    module TestCore.Browser.Model,
    module TestCore.Browser.Scenario,
    module TestCore.Browser.Types,
  )
where

import TestCore.Browser.Config
import TestCore.Browser.Model
import TestCore.Browser.Scenario
import TestCore.Browser.Types
