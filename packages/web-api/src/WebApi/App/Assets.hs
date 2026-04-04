{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Assets
  ( navigationScriptSources,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Config (AppConfig (..))

navigationScriptSources :: AppConfig -> [Text]
navigationScriptSources config =
  case HarchWeb.staticAssetRoots (staticAssets config) of
    primaryRoot : _ -> [HarchWeb.staticAssetHref primaryRoot "navigation.js"]
    [] -> []
