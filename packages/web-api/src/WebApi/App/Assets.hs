{-# LANGUAGE OverloadedStrings #-}

module WebApi.App.Assets
  ( navigationScriptSources,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Config (AppConfig (..))
import WebApi.Route (AppRequestContext (..))

navigationScriptSources :: AppConfig -> AppRequestContext -> [Text]
navigationScriptSources config requestContext =
  case HarchWeb.staticAssetRoots (staticAssets config) of
    primaryRoot : _ -> [HarchWeb.staticAssetHrefWithPrefix (requestPathPrefix requestContext) primaryRoot "navigation.js"]
    [] -> []
