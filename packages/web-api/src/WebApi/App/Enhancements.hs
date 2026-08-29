module WebApi.App.Enhancements
  ( pageEnhancementHooks,
  )
where

import Data.Text (Text)
import WebApi.Route (AppRoute, RouteMetadata (routeEnhancementHooks), routeMetadata)

pageEnhancementHooks :: AppRoute -> [Text]
pageEnhancementHooks = routeEnhancementHooks . routeMetadata
