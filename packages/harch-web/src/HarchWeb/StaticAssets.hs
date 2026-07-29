{-# LANGUAGE OverloadedStrings #-}

-- | Typed static-asset declarations and browser-visible asset URLs.
--
-- Filesystem matching and WAI responses remain server implementation details;
-- this module owns the application-facing asset contract used by documents and
-- route-aware page components.
module HarchWeb.StaticAssets
  ( AssetPath (..),
    CssClass (..),
    CssScope (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    Stylesheet (..),
    cssClassText,
    cssScope,
    defaultStaticAssetContentTypes,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    stylesheet,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.PathPrefix (applyPathPrefix)

data StaticAssetRoot = StaticAssetRoot
  { staticUrlPrefix :: Text,
    staticDirectory :: FilePath
  }
  deriving (Eq, Show)

data StaticAssetsConfig = StaticAssetsConfig
  { staticAssetRoots :: [StaticAssetRoot],
    staticAssetContentTypes :: [(Text, Text)],
    staticCacheControlSeconds :: Maybe Int
  }
  deriving (Eq, Show)

defaultStaticAssetContentTypes :: [(Text, Text)]
defaultStaticAssetContentTypes =
  [ (".css", "text/css; charset=utf-8"),
    (".html", "text/html; charset=utf-8"),
    (".js", "application/javascript; charset=utf-8"),
    (".json", "application/json; charset=utf-8"),
    (".svg", "image/svg+xml"),
    (".txt", "text/plain; charset=utf-8")
  ]

-- | A route-aware reference to an app-owned static file. This stays distinct
-- from filesystem paths so components only pass browser-visible asset URLs.
newtype AssetPath = AssetPath
  { assetPathText :: Text
  }
  deriving (Eq, Show)

-- | An external stylesheet declaration. Inline CSS remains intentionally
-- absent so the default CSP can keep @style-src 'self'@.
newtype Stylesheet = Stylesheet
  { stylesheetAsset :: AssetPath
  }
  deriving (Eq, Show)

-- | A stable namespace for styles authored by one page or component.
newtype CssScope = CssScope
  { cssScopeName :: Text
  }
  deriving (Eq, Show)

-- | A rendered CSS class can either be deliberately global or tied to a
-- component scope.
data CssClass
  = ScopedCssClass CssScope Text
  | GlobalCssClass Text
  deriving (Eq, Show)

stylesheet :: AssetPath -> Stylesheet
stylesheet = Stylesheet

cssScope :: Text -> CssScope
cssScope = CssScope

cssClassText :: CssClass -> Text
cssClassText cssClass =
  case cssClass of
    ScopedCssClass (CssScope scopeName) localName -> "harch-" <> scopeName <> "-" <> localName
    GlobalCssClass className -> className

staticAssetHref :: StaticAssetRoot -> FilePath -> Text
staticAssetHref =
  staticAssetHrefWithPrefix Text.empty

staticAssetHrefWithPrefix :: Text -> StaticAssetRoot -> FilePath -> Text
staticAssetHrefWithPrefix pathPrefix staticRoot assetPath =
  let normalizedPrefix = normalizeStaticPrefix (staticUrlPrefix staticRoot)
      normalizedAssetPath = trimLeadingSlash (Text.pack assetPath)
      assetHref =
        if Text.null normalizedPrefix
          then "/" <> normalizedAssetPath
          else Text.concat [normalizedPrefix, "/", normalizedAssetPath]
   in applyPathPrefix pathPrefix assetHref

normalizeStaticPrefix :: Text -> Text
normalizeStaticPrefix prefix =
  fromMaybe prefix (Text.stripSuffix "/" prefix)

trimLeadingSlash :: Text -> Text
trimLeadingSlash assetPath =
  fromMaybe assetPath (Text.stripPrefix "/" assetPath)
