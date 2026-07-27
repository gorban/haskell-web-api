{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed SSR document authoring and rendering.
module HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationItem (..),
    NavigationRuntime (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    buildDocument,
    buildNavigation,
    buildPageShell,
    generateRuntimeNonce,
    liveRegionAttributes,
    renderDocument,
    renderDocumentWithNonce,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Routing (RouteCodec, routeHref)
import HarchWeb.StaticAssets (AssetPath (..), Stylesheet (..))
import System.IO (IOMode (ReadMode), withBinaryFile)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Text,
    pageBootstrapHooks :: [Text]
  }
  deriving (Eq, Show)

data HtmlAttribute = HtmlAttribute
  { attributeName :: Text,
    attributeValue :: Text
  }
  deriving (Eq, Show)

-- | The two live-region modes that are safe defaults for server-rendered region
-- patches. Keep the role and its announcement urgency together so action UIs do
-- not accidentally render a contradictory role/aria-live pair.
data LiveRegion
  = PoliteStatus
  | AssertiveAlert
  deriving (Eq, Show)

data NavigationItem route = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route
  }
  deriving (Eq, Show)

data NavigationRuntime = NavigationRuntime
  { navigationRuntimePath :: Text,
    navigationRuntimeScript :: Text
  }
  deriving (Eq, Show)

-- | Runtime assets are declared before document rendering so the server can
-- apply the correct CSP policy without inspecting rendered HTML.
data RuntimeDescriptor
  = InlineBootstrap
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  | DeferredModule
      { runtimeDescriptorName :: Text,
        runtimeDescriptorSource :: Text
      }
  deriving (Eq, Show)

newtype RuntimeNonce = RuntimeNonce
  { runtimeNonceValue :: Text
  }
  deriving (Eq, Show)

data ResolvedNavigationItem route = ResolvedNavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route,
    navigationHref :: Text,
    navigationIsActive :: Bool
  }
  deriving (Eq, Show)

data Document route = Document
  { documentTitle :: Text,
    documentBodyAttributes :: [HtmlAttribute],
    documentNavigationAttributes :: [HtmlAttribute],
    documentNavigation :: [ResolvedNavigationItem route],
    documentMainId :: Text,
    documentMainAttributes :: [HtmlAttribute],
    documentMainContent :: Text,
    documentBootstrapHooks :: [Text],
    documentStylesheets :: [Stylesheet],
    documentRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

data PageShell route context = PageShell
  { shellBodyAttributes :: [HtmlAttribute],
    shellNavigationAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: Text,
    shellMainAttributes :: [HtmlAttribute],
    shellStylesheets :: [Stylesheet],
    shellRuntimeDescriptors :: [RuntimeDescriptor]
  }
  deriving (Eq, Show)

generateRuntimeNonce :: IO RuntimeNonce
generateRuntimeNonce =
  withBinaryFile "/dev/urandom" ReadMode $ \randomHandle -> do
    randomBytes <- ByteString.hGet randomHandle 32
    pure
      ( RuntimeNonce
          (TextEncoding.decodeUtf8 (Base64Url.encode randomBytes))
      )

buildNavigation :: (Eq route) => RouteCodec route context -> Page route context -> [NavigationItem route] -> [ResolvedNavigationItem route]
buildNavigation codec page =
  map
    ( \NavigationItem {navigationLabel = itemLabel, navigationRoute = itemRoute} ->
        ResolvedNavigationItem
          { navigationLabel = itemLabel,
            navigationRoute = itemRoute,
            navigationHref = routeHref codec (pageContext page) itemRoute,
            navigationIsActive = pageRoute page == itemRoute
          }
    )

buildDocument :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildDocument codec shell page =
  Document
    { documentTitle = pageTitle page,
      documentBodyAttributes = shellBodyAttributes shell,
      documentNavigationAttributes = shellNavigationAttributes shell,
      documentNavigation = buildNavigation codec page (shellNavigationItems shell),
      documentMainId = shellMainId shell,
      documentMainAttributes = shellMainAttributes shell,
      documentMainContent = pageBody page,
      documentBootstrapHooks = pageBootstrapHooks page,
      documentStylesheets = shellStylesheets shell,
      documentRuntimeDescriptors = shellRuntimeDescriptors shell
    }

renderDocument :: Document route -> Text
renderDocument = renderDocumentWithNonce (RuntimeNonce "development-render-nonce")

renderDocumentWithNonce :: RuntimeNonce -> Document route -> Text
renderDocumentWithNonce runtimeNonce document =
  Text.concat
    [ "<html><head><title>",
      documentTitle document,
      "</title>",
      renderStylesheets (documentStylesheets document),
      renderRuntimeDescriptors runtimeNonce (documentRuntimeDescriptors document),
      "</head><body",
      renderAttributes (documentBodyAttributes document),
      "><nav",
      renderAttributes (documentNavigationAttributes document),
      ">",
      Text.concat (map renderNavigationItem (documentNavigation document)),
      "</nav><main id=\"",
      documentMainId document,
      "\"",
      renderAttributes (documentMainAttributes document <> renderBootstrapHookAttributes (documentBootstrapHooks document)),
      ">",
      documentMainContent document,
      "</main></body></html>"
    ]

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Document route
buildPageShell = buildDocument

liveRegionAttributes :: LiveRegion -> [HtmlAttribute]
liveRegionAttributes liveRegion =
  case liveRegion of
    PoliteStatus -> [HtmlAttribute "role" "status", HtmlAttribute "aria-live" "polite", HtmlAttribute "aria-atomic" "true"]
    AssertiveAlert -> [HtmlAttribute "role" "alert", HtmlAttribute "aria-live" "assertive", HtmlAttribute "aria-atomic" "true"]

renderStylesheets :: [Stylesheet] -> Text
renderStylesheets =
  Text.concat
    . map
      ( \Stylesheet {stylesheetAsset = AssetPath assetPath} ->
          "<link rel=\"stylesheet\" href=\"" <> assetPath <> "\">"
      )

renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = Text.concat . map renderAttribute

renderAttribute :: HtmlAttribute -> Text
renderAttribute attribute =
  Text.concat
    [ " ",
      attributeName attribute,
      "=\"",
      attributeValue attribute,
      "\""
    ]

renderBootstrapHookAttributes :: [Text] -> [HtmlAttribute]
renderBootstrapHookAttributes bootstrapHooks =
  case bootstrapHooks of
    [] -> []
    _ ->
      [ HtmlAttribute
          { attributeName = "data-bootstrap-hooks",
            attributeValue = Text.intercalate "," bootstrapHooks
          }
      ]

renderNavigationItem :: ResolvedNavigationItem route -> Text
renderNavigationItem ResolvedNavigationItem {navigationLabel = itemLabel, navigationHref = itemHref, navigationIsActive = itemIsActive} =
  Text.concat
    [ "<a href=\"",
      itemHref,
      "\"",
      " data-page-link=\"true\"",
      if itemIsActive then " aria-current=\"page\"" else Text.empty,
      ">",
      itemLabel,
      "</a>"
    ]

renderRuntimeDescriptors :: RuntimeNonce -> [RuntimeDescriptor] -> Text
renderRuntimeDescriptors runtimeNonce =
  Text.concat . map (renderRuntimeDescriptor runtimeNonce)

renderRuntimeDescriptor :: RuntimeNonce -> RuntimeDescriptor -> Text
renderRuntimeDescriptor runtimeNonce descriptor =
  case descriptor of
    InlineBootstrap {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script nonce=\"",
          runtimeNonceValue runtimeNonce,
          "\">",
          source,
          "</script>"
        ]
    DeferredModule {runtimeDescriptorSource = source} ->
      Text.concat
        [ "<script type=\"module\" src=\"",
          source,
          "\" defer></script>"
        ]
