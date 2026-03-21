{-# LANGUAGE DuplicateRecordFields #-}

module HarchWeb
  ( Application (..),
    Document (..),
    HtmlAttribute (..),
    NavigationItem (..),
    Page (..),
    PageShell (..),
    Response (..),
    ResponseBody (..),
    ResolvedNavigationItem (..),
    RouteCodec (..),
    RouteRequest (..),
    application,
    buildDocument,
    buildNavigation,
    buildPageShell,
    matchRoute,
    routeHref,
    renderDocument,
    runServer,
    toWaiApplication,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.IO (Handle, hPutStrLn)

data RouteRequest route context = RouteRequest
  { requestRoute :: route,
    requestContext :: context
  }
  deriving (Eq, Show)

data Page route context = Page
  { pageTitle :: Text,
    pageRoute :: route,
    pageContext :: context,
    pageBody :: Text
  }
  deriving (Eq, Show)

data HtmlAttribute = HtmlAttribute
  { attributeName :: Text,
    attributeValue :: Text
  }
  deriving (Eq, Show)

data NavigationItem route = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: route
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
    documentNavigation :: [ResolvedNavigationItem route],
    documentMainId :: Text,
    documentMainContent :: Text
  }
  deriving (Eq, Show)

data PageShell route context = PageShell
  { shellBodyAttributes :: [HtmlAttribute],
    shellNavigationItems :: [NavigationItem route],
    shellMainId :: Text
  }
  deriving (Eq, Show)

data ResponseBody = ResponseBody
  { responseStatus :: Int,
    responseContentType :: Text,
    responseBody :: Text
  }
  deriving (Eq, Show)

data Response route context
  = PageResponse (Page route context)
  | BodyResponse ResponseBody
  deriving (Eq, Show)

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context
  }

data Application route context = Application
  { appName :: Text,
    defaultRequestContext :: context,
    routeCodec :: RouteCodec route context,
    renderResponse :: RouteRequest route context -> Response route context,
    pageShell :: Page route context -> Text
  }

application :: Application route context -> Application route context
application = id

routeHref :: RouteCodec route context -> context -> route -> Text
routeHref codec context route =
  renderRoute codec RouteRequest {requestRoute = route, requestContext = context}

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
      documentNavigation = buildNavigation codec page (shellNavigationItems shell),
      documentMainId = shellMainId shell,
      documentMainContent = pageBody page
    }

renderDocument :: Document route -> Text
renderDocument document =
  Text.concat
    [ Text.pack "<html><head><title>",
      documentTitle document,
      Text.pack "</title></head><body",
      renderAttributes (documentBodyAttributes document),
      Text.pack "><nav>",
      Text.concat (map renderNavigationItem (documentNavigation document)),
      Text.pack "</nav><main id=\"",
      documentMainId document,
      Text.pack "\">",
      documentMainContent document,
      Text.pack "</main></body></html>"
    ]

buildPageShell :: (Eq route) => RouteCodec route context -> PageShell route context -> Page route context -> Text
buildPageShell codec shell = renderDocument . buildDocument codec shell

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path = fromMaybe (notFoundRequest codec context) (parseRoute codec context path)

toWaiApplication :: (Eq route) => Application route context -> Wai.Application
toWaiApplication webApplication request respond =
  respond
    ( toWaiResponse
        webApplication
        ( renderResponse webApplication $
            matchRoute
              (routeCodec webApplication)
              (defaultRequestContext webApplication)
              (waiRequestPath request)
        )
    )

runServer :: (Eq route) => Handle -> config -> Application route context -> IO ()
runServer outputHandle config webApplication =
  let startupResponse =
        toWaiResponse
          webApplication
          ( renderResponse webApplication $
              matchRoute
                (routeCodec webApplication)
                (defaultRequestContext webApplication)
                (Text.pack "/")
          )
   in config `seq` Wai.responseStatus startupResponse `seq` hPutStrLn outputHandle "HTTP Server listening at http://localhost:5001"

renderAttributes :: [HtmlAttribute] -> Text
renderAttributes = Text.concat . map renderAttribute

renderAttribute :: HtmlAttribute -> Text
renderAttribute attribute =
  Text.concat
    [ Text.pack " ",
      attributeName attribute,
      Text.pack "=\"",
      attributeValue attribute,
      Text.pack "\""
    ]

renderNavigationItem :: ResolvedNavigationItem route -> Text
renderNavigationItem ResolvedNavigationItem {navigationLabel = itemLabel, navigationHref = itemHref, navigationIsActive = itemIsActive} =
  Text.concat
    [ Text.pack "<a href=\"",
      itemHref,
      Text.pack "\"",
      if itemIsActive then Text.pack " aria-current=\"page\"" else Text.empty,
      Text.pack ">",
      itemLabel,
      Text.pack "</a>"
    ]

toWaiResponse :: (Eq route) => Application route context -> Response route context -> Wai.Response
toWaiResponse webApplication response =
  case response of
    PageResponse page ->
      Wai.responseLBS
        (if isNotFoundPage webApplication page then Http.status404 else Http.status200)
        [(Http.hContentType, TextEncoding.encodeUtf8 htmlContentType)]
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (pageShell webApplication page)))
    BodyResponse responseBodyValue ->
      Wai.responseLBS
        (Http.mkStatus (responseStatus responseBodyValue) mempty)
        [(Http.hContentType, TextEncoding.encodeUtf8 (responseContentType responseBodyValue))]
        (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (responseBody responseBodyValue)))

isNotFoundPage :: (Eq route) => Application route context -> Page route context -> Bool
isNotFoundPage webApplication page =
  let pageRequestContext = pageContext page
   in pageRequestContext `seq`
        pageRoute page == requestRoute (notFoundRequest (routeCodec webApplication) pageRequestContext)

waiRequestPath :: Wai.Request -> Text
waiRequestPath request =
  if ByteString.null (Wai.rawPathInfo request)
    then Text.pack "/"
    else TextEncoding.decodeUtf8 (Wai.rawPathInfo request)

htmlContentType :: Text
htmlContentType = Text.pack "text/html; charset=utf-8"
