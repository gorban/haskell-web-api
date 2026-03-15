module WebApi
  ( AppConfig (..),
    AcmeChallengeBackend (..),
    AcmeConfig (..),
    AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    CertbotConfig (..),
    Layout (..),
    ListenerConfig (..),
    ListenerScheme (..),
    NavigationItem (..),
    ObservabilityConfig (..),
    RouteSelectionError (..),
    OtlpExporter (..),
    StaticAssetsConfig (..),
    StaticAssetRoot (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    buildApp,
    defaultAppConfig,
    defaultRequestContext,
    buildLayout,
    matchRoute,
    parseRoute,
    renderPage,
    renderLayout,
    renderRoutePath,
    run,
    selectRoute,
  )
where

import Data.Char (isAsciiLower)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import System.IO (Handle)

data ListenerScheme
  = Http
  | Https
  deriving (Eq, Show)

data CertbotConfig = CertbotConfig
  { certbotExecutable :: FilePath,
    certbotArguments :: [Text]
  }
  deriving (Eq, Show)

data AcmeChallengeBackend
  = InProcessHttp01
  | CertbotHttp01 CertbotConfig
  deriving (Eq, Show)

data AcmeConfig = AcmeConfig
  { acmeDirectoryUrl :: Text,
    acmeContactEmails :: [Text],
    acmeChallengeBackend :: AcmeChallengeBackend
  }
  deriving (Eq, Show)

data TlsCertificateSource
  = ManualCertificateFiles
      { certificateFile :: FilePath,
        privateKeyFile :: FilePath
      }
  | AcmeCertificateSource AcmeConfig
  deriving (Eq, Show)

newtype TlsConfig = TlsConfig
  { certificateSource :: TlsCertificateSource
  }
  deriving (Eq, Show)

data ListenerConfig = ListenerConfig
  { listenerHost :: Text,
    listenerPort :: Int,
    listenerScheme :: ListenerScheme,
    listenerTls :: Maybe TlsConfig
  }
  deriving (Eq, Show)

data StaticAssetRoot = StaticAssetRoot
  { staticUrlPrefix :: Text,
    staticDirectory :: FilePath
  }
  deriving (Eq, Show)

data StaticAssetsConfig = StaticAssetsConfig
  { staticAssetRoots :: [StaticAssetRoot],
    staticCacheControlSeconds :: Maybe Int
  }
  deriving (Eq, Show)

data OtlpExporter = OtlpExporter
  { otlpEndpoint :: Text,
    otlpHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

data ObservabilityConfig = ObservabilityConfig
  { tracingExporter :: Maybe OtlpExporter,
    metricsExporter :: Maybe OtlpExporter
  }
  deriving (Eq, Show)

data AppConfig = AppConfig
  { appTitlePrefix :: Text,
    listenerConfigs :: [ListenerConfig],
    staticAssets :: StaticAssetsConfig,
    observability :: ObservabilityConfig
  }
  deriving (Eq, Show)

data AppLocale
  = English
  | French
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestCorrelationId :: Maybe Text
  }
  deriving (Eq, Show)

data RouteSelectionError
  = UnsupportedLocalePrefix Text
  | UnsupportedPath Text
  deriving (Eq, Show)

data AppRoute
  = HomeRoute
  | SecondRoute
  | NotFoundRoute
  deriving (Eq, Show)

data NavigationItem = NavigationItem
  { navigationLabel :: Text,
    navigationRoute :: AppRoute,
    navigationHref :: Text,
    navigationIsActive :: Bool
  }
  deriving (Eq, Show)

data Layout = Layout
  { layoutTitle :: Text,
    layoutNavigation :: [NavigationItem],
    layoutMainContent :: Text
  }
  deriving (Eq, Show)

defaultAppConfig :: AppConfig
defaultAppConfig =
  AppConfig
    { appTitlePrefix = Text.pack "web-api",
      listenerConfigs =
        [ ListenerConfig
            { listenerHost = Text.pack "127.0.0.1",
              listenerPort = 5001,
              listenerScheme = Http,
              listenerTls = Nothing
            }
        ],
      staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [],
            staticCacheControlSeconds = Nothing
          },
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

defaultRequestContext :: AppRequestContext
defaultRequestContext =
  AppRequestContext
    { requestLocale = English,
      requestCorrelationId = Nothing
    }

routeCodec :: HarchWeb.RouteCodec AppRoute AppRequestContext
routeCodec =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = parseRoute,
      HarchWeb.renderRoute = renderRoutePath,
      HarchWeb.notFoundRequest = \requestContext ->
        HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = requestContext}
    }

parseRoute :: AppRequestContext -> Text -> Maybe (HarchWeb.RouteRequest AppRoute AppRequestContext)
parseRoute requestContext path =
  either (const Nothing) Just (selectRoute requestContext path)

selectRoute :: AppRequestContext -> Text -> Either RouteSelectionError (HarchWeb.RouteRequest AppRoute AppRequestContext)
selectRoute requestContext path = do
  (pathLocale, route) <- parseRoutePath path
  pure
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = mergeRequestContext requestContext pathLocale
      }

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  let renderedPath = Text.concat [renderLocalePrefix (requestLocale (HarchWeb.requestContext routeRequest)), renderRouteSuffix (HarchWeb.requestRoute routeRequest)]
   in if Text.null renderedPath then Text.pack "/" else renderedPath

matchRoute :: AppRequestContext -> Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
matchRoute = HarchWeb.matchRoute routeCodec

renderPage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Page AppRoute AppRequestContext
renderPage config routeRequest =
  HarchWeb.Page
    { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, Text.pack ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
      HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
      HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
      HarchWeb.pageBody = routeBody (HarchWeb.requestRoute routeRequest)
    }

routeTitle :: AppRoute -> Text
routeTitle route =
  case route of
    HomeRoute -> Text.pack "Home"
    SecondRoute -> Text.pack "Second"
    NotFoundRoute -> Text.pack "Not Found"

routeBody :: AppRoute -> Text
routeBody route =
  case route of
    HomeRoute -> Text.pack "<h1>Home</h1>"
    SecondRoute -> Text.pack "<h1>Second</h1>"
    NotFoundRoute -> Text.pack "<h1>Not Found</h1>"

mergeRequestContext :: AppRequestContext -> Maybe AppLocale -> AppRequestContext
mergeRequestContext requestContext maybeLocale =
  requestContext
    { requestLocale =
        case maybeLocale of
          Just locale -> locale
          Nothing -> requestLocale requestContext
    }

parseRoutePath :: Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseRoutePath path
  | not (Text.isPrefixOf (Text.pack "/") path) = Left (UnsupportedPath path)
  | path /= Text.pack "/" && Text.isSuffixOf (Text.pack "/") path = Left (UnsupportedPath path)
  | otherwise =
      case drop 1 (Text.splitOn (Text.pack "/") path) of
        [segment]
          | Text.null segment -> Right (Nothing, HomeRoute)
        [segment] -> parseSingleSegmentPath path segment
        [prefix, segment] -> parsePrefixedPath path prefix segment
        _ -> Left (UnsupportedPath path)

parseSingleSegmentPath :: Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parseSingleSegmentPath fullPath segment =
  case routeFromSegment segment of
    Just route -> Right (Nothing, route)
    Nothing ->
      case localeFromPrefix segment of
        Just locale -> Right (Just locale, HomeRoute)
        Nothing
          | looksLikeLocalePrefix segment -> Left (UnsupportedLocalePrefix segment)
          | otherwise -> Left (UnsupportedPath fullPath)

parsePrefixedPath :: Text -> Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parsePrefixedPath fullPath prefix segment =
  case localeFromPrefix prefix of
    Just locale ->
      case routeFromSegment segment of
        Just route -> Right (Just locale, route)
        Nothing -> Left (UnsupportedPath fullPath)
    Nothing
      | looksLikeLocalePrefix prefix -> Left (UnsupportedLocalePrefix prefix)
      | otherwise -> Left (UnsupportedPath fullPath)

routeFromSegment :: Text -> Maybe AppRoute
routeFromSegment segment
  | segment == Text.pack "second" = Just SecondRoute
  | segment == Text.pack "404" = Just NotFoundRoute
  | otherwise = Nothing

localeFromPrefix :: Text -> Maybe AppLocale
localeFromPrefix prefix
  | prefix == Text.pack "en" = Just English
  | prefix == Text.pack "fr" = Just French
  | otherwise = Nothing

looksLikeLocalePrefix :: Text -> Bool
looksLikeLocalePrefix prefix =
  Text.length prefix == 2 && Text.all isAsciiLower prefix

renderLocalePrefix :: AppLocale -> Text
renderLocalePrefix locale =
  case locale of
    English -> Text.empty
    French -> Text.pack "/fr"

renderRouteSuffix :: AppRoute -> Text
renderRouteSuffix route =
  case route of
    HomeRoute -> Text.empty
    SecondRoute -> Text.pack "/second"
    NotFoundRoute -> Text.pack "/404"

appShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Text
appShell config page = renderLayout config (buildLayout config page)

buildLayout :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Layout
buildLayout _config page =
  Layout
    { layoutTitle = HarchWeb.pageTitle page,
      layoutNavigation = navigationItems page,
      layoutMainContent = HarchWeb.pageBody page
    }

navigationItems :: HarchWeb.Page AppRoute AppRequestContext -> [NavigationItem]
navigationItems page =
  [ navigationItem page HomeRoute (Text.pack "Home"),
    navigationItem page SecondRoute (Text.pack "Second")
  ]

navigationItem :: HarchWeb.Page AppRoute AppRequestContext -> AppRoute -> Text -> NavigationItem
navigationItem page route label =
  NavigationItem
    { navigationLabel = label,
      navigationRoute = route,
      navigationHref =
        renderRoutePath
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = route,
              HarchWeb.requestContext = HarchWeb.pageContext page
            },
      navigationIsActive = HarchWeb.pageRoute page == route
    }

renderLayout :: AppConfig -> Layout -> Text
renderLayout config layout =
  Text.concat
    [ Text.pack "<html><head><title>",
      layoutTitle layout,
      Text.pack "</title></head><body data-app=\"",
      appTitlePrefix config,
      Text.pack "\"><nav>",
      Text.concat (map renderNavigationItem (layoutNavigation layout)),
      Text.pack "</nav><main id=\"app-main\">",
      layoutMainContent layout,
      Text.pack "</main></body></html>"
    ]

renderNavigationItem :: NavigationItem -> Text
renderNavigationItem item =
  Text.concat
    [ Text.pack "<a href=\"",
      navigationHref item,
      Text.pack "\"",
      if navigationIsActive item then Text.pack " aria-current=\"page\"" else Text.empty,
      Text.pack ">",
      navigationLabel item,
      Text.pack "</a>"
    ]

renderResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Response AppRoute AppRequestContext
renderResponse config routeRequest = HarchWeb.PageResponse (renderPage config routeRequest)

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  HarchWeb.application
    HarchWeb.Application
      { HarchWeb.appName = Text.pack "web-api",
        HarchWeb.routeCodec = routeCodec,
        HarchWeb.renderResponse = renderResponse config,
        HarchWeb.pageShell = appShell config
      }

run :: Handle -> IO ()
run outputHandle =
  HarchWeb.runServer outputHandle defaultAppConfig (buildApp defaultAppConfig)
