module WebApi
  ( AppConfig (..),
    AcmeChallengeBackend (..),
    AcmeConfig (..),
    AppLocale (..),
    AppPageModel (..),
    AppRequestContext (..),
    AppRoute (..),
    CertbotConfig (..),
    CallToAction (..),
    HomePageModel (..),
    ListenerConfig (..),
    ListenerScheme (..),
    NotFoundPageModel (..),
    ObservabilityConfig (..),
    RouteSelectionError (..),
    SecondPageModel (..),
    OtlpExporter (..),
    StaticAssetsConfig (..),
    StaticAssetRoot (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    buildApp,
    defaultAppConfig,
    defaultRequestContext,
    buildPageModel,
    matchRoute,
    parseRoute,
    renderPage,
    renderPageBody,
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

data CallToAction = CallToAction
  { callToActionLabel :: Text,
    callToActionRoute :: AppRoute,
    callToActionHref :: Text
  }
  deriving (Eq, Show)

data HomePageModel = HomePageModel
  { homeHeading :: Text,
    homeSummary :: Text,
    homePrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data SecondPageModel = SecondPageModel
  { secondHeading :: Text,
    secondSummary :: Text,
    secondHighlights :: [Text],
    secondPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data NotFoundPageModel = NotFoundPageModel
  { notFoundHeading :: Text,
    notFoundSummary :: Text,
    notFoundPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data AppPageModel
  = HomePage HomePageModel
  | SecondPage SecondPageModel
  | NotFoundPage NotFoundPageModel
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
  let pageModel = buildPageModel routeRequest
   in HarchWeb.Page
        { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, Text.pack ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
          HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
          HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
          HarchWeb.pageBody = renderPageBody pageModel
        }

routeTitle :: AppRoute -> Text
routeTitle route =
  case route of
    HomeRoute -> Text.pack "Home"
    SecondRoute -> Text.pack "Second"
    NotFoundRoute -> Text.pack "Not Found"

buildPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel
buildPageModel routeRequest =
  case HarchWeb.requestRoute routeRequest of
    HomeRoute ->
      HomePage
        HomePageModel
          { homeHeading = Text.pack "Home",
            homeSummary = Text.pack "Server-rendered home page with stubbed content.",
            homePrimaryAction = buildCallToAction routeRequest SecondRoute (Text.pack "Browse the second page")
          }
    SecondRoute ->
      SecondPage
        SecondPageModel
          { secondHeading = Text.pack "Second",
            secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
            secondHighlights = [],
            secondPrimaryAction = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
          }
    NotFoundRoute ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = Text.pack "Not Found",
            notFoundSummary = Text.pack "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
          }

buildCallToAction :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppRoute -> Text -> CallToAction
buildCallToAction routeRequest route label =
  CallToAction
    { callToActionLabel = label,
      callToActionRoute = route,
      callToActionHref =
        renderRoutePath
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = route,
              HarchWeb.requestContext = HarchWeb.requestContext routeRequest
            }
    }

renderPageBody :: AppPageModel -> Text
renderPageBody pageModel =
  case pageModel of
    HomePage homePage ->
      Text.concat
        [ Text.pack "<section data-page=\"home\">",
          Text.pack "<h1 data-page-title=\"true\">",
          homeHeading homePage,
          Text.pack "</h1>",
          Text.pack "<p>",
          homeSummary homePage,
          Text.pack "</p>",
          renderCallToAction (homePrimaryAction homePage),
          Text.pack "</section>"
        ]
    SecondPage secondPage ->
      Text.concat
        [ Text.pack "<section data-page=\"second\">",
          Text.pack "<h1 data-page-title=\"true\">",
          secondHeading secondPage,
          Text.pack "</h1>",
          Text.pack "<p>",
          secondSummary secondPage,
          Text.pack "</p>",
          renderHighlights (secondHighlights secondPage),
          renderCallToAction (secondPrimaryAction secondPage),
          Text.pack "</section>"
        ]
    NotFoundPage notFoundPage ->
      Text.concat
        [ Text.pack "<section data-page=\"not-found\">",
          Text.pack "<h1 data-page-title=\"true\">",
          notFoundHeading notFoundPage,
          Text.pack "</h1>",
          Text.pack "<p>",
          notFoundSummary notFoundPage,
          Text.pack "</p>",
          renderCallToAction (notFoundPrimaryAction notFoundPage),
          Text.pack "</section>"
        ]

renderHighlights :: [Text] -> Text
renderHighlights highlights =
  case highlights of
    [] -> Text.pack "<p data-empty-state=\"true\">No highlights yet.</p>"
    _ ->
      Text.concat
        [ Text.pack "<ul>",
          Text.concat (map renderHighlight highlights),
          Text.pack "</ul>"
        ]

renderHighlight :: Text -> Text
renderHighlight highlight =
  Text.concat [Text.pack "<li>", highlight, Text.pack "</li>"]

renderCallToAction :: CallToAction -> Text
renderCallToAction callToAction =
  Text.concat
    [ Text.pack "<p><a href=\"",
      callToActionHref callToAction,
      Text.pack "\" data-page-link=\"true\">",
      callToActionLabel callToAction,
      Text.pack "</a></p>"
    ]

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
parseRoutePath path =
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
        Nothing ->
          if looksLikeLocalePrefix segment
            then Left (UnsupportedLocalePrefix segment)
            else Left (UnsupportedPath fullPath)

parsePrefixedPath :: Text -> Text -> Text -> Either RouteSelectionError (Maybe AppLocale, AppRoute)
parsePrefixedPath fullPath prefix segment =
  case localeFromPrefix prefix of
    Just locale ->
      case routeFromSegment segment of
        Just route -> Right (Just locale, route)
        Nothing -> Left (UnsupportedPath fullPath)
    Nothing ->
      if looksLikeLocalePrefix prefix
        then Left (UnsupportedLocalePrefix prefix)
        else Left (UnsupportedPath fullPath)

routeFromSegment :: Text -> Maybe AppRoute
routeFromSegment segment
  | segment == Text.pack "second" = Just SecondRoute
  | segment == Text.pack "404" = Just NotFoundRoute
routeFromSegment _ = Nothing

localeFromPrefix :: Text -> Maybe AppLocale
localeFromPrefix prefix
  | prefix == Text.pack "en" = Just English
  | prefix == Text.pack "fr" = Just French
localeFromPrefix _ = Nothing

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
appShell config = HarchWeb.buildPageShell routeCodec (appShellConfig config)

appShellConfig :: AppConfig -> HarchWeb.PageShell AppRoute AppRequestContext
appShellConfig config =
  HarchWeb.PageShell
    { HarchWeb.shellBodyAttributes =
        [ HarchWeb.HtmlAttribute
            { HarchWeb.attributeName = Text.pack "data-app",
              HarchWeb.attributeValue = appTitlePrefix config
            }
        ],
      HarchWeb.shellNavigationItems =
        [ HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = Text.pack "Home",
              HarchWeb.navigationRoute = HomeRoute
            },
          HarchWeb.NavigationItem
            { HarchWeb.navigationLabel = Text.pack "Second",
              HarchWeb.navigationRoute = SecondRoute
            }
        ],
      HarchWeb.shellMainId = Text.pack "app-main"
    }

renderResponse :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Response AppRoute AppRequestContext
renderResponse config routeRequest = HarchWeb.PageResponse (renderPage config routeRequest)

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  config `seq`
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
