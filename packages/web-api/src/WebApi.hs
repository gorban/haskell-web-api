module WebApi
  ( AppConfig (..),
    AcmeChallengeBackend (..),
    AcmeConfig (..),
    AppRequestContext (..),
    AppRoute (..),
    CertbotConfig (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    OtlpExporter (..),
    StaticAssetsConfig (..),
    StaticAssetRoot (..),
    TlsCertificateSource (..),
    TlsConfig (..),
    buildApp,
    defaultAppConfig,
    defaultRequestContext,
    matchRoute,
    parseRoute,
    renderPage,
    renderRoutePath,
    run,
  )
where

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

data AppRequestContext = AppRequestContext
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
defaultRequestContext = AppRequestContext

routeCodec :: HarchWeb.RouteCodec AppRoute AppRequestContext
routeCodec =
  HarchWeb.RouteCodec
    { HarchWeb.parseRoute = parseRoute,
      HarchWeb.renderRoute = renderRoutePath,
      HarchWeb.notFoundRequest = \requestContext ->
        HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = requestContext}
    }

parseRoute :: AppRequestContext -> Text -> Maybe (HarchWeb.RouteRequest AppRoute AppRequestContext)
parseRoute requestContext path
  | path == Text.pack "/" =
      Just HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = requestContext}
  | path == Text.pack "/second" =
      Just HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = requestContext}
  | otherwise = Nothing

renderRoutePath :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text
renderRoutePath routeRequest =
  case HarchWeb.requestRoute routeRequest of
    HomeRoute -> Text.pack "/"
    SecondRoute -> Text.pack "/second"
    NotFoundRoute -> Text.pack "/404"

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

appShell :: AppConfig -> HarchWeb.Page AppRoute AppRequestContext -> Text
appShell config page =
  Text.concat
    [ Text.pack "<html><head><title>",
      HarchWeb.pageTitle page,
      Text.pack "</title></head><body data-app=\"",
      appTitlePrefix config,
      Text.pack "\"><main>",
      HarchWeb.pageBody page,
      Text.pack "</main></body></html>"
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
