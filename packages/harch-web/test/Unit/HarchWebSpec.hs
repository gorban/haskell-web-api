module Unit.HarchWebSpec (spec) where

import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import HarchWeb
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiInternal
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import Test.Hspec

newtype TestContext = TestContext
  { requestLanguage :: Text
  }
  deriving (Eq, Show)

data TestRoute
  = KnownRoute
  | DataRoute
  | MissingRoute
  deriving (Eq, Show)

defaultContext :: TestContext
defaultContext = TestContext {requestLanguage = Text.pack "en"}

spanishContext :: TestContext
spanishContext = TestContext {requestLanguage = Text.pack "es"}

sampleCodec :: RouteCodec TestRoute TestContext
sampleCodec =
  RouteCodec
    { parseRoute = parseSampleRoute,
      renderRoute = renderSampleRoute,
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext}
    }

parseSampleRoute :: TestContext -> Text -> Maybe (RouteRequest TestRoute TestContext)
parseSampleRoute routeContext path
  | path == Text.pack "/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
  | path == Text.pack "/es/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}
  | path == Text.pack "/data" =
      Just RouteRequest {requestRoute = DataRoute, requestContext = routeContext}
  | otherwise = Nothing

renderSampleRoute :: RouteRequest TestRoute TestContext -> Text
renderSampleRoute request =
  case (requestLanguage (requestContext request), requestRoute request) of
    (language, KnownRoute)
      | language == Text.pack "es" -> Text.pack "/es/known"
      | otherwise -> Text.pack "/known"
    (_, DataRoute) -> Text.pack "/data"
    (_, MissingRoute) -> Text.pack "/404"

samplePage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
samplePage request =
  Page
    { pageTitle = Text.pack "Known",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = Text.pack "<h1>Known</h1>"
    }

sampleMissingPage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
sampleMissingPage request =
  Page
    { pageTitle = Text.pack "Missing",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = Text.pack "<h1>Missing</h1>"
    }

sampleShell :: PageShell TestRoute TestContext
sampleShell =
  PageShell
    { shellBodyAttributes =
        [ HtmlAttribute
            { attributeName = Text.pack "data-app",
              attributeValue = Text.pack "sample"
            }
        ],
      shellNavigationItems =
        [ NavigationItem
            { navigationLabel = Text.pack "Known",
              navigationRoute = KnownRoute
            },
          NavigationItem
            { navigationLabel = Text.pack "Missing",
              navigationRoute = MissingRoute
            }
        ],
      shellMainId = Text.pack "app-main"
    }

sampleApplication :: Application TestRoute TestContext
sampleApplication =
  Application
    { appName = Text.pack "sample",
      defaultRequestContext = defaultContext,
      routeCodec = sampleCodec,
      renderResponse = pure . renderSampleResponse,
      pageShell = buildPageShell sampleCodec sampleShell
    }

sampleServerConfig :: ServerConfig
sampleServerConfig =
  ServerConfig
    { listenerConfigs =
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

serverConfigWithListeners :: [ListenerConfig] -> ServerConfig
serverConfigWithListeners listeners =
  sampleServerConfig
    { listenerConfigs = listeners
    }

rootPathApplication :: Application TestRoute TestContext
rootPathApplication =
  Application
    { appName = Text.pack "root-path",
      defaultRequestContext = defaultContext,
      routeCodec = rootPathCodec,
      renderResponse = pure . PageResponse . samplePage,
      pageShell = buildPageShell rootPathCodec sampleShell
    }

rootPathCodec :: RouteCodec TestRoute TestContext
rootPathCodec =
  RouteCodec
    { parseRoute = \routeContext path ->
        if path == Text.pack "/"
          then Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
          else Nothing,
      renderRoute = \request ->
        case requestRoute request of
          KnownRoute -> Text.pack "/"
          DataRoute -> Text.pack "/data"
          MissingRoute -> Text.pack "/404",
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext}
    }

renderSampleResponse :: RouteRequest TestRoute TestContext -> Response TestRoute TestContext
renderSampleResponse request =
  case requestRoute request of
    KnownRoute -> PageResponse (samplePage request)
    DataRoute ->
      BodyResponse
        ResponseBody
          { responseStatus = 202,
            responseContentType = Text.pack "application/json",
            responseBody = Text.pack "{\"route\":\"data\"}"
          }
    MissingRoute -> PageResponse (sampleMissingPage request)

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  pure (fromMaybe (error "expected WAI application to produce a response") maybeResponse)

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> modifyIORef' chunksReference (<> [Builder.toLazyByteString builder]))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

waiRequest :: [Text] -> Wai.Request
waiRequest segments =
  Wai.defaultRequest
    { Wai.rawPathInfo = TextEncoding.encodeUtf8 renderedPath,
      Wai.pathInfo = segments
    }
  where
    renderedPath =
      case segments of
        [] -> Text.pack "/"
        _ -> Text.pack "/" <> Text.intercalate (Text.pack "/") segments

spec :: Spec
spec = do
  describe "shared config coverage" $ do
    it "reads exported selectors from the shared server config records" $ do
      let certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]}
          challengeBackend = CertbotHttp01 certbotConfig
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend = challengeBackend
              }
          tlsSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = tlsSource}
          staticRoot = StaticAssetRoot {staticUrlPrefix = Text.pack "/assets", staticDirectory = "public"}
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                otlpHeaders = [(Text.pack "authorization", Text.pack "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just tracingConfig,
                metricsExporter = Nothing
              }
          serverConfig =
            ServerConfig
              { listenerConfigs =
                  [ ListenerConfig
                      { listenerHost = Text.pack "127.0.0.1",
                        listenerPort = 5001,
                        listenerScheme = Https,
                        listenerTls = Just tlsConfig
                      }
                  ],
                staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [staticRoot],
                      staticCacheControlSeconds = Just 3600
                    },
                observability = observabilityConfig
              }
          listenerConfig =
            case listenerConfigs serverConfig of
              [singleListenerConfig] -> singleListenerConfig
              _ -> error "expected exactly one listener config"
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` [Text.pack "certonly", Text.pack "--webroot"]
      acmeDirectoryUrl acmeConfig `shouldBe` Text.pack "https://acme-v02.api.letsencrypt.org/directory"
      acmeContactEmails acmeConfig `shouldBe` [Text.pack "ops@example.com"]
      acmeChallengeBackend acmeConfig `shouldBe` challengeBackend
      certificateSource tlsConfig `shouldBe` tlsSource
      listenerHost listenerConfig `shouldBe` Text.pack "127.0.0.1"
      listenerPort listenerConfig `shouldBe` 5001
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` Text.pack "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots (staticAssets serverConfig) `shouldBe` [staticRoot]
      staticCacheControlSeconds (staticAssets serverConfig) `shouldBe` Just 3600
      otlpEndpoint tracingConfig `shouldBe` Text.pack "http://collector:4318/v1/traces"
      otlpHeaders tracingConfig `shouldBe` [(Text.pack "authorization", Text.pack "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just tracingConfig
      metricsExporter observabilityConfig `shouldBe` Nothing
      observability serverConfig `shouldBe` observabilityConfig
      toServerConfig serverConfig `shouldBe` serverConfig

    it "covers derived Eq and Show instances for the shared server config types" $ do
      let shouldBeParenthesized rendered = do
            case rendered of
              '(' : rest ->
                case reverse rest of
                  ')' : _ -> pure ()
                  _ -> expectationFailure "expected parenthesized rendering"
              _ -> expectationFailure "expected parenthesized rendering"
          certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]}
          otherCertbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = [Text.pack "renew"]}
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend = CertbotHttp01 certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend = InProcessHttp01
              }
          manualCertificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = Text.pack "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls = Just tlsConfig
              }
          otherListenerConfig =
            ListenerConfig
              { listenerHost = Text.pack "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = manualCertificateSource})
              }
          staticRoot = StaticAssetRoot {staticUrlPrefix = Text.pack "/assets", staticDirectory = "public"}
          staticAssetsConfig = StaticAssetsConfig {staticAssetRoots = [staticRoot], staticCacheControlSeconds = Just 3600}
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                otlpHeaders = [(Text.pack "authorization", Text.pack "Bearer token")]
              }
          otherTracingConfig = OtlpExporter {otlpEndpoint = Text.pack "http://other-collector:4318/v1/traces", otlpHeaders = []}
          observabilityConfig = ObservabilityConfig {tracingExporter = Just tracingConfig, metricsExporter = Nothing}
          serverConfig =
            ServerConfig
              { listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                observability = observabilityConfig
              }
      Http `shouldNotBe` Https
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldNotBe` otherCertbotConfig
      InProcessHttp01 `shouldNotBe` CertbotHttp01 certbotConfig
      acmeConfig `shouldBe` acmeConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldBe` manualCertificateSource
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldBe` tlsConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = manualCertificateSource}
      listenerConfig `shouldBe` listenerConfig
      listenerConfig `shouldNotBe` otherListenerConfig
      staticRoot `shouldBe` staticRoot
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = Text.pack "/static", staticDirectory = "public"}
      staticAssetsConfig `shouldBe` staticAssetsConfig
      staticAssetsConfig `shouldNotBe` StaticAssetsConfig {staticAssetRoots = [], staticCacheControlSeconds = Nothing}
      tracingConfig `shouldBe` tracingConfig
      tracingConfig `shouldNotBe` otherTracingConfig
      observabilityConfig `shouldBe` observabilityConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      serverConfig `shouldBe` serverConfig
      serverConfig `shouldNotBe` serverConfig {listenerConfigs = [otherListenerConfig]}
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show certbotConfig `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show (CertbotHttp01 certbotConfig) `shouldBe` "CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})"
      show acmeConfig `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}"
      show manualCertificateSource `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show acmeCertificateSource `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})"
      show (TlsConfig {certificateSource = manualCertificateSource}) `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show listenerConfig `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show staticAssetsConfig `shouldBe` "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}"
      show tracingConfig `shouldBe` "OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}"
      show observabilityConfig `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}"
      show serverConfig `shouldBe` "ServerConfig {listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, observability = ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}}"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 (CertbotHttp01 certbotConfig) "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 tracingConfig "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 serverConfig "")
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [InProcessHttp01, CertbotHttp01 certbotConfig] `shouldBe` "[InProcessHttp01,CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})]"
      show [acmeConfig] `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}]"
      show [manualCertificateSource, acmeCertificateSource] `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})]"
      show [tlsConfig] `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})}]"
      show [listenerConfig] `shouldBe` "[ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig] `shouldBe` "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}]"
      show [tracingConfig] `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig] `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}]"
      show [serverConfig] `shouldBe` "[ServerConfig {listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, observability = ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}}]"

  describe "public record coverage" $ do
    it "reads every exported selector from the public request, page, shell, and document records" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          attribute = HtmlAttribute {attributeName = Text.pack "data-app", attributeValue = Text.pack "sample"}
          page = Page {pageTitle = Text.pack "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Known</h1>"}
          navigationItem = NavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute, navigationHref = Text.pack "/known", navigationIsActive = True}
          document = Document {documentTitle = Text.pack "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = Text.pack "app-main", documentMainContent = Text.pack "<h1>Known</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = Text.pack "app-main"}
          responseBodyValue = ResponseBody {responseStatus = 202, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"route\":\"data\"}"}
          NavigationItem {navigationLabel = navigationItemLabel, navigationRoute = navigationItemRoute} = navigationItem
          ResolvedNavigationItem {navigationLabel = resolvedNavigationItemLabel, navigationRoute = resolvedNavigationItemRoute, navigationHref = resolvedNavigationItemHref, navigationIsActive = resolvedNavigationItemIsActive} = resolvedNavigationItem

      requestRoute request `shouldBe` KnownRoute
      requestContext request `shouldBe` defaultContext
      attributeName attribute `shouldBe` Text.pack "data-app"
      attributeValue attribute `shouldBe` Text.pack "sample"
      pageTitle page `shouldBe` Text.pack "Known"
      pageRoute page `shouldBe` KnownRoute
      pageContext page `shouldBe` defaultContext
      pageBody page `shouldBe` Text.pack "<h1>Known</h1>"
      navigationItemLabel `shouldBe` Text.pack "Known"
      navigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemLabel `shouldBe` Text.pack "Known"
      resolvedNavigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemHref `shouldBe` Text.pack "/known"
      resolvedNavigationItemIsActive `shouldBe` True
      documentTitle document `shouldBe` Text.pack "Known"
      documentBodyAttributes document `shouldBe` [attribute]
      documentNavigation document `shouldBe` [resolvedNavigationItem]
      documentMainId document `shouldBe` Text.pack "app-main"
      documentMainContent document `shouldBe` Text.pack "<h1>Known</h1>"
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` Text.pack "app-main"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      responseStatus responseBodyValue `shouldBe` 202
      responseContentType responseBodyValue `shouldBe` Text.pack "application/json"
      responseBody responseBodyValue `shouldBe` Text.pack "{\"route\":\"data\"}"

    it "exercises derived Eq and Show instances for public HarchWeb records and responses" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          otherRequest = RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
          page = Page {pageTitle = Text.pack "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Known</h1>"}
          otherPage = Page {pageTitle = Text.pack "Missing", pageRoute = MissingRoute, pageContext = defaultContext, pageBody = Text.pack "<h1>Missing</h1>"}
          attribute = HtmlAttribute {attributeName = Text.pack "data-app", attributeValue = Text.pack "sample"}
          otherAttribute = HtmlAttribute {attributeName = Text.pack "lang", attributeValue = Text.pack "en"}
          navigationItem = NavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute}
          otherNavigationItem = NavigationItem {navigationLabel = Text.pack "Missing", navigationRoute = MissingRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Known", navigationRoute = KnownRoute, navigationHref = Text.pack "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = Text.pack "Missing", navigationRoute = MissingRoute, navigationHref = Text.pack "/404", navigationIsActive = False}
          document = Document {documentTitle = Text.pack "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = Text.pack "app-main", documentMainContent = Text.pack "<h1>Known</h1>"}
          otherDocument = Document {documentTitle = Text.pack "Missing", documentBodyAttributes = [otherAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = Text.pack "other-main", documentMainContent = Text.pack "<h1>Missing</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = Text.pack "app-main"}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = Text.pack "other-main"}
          body = ResponseBody {responseStatus = 202, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"route\":\"data\"}"}
          otherBody = ResponseBody {responseStatus = 200, responseContentType = Text.pack "text/html", responseBody = Text.pack "<h1>OK</h1>"}
          pageResponse :: Response TestRoute TestContext
          pageResponse = PageResponse page
          otherPageResponse :: Response TestRoute TestContext
          otherPageResponse = PageResponse otherPage
          bodyResponseValue :: Response TestRoute TestContext
          bodyResponseValue = BodyResponse body
          otherBodyResponseValue :: Response TestRoute TestContext
          otherBodyResponseValue = BodyResponse otherBody

      (request == request) `shouldBe` True
      (request /= otherRequest) `shouldBe` True
      show request `shouldBe` "RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\"}}"
      show [request] `shouldBe` "[RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\"}}]"
      (page == page) `shouldBe` True
      (page /= otherPage) `shouldBe` True
      show page `shouldBe` "Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}"
      show [page] `shouldBe` "[Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}]"
      (attribute == attribute) `shouldBe` True
      (attribute /= otherAttribute) `shouldBe` True
      show attribute `shouldBe` "HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}"
      (navigationItem == navigationItem) `shouldBe` True
      (navigationItem /= otherNavigationItem) `shouldBe` True
      show navigationItem `shouldBe` "NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}"
      (resolvedNavigationItem == resolvedNavigationItem) `shouldBe` True
      (resolvedNavigationItem /= otherResolvedNavigationItem) `shouldBe` True
      show resolvedNavigationItem `shouldBe` "ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}"
      (document == document) `shouldBe` True
      (document /= otherDocument) `shouldBe` True
      show document `shouldBe` "Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainContent = \"<h1>Known</h1>\"}"
      show [document] `shouldBe` "[Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainContent = \"<h1>Known</h1>\"}]"
      (shell == shell) `shouldBe` True
      (shell /= otherShell) `shouldBe` True
      show shell `shouldBe` "PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\"}"
      show [shell] `shouldBe` "[PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\"}]"
      (body == body) `shouldBe` True
      (body /= otherBody) `shouldBe` True
      show body `shouldBe` "ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\"}"
      show [body] `shouldBe` "[ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\"}]"
      (pageResponse == pageResponse) `shouldBe` True
      (pageResponse /= otherPageResponse) `shouldBe` True
      show pageResponse `shouldBe` "PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"})"
      (bodyResponseValue == bodyResponseValue) `shouldBe` True
      (bodyResponseValue /= otherBodyResponseValue) `shouldBe` True
      show bodyResponseValue `shouldBe` "BodyResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\"})"
      show [pageResponse, bodyResponseValue] `shouldBe` "[PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\"}, pageBody = \"<h1>Known</h1>\"}),BodyResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\"})]"

    it "reads the Application fields directly without relying on higher-level helpers" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          codec = routeCodec sampleApplication

      appName sampleApplication `shouldBe` Text.pack "sample"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      parseRoute codec defaultContext (Text.pack "/known") `shouldBe` Just request
      parseRoute codec defaultContext (Text.pack "/data") `shouldBe` Just RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
      renderRoute codec request `shouldBe` Text.pack "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldReturn` PageResponse (samplePage request)
      pageShell sampleApplication (samplePage request)
        `shouldBe` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "application" $ do
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` Text.pack "sample"

    it "can render non-page responses for future API routes" $
      renderResponse sampleApplication (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})
        `shouldReturn` BodyResponse ResponseBody {responseStatus = 202, responseContentType = Text.pack "application/json", responseBody = Text.pack "{\"route\":\"data\"}"}

  describe "matchRoute" $ do
    it "returns parsed routes for supported paths" $
      matchRoute sampleCodec defaultContext (Text.pack "/known")
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}

    it "can derive route context from the matched path" $
      matchRoute sampleCodec defaultContext (Text.pack "/es/known")
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec defaultContext (Text.pack "/missing")
        `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}

  describe "renderRoute" $
    it "can include route context in generated paths" $ do
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})
        `shouldBe` Text.pack "/known"
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})
        `shouldBe` Text.pack "/es/known"

  describe "routeHref" $
    it "reuses route rendering for app-provided navigation targets" $ do
      routeHref sampleCodec defaultContext KnownRoute `shouldBe` Text.pack "/known"
      routeHref sampleCodec spanishContext KnownRoute `shouldBe` Text.pack "/es/known"

  describe "buildNavigation" $
    it "resolves hrefs and active state from the current page context" $
      buildNavigation sampleCodec (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})) (shellNavigationItems sampleShell)
        `shouldBe` [ ResolvedNavigationItem
                       { navigationLabel = Text.pack "Known",
                         navigationRoute = KnownRoute,
                         navigationHref = Text.pack "/es/known",
                         navigationIsActive = True
                       },
                     ResolvedNavigationItem
                       { navigationLabel = Text.pack "Missing",
                         navigationRoute = MissingRoute,
                         navigationHref = Text.pack "/404",
                         navigationIsActive = False
                       }
                   ]

  describe "buildDocument" $
    it "preserves the generic shell contract separately from app-specific page content" $
      buildDocument sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Document
          { documentTitle = Text.pack "Known",
            documentBodyAttributes =
              [ HtmlAttribute
                  { attributeName = Text.pack "data-app",
                    attributeValue = Text.pack "sample"
                  }
              ],
            documentNavigation =
              [ ResolvedNavigationItem
                  { navigationLabel = Text.pack "Known",
                    navigationRoute = KnownRoute,
                    navigationHref = Text.pack "/known",
                    navigationIsActive = True
                  },
                ResolvedNavigationItem
                  { navigationLabel = Text.pack "Missing",
                    navigationRoute = MissingRoute,
                    navigationHref = Text.pack "/404",
                    navigationIsActive = False
                  }
              ],
            documentMainId = Text.pack "app-main",
            documentMainContent = Text.pack "<h1>Known</h1>"
          }

  describe "buildPageShell" $
    it "renders the shared HTML document for the supplied page and shell options" $
      buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "toWaiApplication" $ do
    it "selects request paths through the stored route parser and returns HTML pages" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest [Text.pack "es", Text.pack "known"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "text/html; charset=utf-8"))
      readResponseBody response
        `shouldReturn` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/es/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

    it "treats an empty raw path as the root path" $ do
      response <- performWaiRequest (toWaiApplication rootPathApplication) Wai.defaultRequest
      Wai.responseStatus response `shouldBe` Http.status200
      readResponseBody response
        `shouldReturn` Text.pack "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

    it "renders the not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest [Text.pack "missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "text/html; charset=utf-8"))
      readResponseBody response
        `shouldReturn` Text.pack "<html><head><title>Missing</title></head><body data-app=\"sample\"><nav><a href=\"/known\">Known</a><a href=\"/404\" aria-current=\"page\">Missing</a></nav><main id=\"app-main\"><h1>Missing</h1></main></body></html>"

    it "preserves body-response status, content type, and body" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest [Text.pack "data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      Http.statusMessage (Wai.responseStatus response) `shouldBe` mempty
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "application/json"))
      readResponseBody response `shouldReturn` Text.pack "{\"route\":\"data\"}"

  describe "planServerStartup" $ do
    it "groups HTTP listeners into the expected bind plan" $ do
      let firstEndpoint = ListenerEndpoint {endpointHost = Text.pack "127.0.0.1", endpointPort = 5001}
          secondEndpoint = ListenerEndpoint {endpointHost = Text.pack "0.0.0.0", endpointPort = 5002}
          firstListener = ListenerConfig {listenerHost = endpointHost firstEndpoint, listenerPort = endpointPort firstEndpoint, listenerScheme = Http, listenerTls = Nothing}
          secondListener = ListenerConfig {listenerHost = endpointHost secondEndpoint, listenerPort = endpointPort secondEndpoint, listenerScheme = Http, listenerTls = Nothing}
          httpBindPlan = HttpBindPlan {httpEndpoints = [firstEndpoint, secondEndpoint]}
          startupPlan =
            ServerStartupPlan
              { httpBindPlan = httpBindPlan,
                manualTlsBindPlans = [],
                acmeBindPlans = []
              }
      planServerStartup (serverConfigWithListeners [firstListener, secondListener]) `shouldBe` Right startupPlan
      firstEndpoint `shouldBe` firstEndpoint
      firstEndpoint `shouldNotBe` secondEndpoint
      httpBindPlan `shouldBe` httpBindPlan
      httpBindPlan `shouldNotBe` HttpBindPlan {httpEndpoints = [firstEndpoint]}
      startupPlan `shouldBe` startupPlan
      startupPlan `shouldNotBe` startupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [firstEndpoint]}}
      show firstEndpoint `shouldBe` "ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001}"
      show [firstEndpoint, secondEndpoint] `shouldBe` "[ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]"
      show httpBindPlan `shouldBe` "HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}"
      show [httpBindPlan] `shouldBe` "[HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}]"
      show startupPlan `shouldBe` "ServerStartupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}, manualTlsBindPlans = [], acmeBindPlans = []}"
      show [startupPlan] `shouldBe` "[ServerStartupPlan {httpBindPlan = HttpBindPlan {httpEndpoints = [ListenerEndpoint {endpointHost = \"127.0.0.1\", endpointPort = 5001},ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5002}]}, manualTlsBindPlans = [], acmeBindPlans = []}]"

    it "translates manual certificate files into TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = Text.pack "0.0.0.0", endpointPort = 5443}
          certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource})
              }
          manualPlan =
            ManualTlsBindPlan
              { tlsEndpoint = endpoint,
                tlsCertificateFile = "cert.pem",
                tlsPrivateKeyFile = "key.pem"
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans = [manualPlan],
              acmeBindPlans = []
            }
      manualPlan `shouldBe` manualPlan
      manualPlan `shouldNotBe` manualPlan {tlsCertificateFile = "other.pem"}
      show manualPlan `shouldBe` "ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\"}"
      show [manualPlan] `shouldBe` "[ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\"}]"

    it "translates ACME-backed HTTPS listeners into certificate-management plans" $ do
      let httpEndpoint = ListenerEndpoint {endpointHost = Text.pack "127.0.0.1", endpointPort = 5001}
          endpoint = ListenerEndpoint {endpointHost = Text.pack "0.0.0.0", endpointPort = 5444}
          httpListener =
            ListenerConfig
              { listenerHost = endpointHost httpEndpoint,
                listenerPort = endpointPort httpEndpoint,
                listenerScheme = Http,
                listenerTls = Nothing
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend =
                  CertbotHttp01
                    CertbotConfig
                      { certbotExecutable = "certbot",
                        certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
                      }
              }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource acmeConfig})
              }
          acmePlan =
            AcmeBindPlan
              { acmeEndpoint = endpoint,
                acmeListenerConfig = acmeConfig
              }
      planServerStartup (serverConfigWithListeners [httpListener, listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = [httpEndpoint]},
              manualTlsBindPlans = [],
              acmeBindPlans = [acmePlan]
            }
      acmePlan `shouldBe` acmePlan
      acmePlan `shouldNotBe` acmePlan {acmeEndpoint = ListenerEndpoint {endpointHost = Text.pack "127.0.0.1", endpointPort = 5444}}
      show acmePlan `shouldBe` "AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}"
      show [acmePlan] `shouldBe` "[AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}]"

    it "rejects listeners whose TLS mode does not match their scheme" $ do
      let httpTlsListener =
            ListenerConfig
              { listenerHost = Text.pack "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}})
              }
          httpsWithoutTls =
            ListenerConfig
              { listenerHost = Text.pack "127.0.0.1",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Nothing
              }
      planServerStartup (serverConfigWithListeners [httpTlsListener])
        `shouldBe` Left (InvalidListenerTlsConfiguration httpTlsListener)
      planServerStartup (serverConfigWithListeners [httpsWithoutTls])
        `shouldBe` Left (InvalidListenerTlsConfiguration httpsWithoutTls)
      InvalidListenerTlsConfiguration httpTlsListener `shouldNotBe` InvalidListenerTlsConfiguration httpsWithoutTls
      show (InvalidListenerTlsConfiguration httpTlsListener)
        `shouldBe` "InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}})})"
      show [InvalidListenerTlsConfiguration httpsWithoutTls]
        `shouldBe` "[InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5443, listenerScheme = Https, listenerTls = Nothing})]"

    it "rejects invalid mixed listener configurations before startup" $ do
      let httpListener =
            ListenerConfig
              { listenerHost = Text.pack "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing
              }
          httpsListener =
            ListenerConfig
              { listenerHost = Text.pack "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls =
                  Just
                    TlsConfig
                      { certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
                      }
              }
          duplicateEndpoint = ListenerEndpoint {endpointHost = Text.pack "0.0.0.0", endpointPort = 5001}
      planServerStartup (serverConfigWithListeners [httpListener, httpsListener])
        `shouldBe` Left (DuplicateListenerEndpoint duplicateEndpoint)
      DuplicateListenerEndpoint duplicateEndpoint `shouldBe` DuplicateListenerEndpoint duplicateEndpoint
      DuplicateListenerEndpoint duplicateEndpoint `shouldNotBe` DuplicateListenerEndpoint ListenerEndpoint {endpointHost = Text.pack "127.0.0.1", endpointPort = 5001}
      show (DuplicateListenerEndpoint duplicateEndpoint)
        `shouldBe` "DuplicateListenerEndpoint (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5001})"
      show [DuplicateListenerEndpoint duplicateEndpoint]
        `shouldBe` "[DuplicateListenerEndpoint (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5001})]"

  describe "runServer" $ do
    it "writes the stub startup message to the supplied handle" $
      withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
        runServer outputHandle sampleServerConfig sampleApplication
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"

    it "fails before startup when the listener plan is invalid" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let invalidConfig =
              serverConfigWithListeners
                [ ListenerConfig
                    { listenerHost = Text.pack "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls = Nothing
                    }
                ]
        runServer outputHandle invalidConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Invalid listener startup plan: InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Nothing}))")
