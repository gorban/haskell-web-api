{-# LANGUAGE OverloadedStrings #-}

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
import System.Directory (createDirectoryIfMissing)
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
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
defaultContext = TestContext {requestLanguage = "en"}

spanishContext :: TestContext
spanishContext = TestContext {requestLanguage = "es"}

sampleCodec :: RouteCodec TestRoute TestContext
sampleCodec =
  RouteCodec
    { parseRoute = parseSampleRoute,
      renderRoute = renderSampleRoute,
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext}
    }

parseSampleRoute :: TestContext -> Text -> Maybe (RouteRequest TestRoute TestContext)
parseSampleRoute routeContext path
  | path == "/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
  | path == "/es/known" =
      Just RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}
  | path == "/data" =
      Just RouteRequest {requestRoute = DataRoute, requestContext = routeContext}
  | otherwise = Nothing

renderSampleRoute :: RouteRequest TestRoute TestContext -> Text
renderSampleRoute request =
  case (requestLanguage (requestContext request), requestRoute request) of
    (language, KnownRoute)
      | language == "es" -> "/es/known"
      | otherwise -> "/known"
    (_, DataRoute) -> "/data"
    (_, MissingRoute) -> "/404"

samplePage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
samplePage request =
  Page
    { pageTitle = "Known",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = "<h1>Known</h1>"
    }

sampleMissingPage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
sampleMissingPage request =
  Page
    { pageTitle = "Missing",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = "<h1>Missing</h1>"
    }

sampleShell :: PageShell TestRoute TestContext
sampleShell =
  PageShell
    { shellBodyAttributes =
        [ HtmlAttribute
            { attributeName = "data-app",
              attributeValue = "sample"
            }
        ],
      shellNavigationItems =
        [ NavigationItem
            { navigationLabel = "Known",
              navigationRoute = KnownRoute
            },
          NavigationItem
            { navigationLabel = "Missing",
              navigationRoute = MissingRoute
            }
        ],
      shellMainId = "app-main"
    }

emptyStaticAssets :: StaticAssetsConfig
emptyStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [],
      staticCacheControlSeconds = Nothing
    }

sampleApplicationWithStaticAssets :: StaticAssetsConfig -> Application TestRoute TestContext
sampleApplicationWithStaticAssets staticAssetsConfig =
  Application
    { appName = "sample",
      defaultRequestContext = defaultContext,
      applicationStaticAssets = staticAssetsConfig,
      routeCodec = sampleCodec,
      renderResponse = pure . renderSampleResponse,
      pageShell = buildPageShell sampleCodec sampleShell
    }

sampleApplication :: Application TestRoute TestContext
sampleApplication =
  sampleApplicationWithStaticAssets emptyStaticAssets

sampleServerConfig :: ServerConfig
sampleServerConfig =
  ServerConfig
    { listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
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
    { appName = "root-path",
      defaultRequestContext = defaultContext,
      applicationStaticAssets = emptyStaticAssets,
      routeCodec = rootPathCodec,
      renderResponse = pure . PageResponse . samplePage,
      pageShell = buildPageShell rootPathCodec sampleShell
    }

rootPathCodec :: RouteCodec TestRoute TestContext
rootPathCodec =
  RouteCodec
    { parseRoute = \routeContext path ->
        if path == "/"
          then Just RouteRequest {requestRoute = KnownRoute, requestContext = routeContext}
          else Nothing,
      renderRoute = \request ->
        case requestRoute request of
          KnownRoute -> "/"
          DataRoute -> "/data"
          MissingRoute -> "/404",
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
            responseContentType = "application/json",
            responseBody = "{\"route\":\"data\"}"
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
        [] -> "/"
        _ -> "/" <> Text.intercalate "/" segments

spec :: Spec
spec = do
  describe "shared config coverage" $ do
    it "reads exported selectors from the shared server config records" $ do
      let certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
          challengeBackend = CertbotHttp01 certbotConfig
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeChallengeBackend = challengeBackend
              }
          tlsSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = tlsSource}
          staticRoot = StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = "http://collector:4318/v1/traces",
                otlpHeaders = [("authorization", "Bearer token")]
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
                      { listenerHost = "127.0.0.1",
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
      certbotArguments certbotConfig `shouldBe` ["certonly", "--webroot"]
      acmeDirectoryUrl acmeConfig `shouldBe` "https://acme-v02.api.letsencrypt.org/directory"
      acmeContactEmails acmeConfig `shouldBe` ["ops@example.com"]
      acmeChallengeBackend acmeConfig `shouldBe` challengeBackend
      certificateSource tlsConfig `shouldBe` tlsSource
      listenerHost listenerConfig `shouldBe` "127.0.0.1"
      listenerPort listenerConfig `shouldBe` 5001
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots (staticAssets serverConfig) `shouldBe` [staticRoot]
      staticCacheControlSeconds (staticAssets serverConfig) `shouldBe` Just 3600
      otlpEndpoint tracingConfig `shouldBe` "http://collector:4318/v1/traces"
      otlpHeaders tracingConfig `shouldBe` [("authorization", "Bearer token")]
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
          certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
          otherCertbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["renew"]}
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeChallengeBackend = CertbotHttp01 certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeChallengeBackend = InProcessHttp01
              }
          manualCertificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls = Just tlsConfig
              }
          otherListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = manualCertificateSource})
              }
          staticRoot = StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}
          staticAssetsConfig = StaticAssetsConfig {staticAssetRoots = [staticRoot], staticCacheControlSeconds = Just 3600}
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = "http://collector:4318/v1/traces",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          otherTracingConfig = OtlpExporter {otlpEndpoint = "http://other-collector:4318/v1/traces", otlpHeaders = []}
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
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
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
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = "<h1>Known</h1>"}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainContent = "<h1>Known</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = "app-main"}
          responseBodyValue = ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}"}
          NavigationItem {navigationLabel = navigationItemLabel, navigationRoute = navigationItemRoute} = navigationItem
          ResolvedNavigationItem {navigationLabel = resolvedNavigationItemLabel, navigationRoute = resolvedNavigationItemRoute, navigationHref = resolvedNavigationItemHref, navigationIsActive = resolvedNavigationItemIsActive} = resolvedNavigationItem

      requestRoute request `shouldBe` KnownRoute
      requestContext request `shouldBe` defaultContext
      attributeName attribute `shouldBe` "data-app"
      attributeValue attribute `shouldBe` "sample"
      pageTitle page `shouldBe` "Known"
      pageRoute page `shouldBe` KnownRoute
      pageContext page `shouldBe` defaultContext
      pageBody page `shouldBe` "<h1>Known</h1>"
      navigationItemLabel `shouldBe` "Known"
      navigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemLabel `shouldBe` "Known"
      resolvedNavigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemHref `shouldBe` "/known"
      resolvedNavigationItemIsActive `shouldBe` True
      documentTitle document `shouldBe` "Known"
      documentBodyAttributes document `shouldBe` [attribute]
      documentNavigation document `shouldBe` [resolvedNavigationItem]
      documentMainId document `shouldBe` "app-main"
      documentMainContent document `shouldBe` "<h1>Known</h1>"
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` "app-main"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      responseStatus responseBodyValue `shouldBe` 202
      responseContentType responseBodyValue `shouldBe` "application/json"
      responseBody responseBodyValue `shouldBe` "{\"route\":\"data\"}"

    it "exercises derived Eq and Show instances for public HarchWeb records and responses" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          otherRequest = RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = "<h1>Known</h1>"}
          otherPage = Page {pageTitle = "Missing", pageRoute = MissingRoute, pageContext = defaultContext, pageBody = "<h1>Missing</h1>"}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          otherAttribute = HtmlAttribute {attributeName = "lang", attributeValue = "en"}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          otherNavigationItem = NavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute, navigationHref = "/404", navigationIsActive = False}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainContent = "<h1>Known</h1>"}
          otherDocument = Document {documentTitle = "Missing", documentBodyAttributes = [otherAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = "other-main", documentMainContent = "<h1>Missing</h1>"}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationItems = [navigationItem], shellMainId = "app-main"}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = "other-main"}
          body = ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}"}
          otherBody = ResponseBody {responseStatus = 200, responseContentType = "text/html", responseBody = "<h1>OK</h1>"}
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

      appName sampleApplication `shouldBe` "sample"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      applicationStaticAssets sampleApplication `shouldBe` emptyStaticAssets
      parseRoute codec defaultContext "/known" `shouldBe` Just request
      parseRoute codec defaultContext "/data" `shouldBe` Just RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
      renderRoute codec request `shouldBe` "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldReturn` PageResponse (samplePage request)
      pageShell sampleApplication (samplePage request)
        `shouldBe` "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "application" $ do
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` "sample"

    it "can render non-page responses for future API routes" $
      renderResponse sampleApplication (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})
        `shouldReturn` BodyResponse ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}"}

  describe "matchRoute" $ do
    it "returns parsed routes for supported paths" $
      matchRoute sampleCodec defaultContext "/known"
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}

    it "can derive route context from the matched path" $
      matchRoute sampleCodec defaultContext "/es/known"
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec defaultContext "/missing"
        `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}

  describe "renderRoute" $
    it "can include route context in generated paths" $ do
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})
        `shouldBe` "/known"
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})
        `shouldBe` "/es/known"

  describe "routeHref" $
    it "reuses route rendering for app-provided navigation targets" $ do
      routeHref sampleCodec defaultContext KnownRoute `shouldBe` "/known"
      routeHref sampleCodec spanishContext KnownRoute `shouldBe` "/es/known"

  describe "staticAssetHref" $
    it "renders asset URLs from the configured static prefix" $ do
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}) "app.js"
        `shouldBe` "/assets/app.js"
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/assets/", staticDirectory = "public"}) "/css/app.css"
        `shouldBe` "/assets/css/app.css"
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}) "/img/logo.svg"
        `shouldBe` "/img/logo.svg"

  describe "buildNavigation" $
    it "resolves hrefs and active state from the current page context" $
      buildNavigation sampleCodec (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})) (shellNavigationItems sampleShell)
        `shouldBe` [ ResolvedNavigationItem
                       { navigationLabel = "Known",
                         navigationRoute = KnownRoute,
                         navigationHref = "/es/known",
                         navigationIsActive = True
                       },
                     ResolvedNavigationItem
                       { navigationLabel = "Missing",
                         navigationRoute = MissingRoute,
                         navigationHref = "/404",
                         navigationIsActive = False
                       }
                   ]

  describe "buildDocument" $
    it "preserves the generic shell contract separately from app-specific page content" $
      buildDocument sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Document
          { documentTitle = "Known",
            documentBodyAttributes =
              [ HtmlAttribute
                  { attributeName = "data-app",
                    attributeValue = "sample"
                  }
              ],
            documentNavigation =
              [ ResolvedNavigationItem
                  { navigationLabel = "Known",
                    navigationRoute = KnownRoute,
                    navigationHref = "/known",
                    navigationIsActive = True
                  },
                ResolvedNavigationItem
                  { navigationLabel = "Missing",
                    navigationRoute = MissingRoute,
                    navigationHref = "/404",
                    navigationIsActive = False
                  }
              ],
            documentMainId = "app-main",
            documentMainContent = "<h1>Known</h1>"
          }

  describe "buildPageShell" $
    it "renders the shared HTML document for the supplied page and shell options" $
      buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

  describe "toWaiApplication" $ do
    it "selects request paths through the stored route parser and returns HTML pages" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["es", "known"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/es/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

    it "treats an empty raw path as the root path" $ do
      response <- performWaiRequest (toWaiApplication rootPathApplication) Wai.defaultRequest
      Wai.responseStatus response `shouldBe` Http.status200
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title></head><body data-app=\"sample\"><nav><a href=\"/\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\"><h1>Known</h1></main></body></html>"

    it "renders the not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      readResponseBody response
        `shouldReturn` "<html><head><title>Missing</title></head><body data-app=\"sample\"><nav><a href=\"/known\">Known</a><a href=\"/404\" aria-current=\"page\">Missing</a></nav><main id=\"app-main\"><h1>Missing</h1></main></body></html>"

    it "preserves body-response status, content type, and body" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      Http.statusMessage (Wai.responseStatus response) `shouldBe` mempty
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "serves configured static assets with deterministic cache-control headers" $
      withSystemTempDirectory "harch-web-static" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticCacheControlSeconds = Just 3600
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        firstResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "app.js"])
        secondResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "app.js"])
        Wai.responseStatus firstResponse `shouldBe` Http.status200
        lookup Http.hContentType (Wai.responseHeaders firstResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/javascript; charset=utf-8")
        lookup Http.hCacheControl (Wai.responseHeaders firstResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "public, max-age=3600")
        Wai.responseHeaders secondResponse `shouldBe` Wai.responseHeaders firstResponse
        readResponseBody firstResponse `shouldReturn` "console.log('asset');"
        readResponseBody secondResponse `shouldReturn` "console.log('asset');"

    it "serves root-prefixed static assets with the expected content types and no cache header" $
      withSystemTempDirectory "harch-web-static-root" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = assetDirectory}],
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            expectedResponses =
              [ (["styles.css"], "body{}", "text/css; charset=utf-8"),
                (["index.html"], "<h1>Home</h1>", "text/html; charset=utf-8"),
                (["data.json"], "{\"ok\":true}", "application/json; charset=utf-8"),
                (["logo.svg"], "<svg></svg>", "image/svg+xml"),
                (["note.txt"], "hello", "text/plain; charset=utf-8"),
                (["blob.bin"], "0101", "application/octet-stream")
              ]
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/styles.css") "body{}"
        writeFile (assetDirectory <> "/index.html") "<h1>Home</h1>"
        writeFile (assetDirectory <> "/data.json") "{\"ok\":true}"
        writeFile (assetDirectory <> "/logo.svg") "<svg></svg>"
        writeFile (assetDirectory <> "/note.txt") "hello"
        writeFile (assetDirectory <> "/blob.bin") "0101"
        mapM_
          ( \(segments, expectedBody, expectedContentType) -> do
              response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest segments)
              Wai.responseStatus response `shouldBe` Http.status200
              Wai.responseHeaders response
                `shouldBe` [(Http.hContentType, TextEncoding.encodeUtf8 expectedContentType)]
              readResponseBody response `shouldReturn` expectedBody
          )
          expectedResponses
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) Wai.defaultRequest
        Wai.responseStatus rootResponse `shouldBe` Http.status404
        Wai.responseHeaders rootResponse
          `shouldBe` [(Http.hContentType, TextEncoding.encodeUtf8 "text/plain; charset=utf-8")]
        readResponseBody rootResponse `shouldReturn` "Not Found"

    it "uses the most specific matching static root when multiple prefixes overlap" $
      withSystemTempDirectory "harch-web-static-overlap" $ \tempDirectory -> do
        let publicDirectory = tempDirectory <> "/public"
            adminDirectory = tempDirectory <> "/admin"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots =
                    [ StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = publicDirectory},
                      StaticAssetRoot {staticUrlPrefix = "/assets/admin", staticDirectory = adminDirectory}
                    ],
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True (publicDirectory <> "/admin")
        createDirectoryIfMissing True adminDirectory
        writeFile (publicDirectory <> "/admin/panel.js") "console.log('general');"
        writeFile (adminDirectory <> "/panel.js") "console.log('admin');"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "admin", "panel.js"])
        Wai.responseStatus response `shouldBe` Http.status200
        readResponseBody response `shouldReturn` "console.log('admin');"

    it "returns plain 404 responses for missing or invalid matched static asset paths" $
      withSystemTempDirectory "harch-web-static-missing" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = tempDirectory <> "/public"}],
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        missingResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "missing.js"])
        Wai.responseStatus missingResponse `shouldBe` Http.status404
        lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
        readResponseBody missingResponse `shouldReturn` "Not Found"
        invalidResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "..", "secret.txt"])
        Wai.responseStatus invalidResponse `shouldBe` Http.status404
        readResponseBody invalidResponse `shouldReturn` "Not Found"
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets"])
        Wai.responseStatus rootResponse `shouldBe` Http.status404
        readResponseBody rootResponse `shouldReturn` "Not Found"

    it "keeps cache-control headers on missing static asset responses when configured" $
      withSystemTempDirectory "harch-web-static-missing-cache" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = tempDirectory <> "/public"}],
                  staticCacheControlSeconds = Just 60
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            expectedCacheControl = Just (TextEncoding.encodeUtf8 "public, max-age=60")
        missingResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "missing.js"])
        lookup Http.hCacheControl (Wai.responseHeaders missingResponse) `shouldBe` expectedCacheControl
        invalidResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "..", "secret.txt"])
        lookup Http.hCacheControl (Wai.responseHeaders invalidResponse) `shouldBe` expectedCacheControl
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets"])
        lookup Http.hCacheControl (Wai.responseHeaders rootResponse) `shouldBe` expectedCacheControl

  describe "planServerStartup" $ do
    it "groups HTTP listeners into the expected bind plan" $ do
      let firstEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          secondEndpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5002}
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
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5443}
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
      let httpEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5444}
          httpListener =
            ListenerConfig
              { listenerHost = endpointHost httpEndpoint,
                listenerPort = endpointPort httpEndpoint,
                listenerScheme = Http,
                listenerTls = Nothing
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeChallengeBackend =
                  CertbotHttp01
                    CertbotConfig
                      { certbotExecutable = "certbot",
                        certbotArguments = ["certonly", "--webroot"]
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
      acmePlan `shouldNotBe` acmePlan {acmeEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5444}}
      show acmePlan `shouldBe` "AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}"
      show [acmePlan] `shouldBe` "[AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}]"

    it "rejects listeners whose TLS mode does not match their scheme" $ do
      let httpTlsListener =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}})
              }
          httpsWithoutTls =
            ListenerConfig
              { listenerHost = "127.0.0.1",
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
              { listenerHost = "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing
              }
          httpsListener =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls =
                  Just
                    TlsConfig
                      { certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
                      }
              }
          duplicateEndpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5001}
      planServerStartup (serverConfigWithListeners [httpListener, httpsListener])
        `shouldBe` Left (DuplicateListenerEndpoint duplicateEndpoint)
      DuplicateListenerEndpoint duplicateEndpoint `shouldBe` DuplicateListenerEndpoint duplicateEndpoint
      DuplicateListenerEndpoint duplicateEndpoint `shouldNotBe` DuplicateListenerEndpoint ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
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
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls = Nothing
                    }
                ]
        runServer outputHandle invalidConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Invalid listener startup plan: InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Nothing}))")
