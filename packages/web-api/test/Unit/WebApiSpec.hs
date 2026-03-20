{-# SPEC #-}

import Data.Text (Text)
import qualified Data.Text as Text
import qualified HarchWeb
import System.IO (hClose)
import System.IO.Temp (withSystemTempFile)
import WebApi (AcmeChallengeBackend (..), AcmeConfig (..), AppConfig (..), AppLocale (..), AppPageModel (..), AppRequestContext (..), AppRoute (..), CallToAction (..), CertbotConfig (..), HomePageModel (..), ListenerConfig (..), ListenerScheme (..), NotFoundPageModel (..), ObservabilityConfig (..), OtlpExporter (..), RouteSelectionError (..), SecondPageModel (..), StaticAssetRoot (..), StaticAssetsConfig (..), TlsCertificateSource (..), TlsConfig (..), buildApp, buildPageModel, defaultAppConfig, defaultRequestContext, matchRoute, parseRoute, renderPage, renderPageBody, renderRoutePath, run, selectRoute)

pureApplication :: HarchWeb.Application AppRoute AppRequestContext
pureApplication = buildApp defaultAppConfig

homeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
homeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = defaultRequestContext}

secondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
secondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = defaultRequestContext}

frenchRequestContext :: AppRequestContext
frenchRequestContext = defaultRequestContext {requestLocale = French}

frenchHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = frenchRequestContext}

frenchSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = frenchRequestContext}

notFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
notFoundRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = defaultRequestContext}

pureRouteMatcher :: Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
pureRouteMatcher = matchRoute defaultRequestContext

renderedShell :: AppConfig -> AppRoute -> Text
renderedShell config route =
  let application = buildApp config
      page = renderPage config (HarchWeb.RouteRequest {HarchWeb.requestRoute = route, HarchWeb.requestContext = defaultRequestContext})
   in HarchWeb.pageShell application page

spec = do
  describe "defaultAppConfig" $ do
    it "reserves structured listener, static asset, and observability settings" $
      defaultAppConfig
        `shouldBe` AppConfig
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

    it "can represent manual certificates, certbot-backed ACME, and exporter endpoints" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
              }
          tlsSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = [Text.pack "ops@example.com"],
                  acmeChallengeBackend = CertbotHttp01 certbotConfig
                }
          exporter =
            OtlpExporter
              { otlpEndpoint = Text.pack "http://otel-collector:4318",
                otlpHeaders = [(Text.pack "x-api-key", Text.pack "secret")]
              }
      TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
        `shouldBe` TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
      show tlsSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})"
      show exporter
        `shouldBe` "OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"x-api-key\",\"secret\")]}"

    it "reads exported selectors from the remaining public config and page-model types" $ do
      let manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          inProcessAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com", Text.pack "alerts@example.com"],
                acmeChallengeBackend = InProcessHttp01
              }
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = Text.pack "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = Text.pack "/assets",
                staticDirectory = "public"
              }
          staticConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = Text.pack "http://otel-collector:4318",
                otlpHeaders = [(Text.pack "authorization", Text.pack "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = Text.pack "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just (Text.pack "req-456")
              }
          callToAction =
            CallToAction
              { callToActionLabel = Text.pack "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = Text.pack "/fr"
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = Text.pack "Not Found",
                notFoundSummary = Text.pack "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      case manualCertificateSource of
        source@ManualCertificateFiles {} -> do
          certificateFile source `shouldBe` "cert.pem"
          privateKeyFile source `shouldBe` "key.pem"
        AcmeCertificateSource _ -> expectationFailure "expected manual certificate files"
      acmeDirectoryUrl inProcessAcmeConfig `shouldBe` Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory"
      acmeContactEmails inProcessAcmeConfig `shouldBe` [Text.pack "ops@example.com", Text.pack "alerts@example.com"]
      acmeChallengeBackend inProcessAcmeConfig `shouldBe` InProcessHttp01
      certificateSource tlsConfig `shouldBe` manualCertificateSource
      listenerHost listenerConfig `shouldBe` Text.pack "0.0.0.0"
      listenerPort listenerConfig `shouldBe` 5443
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` Text.pack "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots staticConfig `shouldBe` [staticRoot]
      staticCacheControlSeconds staticConfig `shouldBe` Just 3600
      otlpEndpoint exporter `shouldBe` Text.pack "http://otel-collector:4318"
      otlpHeaders exporter `shouldBe` [(Text.pack "authorization", Text.pack "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just exporter
      metricsExporter observabilityConfig `shouldBe` Just exporter
      appTitlePrefix appConfig `shouldBe` Text.pack "test-app"
      listenerConfigs appConfig `shouldBe` [listenerConfig]
      staticAssets appConfig `shouldBe` staticConfig
      observability appConfig `shouldBe` observabilityConfig
      requestLocale requestContext `shouldBe` French
      requestCorrelationId requestContext `shouldBe` Just (Text.pack "req-456")
      callToActionLabel callToAction `shouldBe` Text.pack "Return home"
      callToActionRoute callToAction `shouldBe` HomeRoute
      callToActionHref callToAction `shouldBe` Text.pack "/fr"
      notFoundHeading notFoundPageModel `shouldBe` Text.pack "Not Found"
      notFoundSummary notFoundPageModel `shouldBe` Text.pack "The requested page could not be found."
      notFoundPrimaryAction notFoundPageModel `shouldBe` callToAction

    it "directly exercises the remaining derived eq and show instances" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = [Text.pack "ops@example.com"],
                  acmeChallengeBackend = CertbotHttp01 certbotConfig
                }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = Text.pack "/assets",
                staticDirectory = "public"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR"],
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = Text.pack "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = Text.pack "/"
                    }
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = Text.pack "Not Found",
                notFoundSummary = Text.pack "The requested page could not be found.",
                notFoundPrimaryAction =
                  CallToAction
                    { callToActionLabel = Text.pack "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = Text.pack "/"
                    }
              }
          homePageModel =
            HomePageModel
              { homeHeading = Text.pack "Home",
                homeSummary = Text.pack "Server-rendered home page with stubbed content.",
                homePrimaryAction =
                  CallToAction
                    { callToActionLabel = Text.pack "Browse the second page",
                      callToActionRoute = SecondRoute,
                      callToActionHref = Text.pack "/second"
                    }
              }
      Http `shouldBe` Http
      Https `shouldBe` Https
      certbotConfig `shouldBe` certbotConfig
      InProcessHttp01 `shouldBe` InProcessHttp01
      CertbotHttp01 certbotConfig `shouldBe` CertbotHttp01 certbotConfig
      TlsConfig {certificateSource = manualCertificateSource}
        `shouldBe` TlsConfig {certificateSource = manualCertificateSource}
      acmeCertificateSource `shouldBe` acmeCertificateSource
      staticRoot `shouldBe` staticRoot
      English `shouldBe` English
      French `shouldBe` French
      HomeRoute `shouldBe` HomeRoute
      SecondRoute `shouldBe` SecondRoute
      NotFoundRoute `shouldBe` NotFoundRoute
      UnsupportedLocalePrefix (Text.pack "de") `shouldBe` UnsupportedLocalePrefix (Text.pack "de")
      UnsupportedPath (Text.pack "/missing") `shouldBe` UnsupportedPath (Text.pack "/missing")
      HomePage homePageModel `shouldBe` HomePage homePageModel
      SecondPage secondPageModel `shouldBe` SecondPage secondPageModel
      NotFoundPage notFoundPageModel `shouldBe` NotFoundPage notFoundPageModel
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show InProcessHttp01 `shouldBe` "InProcessHttp01"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show English `shouldBe` "English"
      show French `shouldBe` "French"
      show (UnsupportedLocalePrefix (Text.pack "de")) `shouldBe` "UnsupportedLocalePrefix \"de\""
      show (UnsupportedPath (Text.pack "/missing")) `shouldBe` "UnsupportedPath \"/missing\""
      show notFoundPageModel
        `shouldBe` "NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (NotFoundPage notFoundPageModel)
        `shouldBe` "NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"

  describe "parseRoute" $ do
    it "maps bare and default-locale paths to the same home route" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/")) `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/en")) `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/404")) `shouldBe` Just NotFoundRoute

    it "parses the second page path" $
      parseRoute defaultRequestContext (Text.pack "/second") `shouldBe` Just secondRequest

    it "lets explicit locale prefixes override the incoming request context" $ do
      parseRoute defaultRequestContext (Text.pack "/fr/second") `shouldBe` Just frenchSecondRequest
      parseRoute frenchRequestContext (Text.pack "/en/second") `shouldBe` Just secondRequest

    it "returns an unsupported-route representation for unknown paths" $
      parseRoute defaultRequestContext (Text.pack "/missing") `shouldBe` Nothing

    it "fails unsupported locale prefixes with a precise route-selection error" $ do
      selectRoute defaultRequestContext (Text.pack "/de") `shouldBe` Left (UnsupportedLocalePrefix (Text.pack "de"))
      selectRoute defaultRequestContext (Text.pack "/de/second") `shouldBe` Left (UnsupportedLocalePrefix (Text.pack "de"))

    it "rejects paths that do not start with a slash" $
      selectRoute defaultRequestContext (Text.pack "second") `shouldBe` Left (UnsupportedPath (Text.pack "second"))

    it "rejects unsupported multi-segment paths" $
      selectRoute defaultRequestContext (Text.pack "/fr/second/extra") `shouldBe` Left (UnsupportedPath (Text.pack "/fr/second/extra"))

    it "rejects unsupported single-segment non-locale paths" $
      selectRoute defaultRequestContext (Text.pack "/missing") `shouldBe` Left (UnsupportedPath (Text.pack "/missing"))

    it "rejects locale-prefixed paths whose trailing segment is unsupported" $ do
      selectRoute defaultRequestContext (Text.pack "/fr/missing") `shouldBe` Left (UnsupportedPath (Text.pack "/fr/missing"))
      selectRoute defaultRequestContext (Text.pack "/other/second") `shouldBe` Left (UnsupportedPath (Text.pack "/other/second"))

    it "merges middleware-supplied and path-derived request inputs deterministically" $ do
      let middlewareContext =
            defaultRequestContext
              { requestLocale = English,
                requestCorrelationId = Just (Text.pack "req-123")
              }
      parseRoute middlewareContext (Text.pack "/fr")
        `shouldBe` Just (HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = middlewareContext {requestLocale = French}})

    it "rejects invalid trailing slashes while keeping the root path valid" $ do
      parseRoute defaultRequestContext (Text.pack "/") `shouldBe` Just homeRequest
      parseRoute defaultRequestContext (Text.pack "/second/") `shouldBe` Nothing
      selectRoute defaultRequestContext (Text.pack "/second/") `shouldBe` Left (UnsupportedPath (Text.pack "/second/"))

  describe "renderRoutePath" $ do
    it "round-trips known routes through the parser" $ do
      parseRoute defaultRequestContext (renderRoutePath homeRequest) `shouldBe` Just homeRequest
      parseRoute defaultRequestContext (renderRoutePath secondRequest) `shouldBe` Just secondRequest
      parseRoute defaultRequestContext (renderRoutePath frenchSecondRequest) `shouldBe` Just frenchSecondRequest

    it "renders locale prefixes only for non-default locales" $ do
      renderRoutePath homeRequest `shouldBe` Text.pack "/"
      renderRoutePath frenchHomeRequest `shouldBe` Text.pack "/fr"
      renderRoutePath secondRequest `shouldBe` Text.pack "/second"
      renderRoutePath frenchSecondRequest `shouldBe` Text.pack "/fr/second"
      renderRoutePath notFoundRequest `shouldBe` Text.pack "/404"

  describe "matchRoute" $ do
    it "matches the home path" $
      pureRouteMatcher (Text.pack "/") `shouldBe` homeRequest

    it "matches the second page path" $
      pureRouteMatcher (Text.pack "/second") `shouldBe` secondRequest

    it "matches locale-prefixed paths with the merged request context" $
      pureRouteMatcher (Text.pack "/fr") `shouldBe` frenchHomeRequest

    it "falls back to the stable not-found route for unknown paths" $
      pureRouteMatcher (Text.pack "/missing") `shouldBe` notFoundRequest

  describe "renderPage" $ do
    it "selects the expected home page model" $
      renderPage defaultAppConfig homeRequest
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"
          }

    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
          }

    it "keeps shared layout data consistent across all routes" $ do
      let config =
            AppConfig
              { appTitlePrefix = Text.pack "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                observability = observability defaultAppConfig
              }
      renderedShell config HomeRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Home</title></head><body data-app=\"test-app\"><nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      renderedShell config SecondRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Second</title></head><body data-app=\"test-app\"><nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      renderedShell config NotFoundRoute
        `shouldBe` Text.pack "<html><head><title>test-app: Not Found</title></head><body data-app=\"test-app\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config =
            AppConfig
              { appTitlePrefix = Text.pack "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                observability = observability defaultAppConfig
              }
      show config
        `shouldBe` "AppConfig {appTitlePrefix = \"test-app\", listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}], staticAssets = StaticAssetsConfig {staticAssetRoots = [], staticCacheControlSeconds = Nothing}, observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}}"
      show defaultRequestContext `shouldBe` "AppRequestContext {requestLocale = English, requestCorrelationId = Nothing}"
      show (renderPage config secondRequest)
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageContext = AppRequestContext {requestLocale = English, requestCorrelationId = Nothing}, pageBody = \"<section data-page=\\\"second\\\"><h1 data-page-title=\\\"true\\\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\\\"true\\\">No highlights yet.</p><p><a href=\\\"/\\\" data-page-link=\\\"true\\\">Return home</a></p></section>\"}"
      renderPage config secondRequest `shouldBe` renderPage config secondRequest

  describe "buildPageModel" $ do
    it "builds stubbed home page data with a navigation affordance" $
      buildPageModel homeRequest
        `shouldBe` HomePage
          HomePageModel
            { homeHeading = Text.pack "Home",
              homeSummary = Text.pack "Server-rendered home page with stubbed content.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = Text.pack "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = Text.pack "/second"
                  }
            }

    it "keeps locale-aware action paths in stubbed page data" $
      buildPageModel frenchHomeRequest
        `shouldBe` HomePage
          HomePageModel
            { homeHeading = Text.pack "Home",
              homeSummary = Text.pack "Server-rendered home page with stubbed content.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = Text.pack "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = Text.pack "/fr/second"
                  }
            }

  describe "renderPageBody" $ do
    it "renders the home page heading and navigation affordance" $
      renderPageBody (buildPageModel homeRequest)
        `shouldBe` Text.pack "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      let homeShell = renderedShell defaultAppConfig HomeRoute
          secondShell = renderedShell defaultAppConfig SecondRoute
      renderPageBody (buildPageModel secondRequest)
        `shouldBe` Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
      Text.isInfixOf (Text.pack "<nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\">") homeShell `shouldBe` True
      Text.isInfixOf (Text.pack "<nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\">") secondShell `shouldBe` True

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      let homeBody = renderPageBody (buildPageModel homeRequest)
          secondBody = renderPageBody (buildPageModel secondRequest)
      Text.isInfixOf (Text.pack "<section data-page=\"home\">") homeBody `shouldBe` True
      Text.isInfixOf (Text.pack "<section data-page=\"second\">") secondBody `shouldBe` True
      Text.isInfixOf (Text.pack "data-page-title=\"true\"") homeBody `shouldBe` True
      Text.isInfixOf (Text.pack "data-page-link=\"true\"") secondBody `shouldBe` True
      Text.isInfixOf (Text.pack "<main") homeBody `shouldBe` False
      Text.isInfixOf (Text.pack "<body") secondBody `shouldBe` False

    it "covers empty and populated highlight rendering branches" $ do
      Text.isInfixOf (Text.pack "<p data-empty-state=\"true\">No highlights yet.</p>") (renderPageBody (buildPageModel secondRequest)) `shouldBe` True
      renderPageBody
        ( SecondPage
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR", Text.pack "Stable routes"],
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = Text.pack "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = Text.pack "/"
                    }
              }
        )
        `shouldBe` Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"

  describe "page shell integration" $ do
    it "marks the active navigation item for each routed page" $ do
      Text.isInfixOf (Text.pack "<a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a>") (renderedShell defaultAppConfig HomeRoute) `shouldBe` True
      Text.isInfixOf (Text.pack "<a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a>") (renderedShell defaultAppConfig SecondRoute) `shouldBe` True
      Text.isInfixOf (Text.pack "aria-current=\"page\"") (renderedShell defaultAppConfig NotFoundRoute) `shouldBe` False

    it "keeps shell output identical for repeated renders of the same page input" $ do
      let application = buildApp defaultAppConfig
          page = renderPage defaultAppConfig frenchSecondRequest
      HarchWeb.pageShell application page `shouldBe` HarchWeb.pageShell application page

    it "keeps not-found pages inside the shared shell" $
      renderedShell defaultAppConfig NotFoundRoute
        `shouldBe` Text.pack "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` Text.pack "web-api"

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/") `shouldBe` parseRoute defaultRequestContext (Text.pack "/")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/fr") `shouldBe` parseRoute defaultRequestContext (Text.pack "/fr")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/second") `shouldBe` parseRoute defaultRequestContext (Text.pack "/second")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/missing") `shouldBe` Nothing
      HarchWeb.renderRoute codec homeRequest `shouldBe` renderRoutePath homeRequest
      HarchWeb.renderRoute codec frenchSecondRequest `shouldBe` renderRoutePath frenchSecondRequest
      HarchWeb.renderRoute codec secondRequest `shouldBe` renderRoutePath secondRequest
      HarchWeb.renderRoute codec notFoundRequest `shouldBe` renderRoutePath notFoundRequest
      HarchWeb.notFoundRequest codec defaultRequestContext `shouldBe` notFoundRequest

    it "stores the same page rendering behavior used by direct page tests" $ do
      HarchWeb.renderResponse pureApplication homeRequest `shouldBe` HarchWeb.PageResponse (renderPage defaultAppConfig homeRequest)
      HarchWeb.renderResponse pureApplication secondRequest `shouldBe` HarchWeb.PageResponse (renderPage defaultAppConfig secondRequest)
      HarchWeb.renderResponse pureApplication notFoundRequest `shouldBe` HarchWeb.PageResponse (renderPage defaultAppConfig notFoundRequest)

    it "is structurally complete enough to render supported and not-found shells" $ do
      let homePage = renderPage defaultAppConfig homeRequest
          secondPage = renderPage defaultAppConfig secondRequest
          notFoundPage = renderPage defaultAppConfig notFoundRequest
      HarchWeb.pageShell pureApplication homePage
        `shouldBe` Text.pack "<html><head><title>web-api: Home</title></head><body data-app=\"web-api\"><nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication secondPage
        `shouldBe` Text.pack "<html><head><title>web-api: Second</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication notFoundPage
        `shouldBe` Text.pack "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

    it "can grow from page responses to API responses without changing route matching" $
      case HarchWeb.renderResponse pureApplication homeRequest of
        HarchWeb.PageResponse page -> HarchWeb.pageRoute page `shouldBe` HomeRoute
        HarchWeb.BodyResponse _ -> expectationFailure "expected page response"

  describe "run" $
    it "writes startup output to the supplied handle for isolated tests" $
      withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
        run outputHandle
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
