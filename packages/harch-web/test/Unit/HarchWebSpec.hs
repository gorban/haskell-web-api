{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWebSpec (spec) where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, newMVar, putMVar, readMVar, threadDelay)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isHexDigit)
import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, isPrefixOf, isSuffixOf)
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
import HarchWeb.Observability qualified as Observability
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Socket.ByteString qualified as SocketByteString
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Internal qualified as WaiInternal
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Posix.Signals (raiseSignal, sigINT, sigTERM)
import System.Process (callProcess, readProcessWithExitCode)
import Test.Hspec
import Text.Read (readMaybe)

data TestContext = TestContext
  { requestLanguage :: Text,
    requestPathPrefix :: Text
  }
  deriving (Eq, Show)

data CapturedCollectorRequest = CapturedCollectorRequest
  { capturedCollectorMethod :: ByteString.ByteString,
    capturedCollectorPath :: ByteString.ByteString,
    capturedCollectorHeaders :: [Http.Header],
    capturedCollectorBody :: LazyByteString.ByteString
  }

data TestRoute
  = KnownRoute
  | QueryRoute Text
  | DataRoute
  | MissingRoute
  deriving (Eq, Show)

defaultContext :: TestContext
defaultContext = TestContext {requestLanguage = "en", requestPathPrefix = ""}

spanishContext :: TestContext
spanishContext = TestContext {requestLanguage = "es", requestPathPrefix = ""}

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
  | Just queryString <- Text.stripPrefix "/query?" path =
      Just RouteRequest {requestRoute = QueryRoute queryString, requestContext = routeContext}
  | path == "/data" =
      Just RouteRequest {requestRoute = DataRoute, requestContext = routeContext}
  | otherwise = Nothing

renderSampleRoute :: RouteRequest TestRoute TestContext -> Text
renderSampleRoute request =
  applyTestPathPrefix
    (requestPathPrefix (requestContext request))
    ( case (requestLanguage (requestContext request), requestRoute request) of
        (language, KnownRoute)
          | language == "es" -> "/es/known"
          | otherwise -> "/known"
        (_, QueryRoute queryString) -> "/query?" <> queryString
        (_, DataRoute) -> "/data"
        (_, MissingRoute) -> "/404"
    )

applyTestPathPrefix :: Text -> Text -> Text
applyTestPathPrefix pathPrefix path
  | Text.null pathPrefix = path
  | path == "/" = pathPrefix
  | otherwise = pathPrefix <> path

sampleRequestContextFromRequest :: Bool -> Wai.Request -> TestContext -> TestContext
sampleRequestContextFromRequest trustProxyHeaders request requestContext =
  requestContext
    { requestPathPrefix =
        if trustProxyHeaders
          then
            maybe
              ""
              normalizeTestPathPrefix
              ( lookup "X-Forwarded-Prefix" (Wai.requestHeaders request)
                  >>= firstTestHeaderValue
              )
          else ""
    }

normalizeTestPathPrefix :: Text -> Text
normalizeTestPathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> ""
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
      normalizedPrefix =
        Text.dropWhileEnd (== '/') slashPrefixedPrefix
   in normalizedPrefix

firstTestHeaderValue :: ByteString.ByteString -> Maybe Text
firstTestHeaderValue headerValue =
  case filter (not . Text.null) (map Text.strip (Text.splitOn "," (TextEncoding.decodeUtf8 headerValue))) of
    [] -> Nothing
    firstValue : _ -> Just firstValue

samplePage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
samplePage request =
  Page
    { pageTitle = "Known",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = "<h1>Known</h1>",
      pageBootstrapHooks = []
    }

sampleMissingPage :: RouteRequest TestRoute TestContext -> Page TestRoute TestContext
sampleMissingPage request =
  Page
    { pageTitle = "Missing",
      pageRoute = requestRoute request,
      pageContext = requestContext request,
      pageBody = "<h1>Missing</h1>",
      pageBootstrapHooks = []
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
      shellNavigationAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-region",
              attributeValue = "primary"
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
      shellMainId = "app-main",
      shellMainAttributes =
        [ HtmlAttribute
            { attributeName = "data-navigation-content",
              attributeValue = "true"
            }
        ],
      shellStylesheets = [],
      shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]
    }

emptyStaticAssets :: StaticAssetsConfig
emptyStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

defaultRequestPolicy :: RequestPolicyConfig
defaultRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing,
      trustForwardedHeaders = False,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }

sampleApplicationWithStaticAssets :: StaticAssetsConfig -> Application TestRoute TestContext
sampleApplicationWithStaticAssets staticAssetsConfig =
  sampleApplicationWithConfig staticAssetsConfig defaultRequestPolicy

sampleApplicationWithConfig :: StaticAssetsConfig -> RequestPolicyConfig -> Application TestRoute TestContext
sampleApplicationWithConfig staticAssetsConfig requestPolicyConfig =
  Application
    { appName = "sample",
      defaultRequestContext = defaultContext,
      requestContextFromRequest = sampleRequestContextFromRequest (trustForwardedHeaders requestPolicyConfig),
      applicationNavigationRuntime = Nothing,
      applicationStaticAssets = staticAssetsConfig,
      applicationRequestPolicy = requestPolicyConfig,
      applicationRequestMiddleware = [],
      routeCodec = sampleCodec,
      renderResponse = pure . renderSampleResponse,
      handleClientAction = const (pure Nothing),
      pageShell = buildPageShell sampleCodec sampleShell,
      reportRequestObservability = const (pure ()),
      reportConnectionObservability = const (pure ()),
      reportApplicationLog = const (pure ())
    }

sampleApplication :: Application TestRoute TestContext
sampleApplication =
  sampleApplicationWithStaticAssets emptyStaticAssets

trustedForwardedApplication :: Application TestRoute TestContext
trustedForwardedApplication =
  sampleApplicationWithConfig
    emptyStaticAssets
    defaultRequestPolicy
      { trustForwardedHeaders = True
      }

sampleServerConfig :: ServerConfig
sampleServerConfig =
  ServerConfig
    { listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
              listenerPort = 5001,
              listenerScheme = Http,
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          },
      requestPolicy = defaultRequestPolicy,
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

httpRuntimeListener :: Text -> Int -> ListenerConfig
httpRuntimeListener host port =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Http,
      listenerTls = Nothing,
      listenerAcme = Nothing
    }

acmeHttpsListenerWithDomains :: Text -> Int -> [Text] -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithDomains =
  acmeHttpsListenerWithDomainsAndChallengePort 80

acmeHttpsListenerWithDomainsAndChallengePort :: Int -> Text -> Int -> [Text] -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithDomainsAndChallengePort challengePort host port contactEmails domains challengeBackend =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Https,
      listenerTls =
        Just
          TlsConfig
            { certificateSource =
                AcmeCertificateSource
                  AcmeConfig
                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                      acmeContactEmails = contactEmails,
                      acmeDomains = domains,
                      acmeHttp01Port = challengePort,
                      acmeCertificateDirectory = Nothing,
                      acmeCertbotConfig = challengeBackend
                    }
            },
      listenerAcme = Nothing
    }

sharedHttpsListener :: Text -> Int -> FilePath -> ListenerConfig
sharedHttpsListener host port certificateDirectory =
  sharedHttpsListenerWithStartupMode host port certificateDirectory (AwaitCertificateFiles Nothing)

sharedHttpsListenerWithStartupMode :: Text -> Int -> FilePath -> TlsStartupMode -> ListenerConfig
sharedHttpsListenerWithStartupMode host port certificateDirectory startupMode =
  ListenerConfig
    { listenerHost = host,
      listenerPort = port,
      listenerScheme = Https,
      listenerTls =
        Just
          TlsConfig
            { certificateSource =
                SharedCertificateFiles
                  { certificateDirectory = certificateDirectory,
                    sharedCertificateStartupMode = startupMode
                  }
            },
      listenerAcme = Nothing
    }

acmeHttpsListenerWithContacts :: Text -> Int -> [Text] -> CertbotConfig -> ListenerConfig
acmeHttpsListenerWithContacts host port contactEmails =
  acmeHttpsListenerWithDomains host port contactEmails []

acmeHttpsListener :: Text -> Int -> CertbotConfig -> ListenerConfig
acmeHttpsListener host port =
  acmeHttpsListenerWithContacts host port ["ops@example.com"]

certbotHttp01Backend :: [Text] -> CertbotConfig
certbotHttp01Backend =
  certbotHttp01BackendWithExecutable "certbot"

certbotHttp01BackendWithExecutable :: FilePath -> [Text] -> CertbotConfig
certbotHttp01BackendWithExecutable executablePath certbotArguments =
  CertbotConfig
    { certbotExecutable = executablePath,
      certbotArguments = certbotArguments
    }

runtimeAcmePlanWithCertbotConfig :: CertbotConfig -> RuntimeAcmeBindPlan
runtimeAcmePlanWithCertbotConfig certbotConfig =
  RuntimeAcmeBindPlan
    { runtimeAcmeEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5443},
      runtimeAcmeTlsEndpoint = Just ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5443},
      runtimeAcmeListenerConfig =
        AcmeConfig
          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = ["ops@example.com"],
            acmeDomains = ["example.com", "www.example.com"],
            acmeHttp01Port = 80,
            acmeCertificateDirectory = Just ".tls/example.com",
            acmeCertbotConfig = certbotConfig
          }
    }

withCustomFakeCertbotExecutable :: [String] -> (FilePath -> IO a) -> IO a
withCustomFakeCertbotExecutable scriptLines action =
  withSystemTempDirectory "fake-certbot" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath

withFakeCertbotExecutable :: FilePath -> FilePath -> (FilePath -> IO a) -> IO a
withFakeCertbotExecutable certificatePath privateKeyPath =
  withCustomFakeCertbotExecutable
    ( fakeCertbotScriptPreamble
        <> [ "mkdir -p \"$config_dir/live/$cert_name\"",
             "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
             "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
           ]
    )

withFailingFakeCertbotExecutable :: (FilePath -> IO a) -> IO a
withFailingFakeCertbotExecutable =
  withCustomFakeCertbotExecutable
    [ "#!/bin/sh",
      "logs_dir=''",
      "while [ \"$#\" -gt 0 ]; do",
      "  case \"$1\" in",
      "    --logs-dir) logs_dir=\"$2\"; shift 2 ;;",
      "    *) shift ;;",
      "  esac",
      "done",
      "if [ -n \"$logs_dir\" ]; then",
      "  mkdir -p \"$logs_dir\"",
      "  printf '%s\\n' 'fake letsencrypt detail' > \"$logs_dir/letsencrypt.log\"",
      "fi",
      "echo fake certbot failure >&2",
      "exit 42"
    ]

fakeCertbotScriptPreamble :: [String]
fakeCertbotScriptPreamble =
  [ "#!/bin/sh",
    "set -eu",
    "config_dir=''",
    "cert_name=''",
    "domain=''",
    "while [ \"$#\" -gt 0 ]; do",
    "  case \"$1\" in",
    "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
    "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
    "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
    "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
    "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
    "    *) shift ;;",
    "  esac",
    "done",
    "if [ -z \"$cert_name\" ]; then",
    "  cert_name=\"${domain%%,*}\"",
    "fi"
  ]

rootPathApplication :: Application TestRoute TestContext
rootPathApplication =
  Application
    { appName = "root-path",
      defaultRequestContext = defaultContext,
      requestContextFromRequest = sampleRequestContextFromRequest True,
      applicationNavigationRuntime = Nothing,
      applicationStaticAssets = emptyStaticAssets,
      applicationRequestPolicy = defaultRequestPolicy {trustForwardedHeaders = True},
      applicationRequestMiddleware = [],
      routeCodec = rootPathCodec,
      renderResponse = pure . PageResponse . samplePage,
      handleClientAction = const (pure Nothing),
      pageShell = buildPageShell rootPathCodec sampleShell,
      reportRequestObservability = const (pure ()),
      reportConnectionObservability = const (pure ()),
      reportApplicationLog = const (pure ())
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
          KnownRoute -> applyTestPathPrefix (requestPathPrefix (requestContext request)) "/"
          QueryRoute queryString -> applyTestPathPrefix (requestPathPrefix (requestContext request)) ("/query?" <> queryString)
          DataRoute -> applyTestPathPrefix (requestPathPrefix (requestContext request)) "/data"
          MissingRoute -> applyTestPathPrefix (requestPathPrefix (requestContext request)) "/404",
      notFoundRequest = \routeContext -> routeContext `seq` RouteRequest {requestRoute = MissingRoute, requestContext = routeContext}
    }

renderSampleResponse :: RouteRequest TestRoute TestContext -> Response TestRoute TestContext
renderSampleResponse request =
  case requestRoute request of
    KnownRoute -> PageResponse (samplePage request)
    QueryRoute queryString ->
      BodyResponse
        ResponseBody
          { responseStatus = 200,
            responseContentType = "text/plain; charset=utf-8",
            responseBody = queryString,
            responseObservabilityAttributes = [],
            responseLogEntries = []
          }
    DataRoute ->
      BodyResponse
        ResponseBody
          { responseStatus = 202,
            responseContentType = "application/json",
            responseBody = "{\"route\":\"data\"}",
            responseObservabilityAttributes = [],
            responseLogEntries = []
          }
    MissingRoute -> PageResponse (sampleMissingPage request)

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  pure (fromMaybe (error "expected WAI application to produce a response") maybeResponse)

nextRequestBodyChunk :: IORef [ByteString.ByteString] -> IO ByteString.ByteString
nextRequestBodyChunk chunksReference =
  atomicModifyIORef' chunksReference $ \case
    [] -> ([], ByteString.empty)
    chunk : remainingChunks -> (remainingChunks, chunk)

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

waiRequestWithRemoteHostAndHeaders ::
  [Text] ->
  Socket.SockAddr ->
  Http.RequestHeaders ->
  Wai.Request
waiRequestWithRemoteHostAndHeaders segments remoteHost headers =
  (waiRequest segments)
    { Wai.remoteHost = remoteHost,
      Wai.requestHeaders = headers
    }

spec :: Spec
spec = do
  describe "shared config coverage" $ do
    it "reads exported selectors from the shared server config records" $ do
      let certbotConfig = CertbotConfig {certbotExecutable = "certbot", certbotArguments = ["certonly", "--webroot"]}
          challengeBackend = certbotConfig
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = challengeBackend
              }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/harch-web/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
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
          observabilityStartup =
            OtlpExporterStartup
              { startupSignal = TracingSignal,
                startupEndpoint = "http://collector:4318/v1/traces",
                startupHeaders = [("authorization", "Bearer token")]
              }
          observabilityPlan =
            ObservabilityStartupPlan
              { startupExporters = [observabilityStartup]
              }
          strictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 31536000,
                strictTransportSecurityIncludeSubDomains = True,
                strictTransportSecurityPreload = True
              }
          requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443,
                strictTransportSecurity = Just strictTransportSecurityConfig,
                trustForwardedHeaders = False,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
          serverConfig =
            ServerConfig
              { listenerConfigs =
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5001,
                        listenerScheme = Https,
                        listenerTls = Just tlsConfig,
                        listenerAcme = Nothing
                      }
                  ],
                staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [staticRoot],
                      staticAssetContentTypes = defaultStaticAssetContentTypes,
                      staticCacheControlSeconds = Just 3600
                    },
                requestPolicy = requestPolicyConfig,
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
      acmeDomains acmeConfig `shouldBe` ["example.com", "www.example.com"]
      acmeCertificateDirectory acmeConfig `shouldBe` Nothing
      acmeCertbotConfig acmeConfig `shouldBe` challengeBackend
      case sharedCertificateSource of
        SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode} -> do
          sharedDirectory `shouldBe` "/var/lib/harch-web/shared-certs"
          startupMode `shouldBe` AwaitCertificateFiles Nothing
        _ ->
          expectationFailure "expected shared certificate files"
      certificateSource tlsConfig `shouldBe` tlsSource
      listenerHost listenerConfig `shouldBe` "127.0.0.1"
      listenerPort listenerConfig `shouldBe` 5001
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots (staticAssets serverConfig) `shouldBe` [staticRoot]
      staticCacheControlSeconds (staticAssets serverConfig) `shouldBe` Just 3600
      redirectHttpToHttps requestPolicyConfig `shouldBe` True
      httpsRedirectPort requestPolicyConfig `shouldBe` Just 5443
      strictTransportSecurity requestPolicyConfig `shouldBe` Just strictTransportSecurityConfig
      corsPolicy requestPolicyConfig `shouldBe` defaultCorsPolicyConfig
      responseSecurityHeaders requestPolicyConfig `shouldBe` defaultResponseSecurityHeadersConfig
      strictTransportSecurityMaxAgeSeconds strictTransportSecurityConfig `shouldBe` 31536000
      strictTransportSecurityIncludeSubDomains strictTransportSecurityConfig `shouldBe` True
      strictTransportSecurityPreload strictTransportSecurityConfig `shouldBe` True
      requestPolicy serverConfig `shouldBe` requestPolicyConfig
      otlpEndpoint tracingConfig `shouldBe` "http://collector:4318/v1/traces"
      otlpHeaders tracingConfig `shouldBe` [("authorization", "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just tracingConfig
      metricsExporter observabilityConfig `shouldBe` Nothing
      observability serverConfig `shouldBe` observabilityConfig
      startupSignal observabilityStartup `shouldBe` TracingSignal
      startupEndpoint observabilityStartup `shouldBe` "http://collector:4318/v1/traces"
      startupHeaders observabilityStartup `shouldBe` [("authorization", "Bearer token")]
      startupExporters observabilityPlan `shouldBe` [observabilityStartup]
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
          strictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 31536000,
                strictTransportSecurityIncludeSubDomains = True,
                strictTransportSecurityPreload = True
              }
          otherStrictTransportSecurityConfig =
            StrictTransportSecurityConfig
              { strictTransportSecurityMaxAgeSeconds = 60,
                strictTransportSecurityIncludeSubDomains = False,
                strictTransportSecurityPreload = False
              }
          corsPolicyConfig =
            defaultCorsPolicyConfig
              { corsAllowedOrigins = ["https://client.example.com"],
                corsMaxAgeSeconds = Just 600
              }
          otherCorsPolicyConfig =
            defaultCorsPolicyConfig
              { corsAllowedOrigins = ["https://admin.example.com"]
              }
          responseSecurityHeadersConfig =
            defaultResponseSecurityHeadersConfig
              { frameOptions = Just "SAMEORIGIN"
              }
          otherResponseSecurityHeadersConfig =
            defaultResponseSecurityHeadersConfig
              { contentSecurityPolicy = Just "default-src 'none'"
              }
          requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443,
                strictTransportSecurity = Just strictTransportSecurityConfig,
                trustForwardedHeaders = False,
                corsPolicy = corsPolicyConfig,
                responseSecurityHeaders = responseSecurityHeadersConfig
              }
          otherRequestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = False,
                httpsRedirectPort = Nothing,
                strictTransportSecurity = Just otherStrictTransportSecurityConfig,
                trustForwardedHeaders = False,
                corsPolicy =
                  defaultCorsPolicyConfig
                    { corsAllowedOrigins = ["https://app.example.com"]
                    },
                responseSecurityHeaders =
                  defaultResponseSecurityHeadersConfig
                    { frameOptions = Just "SAMEORIGIN"
                    }
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig = certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["staging.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Just "/var/lib/harch-web/staging-certs",
                acmeCertbotConfig = otherCertbotConfig
              }
          manualCertificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/harch-web/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          otherListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = manualCertificateSource}),
                listenerAcme = Nothing
              }
          httpAcmeListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 80,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Just otherAcmeConfig
              }
          staticRoot = StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          tracingConfig =
            OtlpExporter
              { otlpEndpoint = "http://collector:4318/v1/traces",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          otherTracingConfig = OtlpExporter {otlpEndpoint = "http://other-collector:4318/v1/traces", otlpHeaders = []}
          observabilityConfig = ObservabilityConfig {tracingExporter = Just tracingConfig, metricsExporter = Nothing}
          exporterStartup =
            OtlpExporterStartup
              { startupSignal = TracingSignal,
                startupEndpoint = "http://collector:4318/v1/traces",
                startupHeaders = [("authorization", "Bearer token")]
              }
          otherExporterStartup =
            OtlpExporterStartup
              { startupSignal = MetricsSignal,
                startupEndpoint = "http://collector:4318/v1/metrics",
                startupHeaders = []
              }
          observabilityPlan = ObservabilityStartupPlan {startupExporters = [exporterStartup]}
          serverConfig =
            ServerConfig
              { listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicyConfig,
                observability = observabilityConfig
              }
      Http `shouldNotBe` Https
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldNotBe` otherCertbotConfig
      strictTransportSecurityConfig `shouldBe` strictTransportSecurityConfig
      strictTransportSecurityConfig `shouldNotBe` otherStrictTransportSecurityConfig
      corsPolicyConfig `shouldBe` corsPolicyConfig
      corsPolicyConfig `shouldNotBe` otherCorsPolicyConfig
      responseSecurityHeadersConfig `shouldBe` responseSecurityHeadersConfig
      responseSecurityHeadersConfig `shouldNotBe` otherResponseSecurityHeadersConfig
      requestPolicyConfig `shouldBe` requestPolicyConfig
      requestPolicyConfig `shouldNotBe` otherRequestPolicyConfig
      acmeConfig `shouldBe` acmeConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldBe` manualCertificateSource
      sharedCertificateSource `shouldBe` sharedCertificateSource
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      sharedCertificateSource `shouldNotBe` manualCertificateSource
      sharedCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldBe` tlsConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = manualCertificateSource}
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = sharedCertificateSource}
      listenerConfig `shouldBe` listenerConfig
      listenerConfig `shouldNotBe` otherListenerConfig
      httpAcmeListenerConfig `shouldNotBe` listenerConfig
      staticRoot `shouldBe` staticRoot
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
      staticAssetsConfig `shouldBe` staticAssetsConfig
      staticAssetsConfig
        `shouldNotBe` StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
      tracingConfig `shouldBe` tracingConfig
      tracingConfig `shouldNotBe` otherTracingConfig
      observabilityConfig `shouldBe` observabilityConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      ManualTlsCredentials `shouldBe` ManualTlsCredentials
      ManualTlsCredentials `shouldNotBe` SharedTlsCredentials
      AwaitCertificateFiles Nothing `shouldBe` AwaitCertificateFiles Nothing
      AwaitCertificateFiles Nothing `shouldNotBe` RequireCertificateFiles
      TracingSignal `shouldBe` TracingSignal
      TracingSignal `shouldNotBe` MetricsSignal
      exporterStartup `shouldBe` exporterStartup
      exporterStartup `shouldNotBe` otherExporterStartup
      observabilityPlan `shouldBe` observabilityPlan
      observabilityPlan `shouldNotBe` ObservabilityStartupPlan {startupExporters = []}
      serverConfig `shouldBe` serverConfig
      serverConfig `shouldNotBe` serverConfig {listenerConfigs = [otherListenerConfig]}
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show certbotConfig `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show strictTransportSecurityConfig `shouldBe` "StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}"
      show corsPolicyConfig `shouldBe` "CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}"
      show responseSecurityHeadersConfig `shouldContain` "ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show requestPolicyConfig `shouldContain` "corsPolicy = CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}"
      show requestPolicyConfig `shouldContain` "responseSecurityHeaders = ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show certbotConfig `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show acmeConfig `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}"
      show manualCertificateSource `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show sharedCertificateSource `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show acmeCertificateSource `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})"
      show (TlsConfig {certificateSource = manualCertificateSource}) `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show listenerConfig `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})})}"
      show httpAcmeListenerConfig `shouldBe` "ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 80, listenerScheme = Http, listenerTls = Nothing, listenerAcme = AcmeConfig {acmeDirectoryUrl = \"https://acme-staging-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"staging.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Just \"/var/lib/harch-web/staging-certs\", acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"renew\"]}}}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show staticAssetsConfig
        `shouldBe` ( "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}"
                   )
      show tracingConfig `shouldBe` "OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}"
      show observabilityConfig `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}"
      show ManualTlsCredentials `shouldBe` "ManualTlsCredentials"
      show (AwaitCertificateFiles (Just 15)) `shouldBe` "AwaitCertificateFiles {certificateWaitTimeoutSeconds = Just 15}"
      show TracingSignal `shouldBe` "TracingSignal"
      show exporterStartup `shouldBe` "OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}"
      show observabilityPlan `shouldBe` "ObservabilityStartupPlan {startupExporters = [OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]}"
      show serverConfig `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 strictTransportSecurityConfig "")
      shouldBeParenthesized (showsPrec 11 corsPolicyConfig "")
      shouldBeParenthesized (showsPrec 11 responseSecurityHeadersConfig "")
      shouldBeParenthesized (showsPrec 11 requestPolicyConfig "")
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 sharedCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 httpAcmeListenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 tracingConfig "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 exporterStartup "")
      shouldBeParenthesized (showsPrec 11 observabilityPlan "")
      shouldBeParenthesized (showsPrec 11 serverConfig "")
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [ManualTlsCredentials, SharedTlsCredentials] `shouldBe` "[ManualTlsCredentials,SharedTlsCredentials]"
      show [RequireCertificateFiles, AwaitCertificateFiles Nothing] `shouldBe` "[RequireCertificateFiles,AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [strictTransportSecurityConfig] `shouldBe` "[StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}]"
      show [corsPolicyConfig] `shouldBe` "[CorsPolicyConfig {corsAllowedOrigins = [\"https://client.example.com\"], corsAllowedMethods = [\"GET\",\"HEAD\",\"OPTIONS\"], corsAllowedHeaders = [\"Content-Type\",\"X-Requested-With\"], corsMaxAgeSeconds = Just 600}]"
      show [responseSecurityHeadersConfig] `shouldContain` "[ResponseSecurityHeadersConfig {contentSecurityPolicy = Just \"default-src 'self'; base-uri 'self'; object-src 'none'; frame-ancestors 'none'; form-action 'self'; script-src 'self'; style-src 'self'; img-src 'self' data:; font-src 'self'; connect-src 'self'\""
      show [requestPolicyConfig] `shouldContain` "RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True}), trustForwardedHeaders = False"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [acmeConfig] `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}]"
      show [manualCertificateSource, sharedCertificateSource, acmeCertificateSource] `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},SharedCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})]"
      show [tlsConfig] `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})}]"
      show [listenerConfig] `shouldBe` "[ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})})}]"
      show [httpAcmeListenerConfig] `shouldBe` "[ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 80, listenerScheme = Http, listenerTls = Nothing, listenerAcme = AcmeConfig {acmeDirectoryUrl = \"https://acme-staging-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"staging.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Just \"/var/lib/harch-web/staging-certs\", acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"renew\"]}}}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` ( "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}]"
                   )
      show [tracingConfig] `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig] `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}]"
      show [TracingSignal, MetricsSignal] `shouldBe` "[TracingSignal,MetricsSignal]"
      show [exporterStartup] `shouldBe` "[OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityPlan] `shouldBe` "[ObservabilityStartupPlan {startupExporters = [OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]}]"
      show [serverConfig] `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)

  describe "public record coverage" $ do
    it "reads every exported selector from the public request, page, shell, and document records" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          navigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "primary"}
          mainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "true"}
          localTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5001, localServerBaseUrl = "http://127.0.0.1:5001"}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = "<h1>Known</h1>", pageBootstrapHooks = ["known-page"]}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          navigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/navigation.js", navigationRuntimeScript = "console.log('nav');"}
          stylesheetPath = AssetPath "/assets/sample.css"
          stylesheetValue = stylesheet stylesheetPath
          scopedCssScope = cssScope "sample"
          scopedCssClass = ScopedCssClass scopedCssScope "title"
          globalCssClass = GlobalCssClass "visually-hidden"
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentStylesheets = [stylesheetValue], documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellStylesheets = [stylesheetValue], shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          responseBodyValue = ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = []}
          clientActionRequest = ClientActionRequest {clientActionMethod = "POST", clientActionPath = "/actions/subscribe", clientActionFields = [("email", "ada@example.com")], clientActionCsrfToken = Nothing, clientActionContext = defaultContext}
          regionPatch = RegionPatch {regionPatchId = "status-region", regionPatchHtml = "<p>Ready</p>"}
          clientActionResponse = ClientActionResponse {clientActionStatus = 200, clientActionPatches = [regionPatch], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}
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
      pageBootstrapHooks page `shouldBe` ["known-page"]
      navigationItemLabel `shouldBe` "Known"
      navigationItemRoute `shouldBe` KnownRoute
      navigationRuntimePath navigationRuntime `shouldBe` "/assets/navigation.js"
      navigationRuntimeScript navigationRuntime `shouldBe` "console.log('nav');"
      navigationRuntimeScriptSource "/app" navigationRuntime `shouldBe` "/app/assets/navigation.js"
      navigationRuntimeResponse navigationRuntime "/assets/navigation.js"
        `shouldBe` Just
          ResponseBody
            { responseStatus = 200,
              responseContentType = "application/javascript; charset=utf-8",
              responseBody = "console.log('nav');",
              responseObservabilityAttributes = [],
              responseLogEntries = []
            }
      navigationRuntimeResponse navigationRuntime "/assets/missing.js" `shouldBe` Nothing
      navigationRuntimePath defaultNavigationRuntime `shouldBe` "/assets/navigation.js"
      navigationRuntimeScript defaultNavigationRuntime `shouldBe` defaultNavigationRuntimeScript
      Text.isInfixOf "function navigateTo" defaultNavigationRuntimeScript `shouldBe` True
      assetPathText stylesheetPath `shouldBe` "/assets/sample.css"
      stylesheetAsset stylesheetValue `shouldBe` AssetPath "/assets/sample.css"
      cssScopeName scopedCssScope `shouldBe` "sample"
      cssClassText scopedCssClass `shouldBe` "harch-sample-title"
      cssClassText globalCssClass `shouldBe` "visually-hidden"
      resolvedNavigationItemLabel `shouldBe` "Known"
      resolvedNavigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemHref `shouldBe` "/known"
      resolvedNavigationItemIsActive `shouldBe` True
      documentTitle document `shouldBe` "Known"
      documentBodyAttributes document `shouldBe` [attribute]
      documentNavigationAttributes document `shouldBe` [navigationAttribute]
      documentNavigation document `shouldBe` [resolvedNavigationItem]
      documentMainId document `shouldBe` "app-main"
      documentMainAttributes document `shouldBe` [mainAttribute]
      documentMainContent document `shouldBe` "<h1>Known</h1>"
      documentBootstrapHooks document `shouldBe` ["known-page"]
      documentStylesheets document `shouldBe` [stylesheetValue]
      documentRuntimeDescriptors document `shouldBe` [DeferredModule "navigation" "/assets/navigation.js"]
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationAttributes shell `shouldBe` [navigationAttribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` "app-main"
      shellMainAttributes shell `shouldBe` [mainAttribute]
      shellStylesheets shell `shouldBe` [stylesheetValue]
      shellRuntimeDescriptors shell `shouldBe` [DeferredModule "navigation" "/assets/navigation.js"]
      localServerHost localTestServer `shouldBe` "127.0.0.1"
      localServerPort localTestServer `shouldBe` 5001
      localServerBaseUrl localTestServer `shouldBe` "http://127.0.0.1:5001"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      applicationNavigationRuntime sampleApplication `shouldBe` Nothing
      length (applicationRequestMiddleware sampleApplication) `shouldBe` 0
      responseStatus responseBodyValue `shouldBe` 202
      responseContentType responseBodyValue `shouldBe` "application/json"
      responseBody responseBodyValue `shouldBe` "{\"route\":\"data\"}"
      responseObservabilityAttributes responseBodyValue `shouldBe` []
      responseLogEntries responseBodyValue `shouldBe` []
      clientActionMethod clientActionRequest `shouldBe` "POST"
      clientActionPath clientActionRequest `shouldBe` "/actions/subscribe"
      clientActionFields clientActionRequest `shouldBe` [("email", "ada@example.com")]
      clientActionCsrfToken clientActionRequest `shouldBe` Nothing
      clientActionContext clientActionRequest `shouldBe` defaultContext
      regionPatchId regionPatch `shouldBe` "status-region"
      regionPatchHtml regionPatch `shouldBe` "<p>Ready</p>"
      clientActionStatus clientActionResponse `shouldBe` 200
      clientActionPatches clientActionResponse `shouldBe` [regionPatch]
      clientActionFocusId clientActionResponse `shouldBe` Nothing
      clientActionHeaders clientActionResponse `shouldBe` []
      clientActionObservabilityAttributes clientActionResponse `shouldBe` []
      clientActionLogEntries clientActionResponse `shouldBe` []
      let diagnostics = responseDiagnostics (ClientActionBodyResponse clientActionResponse :: Response TestRoute TestContext)
      diagnosticObservabilityAttributes diagnostics `shouldBe` []
      diagnosticLogEntries diagnostics `shouldBe` []

    it "exercises derived Eq and Show instances for public HarchWeb records and responses" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          otherRequest = RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = "<h1>Known</h1>", pageBootstrapHooks = ["known-page"]}
          otherPage = Page {pageTitle = "Missing", pageRoute = MissingRoute, pageContext = defaultContext, pageBody = "<h1>Missing</h1>", pageBootstrapHooks = []}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          otherAttribute = HtmlAttribute {attributeName = "lang", attributeValue = "en"}
          navigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "primary"}
          otherNavigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "secondary"}
          mainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "true"}
          otherMainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "false"}
          localTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5001, localServerBaseUrl = "http://127.0.0.1:5001"}
          otherLocalTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5002, localServerBaseUrl = "http://127.0.0.1:5002"}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          otherNavigationItem = NavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute}
          navigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/navigation.js", navigationRuntimeScript = "console.log('nav');"}
          otherNavigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/other-navigation.js", navigationRuntimeScript = "console.log('other');"}
          inlineBootstrap = InlineBootstrap "capture" "window.capture = true;"
          otherInlineBootstrap = InlineBootstrap "other-capture" "window.capture = false;"
          runtimeNonce = RuntimeNonce "test-nonce"
          otherRuntimeNonce = RuntimeNonce "other-nonce"
          stylesheetPath = AssetPath "/assets/sample.css"
          otherStylesheetPath = AssetPath "/assets/other.css"
          stylesheetValue = stylesheet stylesheetPath
          otherStylesheetValue = stylesheet otherStylesheetPath
          scopedCssScope = cssScope "sample"
          otherScopedCssScope = cssScope "other"
          scopedCssClass = ScopedCssClass scopedCssScope "title"
          otherScopedCssClass = ScopedCssClass otherScopedCssScope "title"
          globalCssClass = GlobalCssClass "visually-hidden"
          otherGlobalCssClass = GlobalCssClass "other-global"
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute, navigationHref = "/404", navigationIsActive = False}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentStylesheets = [], documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          otherDocument = Document {documentTitle = "Missing", documentBodyAttributes = [otherAttribute], documentNavigationAttributes = [otherNavigationAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = "other-main", documentMainAttributes = [otherMainAttribute], documentMainContent = "<h1>Missing</h1>", documentBootstrapHooks = [], documentStylesheets = [], documentRuntimeDescriptors = []}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellStylesheets = [], shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationAttributes = [otherNavigationAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = "other-main", shellMainAttributes = [otherMainAttribute], shellStylesheets = [], shellRuntimeDescriptors = []}
          body = ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = []}
          otherBody = ResponseBody {responseStatus = 200, responseContentType = "text/html", responseBody = "<h1>OK</h1>", responseObservabilityAttributes = [Observability.ObservabilityAttribute {Observability.attributeName = "exception.type", Observability.attributeValue = Observability.TextAttribute "SampleError"}], responseLogEntries = ["ERROR sample"]}
          pageMetadata = ResponseBody {responseStatus = 500, responseContentType = "text/html; charset=utf-8", responseBody = "", responseObservabilityAttributes = [Observability.ObservabilityAttribute {Observability.attributeName = "exception.type", Observability.attributeValue = Observability.TextAttribute "SampleError"}], responseLogEntries = ["ERROR page"]}
          otherPageMetadata = ResponseBody {responseStatus = 503, responseContentType = "text/html; charset=utf-8", responseBody = "", responseObservabilityAttributes = [], responseLogEntries = ["ERROR other page"]}
          pageResponse :: Response TestRoute TestContext
          pageResponse = PageResponse page
          otherPageResponse :: Response TestRoute TestContext
          otherPageResponse = PageResponse otherPage
          pageResponseWithMetadata :: Response TestRoute TestContext
          pageResponseWithMetadata = PageResponseWithMetadata pageMetadata page
          otherPageResponseWithMetadata :: Response TestRoute TestContext
          otherPageResponseWithMetadata = PageResponseWithMetadata otherPageMetadata otherPage
          bodyResponseValue :: Response TestRoute TestContext
          bodyResponseValue = BodyResponse body
          otherBodyResponseValue :: Response TestRoute TestContext
          otherBodyResponseValue = BodyResponse otherBody
          redirectResponseValue :: Response TestRoute TestContext
          redirectResponseValue = RedirectResponse body "/spaces"
          otherRedirectResponseValue :: Response TestRoute TestContext
          otherRedirectResponseValue = RedirectResponse otherBody "/other"
          clientActionRequest = ClientActionRequest {clientActionMethod = "POST", clientActionPath = "/actions/subscribe", clientActionFields = [("email", "ada@example.com")], clientActionCsrfToken = Just "csrf-token", clientActionContext = defaultContext}
          otherClientActionRequest = ClientActionRequest {clientActionMethod = "GET", clientActionPath = "/actions/other", clientActionFields = [], clientActionCsrfToken = Nothing, clientActionContext = spanishContext}
          regionPatch = RegionPatch {regionPatchId = "status-region", regionPatchHtml = "<p>Ready</p>"}
          otherRegionPatch = RegionPatch {regionPatchId = "other-region", regionPatchHtml = "<p>Other</p>"}
          clientActionResponse = ClientActionResponse {clientActionStatus = 200, clientActionPatches = [regionPatch], clientActionFocusId = Just "email", clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}
          otherClientActionResponse = ClientActionResponse {clientActionStatus = 422, clientActionPatches = [otherRegionPatch], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}

      (request == request) `shouldBe` True
      (request /= otherRequest) `shouldBe` True
      show request `shouldBe` "RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}}"
      show [request] `shouldBe` "[RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}}]"
      (page == page) `shouldBe` True
      (page /= otherPage) `shouldBe` True
      show page `shouldBe` "Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}"
      show [page] `shouldBe` "[Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}]"
      (attribute == attribute) `shouldBe` True
      (attribute /= otherAttribute) `shouldBe` True
      show attribute `shouldBe` "HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}"
      (navigationItem == navigationItem) `shouldBe` True
      (navigationItem /= otherNavigationItem) `shouldBe` True
      show navigationItem `shouldBe` "NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}"
      (navigationRuntime == navigationRuntime) `shouldBe` True
      (navigationRuntime /= otherNavigationRuntime) `shouldBe` True
      show navigationRuntime `shouldBe` "NavigationRuntime {navigationRuntimePath = \"/assets/navigation.js\", navigationRuntimeScript = \"console.log('nav');\"}"
      show [navigationRuntime] `shouldBe` "[NavigationRuntime {navigationRuntimePath = \"/assets/navigation.js\", navigationRuntimeScript = \"console.log('nav');\"}]"
      (inlineBootstrap == inlineBootstrap) `shouldBe` True
      (inlineBootstrap /= otherInlineBootstrap) `shouldBe` True
      show inlineBootstrap `shouldBe` "InlineBootstrap {runtimeDescriptorName = \"capture\", runtimeDescriptorSource = \"window.capture = true;\"}"
      (runtimeNonce == runtimeNonce) `shouldBe` True
      (runtimeNonce /= otherRuntimeNonce) `shouldBe` True
      show runtimeNonce `shouldBe` "RuntimeNonce {runtimeNonceValue = \"test-nonce\"}"
      show [runtimeNonce] `shouldBe` "[RuntimeNonce {runtimeNonceValue = \"test-nonce\"}]"
      (stylesheetPath == stylesheetPath) `shouldBe` True
      (stylesheetPath /= otherStylesheetPath) `shouldBe` True
      show stylesheetPath `shouldBe` "AssetPath {assetPathText = \"/assets/sample.css\"}"
      show [stylesheetPath] `shouldBe` "[AssetPath {assetPathText = \"/assets/sample.css\"}]"
      (stylesheetValue == stylesheetValue) `shouldBe` True
      (stylesheetValue /= otherStylesheetValue) `shouldBe` True
      show stylesheetValue `shouldBe` "Stylesheet {stylesheetAsset = AssetPath {assetPathText = \"/assets/sample.css\"}}"
      (scopedCssScope == scopedCssScope) `shouldBe` True
      (scopedCssScope /= otherScopedCssScope) `shouldBe` True
      show scopedCssScope `shouldBe` "CssScope {cssScopeName = \"sample\"}"
      show [scopedCssScope] `shouldBe` "[CssScope {cssScopeName = \"sample\"}]"
      (scopedCssClass == scopedCssClass) `shouldBe` True
      (scopedCssClass /= otherScopedCssClass) `shouldBe` True
      (globalCssClass == globalCssClass) `shouldBe` True
      (globalCssClass /= otherGlobalCssClass) `shouldBe` True
      show scopedCssClass `shouldBe` "ScopedCssClass (CssScope {cssScopeName = \"sample\"}) \"title\""
      show globalCssClass `shouldBe` "GlobalCssClass \"visually-hidden\""
      show [scopedCssClass, globalCssClass]
        `shouldBe` "[ScopedCssClass (CssScope {cssScopeName = \"sample\"}) \"title\",GlobalCssClass \"visually-hidden\"]"
      (resolvedNavigationItem == resolvedNavigationItem) `shouldBe` True
      (resolvedNavigationItem /= otherResolvedNavigationItem) `shouldBe` True
      show resolvedNavigationItem `shouldBe` "ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}"
      (document == document) `shouldBe` True
      (document /= otherDocument) `shouldBe` True
      show document `shouldContain` "documentRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      show [document] `shouldContain` "documentRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      (localTestServer == localTestServer) `shouldBe` True
      (localTestServer /= otherLocalTestServer) `shouldBe` True
      show localTestServer `shouldBe` "LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}"
      show [localTestServer] `shouldBe` "[LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}]"
      (shell == shell) `shouldBe` True
      (shell /= otherShell) `shouldBe` True
      show shell `shouldContain` "shellRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      show [shell] `shouldContain` "shellRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      (body == body) `shouldBe` True
      (body /= otherBody) `shouldBe` True
      show body `shouldBe` "ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []}"
      show [body] `shouldBe` "[ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []}]"
      (pageMetadata == pageMetadata) `shouldBe` True
      (pageMetadata /= otherPageMetadata) `shouldBe` True
      show pageMetadata `shouldBe` "ResponseBody {responseStatus = 500, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"]}"
      (pageResponse == pageResponse) `shouldBe` True
      (pageResponse /= otherPageResponse) `shouldBe` True
      show pageResponse `shouldBe` "PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]})"
      (pageResponseWithMetadata == pageResponseWithMetadata) `shouldBe` True
      (pageResponseWithMetadata /= otherPageResponseWithMetadata) `shouldBe` True
      show pageResponseWithMetadata `shouldBe` "PageResponseWithMetadata (ResponseBody {responseStatus = 500, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"]}) (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]})"
      (bodyResponseValue == bodyResponseValue) `shouldBe` True
      (bodyResponseValue /= otherBodyResponseValue) `shouldBe` True
      show bodyResponseValue `shouldBe` "BodyResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []})"
      (redirectResponseValue == redirectResponseValue) `shouldBe` True
      (redirectResponseValue /= otherRedirectResponseValue) `shouldBe` True
      show redirectResponseValue `shouldBe` "RedirectResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []}) \"/spaces\""
      show [pageResponse, pageResponseWithMetadata, bodyResponseValue] `shouldBe` "[PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),PageResponseWithMetadata (ResponseBody {responseStatus = 500, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"]}) (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),BodyResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []})]"
      (clientActionRequest == clientActionRequest) `shouldBe` True
      (clientActionRequest /= otherClientActionRequest) `shouldBe` True
      show clientActionRequest `shouldBe` "ClientActionRequest {clientActionMethod = \"POST\", clientActionPath = \"/actions/subscribe\", clientActionFields = [(\"email\",\"ada@example.com\")], clientActionCsrfToken = Just \"csrf-token\", clientActionContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}}"
      show [clientActionRequest] `shouldContain` "ClientActionRequest {clientActionMethod = \"POST\""
      (regionPatch == regionPatch) `shouldBe` True
      (regionPatch /= otherRegionPatch) `shouldBe` True
      show regionPatch `shouldBe` "RegionPatch {regionPatchId = \"status-region\", regionPatchHtml = \"<p>Ready</p>\"}"
      show [regionPatch] `shouldContain` "RegionPatch {regionPatchId = \"status-region\""
      (clientActionResponse == clientActionResponse) `shouldBe` True
      (clientActionResponse /= otherClientActionResponse) `shouldBe` True
      show clientActionResponse `shouldBe` "ClientActionResponse {clientActionStatus = 200, clientActionPatches = [RegionPatch {regionPatchId = \"status-region\", regionPatchHtml = \"<p>Ready</p>\"}], clientActionFocusId = Just \"email\", clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}"
      show [clientActionResponse] `shouldContain` "ClientActionResponse {clientActionStatus = 200"

    it "reads the Application fields directly without relying on higher-level helpers" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          codec = routeCodec sampleApplication

      appName sampleApplication `shouldBe` "sample"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      applicationStaticAssets sampleApplication `shouldBe` emptyStaticAssets
      handleClientAction sampleApplication ClientActionRequest {clientActionMethod = "POST", clientActionPath = "/actions/subscribe", clientActionFields = [], clientActionCsrfToken = Nothing, clientActionContext = defaultContext}
        `shouldReturn` Nothing
      parseRoute codec defaultContext "/known" `shouldBe` Just request
      parseRoute codec defaultContext "/data" `shouldBe` Just RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
      renderRoute codec request `shouldBe` "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldReturn` PageResponse (samplePage request)
      renderDocument (pageShell sampleApplication (samplePage request))
        `shouldBe` "<html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"
      Text.isInfixOf
        "<script nonce=\"development-render-nonce\">window.capture = true;</script>"
        ( renderDocument
            ( (pageShell sampleApplication (samplePage request))
                { documentRuntimeDescriptors = [InlineBootstrap "capture" "window.capture = true;"]
                }
            )
        )
        `shouldBe` True
      reportRequestObservability
        sampleApplication
        ( Observability.buildRequestObservability
            "GET"
            "http"
            "/known"
            "/known"
            200
            Observability.PageResponseKind
            []
        )
      reportApplicationLog sampleApplication "ignored log entry"

  describe "application" $ do
    it "preserves the supplied application description" $
      appName (application sampleApplication) `shouldBe` "sample"

    it "can render non-page responses for future API routes" $
      renderResponse sampleApplication (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})
        `shouldReturn` BodyResponse ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = []}

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
      routeHref sampleCodec (defaultContext {requestPathPrefix = "/app"}) KnownRoute `shouldBe` "/app/known"

  describe "staticAssetHref" $
    it "renders asset URLs from the configured static prefix" $ do
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}) "app.js"
        `shouldBe` "/assets/app.js"
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/assets/", staticDirectory = "public"}) "/css/app.css"
        `shouldBe` "/assets/css/app.css"
      staticAssetHref (StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}) "/img/logo.svg"
        `shouldBe` "/img/logo.svg"
      staticAssetHrefWithPrefix "/app" (StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}) ""
        `shouldBe` "/app"
      staticAssetHrefWithPrefix "/app" (StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}) "app.js"
        `shouldBe` "/app/assets/app.js"

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
            documentNavigationAttributes =
              [ HtmlAttribute
                  { attributeName = "data-navigation-region",
                    attributeValue = "primary"
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
            documentMainAttributes =
              [ HtmlAttribute
                  { attributeName = "data-navigation-content",
                    attributeValue = "true"
                  }
              ],
            documentMainContent = "<h1>Known</h1>",
            documentBootstrapHooks = [],
            documentStylesheets = [],
            documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]
          }

  describe "buildPageShell" $ do
    it "renders typed external stylesheets before nonce-bound runtime descriptors" $ do
      let document =
            (buildDocument sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})))
              { documentStylesheets = [stylesheet (AssetPath "/assets/sample.css")]
              }
      Text.isInfixOf
        "<title>Known</title><link rel=\"stylesheet\" href=\"/assets/sample.css\"><script type=\"module\" src=\"/assets/navigation.js\" defer></script>"
        (renderDocument document)
        `shouldBe` True

    it "renders the shared HTML document for the supplied page and shell options" $
      renderDocument (buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})))
        `shouldBe` "<html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "renders bootstrap hook metadata only for pages that opt in" $
      renderDocument
        ( buildPageShell
            sampleCodec
            sampleShell
            ( Page
                { pageTitle = "Known",
                  pageRoute = KnownRoute,
                  pageContext = defaultContext,
                  pageBody = "<h1>Known</h1>",
                  pageBootstrapHooks = ["known-page", "hydrate-known"]
                }
            )
        )
        `shouldBe` "<html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"known-page,hydrate-known\"><h1>Known</h1></main></body></html>"

  describe "runRequestMiddlewarePipeline" $ do
    it "runs in declaration order, carries context forward, and stops after a halt" $ do
      visitedMiddleware <- newIORef ([] :: [Text])
      let responseBodyValue = ResponseBody {responseStatus = 401, responseContentType = "text/plain; charset=utf-8", responseBody = "Sign in required", responseObservabilityAttributes = [], responseLogEntries = []}
          continuedResult = ContinueMiddleware spanishContext
          haltedResult = HaltMiddleware spanishContext responseBodyValue
          enrichMiddleware =
            RequestMiddleware $ \request requestContext -> do
              Wai.pathInfo request `shouldBe` []
              modifyIORef' visitedMiddleware (<> ["enrich"])
              pure (ContinueMiddleware requestContext {requestLanguage = "es"})
          haltMiddleware =
            RequestMiddleware $ \_ requestContext -> do
              modifyIORef' visitedMiddleware (<> ["halt"])
              pure (HaltMiddleware requestContext responseBodyValue)
          skippedMiddleware =
            RequestMiddleware $ \_ requestContext -> do
              modifyIORef' visitedMiddleware (<> ["skipped"])
              pure (ContinueMiddleware requestContext)
      runRequestMiddleware enrichMiddleware Wai.defaultRequest defaultContext
        `shouldReturn` continuedResult
      runRequestMiddlewarePipeline [] Wai.defaultRequest defaultContext
        `shouldReturn` ContinueMiddleware defaultContext
      runRequestMiddlewarePipeline [enrichMiddleware, haltMiddleware, skippedMiddleware] Wai.defaultRequest defaultContext
        `shouldReturn` haltedResult
      readIORef visitedMiddleware `shouldReturn` ["enrich", "enrich", "halt"]
      continuedResult == continuedResult `shouldBe` True
      continuedResult == haltedResult `shouldBe` False
      continuedResult /= haltedResult `shouldBe` True
      show continuedResult `shouldBe` "ContinueMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"})"
      show haltedResult `shouldBe` "HaltMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"}) (ResponseBody {responseStatus = 401, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = []})"
      show [continuedResult, haltedResult] `shouldBe` "[ContinueMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"}),HaltMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"}) (ResponseBody {responseStatus = 401, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = []})]"
      showList [continuedResult, haltedResult] "" `shouldBe` "[ContinueMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"}),HaltMiddleware (TestContext {requestLanguage = \"es\", requestPathPrefix = \"\"}) (ResponseBody {responseStatus = 401, responseContentType = \"text/plain; charset=utf-8\", responseBody = \"Sign in required\", responseObservabilityAttributes = [], responseLogEntries = []})]"

  describe "toWaiApplication" $ do
    it "runs app middleware for dynamic routes while preserving transformed context" $ do
      let middlewareApplication =
            sampleApplication
              { applicationRequestMiddleware =
                  [ RequestMiddleware $ \request requestContext -> do
                      Wai.pathInfo request `shouldBe` ["known"]
                      pure (ContinueMiddleware requestContext {requestLanguage = "es"})
                  ]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["known"])
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/es/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "halts dynamic requests without bypassing framework response headers" $ do
      let responseBodyValue = ResponseBody {responseStatus = 401, responseContentType = "text/plain; charset=utf-8", responseBody = "Sign in required", responseObservabilityAttributes = [], responseLogEntries = []}
          middlewareApplication =
            sampleApplication
              { applicationRequestMiddleware =
                  [RequestMiddleware $ \_ requestContext -> pure (HaltMiddleware requestContext responseBodyValue)]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["data"])
      Wai.responseStatus response `shouldBe` Http.status401
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8"
      lookup "Content-Security-Policy" (Wai.responseHeaders response) `shouldSatisfy` (/= Nothing)
      readResponseBody response `shouldReturn` "Sign in required"

    it "does not run app middleware for framework static responses" $ do
      middlewareRan <- newIORef False
      let middlewareApplication =
            sampleApplication
              { applicationNavigationRuntime = Just defaultNavigationRuntime,
                applicationRequestMiddleware =
                  [ RequestMiddleware $ \_ requestContext -> do
                      writeIORef middlewareRan True
                      pure (ContinueMiddleware requestContext)
                  ]
              }
      response <- performWaiRequest (toWaiApplication middlewareApplication) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      readIORef middlewareRan `shouldReturn` False

    it "serves the configured navigation runtime before app route matching" $ do
      requestObservabilityReference <- newIORef Nothing
      let runtimeApplication =
            sampleApplication
              { applicationNavigationRuntime = Just defaultNavigationRuntime,
                reportRequestObservability = writeIORef requestObservabilityReference . Just
              }
      response <- performWaiRequest (toWaiApplication runtimeApplication) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/javascript; charset=utf-8")
      responseBodyText <- readResponseBody response
      Text.isInfixOf "function navigateTo" responseBodyText `shouldBe` True
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap (Observability.requestSpanDisplayName . Observability.observabilityRequestSpan) maybeRequestObservability
        `shouldBe` Just "GET /assets/navigation.js"

    it "selects request paths through the stored route parser and returns HTML pages" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["es", "known"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/es/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" responseBody `shouldBe` True

    it "uses the route-resolved context for captured actions and returns typed region patches" $ do
      actionRequestReference <- newIORef Nothing
      requestObservabilityReference <- newIORef Nothing
      logEntriesReference <- newIORef []
      let failureAttribute = Observability.ObservabilityAttribute "error.type" (Observability.TextAttribute "RegistrationStoreUnavailable")
          actionApplication =
            sampleApplication
              { handleClientAction = \actionRequest -> do
                  writeIORef actionRequestReference (Just actionRequest)
                  pure
                    ( Just
                        ClientActionResponse
                          { clientActionStatus = 422,
                            clientActionPatches = [RegionPatch "status-region" "<p id=\"status-region\">Enter a valid email address.</p>"],
                            clientActionFocusId = Just "email",
                            clientActionHeaders = [("Set-Cookie", "session=opaque")],
                            clientActionObservabilityAttributes = [failureAttribute],
                            clientActionLogEntries = ["private registration failure detail"]
                          }
                    ),
                reportRequestObservability = writeIORef requestObservabilityReference . Just,
                reportApplicationLog = \entry -> modifyIORef' logEntriesReference (<> [entry])
              }
      actionBodyChunks <- newIORef ["email=ada%40example.com&_csrf=csrf-token&intent=subscribe&blank&invalid=%FF"]
      let capturedActionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["es", "known"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders = [("X-Harch-Action", "1"), (Http.hContentType, "application/x-www-form-urlencoded")]
                  }
              )
      response <- performWaiRequest (toWaiApplication actionApplication) capturedActionRequest
      maybeCapturedActionRequest <- readIORef actionRequestReference
      maybeCapturedActionRequest
        `shouldBe` Just
          ClientActionRequest
            { clientActionMethod = "POST",
              clientActionPath = "/es/known",
              clientActionFields = [("email", "ada@example.com"), ("_csrf", "csrf-token"), ("intent", "subscribe"), ("blank", ""), ("invalid", "�")],
              clientActionCsrfToken = Just "csrf-token",
              clientActionContext = spanishContext
            }
      Http.statusCode (Wai.responseStatus response) `shouldBe` 422
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/json; charset=utf-8"
      lookup "Set-Cookie" (Wai.responseHeaders response) `shouldBe` Just "session=opaque"
      readResponseBody response
        `shouldReturn` "{\"patches\":[{\"id\":\"status-region\",\"html\":\"<p id=\\\"status-region\\\">Enter a valid email address.</p>\"}],\"focusId\":\"email\"}"
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap (Observability.requestSpanAttributes . Observability.observabilityRequestSpan) maybeRequestObservability
        `shouldSatisfy` maybe False (hasTextAttribute "error.type" "RegistrationStoreUnavailable")
      capturedLogEntries <- readIORef logEntriesReference
      capturedLogEntries `shouldSatisfy` any (Text.isInfixOf "private registration failure detail")

    it "falls back to the SSR response when an action is not handled" $ do
      actionBodyChunks <- newIORef []
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["known"])
                  { Wai.requestHeaders = [("X-Harch-Action", "1")]
                  }
              )
      response <- performWaiRequest (toWaiApplication sampleApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")

    it "renders typed redirects with the location header and standard response metadata" $ do
      let typedRedirect = redirectResponse 302 "/spaces" :: Response TestRoute TestContext
          redirectApplication = sampleApplication {renderResponse = const (pure typedRedirect)}
          diagnostics = responseDiagnostics typedRedirect
      diagnosticObservabilityAttributes diagnostics `shouldBe` []
      diagnosticLogEntries diagnostics `shouldBe` []
      responseStatusCode redirectApplication typedRedirect `shouldBe` 302
      responseKind typedRedirect `shouldBe` Observability.BodyResponseKind
      response <- performWaiRequest (toWaiApplication redirectApplication) (waiRequest ["known"])
      Wai.responseStatus response `shouldBe` Http.status302
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "/spaces"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/plain; charset=utf-8"
      readResponseBody response `shouldReturn` ""

    it "serializes action responses with no patches or focus target" $ do
      let actionApplication =
            sampleApplication
              { handleClientAction = const (pure (Just ClientActionResponse {clientActionStatus = 204, clientActionPatches = [], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}))
              }
      actionBodyChunks <- newIORef []
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["actions", "empty"])
                  { Wai.requestHeaders = [("X-Harch-Action", "1")]
                  }
              )
      response <- performWaiRequest (toWaiApplication actionApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status204
      readResponseBody response `shouldReturn` "{\"patches\":[],\"focusId\":null}"

    it "adds the page nonce to custom CSP script sources, including policies without script-src" $ do
      let applicationWithPolicy policy =
            sampleApplicationWithConfig
              emptyStaticAssets
              (defaultRequestPolicy {responseSecurityHeaders = defaultResponseSecurityHeadersConfig {contentSecurityPolicy = Just policy}})
      missingScriptSourceResponse <- performWaiRequest (toWaiApplication (applicationWithPolicy "default-src 'self'")) (waiRequest ["known"])
      noneScriptSourceResponse <- performWaiRequest (toWaiApplication (applicationWithPolicy "script-src 'none'")) (waiRequest ["known"])
      let missingScriptSourcePolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders missingScriptSourceResponse)))
          noneScriptSourcePolicy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders noneScriptSourceResponse)))
      Text.isInfixOf "; script-src 'nonce-" missingScriptSourcePolicy `shouldBe` True
      Text.isInfixOf "script-src 'nonce-" noneScriptSourcePolicy `shouldBe` True
      Text.isInfixOf "'none'" noneScriptSourcePolicy `shouldBe` False

    it "uses the page nonce for page responses with metadata" $ do
      let metadata =
            ResponseBody
              { responseStatus = 422,
                responseContentType = "text/html; charset=utf-8",
                responseBody = "",
                responseObservabilityAttributes = [],
                responseLogEntries = []
              }
          metadataApplication =
            sampleApplication
              { renderResponse = pure . PageResponseWithMetadata metadata . samplePage,
                pageShell =
                  \page ->
                    (pageShell sampleApplication page)
                      { documentRuntimeDescriptors = [InlineBootstrap "capture" "window.capture = true;"]
                      }
              }
      response <- performWaiRequest (toWaiApplication metadataApplication) (waiRequest ["known"])
      let policy = TextEncoding.decodeUtf8 (fromMaybe "" (lookup "Content-Security-Policy" (Wai.responseHeaders response)))
      Http.statusCode (Wai.responseStatus response) `shouldBe` 422
      Text.isInfixOf "script-src 'self' 'nonce-" policy `shouldBe` True
      responseBody <- readResponseBody response
      Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True

    it "passes raw query strings to the stored route parser while keeping request paths path-only" $ do
      requestObservabilityReference <- newIORef Nothing
      let queryApplication =
            sampleApplication
              { reportRequestObservability = writeIORef requestObservabilityReference . Just
              }
          queryRequest =
            (waiRequest ["query"])
              { Wai.rawQueryString = "?q=server%20rendering"
              }
      response <- performWaiRequest (toWaiApplication queryApplication) queryRequest
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
      readResponseBody response `shouldReturn` "q=server%20rendering"
      maybeRequestObservability <- readIORef requestObservabilityReference
      fmap
        ( filter ((== "url.path") . Observability.attributeName)
            . Observability.requestSpanAttributes
            . Observability.observabilityRequestSpan
        )
        maybeRequestObservability
        `shouldBe` Just
          [ Observability.ObservabilityAttribute
              { Observability.attributeName = "url.path",
                Observability.attributeValue = Observability.TextAttribute "/query"
              }
          ]

    it "treats an empty raw path as the root path" $ do
      response <- performWaiRequest (toWaiApplication rootPathApplication) Wai.defaultRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "normalizes forwarded root prefixes for route matching and rendered root links" $ do
      let prefixedRootRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "app")]
              }
      response <- performWaiRequest (toWaiApplication rootPathApplication) prefixedRootRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "uses forwarded path prefixes for route matching and rendered navigation links" $ do
      let prefixedRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app/known",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
      response <- performWaiRequest (toWaiApplication trustedForwardedApplication) prefixedRequest
      Wai.responseStatus response `shouldBe` Http.status200
      responseBody <- readResponseBody response
      Text.isInfixOf "<a href=\"/app/known\" data-page-link=\"true\" aria-current=\"page\">Known</a>" responseBody `shouldBe` True

    it "renders the not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      responseBody <- readResponseBody response
      Text.isInfixOf "<h1>Missing</h1>" responseBody `shouldBe` True

    it "preserves body-response status, content type, and body" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      Http.statusMessage (Wai.responseStatus response) `shouldBe` mempty
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "handles secure requests through the same response-selection path" $ do
      let secureRequest =
            (waiRequest ["data"])
              { Wai.isSecure = True,
                Wai.requestMethod = "POST"
              }
      response <- performWaiRequest (toWaiApplication sampleApplication) secureRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "redirects insecure requests to HTTPS before rendering the application response" $ do
      let redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))
              { renderResponse = \_ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}))
              }
          redirectRequest =
            (waiRequest ["data"])
              { Wai.rawQueryString = "?from=plain-http",
                Wai.requestHeaders = [("Host", "app.example.com:80")]
              }
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com/data?from=plain-http"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
      readResponseBody response `shouldReturn` "Redirecting to HTTPS"

    it "rewrites redirects to the configured HTTPS listener port" $ do
      let redirectRequest =
            (waiRequest ["data"])
              { Wai.rawQueryString = "?from=runtime-http",
                Wai.requestHeaders = [("Host", "app.example.com:5001")]
              }
          requestPolicyConfig =
            defaultRequestPolicy
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com:5443/data?from=runtime-http"

    it "drops the default HTTPS port from redirect locations when configured explicitly" $ do
      let redirectRequest =
            (waiRequest ["data"])
              { Wai.requestHeaders = [("Host", "app.example.com:80")]
              }
          requestPolicyConfig =
            defaultRequestPolicy
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 443
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com/data"

    it "keeps forwarded path prefixes in HTTPS redirect locations" $ do
      let redirectRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/second",
                Wai.rawQueryString = "?from=plain",
                Wai.requestHeaders =
                  [ ("Host", "app.example.com"),
                    ("X-Forwarded-Prefix", "/app")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True, trustForwardedHeaders = True}))) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com/app/second?from=plain"

    it "does not redirect ACME http-01 challenge paths" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443
              }
          acmeRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token",
                Wai.requestHeaders = [("Host", "app.example.com:5001")]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) acmeRequest
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Nothing

    it "redirects the root path without requiring an explicit :80 host suffix" $ do
      let redirectRequest =
            Wai.defaultRequest
              { Wai.requestHeaders = [("Host", "app.example.com")]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://app.example.com/"

    it "uses forwarded HTTPS context to skip redirects and emit HSTS headers" $ do
      let requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Nothing,
                strictTransportSecurity =
                  Just
                    StrictTransportSecurityConfig
                      { strictTransportSecurityMaxAgeSeconds = 31536000,
                        strictTransportSecurityIncludeSubDomains = True,
                        strictTransportSecurityPreload = True
                      },
                trustForwardedHeaders = True,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
          proxiedHttpsRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              (Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1)))
              [ ("Host", "app.example.com"),
                ("X-Forwarded-Proto", "https")
              ]
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) proxiedHttpsRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Strict-Transport-Security" (Wai.responseHeaders response)
        `shouldBe` Just "max-age=31536000; includeSubDomains; preload"
      readResponseBody response `shouldReturn` "{\"route\":\"data\"}"

    it "does not emit HSTS headers for requests whose effective scheme stays HTTP" $ do
      let requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = False,
                httpsRedirectPort = Nothing,
                strictTransportSecurity =
                  Just
                    StrictTransportSecurityConfig
                      { strictTransportSecurityMaxAgeSeconds = 31536000,
                        strictTransportSecurityIncludeSubDomains = True,
                        strictTransportSecurityPreload = False
                      },
                trustForwardedHeaders = False,
                corsPolicy = defaultCorsPolicyConfig,
                responseSecurityHeaders = defaultResponseSecurityHeadersConfig
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup "Strict-Transport-Security" (Wai.responseHeaders response) `shouldBe` Nothing

    it "emits strict default response security headers without enabling cross-origin reads" $ do
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets defaultRequestPolicy)) (waiRequest ["data"])
      let headers = Wai.responseHeaders response
      lookup "Content-Security-Policy" headers `shouldBe` Just (TextEncoding.encodeUtf8 defaultContentSecurityPolicy)
      lookup "X-Content-Type-Options" headers `shouldBe` Just "nosniff"
      lookup "X-XSS-Protection" headers `shouldBe` Just "1; mode=block"
      lookup "Referrer-Policy" headers `shouldBe` Just "strict-origin-when-cross-origin"
      lookup "Permissions-Policy" headers `shouldBe` Just "accelerometer=(), camera=(), geolocation=(), gyroscope=(), magnetometer=(), microphone=(), payment=(), usb=()"
      lookup "X-Frame-Options" headers `shouldBe` Just "DENY"
      lookup "Access-Control-Allow-Origin" headers `shouldBe` Nothing

    it "allows response security headers to be disabled explicitly" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { responseSecurityHeaders =
                  ResponseSecurityHeadersConfig
                    { contentSecurityPolicy = Nothing,
                      contentTypeOptionsNoSniff = False,
                      xssProtection = Nothing,
                      referrerPolicy = Nothing,
                      permissionsPolicy = Nothing,
                      frameOptions = Nothing
                    }
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) (waiRequest ["data"])
      let headers = Wai.responseHeaders response
      lookup "Content-Security-Policy" headers `shouldBe` Nothing
      lookup "X-Content-Type-Options" headers `shouldBe` Nothing
      lookup "X-XSS-Protection" headers `shouldBe` Nothing
      lookup "Referrer-Policy" headers `shouldBe` Nothing
      lookup "Permissions-Policy" headers `shouldBe` Nothing
      lookup "X-Frame-Options" headers `shouldBe` Nothing

    it "applies response security headers to HTTPS redirects" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { redirectHttpToHttps = True
              }
          redirectRequest =
            (waiRequest [])
              { Wai.requestHeaders = [("Host", "app.example.com")]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup "Content-Security-Policy" (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 defaultContentSecurityPolicy)

    it "reports request observability for HTTPS redirects with the externally visible request path" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True, trustForwardedHeaders = True}))
              { renderResponse = \_ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})),
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
          redirectRequest =
            waiRequestWithRemoteHostAndHeaders
              ["second"]
              directRemoteHost
              [ ("Host", "app.example.com"),
                ("X-Forwarded-Prefix", "/app")
              ]
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/second"
                       "/app/second"
                       308
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                   ]
      mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "propagates incoming W3C trace context for HTTPS redirect observability" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          traceContext =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          redirectingApplication =
            (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))
              { renderResponse = \_ -> expectationFailure "expected HTTPS redirect before application rendering" >> pure (renderSampleResponse (RouteRequest {requestRoute = DataRoute, requestContext = defaultContext})),
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
          redirectRequest =
            waiRequestWithRemoteHostAndHeaders
              ["second"]
              directRemoteHost
              [ ("Host", "app.example.com"),
                ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
                ("tracestate", "vendor=value")
              ]
      response <- performWaiRequest (toWaiApplication redirectingApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.withRequestTraceContext
                           traceContext
                           ( Observability.buildRequestObservability
                               "GET"
                               "http"
                               "/second"
                               "/second"
                               308
                               Observability.BodyResponseKind
                               [clientAddressAttribute, peerAddressAttribute]
                           )
                       ]

    it "adds CORS headers only for explicitly allowed origins" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  defaultCorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"]
                    }
              }
          allowedRequest =
            (waiRequest ["data"])
              { Wai.requestHeaders = [("Origin", "https://client.example.com")]
              }
          blockedRequest =
            (waiRequest ["data"])
              { Wai.requestHeaders = [("Origin", "https://evil.example.com")]
              }
      allowedResponse <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) allowedRequest
      blockedResponse <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) blockedRequest
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders allowedResponse) `shouldBe` Just "https://client.example.com"
      lookup "Vary" (Wai.responseHeaders allowedResponse) `shouldBe` Just "Origin"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders allowedResponse) `shouldBe` Nothing
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders blockedResponse) `shouldBe` Nothing

    it "answers allowed CORS preflight requests with constrained methods and headers" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET", "HEAD"],
                      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
                      corsMaxAgeSeconds = Just 600
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Just "https://client.example.com"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Just "GET, HEAD"
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Just "Content-Type, X-Requested-With"
      lookup "Access-Control-Max-Age" (Wai.responseHeaders response) `shouldBe` Just "600"
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Nothing
      readResponseBody response `shouldReturn` ""

    it "does not answer CORS preflight requests for disallowed methods" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "DELETE")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status202
      lookup "Access-Control-Allow-Origin" (Wai.responseHeaders response) `shouldBe` Just "https://client.example.com"
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Nothing

    it "omits optional CORS preflight headers when they are not configured" $ do
      let requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequest ["data"])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      lookup "Access-Control-Allow-Methods" (Wai.responseHeaders response) `shouldBe` Just "GET"
      lookup "Access-Control-Allow-Headers" (Wai.responseHeaders response) `shouldBe` Nothing
      lookup "Access-Control-Max-Age" (Wai.responseHeaders response) `shouldBe` Nothing

    it "reports request observability for allowed CORS preflight responses" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          requestPolicyConfig =
            defaultRequestPolicy
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://client.example.com"],
                      corsAllowedMethods = ["GET"],
                      corsAllowedHeaders = [],
                      corsMaxAgeSeconds = Nothing
                    }
              }
          preflightRequest =
            (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost [])
              { Wai.requestMethod = "OPTIONS",
                Wai.requestHeaders =
                  [ ("Origin", "https://client.example.com"),
                    ("Access-Control-Request-Method", "GET")
                  ]
              }
          applicationWithObservability =
            (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication applicationWithObservability) preflightRequest
      Wai.responseStatus response `shouldBe` Http.status204
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       "OPTIONS"
                       "http"
                       "/data"
                       "/data"
                       204
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "extracts incoming W3C trace context into request observability without changing local request attributes" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          traceContext =
            Observability.RequestTraceContext
              { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                Observability.traceContextState = Just "vendor=value"
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          tracedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("traceparent", "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"),
                ("tracestate", "vendor=value")
              ]
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) tracedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.withRequestTraceContext
                       traceContext
                       ( Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       )
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores malformed W3C traceparent headers" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          tracedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("traceparent", "00-00000000000000000000000000000000-00f067aa0ba902b7-01")]
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) tracedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/data"
                       "/data"
                       202
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores forwarded client, scheme, and prefix headers unless trust is enabled" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          forwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("X-Forwarded-For", "203.0.113.10, 10.0.0.1"),
                ("X-Forwarded-Proto", "https"),
                ("X-Forwarded-Prefix", "/app")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) forwardedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       ]

    it "reports body-response observability attributes and logs through the application hooks" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let failureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "SampleError"
              }
          proxiedRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          proxiedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              proxiedRemoteHost
              [ ("X-Forwarded-For", "203.0.113.10, 10.0.0.1"),
                ("X-Forwarded-Proto", "https"),
                ("X-Forwarded-Prefix", "/app")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.10"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "x-forwarded-for"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.10, 10.0.0.1"
              }
          forwardedProtoAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_proto",
                Observability.attributeValue = Observability.TextAttribute "https"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderResponse =
                  \_ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = 503,
                            responseContentType = "application/json",
                            responseBody = "{\"error\":\"data-unavailable\"}",
                            responseObservabilityAttributes = [failureAttribute],
                            responseLogEntries = ["Sample failure log"]
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) proxiedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 503
      readResponseBody response `shouldReturn` "{\"error\":\"data-unavailable\"}"
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "https"
                           "/data"
                           "/app/data"
                           503
                           Observability.BodyResponseKind
                           [ clientAddressAttribute,
                             peerAddressAttribute,
                             clientAddressSourceAttribute,
                             forwardedForAttribute,
                             forwardedProtoAttribute,
                             forwardedPrefixAttribute,
                             failureAttribute
                           ]
                       ]
      readIORef logEntriesReference
        `shouldReturn` [ "[client.address=\"203.0.113.10\" network.peer.address=\"127.0.0.1\" harch.client.address.source=\"x-forwarded-for\" http.request.header.x_forwarded_for=\"203.0.113.10, 10.0.0.1\" http.request.header.x_forwarded_proto=\"https\" http.request.header.x_forwarded_prefix=\"/app\" url.scheme=\"https\"] Sample failure log"
                       ]

    it "enriches request observability with safe forwarded, user-agent, referrer, and request-source attributes" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          enrichedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [ ("Forwarded", "for=\"198.51.100.7\";proto=\"https\""),
                ("User-Agent", "curl/8.7.1"),
                ("Referer", "https://client.example.com/path?secret=token#fragment"),
                ("X-Requested-With", "tiny-navigation")
              ]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.7"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "forwarded"
              }
          forwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=\"198.51.100.7\";proto=\"https\""
              }
          userAgentAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "user_agent.original",
                Observability.attributeValue = Observability.TextAttribute "curl/8.7.1"
              }
          refererAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.referer",
                Observability.attributeValue = Observability.TextAttribute "https://client.example.com/path"
              }
          requestedWithAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_requested_with",
                Observability.attributeValue = Observability.TextAttribute "tiny-navigation"
              }
          requestSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.request.source",
                Observability.attributeValue = Observability.TextAttribute "enhanced-navigation"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderResponse =
                  \_ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = 202,
                            responseContentType = "application/json",
                            responseBody = "{\"route\":\"data\"}",
                            responseObservabilityAttributes = [],
                            responseLogEntries = ["Enriched source log"]
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) enrichedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "https"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [ clientAddressAttribute,
                             peerAddressAttribute,
                             clientAddressSourceAttribute,
                             forwardedAttribute,
                             userAgentAttribute,
                             refererAttribute,
                             requestedWithAttribute,
                             requestSourceAttribute
                           ]
                       ]
      readIORef logEntriesReference
        `shouldReturn` [ "[client.address=\"198.51.100.7\" network.peer.address=\"127.0.0.1\" harch.client.address.source=\"forwarded\" http.request.header.forwarded=\"for=\\\"198.51.100.7\\\";proto=\\\"https\\\"\" user_agent.original=\"curl/8.7.1\" http.request.header.referer=\"https://client.example.com/path\" http.request.header.x_requested_with=\"tiny-navigation\" harch.request.source=\"enhanced-navigation\" url.scheme=\"https\"] Enriched source log"
                       ]

    it "parses unquoted Forwarded values and ignores empty trusted forwarded elements" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          emptyForwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", " , ")]
          emptyForwardedForRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", "for=\"\";proto=http")]
          unquotedForwardedRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("Forwarded", "for=203.0.113.8;proto=http")]
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          emptyForwardedClientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          emptyForwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute ","
              }
          emptyForwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=\"\";proto=http"
              }
          unquotedForwardedClientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "203.0.113.8"
              }
          unquotedForwardedSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "forwarded"
              }
          unquotedForwardedAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.forwarded",
                Observability.attributeValue = Observability.TextAttribute "for=203.0.113.8;proto=http"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) emptyForwardedRequest
        `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) emptyForwardedForRequest
        `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) unquotedForwardedRequest
        `shouldReturn` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [ emptyForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             emptyForwardedAttribute
                           ],
                         Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [ emptyForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             emptyForwardedForAttribute
                           ],
                         Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [ unquotedForwardedClientAddressAttribute,
                             peerAddressAttribute,
                             unquotedForwardedSourceAttribute,
                             unquotedForwardedAttribute
                           ]
                       ]

    it "classifies scripted, API, manual, and browser-like request sources" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
          requestWithSource headers =
            performWaiRequest
              (toWaiApplication diagnosticApplication)
              (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost headers)
          requestSourceValues requestObservabilityValue =
            [ sourceValue
            | Observability.ObservabilityAttribute
                { Observability.attributeName = "harch.request.source",
                  Observability.attributeValue = Observability.TextAttribute sourceValue
                } <-
                Observability.requestSpanAttributes
                  (Observability.observabilityRequestSpan requestObservabilityValue)
            ]
      _ <- requestWithSource [("X-Requested-With", "XMLHttpRequest")]
      _ <- requestWithSource [("X-Requested-With", "custom-script")]
      _ <- requestWithSource [("Accept", "application/json")]
      _ <- requestWithSource [("User-Agent", "curl/8.7.1")]
      _ <- requestWithSource [("User-Agent", "Mozilla/5.0")]
      _ <- requestWithSource [("Forwarded", " , ")]
      _ <- requestWithSource [("Forwarded", "for=\"\";proto=http")]
      _ <- requestWithSource [("Forwarded", "for=203.0.113.8;proto=http")]
      fmap (map requestSourceValues) (readIORef requestObservabilityReference)
        `shouldReturn` [ ["xml-http-request"],
                         ["scripted-request"],
                         ["api-client"],
                         ["manual-client"],
                         ["browser-or-client"],
                         [],
                         [],
                         []
                       ]

    it "falls back to the direct peer address and request security when forwarding headers are absent" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          directRequest =
            (waiRequestWithRemoteHostAndHeaders ["data"] directRemoteHost [])
              { Wai.isSecure = True
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { renderResponse =
                  \_ ->
                    pure $
                      BodyResponse
                        ResponseBody
                          { responseStatus = 202,
                            responseContentType = "application/json",
                            responseBody = "{\"route\":\"data\"}",
                            responseObservabilityAttributes = [],
                            responseLogEntries = ["Direct peer log"]
                          },
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) directRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "https"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       ]
      readIORef logEntriesReference
        `shouldReturn` ["[client.address=\"127.0.0.1\" network.peer.address=\"127.0.0.1\" url.scheme=\"https\"] Direct peer log"]

    it "preserves page response semantics while surfacing page-level failure status, observability, and logs" $ do
      requestObservabilityReference <- newIORef []
      logEntriesReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          pageRequest =
            waiRequestWithRemoteHostAndHeaders ["known"] directRemoteHost []
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          failureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "SampleError"
              }
          diagnosticApplication =
            sampleApplication
              { renderResponse =
                  pure
                    . PageResponseWithMetadata
                      ResponseBody
                        { responseStatus = 500,
                          responseContentType = "text/html; charset=utf-8",
                          responseBody = "",
                          responseObservabilityAttributes = [failureAttribute],
                          responseLogEntries = ["Sample page failure log"]
                        }
                    . samplePage,
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) pageRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 500
      Http.statusMessage (Wai.responseStatus response) `shouldBe` ""
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/html; charset=utf-8"
      readResponseBody response `shouldReturn` renderDocument (pageShell diagnosticApplication (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})))
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/known"
                           "/known"
                           500
                           Observability.PageResponseKind
                           [clientAddressAttribute, peerAddressAttribute, failureAttribute]
                       ]
      readIORef logEntriesReference
        `shouldReturn` ["[client.address=\"127.0.0.1\" network.peer.address=\"127.0.0.1\" url.scheme=\"http\"] Sample page failure log"]

    it "retains measured request timing across page and body response variants" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedPrefixAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                Observability.attributeValue = Observability.TextAttribute "/app"
              }
          pageFailureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "PageFailure"
              }
          bodyFailureAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "exception.type",
                Observability.attributeValue = Observability.TextAttribute "BodyFailure"
              }
          diagnosticApplication =
            sampleApplication
              { applicationRequestPolicy = defaultRequestPolicy {trustForwardedHeaders = True},
                requestContextFromRequest = sampleRequestContextFromRequest True,
                renderResponse =
                  \request ->
                    pure $
                      case (requestRoute request, requestLanguage (requestContext request), requestPathPrefix (requestContext request)) of
                        (KnownRoute, "es", _) ->
                          PageResponseWithMetadata
                            ResponseBody
                              { responseStatus = 500,
                                responseContentType = "text/html; charset=utf-8",
                                responseBody = "",
                                responseObservabilityAttributes = [pageFailureAttribute],
                                responseLogEntries = []
                              }
                            (samplePage request)
                        (KnownRoute, _, _) ->
                          PageResponse (samplePage request)
                        (DataRoute, _, "/app") ->
                          BodyResponse
                            ResponseBody
                              { responseStatus = 503,
                                responseContentType = "application/json",
                                responseBody = "{\"error\":\"body-failure\"}",
                                responseObservabilityAttributes = [bodyFailureAttribute],
                                responseLogEntries = []
                              }
                        _ ->
                          renderSampleResponse request,
                reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
          pageSuccessRequest =
            waiRequestWithRemoteHostAndHeaders
              ["known"]
              directRemoteHost
              []
          pageFailureRequest =
            waiRequestWithRemoteHostAndHeaders
              ["es", "known"]
              directRemoteHost
              []
          bodySuccessRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              []
          bodyFailureRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              directRemoteHost
              [("X-Forwarded-Prefix", "/app")]
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) pageSuccessRequest
        `shouldReturn` 200
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) pageFailureRequest
        `shouldReturn` 500
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) bodySuccessRequest
        `shouldReturn` 202
      Http.statusCode . Wai.responseStatus
        <$> performWaiRequest (toWaiApplication diagnosticApplication) bodyFailureRequest
        `shouldReturn` 503
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/known"
                       "/known"
                       200
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute],
                     Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/es/known"
                       "/es/known"
                       500
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute, pageFailureAttribute],
                     Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/data"
                       "/data"
                       202
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute],
                     Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/data"
                       "/app/data"
                       503
                       Observability.BodyResponseKind
                       [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute, bodyFailureAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "ignores empty forwarded-for tokens while still honoring forwarded plain-http scheme" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          forwardedRequest =
            ( waiRequestWithRemoteHostAndHeaders
                ["data"]
                directRemoteHost
                [ ("X-Forwarded-For", " , "),
                  ("X-Forwarded-Proto", "http")
                ]
            )
              { Wai.isSecure = True
              }
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute ","
              }
          forwardedProtoAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_proto",
                Observability.attributeValue = Observability.TextAttribute "http"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) forwardedRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [ clientAddressAttribute,
                             peerAddressAttribute,
                             forwardedForAttribute,
                             forwardedProtoAttribute
                           ]
                       ]

    it "renders non-inet peer addresses into forwarded diagnostics" $ do
      requestObservabilityReference <- newIORef []
      let unixSocketRequest =
            waiRequestWithRemoteHostAndHeaders
              ["data"]
              (Socket.SockAddrUnix "/tmp/harch-web.sock")
              [("X-Forwarded-For", "198.51.100.24")]
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.24"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "/tmp/harch-web.sock"
              }
          forwardedForAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "http.request.header.x_forwarded_for",
                Observability.attributeValue = Observability.TextAttribute "198.51.100.24"
              }
          clientAddressSourceAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "harch.client.address.source",
                Observability.attributeValue = Observability.TextAttribute "x-forwarded-for"
              }
          diagnosticApplication =
            trustedForwardedApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [stripVolatileRequestTiming requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) unixSocketRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/data"
                           "/data"
                           202
                           Observability.BodyResponseKind
                           [clientAddressAttribute, peerAddressAttribute, clientAddressSourceAttribute, forwardedForAttribute]
                       ]

    it "groups unmatched requests under a stable not-found span display name while keeping the concrete missing path in attributes" $ do
      requestObservabilityReference <- newIORef []
      let directRemoteHost =
            Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
          missingRequest =
            waiRequestWithRemoteHostAndHeaders
              ["favicon.ico"]
              directRemoteHost
              []
          clientAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "client.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          peerAddressAttribute =
            Observability.ObservabilityAttribute
              { Observability.attributeName = "network.peer.address",
                Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
              }
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) missingRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 404
      capturedRequestObservability <- readIORef requestObservabilityReference
      map stripVolatileRequestTiming capturedRequestObservability
        `shouldBe` [ Observability.buildRequestObservability
                       "GET"
                       "http"
                       "/favicon.ico"
                       "/404"
                       404
                       Observability.PageResponseKind
                       [clientAddressAttribute, peerAddressAttribute]
                   ]
      mapM_ expectMeasuredRequestTiming capturedRequestObservability

    it "reports request observability for root-prefixed static asset responses with a wildcard route" $
      withSystemTempDirectory "harch-web-static-observability-root" $ \tempDirectory -> do
        requestObservabilityReference <- newIORef []
        let directRemoteHost =
              Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
            assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            clientAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "client.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            peerAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            staticApplication =
              (sampleApplicationWithStaticAssets assetConfig)
                { reportRequestObservability = \requestObservabilityValue ->
                    modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                }
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/styles.css") "body{}"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequestWithRemoteHostAndHeaders ["styles.css"] directRemoteHost [])
        Wai.responseStatus response `shouldBe` Http.status200
        capturedRequestObservability <- readIORef requestObservabilityReference
        map stripVolatileRequestTiming capturedRequestObservability
          `shouldBe` [ Observability.buildRequestObservability
                         "GET"
                         "http"
                         "/styles.css"
                         "/*"
                         200
                         Observability.BodyResponseKind
                         [clientAddressAttribute, peerAddressAttribute]
                     ]
        mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "reports request observability for matched static asset misses with the prefixed wildcard route" $
      withSystemTempDirectory "harch-web-static-observability-missing" $ \tempDirectory -> do
        requestObservabilityReference <- newIORef []
        let directRemoteHost =
              Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1))
            assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            forwardedPrefixAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                  Observability.attributeValue = Observability.TextAttribute "/app"
                }
            clientAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "client.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            peerAddressAttribute =
              Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            missingRequest =
              waiRequestWithRemoteHostAndHeaders
                ["app", "assets", "missing.js"]
                directRemoteHost
                [("X-Forwarded-Prefix", "/app")]
            staticApplication =
              (sampleApplicationWithConfig assetConfig (defaultRequestPolicy {trustForwardedHeaders = True}))
                { reportRequestObservability = \requestObservabilityValue ->
                    modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                }
        createDirectoryIfMissing True assetDirectory
        response <- performWaiRequest (toWaiApplication staticApplication) missingRequest
        Wai.responseStatus response `shouldBe` Http.status404
        capturedRequestObservability <- readIORef requestObservabilityReference
        map stripVolatileRequestTiming capturedRequestObservability
          `shouldBe` [ Observability.buildRequestObservability
                         "GET"
                         "http"
                         "/assets/missing.js"
                         "/app/assets/*"
                         404
                         Observability.BodyResponseKind
                         [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                     ]
        mapM_ expectMeasuredRootRequestTiming capturedRequestObservability

    it "serves configured static assets with deterministic cache-control headers" $
      withSystemTempDirectory "harch-web-static" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
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

    it "applies HSTS headers to static asset responses when the effective request scheme is HTTPS" $
      withSystemTempDirectory "harch-web-static-hsts" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            requestPolicyConfig =
              RequestPolicyConfig
                { redirectHttpToHttps = False,
                  httpsRedirectPort = Nothing,
                  strictTransportSecurity =
                    Just
                      StrictTransportSecurityConfig
                        { strictTransportSecurityMaxAgeSeconds = 86400,
                          strictTransportSecurityIncludeSubDomains = False,
                          strictTransportSecurityPreload = False
                        },
                  trustForwardedHeaders = True,
                  corsPolicy = defaultCorsPolicyConfig,
                  responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                }
            staticApplication = sampleApplicationWithConfig assetConfig requestPolicyConfig
            proxiedHttpsRequest =
              waiRequestWithRemoteHostAndHeaders
                ["assets", "app.js"]
                (Socket.SockAddrInet 4123 (Socket.tupleToHostAddress (127, 0, 0, 1)))
                [("X-Forwarded-Proto", "https")]
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        response <- performWaiRequest (toWaiApplication staticApplication) proxiedHttpsRequest
        Wai.responseStatus response `shouldBe` Http.status200
        lookup "Strict-Transport-Security" (Wai.responseHeaders response)
          `shouldBe` Just "max-age=86400"
        readResponseBody response `shouldReturn` "console.log('asset');"

    it "strips forwarded path prefixes before serving static assets" $
      withSystemTempDirectory "harch-web-static-prefix" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            prefixedRequest =
              Wai.defaultRequest
                { Wai.rawPathInfo = "/app/assets/app.js",
                  Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
                }
            staticApplication =
              sampleApplicationWithConfig
                assetConfig
                defaultRequestPolicy
                  { trustForwardedHeaders = True
                  }
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/app.js") "console.log('asset');"
        response <- performWaiRequest (toWaiApplication staticApplication) prefixedRequest
        Wai.responseStatus response `shouldBe` Http.status200
        readResponseBody response `shouldReturn` "console.log('asset');"

    it "serves root-prefixed static assets with the expected content types and no cache header" $
      withSystemTempDirectory "harch-web-static-root" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            expectedResponses =
              [ (["styles.css"], "body{}", "text/css; charset=utf-8"),
                (["index.html"], "<h1>Home</h1>", "text/html; charset=utf-8"),
                (["data.json"], "{\"ok\":true}", "application/json; charset=utf-8"),
                (["logo.svg"], "<svg></svg>", "image/svg+xml"),
                (["note.txt"], "hello", "text/plain; charset=utf-8")
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
              lookup Http.hContentType (Wai.responseHeaders response)
                `shouldBe` Just (TextEncoding.encodeUtf8 expectedContentType)
              lookup Http.hCacheControl (Wai.responseHeaders response) `shouldBe` Nothing
              readResponseBody response `shouldReturn` expectedBody
          )
          expectedResponses
        rootResponse <- performWaiRequest (toWaiApplication staticApplication) Wai.defaultRequest
        Wai.responseStatus rootResponse `shouldBe` Http.status404
        lookup Http.hContentType (Wai.responseHeaders rootResponse)
          `shouldBe` Just (TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
        lookup Http.hCacheControl (Wai.responseHeaders rootResponse) `shouldBe` Nothing
        readResponseBody rootResponse `shouldReturn` "Not Found"
        unsupportedExtensionResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["blob.bin"])
        Wai.responseStatus unsupportedExtensionResponse `shouldBe` Http.status404
        readResponseBody unsupportedExtensionResponse `shouldReturn` "Not Found"

    it "serves configured extensionless static assets when the empty extension is explicitly allowlisted" $
      withSystemTempDirectory "harch-web-static-extensionless" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes <> [("", "application/octet-stream")],
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True assetDirectory
        writeFile (assetDirectory <> "/download") "raw"
        response <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "download"])
        Wai.responseStatus response `shouldBe` Http.status200
        lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/octet-stream"
        readResponseBody response `shouldReturn` "raw"

    it "serves visible nested static assets but rejects hidden files and hidden directories" $
      withSystemTempDirectory "harch-web-static-hidden" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Nothing
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
        createDirectoryIfMissing True (assetDirectory <> "/scripts")
        createDirectoryIfMissing True (assetDirectory <> "/.hidden")
        writeFile (assetDirectory <> "/scripts/app.js") "console.log('nested');"
        writeFile (assetDirectory <> "/.env") "SECRET=true"
        writeFile (assetDirectory <> "/.hidden/app.js") "console.log('hidden');"
        nestedResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "scripts", "app.js"])
        Wai.responseStatus nestedResponse `shouldBe` Http.status200
        readResponseBody nestedResponse `shouldReturn` "console.log('nested');"
        hiddenFileResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", ".env"])
        Wai.responseStatus hiddenFileResponse `shouldBe` Http.status404
        readResponseBody hiddenFileResponse `shouldReturn` "Not Found"
        hiddenDirectoryResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", ".hidden", "app.js"])
        Wai.responseStatus hiddenDirectoryResponse `shouldBe` Http.status404
        readResponseBody hiddenDirectoryResponse `shouldReturn` "Not Found"

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
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
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
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
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
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
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
        unsupportedExtensionResponse <- performWaiRequest (toWaiApplication staticApplication) (waiRequest ["assets", "secret.bin"])
        lookup Http.hCacheControl (Wai.responseHeaders unsupportedExtensionResponse) `shouldBe` expectedCacheControl

  describe "planServerStartup" $ do
    it "groups HTTP listeners into the expected bind plan" $ do
      let firstEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          secondEndpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5002}
          firstListener = ListenerConfig {listenerHost = endpointHost firstEndpoint, listenerPort = endpointPort firstEndpoint, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}
          secondListener = ListenerConfig {listenerHost = endpointHost secondEndpoint, listenerPort = endpointPort secondEndpoint, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}
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
                listenerTls = Just (TlsConfig {certificateSource = certificateSource}),
                listenerAcme = Nothing
              }
          manualPlan =
            ManualTlsBindPlan
              { tlsEndpoint = endpoint,
                tlsCertificateFile = "cert.pem",
                tlsPrivateKeyFile = "key.pem",
                tlsCredentialSourceKind = ManualTlsCredentials,
                tlsStartupMode = RequireCertificateFiles
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
      show manualPlan `shouldBe` "ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\", tlsCredentialSourceKind = ManualTlsCredentials, tlsStartupMode = RequireCertificateFiles}"
      show [manualPlan] `shouldBe` "[ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5443}, tlsCertificateFile = \"cert.pem\", tlsPrivateKeyFile = \"key.pem\", tlsCredentialSourceKind = ManualTlsCredentials, tlsStartupMode = RequireCertificateFiles}]"

    it "translates shared certificate directories into TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5444}
          certificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/harch-web/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource}),
                listenerAcme = Nothing
              }
          manualPlan =
            ManualTlsBindPlan
              { tlsEndpoint = endpoint,
                tlsCertificateFile = "/var/lib/harch-web/shared-certs/fullchain.pem",
                tlsPrivateKeyFile = "/var/lib/harch-web/shared-certs/privkey.pem",
                tlsCredentialSourceKind = SharedTlsCredentials,
                tlsStartupMode = AwaitCertificateFiles Nothing
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans = [manualPlan],
              acmeBindPlans = []
            }
      manualPlan `shouldBe` manualPlan
      manualPlan `shouldNotBe` manualPlan {tlsPrivateKeyFile = "other-privkey.pem"}
      show manualPlan `shouldBe` "ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, tlsCertificateFile = \"/var/lib/harch-web/shared-certs/fullchain.pem\", tlsPrivateKeyFile = \"/var/lib/harch-web/shared-certs/privkey.pem\", tlsCredentialSourceKind = SharedTlsCredentials, tlsStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show [manualPlan] `shouldBe` "[ManualTlsBindPlan {tlsEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, tlsCertificateFile = \"/var/lib/harch-web/shared-certs/fullchain.pem\", tlsPrivateKeyFile = \"/var/lib/harch-web/shared-certs/privkey.pem\", tlsCredentialSourceKind = SharedTlsCredentials, tlsStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}]"

    it "translates fail-fast shared certificate directories into immediate TLS startup parameters" $ do
      let endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5445}
          certificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/harch-web/preprovisioned-certs",
                sharedCertificateStartupMode = RequireCertificateFiles
              }
          listener =
            ListenerConfig
              { listenerHost = endpointHost endpoint,
                listenerPort = endpointPort endpoint,
                listenerScheme = Https,
                listenerTls = Just (TlsConfig {certificateSource = certificateSource}),
                listenerAcme = Nothing
              }
      planServerStartup (serverConfigWithListeners [listener])
        `shouldBe` Right
          ServerStartupPlan
            { httpBindPlan = HttpBindPlan {httpEndpoints = []},
              manualTlsBindPlans =
                [ ManualTlsBindPlan
                    { tlsEndpoint = endpoint,
                      tlsCertificateFile = "/var/lib/harch-web/preprovisioned-certs/fullchain.pem",
                      tlsPrivateKeyFile = "/var/lib/harch-web/preprovisioned-certs/privkey.pem",
                      tlsCredentialSourceKind = SharedTlsCredentials,
                      tlsStartupMode = RequireCertificateFiles
                    }
                ],
              acmeBindPlans = []
            }

    it "translates ACME-backed HTTPS listeners into certificate-management plans" $ do
      let httpEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = 5001}
          endpoint = ListenerEndpoint {endpointHost = "0.0.0.0", endpointPort = 5444}
          httpListener =
            ListenerConfig
              { listenerHost = endpointHost httpEndpoint,
                listenerPort = endpointPort httpEndpoint,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig =
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
                listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource acmeConfig}),
                listenerAcme = Nothing
              }
          acmePlan =
            AcmeBindPlan
              { acmeEndpoint = endpoint,
                acmeTlsEndpoint = Just endpoint,
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
      show acmePlan `shouldBe` "AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeTlsEndpoint = Just (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}), acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}}"
      show [acmePlan] `shouldBe` "[AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeTlsEndpoint = Just (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}), acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}}]"

    it "rejects listeners whose TLS mode does not match their scheme" $ do
      let httpTlsListener =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Just (TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}),
                listenerAcme = Nothing
              }
          httpsWithoutTls =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Nothing,
                listenerAcme = Nothing
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

    it "rejects ACME producer config attached to HTTPS listeners" $ do
      let httpsAcmeProducerListener =
            ListenerConfig
              { listenerHost = "127.0.0.1",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Nothing,
                listenerAcme =
                  Just
                    AcmeConfig
                      { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                        acmeContactEmails = ["ops@example.com"],
                        acmeDomains = ["example.com"],
                        acmeHttp01Port = 80,
                        acmeCertificateDirectory = Just ".tls/example.com",
                        acmeCertbotConfig = certbotHttp01Backend []
                      }
              }
      planServerStartup (serverConfigWithListeners [httpsAcmeProducerListener])
        `shouldBe` Left (InvalidListenerAcmeConfiguration httpsAcmeProducerListener)

    it "rejects invalid mixed listener configurations before startup" $ do
      let httpListener =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
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
                      },
                listenerAcme = Nothing
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

  describe "reloadTlsCredentialsIfChanged" $ do
    it "fails explicitly when initial TLS files exist but do not load as credentials" $
      withSystemTempDirectory "harch-web-reloading-tls" $ \tempDirectory -> do
        let certificatePath = tempDirectory </> "fullchain.pem"
            privateKeyPath = tempDirectory </> "privkey.pem"
        writeFile certificatePath "not a certificate"
        writeFile privateKeyPath "not a private key"
        startupResult <- try (loadReloadingTlsCredentials certificatePath privateKeyPath)
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException `shouldSatisfy` ("Failed to load manual TLS credentials from " `isInfixOf`)
            renderedException `shouldSatisfy` (certificatePath `isInfixOf`)
            renderedException `shouldSatisfy` (privateKeyPath `isInfixOf`)
          Right _ ->
            expectationFailure "Expected invalid TLS credentials to fail during initial load"

    it "reloads rewritten TLS files and keeps the last valid credentials across missing or invalid updates" $
      withSystemTempDirectory "harch-web-reloading-tls" $ \tempDirectory -> do
        let certificatePath = tempDirectory </> "fullchain.pem"
            privateKeyPath = tempDirectory </> "privkey.pem"
        writeFile certificatePath manualTlsCertificatePem
        writeFile privateKeyPath manualTlsPrivateKeyPem
        reloadingTlsCredentials <- loadReloadingTlsCredentials certificatePath privateKeyPath
        initialCredentials <- show <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
        threadDelay 100000
        writeFile certificatePath manualTlsCertificatePem
        writeFile privateKeyPath manualTlsPrivateKeyPem
        show <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
          `shouldReturn` initialCredentials
        threadDelay 100000
        writeFile certificatePath "not a certificate"
        writeFile privateKeyPath "not a private key"
        show <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
          `shouldReturn` initialCredentials
        removePathForcibly certificatePath
        removePathForcibly privateKeyPath
        show <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
          `shouldReturn` initialCredentials

  describe "loadTlsCredentialSnapshotOrThrowWithLoader" $ do
    it "fails explicitly when the TLS credential files disappear during startup loading" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                "Manual TLS"
                certificatePath
                privateKeyPath
                (pure Nothing)
            )
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException
              `shouldBe` ("user error (Failed to load manual TLS credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": credential files disappeared while loading)")
          Right _ ->
            expectationFailure "Expected disappearing TLS credential files to fail during startup loading"

    it "fails explicitly when the TLS loader returns a startup credential error" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                "Manual TLS"
                certificatePath
                privateKeyPath
                (pure (Just (Left "synthetic TLS credential error")))
            )
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException
              `shouldBe` ("user error (Failed to load manual TLS credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": synthetic TLS credential error)")
          Right _ ->
            expectationFailure "Expected startup TLS credential errors to surface explicitly"

    it "handles an empty TLS label when surfacing startup credential errors" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                ""
                certificatePath
                privateKeyPath
                (pure (Just (Left "synthetic TLS credential error")))
            )
        case startupResult of
          Left exception ->
            show (exception :: IOError)
              `shouldBe` ("user error (Failed to load  credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": synthetic TLS credential error)")
          Right _ ->
            expectationFailure "Expected empty-label startup TLS credential errors to surface explicitly"

  describe "startWarpRuntimeServerOnSocket" $ do
    it "surfaces startup exceptions that happen before the runtime server becomes ready" $
      startWarpRuntimeServerOnSocket (\_ -> ioError (userError "synthetic runtime startup failure"))
        `shouldThrow` (\exception -> show (exception :: IOError) == "user error (synthetic runtime startup failure)")

  describe "startManualTlsRuntimeServerWithStarter" $ do
    it "closes the listener socket when TLS startup throws before the server thread starts" $
      withUnusedLoopbackPort $ \httpsPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath -> do
          let manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = ManualTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles
                  }
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> ioError (userError "synthetic tls startup failure"))
            manualTlsPlan
            (toWaiApplication sampleApplication)
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (synthetic tls startup failure)")
          reboundSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
          Socket.bind reboundSocket (Socket.SockAddrInet (fromIntegral httpsPort) (Socket.tupleToHostAddress (127, 0, 0, 1)))
          Socket.close reboundSocket

    it "fails explicitly when shared TLS is configured to fail fast and the certificate files are missing" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-fail-fast" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = RequireCertificateFiles
                  }
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            (toWaiApplication sampleApplication)
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Shared TLS certificate file does not exist: " <> certificatePath <> ")")

    it "fails explicitly when shared TLS wait mode reaches its configured timeout" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 0)
                  }
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            (toWaiApplication sampleApplication)
            (const (pure ()))
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Timed out waiting for shared TLS certificate files at " <> certificatePath <> " and " <> privateKeyPath <> " after 0 seconds)")

    it "includes shared TLS loader errors when wait mode times out on invalid certificate files" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-invalid-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 0)
                  }
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            (toWaiApplication sampleApplication)
            (const (pure ()))
            `shouldThrow` ( \exception ->
                              let renderedException = show (exception :: IOError)
                               in length renderedException `seq`
                                    ( "user error (Timed out waiting for shared TLS credentials at " `isPrefixOf` renderedException
                                        && certificatePath `isInfixOf` renderedException
                                        && privateKeyPath `isInfixOf` renderedException
                                        && " after 0 seconds: " `isInfixOf` renderedException
                                    )
                          )

    it "keeps retrying shared TLS wait mode until a nonzero timeout expires" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-retrying-timeout" $ \sharedDirectory -> do
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles (Just 1)
                  }
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          startManualTlsRuntimeServerWithStarter
            (\_ _ _ _ _ -> expectationFailure "unexpected TLS starter invocation" >> pure undefined)
            manualTlsPlan
            (toWaiApplication sampleApplication)
            (const (pure ()))
            `shouldThrow` ( \exception ->
                              let renderedException = show (exception :: IOError)
                               in length renderedException `seq`
                                    ( "user error (Timed out waiting for shared TLS credentials at " `isPrefixOf` renderedException
                                        && certificatePath `isInfixOf` renderedException
                                        && privateKeyPath `isInfixOf` renderedException
                                        && " after 1 seconds: " `isInfixOf` renderedException
                                    )
                          )

    it "waits for shared TLS certificate files before invoking the TLS starter" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-wait-starter" $ \sharedDirectory -> do
          starterInvoked <- newIORef False
          let (certificatePath, privateKeyPath) = (sharedDirectory </> "fullchain.pem", sharedDirectory </> "privkey.pem")
              manualTlsPlan =
                ManualTlsBindPlan
                  { tlsEndpoint = ListenerEndpoint {endpointHost = "127.0.0.1", endpointPort = httpsPort},
                    tlsCertificateFile = certificatePath,
                    tlsPrivateKeyFile = privateKeyPath,
                    tlsCredentialSourceKind = SharedTlsCredentials,
                    tlsStartupMode = AwaitCertificateFiles Nothing
                  }
          _ <- forkIO $ do
            threadDelay 100000
            writeFile certificatePath manualTlsCertificatePem
            writeFile privateKeyPath manualTlsPrivateKeyPem
          _ <-
            startManualTlsRuntimeServerWithStarter
              (\_ _ socket _ _ -> writeIORef starterInvoked True >> Socket.close socket >> forkIO (pure ()))
              manualTlsPlan
              (toWaiApplication sampleApplication)
              (const (pure ()))
          readIORef starterInvoked `shouldReturn` True

  describe "planObservabilityStartup" $ do
    it "produces no exporter startup actions when tracing and metrics are disabled" $
      planObservabilityStartup ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
        `shouldBe` ObservabilityStartupPlan {startupExporters = []}

    it "translates OTLP tracing and metrics exporters into deterministic startup parameters" $
      planObservabilityStartup
        ObservabilityConfig
          { tracingExporter =
              Just
                OtlpExporter
                  { otlpEndpoint = "http://collector:4318/v1/traces",
                    otlpHeaders = [("authorization", "Bearer tracing")]
                  },
            metricsExporter =
              Just
                OtlpExporter
                  { otlpEndpoint = "http://collector:4318/v1/metrics",
                    otlpHeaders = [("x-scope", "metrics")]
                  }
          }
        `shouldBe` ObservabilityStartupPlan
          { startupExporters =
              [ OtlpExporterStartup
                  { startupSignal = TracingSignal,
                    startupEndpoint = "http://collector:4318/v1/traces",
                    startupHeaders = [("authorization", "Bearer tracing")]
                  },
                OtlpExporterStartup
                  { startupSignal = MetricsSignal,
                    startupEndpoint = "http://collector:4318/v1/metrics",
                    startupHeaders = [("x-scope", "metrics")]
                  }
              ]
          }

  describe "exportRequestObservabilityToOtlp" $ do
    it "posts OTLP trace payloads with request attributes, resource attributes, and custom headers" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = [("authorization", "Bearer sample-token")]
            }
          ( Observability.buildRequestObservability
              "GET"
              "https"
              "/known"
              "/known"
              503
              Observability.PageResponseKind
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "ExampleFailure"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.system",
                    Observability.attributeValue = Observability.TextAttribute "postgresql"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.name",
                    Observability.attributeValue = Observability.TextAttribute "load-second-page-summary"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.query.template",
                    Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.start_monotonic_ns",
                    Observability.attributeValue = Observability.IntAttribute 3000000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 1250000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.system",
                    Observability.attributeValue = Observability.TextAttribute "postgresql"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.name",
                    Observability.attributeValue = Observability.TextAttribute "load-home-page-summary"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.query.template",
                    Observability.attributeValue = Observability.TextAttribute "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.start_monotonic_ns",
                    Observability.attributeValue = Observability.IntAttribute (-1)
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute (-1)
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.system",
                    Observability.attributeValue = Observability.TextAttribute "postgresql"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.name",
                    Observability.attributeValue = Observability.TextAttribute "load-health-check"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.query.template",
                    Observability.attributeValue = Observability.TextAttribute "SELECT 1;"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.start_monotonic_ns",
                    Observability.attributeValue = Observability.IntAttribute 1000000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 5000000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.request-policy.start_offset_ns",
                    Observability.attributeValue = Observability.IntAttribute 0
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.request-policy.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 250000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.route-match.start_offset_ns",
                    Observability.attributeValue = Observability.IntAttribute 500000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.route-match.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 750000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.render-response.start_offset_ns",
                    Observability.attributeValue = Observability.IntAttribute 1500000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.phase.render-response.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 3000000
                  }
              ]
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup Http.hContentType requestHeaders `shouldBe` Just "application/json"
        lookup "authorization" requestHeaders `shouldBe` Just "Bearer sample-token"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"service.name\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"sample-app\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"telemetry.sdk.language\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /known\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_CLIENT\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"parentSpanId\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb request policy\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb route match\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"HarchWeb render response\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-second-page-summary\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-home-page-summary\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB load-health-check\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"harch.span.phase\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"request-policy\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"route-match\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"render-response\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"exception.type\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"db.operation.name\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"db.query.template\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.start_monotonic_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.duration_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.request-policy.start_offset_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.request-policy.duration_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.route-match.start_offset_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.route-match.duration_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.render-response.start_offset_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.phase.render-response.duration_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.start_offset_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.duration_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"db.operation.start_monotonic_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"db.operation.duration_ns\""
        Text.count "\"name\":\"GET /known\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"HarchWeb request policy\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"HarchWeb route match\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"HarchWeb render response\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"DB load-second-page-summary\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"DB load-home-page-summary\"" requestBodyText `shouldBe` 1
        Text.count "\"name\":\"DB load-health-check\"" requestBodyText `shouldBe` 1
        Text.count "\"key\":\"db.system\"" requestBodyText `shouldBe` 3
        Text.count "\"key\":\"db.operation.name\"" requestBodyText `shouldBe` 3
        Text.count "\"key\":\"db.query.template\"" requestBodyText `shouldBe` 3
        Text.count "\"kind\":\"SPAN_KIND_SERVER\"" requestBodyText `shouldBe` 1
        Text.count "\"kind\":\"SPAN_KIND_INTERNAL\"" requestBodyText `shouldBe` 3
        Text.count "\"kind\":\"SPAN_KIND_CLIENT\"" requestBodyText `shouldBe` 3
        extractQuotedJsonField "traceId" requestBodyText
          `shouldSatisfy` maybe False (\traceId -> Text.length traceId == 32 && Text.all isHexDigit traceId)
        extractQuotedJsonField "spanId" requestBodyText
          `shouldSatisfy` maybe False (\spanId -> Text.length spanId == 16 && Text.all isHexDigit spanId)
        expectPlausibleEpochNanoTimestamps requestBodyText
        let startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
            durations = zipWith (-) endTimes startTimes
        case (startTimes, endTimes, durations) of
          ( [ rootStart,
              requestPolicyStart,
              routeMatchStart,
              renderResponseStart,
              secondPageDbStart,
              homePageDbStart,
              healthCheckDbStart
              ],
            [ rootEnd,
              requestPolicyEnd,
              routeMatchEnd,
              renderResponseEnd,
              secondPageDbEnd,
              homePageDbEnd,
              healthCheckDbEnd
              ],
            [ rootDuration,
              requestPolicyDuration,
              routeMatchDuration,
              renderResponseDuration,
              secondPageDbDuration,
              homePageDbDuration,
              healthCheckDbDuration
              ]
            ) -> do
              rootDuration `shouldBe` 5000000
              rootEnd - rootStart `shouldBe` rootDuration
              requestPolicyStart - rootStart `shouldBe` 0
              requestPolicyDuration `shouldBe` 250000
              requestPolicyEnd `shouldBe` requestPolicyStart + requestPolicyDuration
              routeMatchStart - rootStart `shouldBe` 500000
              routeMatchDuration `shouldBe` 750000
              routeMatchEnd `shouldBe` routeMatchStart + routeMatchDuration
              renderResponseStart - rootStart `shouldBe` 1500000
              renderResponseDuration `shouldBe` 3000000
              renderResponseEnd `shouldBe` renderResponseStart + renderResponseDuration
              secondPageDbStart - rootStart `shouldBe` 2000000
              secondPageDbDuration `shouldBe` 1250000
              secondPageDbEnd `shouldBe` secondPageDbStart + secondPageDbDuration
              homePageDbStart `shouldBe` rootStart
              homePageDbDuration `shouldBe` rootDuration
              homePageDbEnd `shouldBe` rootEnd
              healthCheckDbStart `shouldBe` rootStart
              healthCheckDbDuration `shouldBe` rootDuration
              healthCheckDbEnd `shouldBe` rootEnd
              mapM_
                (`shouldSatisfy` (<= rootEnd))
                [requestPolicyEnd, routeMatchEnd, renderResponseEnd, secondPageDbEnd, homePageDbEnd, healthCheckDbEnd]
          _ ->
            expectationFailure "expected rooted OTLP timing data for one request span, three phase spans, and three DB spans"

    it "omits runtime phase child spans when request timing has only a measured root duration" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.buildRequestObservability
              "GET"
              "http"
              "/assets/app.js"
              "/assets/*"
              200
              Observability.BodyResponseKind
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.start_monotonic_ns",
                    Observability.attributeValue = Observability.IntAttribute 1000000
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.request.duration_ns",
                    Observability.attributeValue = Observability.IntAttribute 5000000
                  }
              ]
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /assets/*\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb request policy\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb route match\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"name\":\"HarchWeb render response\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.span.phase\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.start_monotonic_ns\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"harch.request.duration_ns\""
        Text.count "\"name\":\"GET /assets/*\"" requestBodyText `shouldBe` 1

    it "reuses incoming W3C trace context for OTLP request exports" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.withRequestTraceContext
              Observability.RequestTraceContext
                { Observability.traceContextTraceId = "4bf92f3577b34da6a3ce929d0e0e4736",
                  Observability.traceContextParentSpanId = "00f067aa0ba902b7",
                  Observability.traceContextState = Just "vendor=value"
                }
              ( Observability.buildRequestObservability
                  "GET"
                  "http"
                  "/known"
                  "/known"
                  200
                  Observability.BodyResponseKind
                  [ Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.start_monotonic_ns",
                        Observability.attributeValue = Observability.IntAttribute 1000000
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "harch.request.duration_ns",
                        Observability.attributeValue = Observability.IntAttribute 5000000
                      }
                  ]
              )
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"traceId\":\"4bf92f3577b34da6a3ce929d0e0e4736\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"parentSpanId\":\"00f067aa0ba902b7\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"traceState\":\"vendor=value\""
        extractQuotedJsonField "traceId" requestBodyText `shouldBe` Just "4bf92f3577b34da6a3ce929d0e0e4736"
        Text.count "\"parentSpanId\":\"00f067aa0ba902b7\"" requestBodyText `shouldBe` 1

    it "uses an intentional fallback duration when direct request exports lack runtime timing metadata" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportRequestObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.buildRequestObservability
              "GET"
              "http"
              "/health"
              "/health"
              200
              Observability.BodyResponseKind
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.system",
                    Observability.attributeValue = Observability.TextAttribute "postgresql"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.operation.name",
                    Observability.attributeValue = Observability.TextAttribute "ping-database"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "db.query.template",
                    Observability.attributeValue = Observability.TextAttribute "SELECT 1;"
                  }
              ]
          )
        CapturedCollectorRequest {capturedCollectorBody = requestBody} <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
            startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
            durations = zipWith (-) endTimes startTimes
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /health\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"DB ping-database\""
        Text.count "\"kind\":\"SPAN_KIND_SERVER\"" requestBodyText `shouldBe` 1
        Text.count "\"kind\":\"SPAN_KIND_CLIENT\"" requestBodyText `shouldBe` 1
        startTimes `shouldSatisfy` ((== 2) . length)
        endTimes `shouldSatisfy` ((== 2) . length)
        durations `shouldBe` [2000, 2000]
        case startTimes of
          [rootStart, childStart] -> rootStart `shouldBe` childStart
          _ -> expectationFailure "expected root and child OTLP spans"

    it "fails explicitly when the collector rejects the export request" $
      withOtlpCollector Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        exportResult <-
          try
            ( exportRequestObservabilityToOtlp
                "sample-app"
                OtlpExporter
                  { otlpEndpoint = collectorUrl,
                    otlpHeaders = []
                  }
                ( Observability.buildRequestObservability
                    "GET"
                    "http"
                    "/"
                    "/"
                    200
                    Observability.BodyResponseKind
                    [ Observability.ObservabilityAttribute
                        { Observability.attributeName = "harch.request.duration_ns",
                          Observability.attributeValue = Observability.IntAttribute (-1)
                        }
                    ]
                )
            ) ::
            IO (Either IOError ())
        _ <- readMVar capturedRequestReference
        case exportResult of
          Left exportError -> do
            show exportError `shouldContain` "OTLP trace export failed with status 503"
            show exportError `shouldContain` "collector unavailable"
          Right () ->
            expectationFailure "expected OTLP export to fail when the collector returns a non-2xx status"

  describe "exportConnectionObservabilityToOtlp" $ do
    it "posts OTLP trace payloads for connection-level observability" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportConnectionObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = [("authorization", "Bearer sample-token")]
            }
          ( Observability.buildConnectionObservability
              "CONNECTION insecure-connection-denied"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "InsecureConnectionDenied"
                  }
              ]
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup Http.hContentType requestHeaders `shouldBe` Just "application/json"
        lookup "authorization" requestHeaders `shouldBe` Just "Bearer sample-token"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION insecure-connection-denied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""
        expectPlausibleEpochNanoTimestamps requestBodyText
        let startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
        startTimes `shouldSatisfy` ((== 1) . length)
        endTimes `shouldSatisfy` ((== 1) . length)
        zipWith (-) endTimes startTimes `shouldBe` [1000]

    it "posts OTLP trace payloads for prematurely closed connection observability" $
      withOtlpCollector Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        exportConnectionObservabilityToOtlp
          "sample-app"
          OtlpExporter
            { otlpEndpoint = collectorUrl,
              otlpHeaders = []
            }
          ( Observability.buildConnectionObservability
              "CONNECTION client-closed-connection-prematurely"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "ClientClosedConnectionPrematurely"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "harch.connection.event",
                    Observability.attributeValue = Observability.TextAttribute "client-closed-connection-prematurely"
                  }
              ]
          )
        CapturedCollectorRequest
          { capturedCollectorMethod = requestMethod,
            capturedCollectorPath = requestPath,
            capturedCollectorHeaders = requestHeaders,
            capturedCollectorBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 (LazyByteString.toStrict requestBody)
            startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" requestBodyText
            endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" requestBodyText
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup Http.hContentType requestHeaders `shouldBe` Just "application/json"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION client-closed-connection-prematurely\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_INTERNAL\""
        requestBodyText `shouldNotSatisfy` Text.isInfixOf "\"kind\":\"SPAN_KIND_SERVER\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"ClientClosedConnectionPrematurely\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"harch.connection.event\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"client-closed-connection-prematurely\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""
        expectPlausibleEpochNanoTimestamps requestBodyText
        startTimes `shouldSatisfy` ((== 1) . length)
        endTimes `shouldSatisfy` ((== 1) . length)
        zipWith (-) endTimes startTimes `shouldBe` [1000]

  describe "runServer" $ do
    it "serves responses on the configured HTTP listener and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          serverThreadId <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          firstResponseText <- waitForServerResponse completionReference unusedPort "/known"
          Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
          threadDelay 50000
          secondResponseText <- readLoopbackHttpResponse unusedPort "/known"
          Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForServerExit completionReference
          hClose outputHandle
          readFile outputPath `shouldReturn` ("HTTP Server listening at http://127.0.0.1:" <> show unusedPort <> "\n")

    it "stops listeners and returns normally when it receives SIGTERM" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-sigterm-output.txt" $ \_ outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          _ <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          _ <- waitForServerResponse completionReference unusedPort "/known"
          raiseSignal sigTERM
          waitForServerExit completionReference
          completionResult <- readIORef completionReference
          case completionResult of
            Just (Right ()) -> pure ()
            Just (Left exception) -> expectationFailure ("expected SIGTERM shutdown to succeed, but got: " <> displayException exception)
            Nothing -> expectationFailure "expected SIGTERM shutdown to complete"
          hClose outputHandle

    it "stops listeners and returns normally when it receives SIGINT" $
      withUnusedLoopbackPort $ \unusedPort ->
        withSystemTempFile "harch-web-sigint-output.txt" $ \_ outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = unusedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
          _ <- forkIO $ do
            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          _ <- waitForServerResponse completionReference unusedPort "/known"
          raiseSignal sigINT
          waitForServerExit completionReference
          completionResult <- readIORef completionReference
          case completionResult of
            Just (Right ()) -> pure ()
            Just (Left exception) -> expectationFailure ("expected SIGINT shutdown to succeed, but got: " <> displayException exception)
            Nothing -> expectationFailure "expected SIGINT shutdown to complete"
          hClose outputHandle

    it "fails before startup when the listener plan is invalid" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let invalidConfig =
              serverConfigWithListeners
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    }
                ]
        runServer outputHandle invalidConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Invalid listener startup plan: InvalidListenerTlsConfiguration (ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Nothing}))")

    it "serves responses on the configured manual TLS listener and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
            completionReference <- newIORef Nothing
            let manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      { certificateFile = certificatePath,
                                        privateKeyFile = privateKeyPath
                                      }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig sampleApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            firstResponseText <- waitForHttpsServerResponse completionReference unusedPort "/known"
            Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
            threadDelay 50000
            secondResponseText <- readLoopbackHttpsResponse unusedPort "/known"
            Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
            completionResult <- readIORef completionReference
            completionResult `shouldSatisfy` isNothing
            killThread serverThreadId
            waitForServerExit completionReference
            hClose outputHandle
            readFile outputPath `shouldReturn` ("HTTPS Server listening at https://127.0.0.1:" <> show unusedPort <> "\n")

    it "reports plaintext connections to an HTTPS listener as connection observability with peer addresses" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            connectionObservabilityReference <- newIORef []
            let observingApplication =
                  sampleApplication
                    { reportConnectionObservability = \connectionObservabilityValue ->
                        modifyIORef' connectionObservabilityReference (connectionObservabilityValue :)
                    }
                manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      { certificateFile = certificatePath,
                                        privateKeyFile = privateKeyPath
                                      }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig observingApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            _ <- waitForHttpsServerResponse completionReference unusedPort "/known"
            _ <- readLoopbackHttpResponseBytesWithHostResult unusedPort "127.0.0.1" "/known"
            connectionObservability <-
              waitForConnectionObservability connectionObservabilityReference "insecure-connection-denied"
            let connectionSpan = Observability.observabilityConnectionSpan connectionObservability
            Observability.requestSpanDisplayName connectionSpan `shouldBe` "CONNECTION insecure-connection-denied"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "client.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "network.peer.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "url.scheme" "https"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "harch.connection.event" "insecure-connection-denied"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "exception.type" "InsecureConnectionDenied"
            killThread serverThreadId
            waitForServerExit completionReference

    it "reports prematurely closed HTTPS listener connections as connection observability with peer addresses" $
      withUnusedLoopbackPort $ \unusedPort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            connectionObservabilityReference <- newIORef []
            let observingApplication =
                  sampleApplication
                    { reportConnectionObservability = \connectionObservabilityValue ->
                        modifyIORef' connectionObservabilityReference (connectionObservabilityValue :)
                    }
                manualTlsConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = unusedPort,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource =
                                    ManualCertificateFiles
                                      { certificateFile = certificatePath,
                                        privateKeyFile = privateKeyPath
                                      }
                                },
                          listenerAcme = Nothing
                        }
                    ]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle manualTlsConfig observingApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            _ <- waitForHttpsServerResponse completionReference unusedPort "/known"
            connectAndCloseLoopbackSocket unusedPort
            connectionObservability <-
              waitForConnectionObservability connectionObservabilityReference "client-closed-connection-prematurely"
            let connectionSpan = Observability.observabilityConnectionSpan connectionObservability
            Observability.requestSpanDisplayName connectionSpan `shouldBe` "CONNECTION client-closed-connection-prematurely"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "client.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "network.peer.address" "127.0.0.1"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "url.scheme" "https"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "harch.connection.event" "client-closed-connection-prematurely"
            Observability.requestSpanAttributes connectionSpan
              `shouldSatisfy` hasTextAttribute "exception.type" "ClientClosedConnectionPrematurely"
            killThread serverThreadId
            waitForServerExit completionReference

    it "fails explicitly when a manual TLS certificate file is missing" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        withManualTlsFiles $ \_ privateKeyPath -> do
          let manualTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    { certificateFile = "missing-cert.pem",
                                      privateKeyFile = privateKeyPath
                                    }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          runServer outputHandle manualTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Manual TLS certificate file does not exist: missing-cert.pem)")

    it "fails explicitly when a manual TLS private key file is missing" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        withManualTlsFiles $ \certificatePath _ -> do
          let manualTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    { certificateFile = certificatePath,
                                      privateKeyFile = "missing-key.pem"
                                    }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          runServer outputHandle manualTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Manual TLS private key file does not exist: missing-key.pem)")

    it "fails when manual TLS certificate contents cannot be loaded at runtime" $
      withSystemTempDirectory "harch-web-invalid-tls" $ \tempDirectory ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certificatePath = tempDirectory </> "cert.pem"
              privateKeyPath = tempDirectory </> "key.pem"
              invalidTlsConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = 5443,
                        listenerScheme = Https,
                        listenerTls =
                          Just
                            TlsConfig
                              { certificateSource =
                                  ManualCertificateFiles
                                    { certificateFile = certificatePath,
                                      privateKeyFile = privateKeyPath
                                    }
                              },
                        listenerAcme = Nothing
                      }
                  ]
          writeFile certificatePath "not a certificate"
          writeFile privateKeyPath "not a private key"
          runServer outputHandle invalidTlsConfig sampleApplication
            `shouldThrow` anyException

    it "fails explicitly when ACME runtime listeners are missing any HTTP challenge listener" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [acmeHttpsListener "127.0.0.1" 5443 (certbotHttp01Backend [])]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime listeners do not have an HTTP port 80 challenge listener" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "0.0.0.0" 5001,
                  acmeHttpsListener "0.0.0.0" 5443 (certbotHttp01Backend [])
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime challenge listeners do not match the HTTPS host" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "127.0.0.1" 80,
                  acmeHttpsListener "0.0.0.0" 5443 (certbotHttp01Backend [])
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly after resolving an exact-host ACME challenge listener without ACME domains" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" challengePort,
                    acmeHttpsListenerWithDomainsAndChallengePort challengePort "127.0.0.1" 5443 ["ops@example.com"] [] (certbotHttp01Backend [])
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains.)")

    it "starts certbot-backed ACME listeners on the configured http-01 port and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotPath ->
                withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
                  completionReference <- newIORef Nothing
                  logEntriesReference <- newIORef []
                  let acmeConfig =
                        AcmeConfig
                          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                            acmeContactEmails = ["ops@example.com"],
                            acmeDomains = ["loopback.example"],
                            acmeHttp01Port = challengePort,
                            acmeCertificateDirectory = Nothing,
                            acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                          }
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "0.0.0.0" challengePort,
                            ListenerConfig
                              { listenerHost = "127.0.0.1",
                                listenerPort = httpsPort,
                                listenerScheme = Https,
                                listenerTls =
                                  Just
                                    TlsConfig
                                      { certificateSource = AcmeCertificateSource acmeConfig
                                      },
                                listenerAcme = Nothing
                              }
                          ]
                      observingApplication =
                        sampleApplication
                          { reportApplicationLog = \logEntry ->
                              modifyIORef' logEntriesReference (<> [logEntry])
                          }
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle acmeTlsConfig observingApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  threadDelay 50000
                  secondResponseText <- readLoopbackHttpsResponse httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
                  completionResult <- readIORef completionReference
                  completionResult `shouldSatisfy` isNothing
                  killThread serverThreadId
                  waitForServerExit completionReference
                  hClose outputHandle
                  readFile outputPath
                    `shouldReturn` unlines
                      [ "HTTP Server listening at http://0.0.0.0:" <> show challengePort,
                        "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                      ]
                  readIORef logEntriesReference
                    `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:" <> Text.pack (show httpsPort),
                                     "Launching certbot for ACME listener on 127.0.0.1:" <> Text.pack (show httpsPort),
                                     "ACME certbot webroot unregistered for listener 127.0.0.1:" <> Text.pack (show httpsPort)
                                   ]

    it "lets shared HTTPS listeners reuse certificates issued by the certbot-backed ACME backend" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \sharedHttpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotPath ->
                withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
                  withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                    completionReference <- newIORef Nothing
                    logEntriesReference <- newIORef []
                    let acmeConfig =
                          AcmeConfig
                            { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                              acmeContactEmails = ["ops@example.com"],
                              acmeDomains = ["loopback.example"],
                              acmeHttp01Port = challengePort,
                              acmeCertificateDirectory = Just sharedDirectory,
                              acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                            }
                        runtimeConfig =
                          serverConfigWithListeners
                            [ ListenerConfig
                                { listenerHost = "127.0.0.1",
                                  listenerPort = challengePort,
                                  listenerScheme = Http,
                                  listenerTls = Nothing,
                                  listenerAcme = Just acmeConfig
                                },
                              sharedHttpsListener "127.0.0.1" sharedHttpsPort sharedDirectory
                            ]
                        observingApplication =
                          sampleApplication
                            { reportApplicationLog = \logEntry ->
                                modifyIORef' logEntriesReference (<> [logEntry])
                            }
                    serverThreadId <- forkIO $ do
                      result <- try (runServer outputHandle runtimeConfig observingApplication) :: IO (Either SomeException ())
                      writeIORef completionReference (Just result)
                    sharedResponseText <- waitForHttpsServerResponse completionReference sharedHttpsPort "/known"
                    Text.isInfixOf "<h1>Known</h1>" sharedResponseText `shouldBe` True
                    readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` manualTlsCertificatePem
                    readFile (sharedDirectory </> "privkey.pem") `shouldReturn` manualTlsPrivateKeyPem
                    killThread serverThreadId
                    waitForServerExit completionReference
                    readIORef logEntriesReference
                      `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:" <> Text.pack (show challengePort),
                                       "Launching certbot for ACME listener on 127.0.0.1:" <> Text.pack (show challengePort),
                                       "ACME certbot webroot unregistered for listener 127.0.0.1:" <> Text.pack (show challengePort),
                                       "Published ACME certificate files to shared directory " <> Text.pack sharedDirectory
                                     ]

    it "shuts down HTTP ACME producers that only publish certificates without starting HTTPS" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withFakeCertbotExecutable certificatePath privateKeyPath $
            \certbotPath ->
              withSystemTempDirectory "harch-web-published-certs" $ \sharedDirectory ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  completionReference <- newIORef Nothing
                  let acmeConfig =
                        AcmeConfig
                          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                            acmeContactEmails = ["ops@example.com"],
                            acmeDomains = ["loopback.example"],
                            acmeHttp01Port = challengePort,
                            acmeCertificateDirectory = Just sharedDirectory,
                            acmeCertbotConfig = certbotHttp01BackendWithExecutable certbotPath []
                          }
                      runtimeConfig =
                        serverConfigWithListeners
                          [ ListenerConfig
                              { listenerHost = "127.0.0.1",
                                listenerPort = challengePort,
                                listenerScheme = Http,
                                listenerTls = Nothing,
                                listenerAcme = Just acmeConfig
                              }
                          ]
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  responseText <- waitForServerResponse completionReference challengePort "/known"
                  Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
                  killThread serverThreadId
                  waitForServerExit completionReference

    it "fails explicitly when ACME listeners cannot launch certbot" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
          withEmptyExecutablePath $ do
            logEntriesReference <- newIORef []
            let acmeConfig =
                  AcmeConfig
                    { acmeDirectoryUrl = "http://127.0.0.1:14000/directory",
                      acmeContactEmails = ["ops@example.com"],
                      acmeDomains = ["loopback.example"],
                      acmeHttp01Port = challengePort,
                      acmeCertificateDirectory = Nothing,
                      acmeCertbotConfig = certbotHttp01Backend []
                    }
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = 5443,
                          listenerScheme = Https,
                          listenerTls =
                            Just
                              TlsConfig
                                { certificateSource = AcmeCertificateSource acmeConfig
                                },
                          listenerAcme = Nothing
                        }
                    ]
                observingApplication =
                  sampleApplication
                    { reportApplicationLog = \logEntry ->
                        modifyIORef' logEntriesReference (<> [logEntry])
                    }
            runServer outputHandle acmeTlsConfig observingApplication
              `shouldThrow` (\exception -> "user error (Failed to launch certbot for ACME listener on 127.0.0.1:5443:" `isPrefixOf` show (exception :: IOError))
            logEntries <- readIORef logEntriesReference
            case logEntries of
              [registeredLog, launchLog, failureLog, unregisterLog] -> do
                registeredLog `shouldBe` "ACME certbot webroot registered for listener 127.0.0.1:5443"
                launchLog `shouldBe` "Launching certbot for ACME listener on 127.0.0.1:5443"
                failureLog `shouldSatisfy` Text.isPrefixOf "Failed to launch certbot for ACME listener on 127.0.0.1:5443: "
                unregisterLog `shouldBe` "ACME certbot webroot unregistered for listener 127.0.0.1:5443"
              _ ->
                expectationFailure ("Expected four ACME certbot lifecycle logs, got: " <> show logEntries)

    it "fails explicitly when certbot-backed ACME listeners do not have the declared http-01 port listener" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \otherPort ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let declaredPort = challengePort
                certbotBackend =
                  certbotHttp01Backend
                    ["certonly", "--http-01-port", Text.pack (show declaredPort), "--cert-name", "loopback.example"]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" otherPort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port " <> show declaredPort <> " for http-01 challenges.)")

    it "fails explicitly when HTTP ACME producers declare a mismatched http-01 port" $
      withUnusedLoopbackPort $ \httpPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeConfig =
                AcmeConfig
                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                    acmeContactEmails = ["ops@example.com"],
                    acmeDomains = ["example.com"],
                    acmeHttp01Port = httpPort + 1,
                    acmeCertificateDirectory = Just ".tls/example.com",
                    acmeCertbotConfig = certbotHttp01Backend []
                  }
              runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = httpPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Just acmeConfig
                      }
                  ]
          runServer outputHandle runtimeConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:" <> show httpPort <> " requires the configured http-01 port to match its HTTP listener port " <> show httpPort <> ".)")

    it "fails explicitly when HTTP ACME producers do not publish a certificate directory" $
      withUnusedLoopbackPort $ \httpPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeConfig =
                AcmeConfig
                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                    acmeContactEmails = ["ops@example.com"],
                    acmeDomains = ["example.com"],
                    acmeHttp01Port = httpPort,
                    acmeCertificateDirectory = Nothing,
                    acmeCertbotConfig = certbotHttp01Backend []
                  }
              runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = httpPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Just acmeConfig
                      }
                  ]
          runServer outputHandle runtimeConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:" <> show httpPort <> " requires an ACME certificate directory so HTTPS listeners can consume published certificates.)")

    it "derives certonly plus non-interactive certbot defaults when certbot args are omitted" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "command=''",
            "has_non_interactive=0",
            "has_agree_tos=0",
            "has_webroot=0",
            "webroot_path=''",
            "http_port=''",
            "server_url=''",
            "email=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    certonly) command='certonly'; shift ;;",
            "    --non-interactive|-n) has_non_interactive=1; shift ;;",
            "    --agree-tos) has_agree_tos=1; shift ;;",
            "    --webroot) has_webroot=1; shift ;;",
            "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    --http-01-port) http_port=\"$2\"; shift 2 ;;",
            "    --server) server_url=\"$2\"; shift 2 ;;",
            "    --email|-m) email=\"$2\"; shift 2 ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$command\" = certonly",
            "test \"$has_non_interactive\" = 1",
            "test \"$has_agree_tos\" = 1",
            "test \"$has_webroot\" = 1",
            "test -n \"$webroot_path\"",
            "test -z \"$http_port\"",
            "test \"$server_url\" = https://acme-v02.api.letsencrypt.org/directory",
            "test \"$email\" = ops@example.com",
            "test \"$domain\" = example.com,www.example.com",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let certbotConfig = CertbotConfig {certbotExecutable = certbotExecutable, certbotArguments = []}
            (manualTlsBindPlan, stateDirectory) <-
              prepareCertbotManualTlsBindPlan
                (runtimeAcmePlanWithCertbotConfig certbotConfig)
                certbotConfig
            removePathForcibly stateDirectory
            manualTlsBindPlan `shouldSatisfy` (/= Nothing)

    it "does not duplicate explicit certbot command and agreement flags when already configured" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "certonly_count=0",
            "non_interactive_count=0",
            "agree_tos_count=0",
            "webroot_count=0",
            "webroot_path=''",
            "http_port=''",
            "server_url=''",
            "email=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    certonly) certonly_count=$((certonly_count + 1)); shift ;;",
            "    --non-interactive|-n) non_interactive_count=$((non_interactive_count + 1)); shift ;;",
            "    --agree-tos) agree_tos_count=$((agree_tos_count + 1)); shift ;;",
            "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
            "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    --http-01-port) http_port=\"$2\"; shift 2 ;;",
            "    --server) server_url=\"$2\"; shift 2 ;;",
            "    --email|-m) email=\"$2\"; shift 2 ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$certonly_count\" = 1",
            "test \"$non_interactive_count\" = 1",
            "test \"$agree_tos_count\" = 1",
            "test \"$webroot_count\" = 1",
            "test -n \"$webroot_path\"",
            "test \"$http_port\" = 8080",
            "test \"$server_url\" = https://acme-staging.example/directory",
            "test \"$email\" = already-set@example.com",
            "test \"$domain\" = configured.example",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let certbotConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "certonly",
                          "--non-interactive",
                          "--agree-tos",
                          "--webroot",
                          "--http-01-port",
                          "8080",
                          "--server",
                          "https://acme-staging.example/directory",
                          "--email",
                          "already-set@example.com",
                          "--domains",
                          "configured.example"
                        ]
                    }
            (manualTlsBindPlan, stateDirectory) <-
              prepareCertbotManualTlsBindPlan
                (runtimeAcmePlanWithCertbotConfig certbotConfig)
                certbotConfig
            removePathForcibly stateDirectory
            manualTlsBindPlan `shouldSatisfy` (/= Nothing)

    it "derives the webroot authenticator when only a certbot webroot path is preconfigured" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withSystemTempDirectory "harch-web-configured-webroot" $ \configuredWebrootPath ->
          withCustomFakeCertbotExecutable
            [ "#!/bin/sh",
              "set -eu",
              "config_dir=''",
              "cert_name=''",
              "domain=''",
              "webroot_count=0",
              "webroot_path=''",
              "while [ \"$#\" -gt 0 ]; do",
              "  case \"$1\" in",
              "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
              "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
              "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
              "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
              "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
              "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
              "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
              "    *) shift ;;",
              "  esac",
              "done",
              "test \"$webroot_count\" = 1",
              "test \"$webroot_path\" = " <> show configuredWebrootPath,
              "if [ -z \"$cert_name\" ]; then",
              "  cert_name=\"${domain%%,*}\"",
              "fi",
              "mkdir -p \"$config_dir/live/$cert_name\"",
              "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
              "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
            ]
            $ \certbotExecutable -> do
              let certbotConfig =
                    CertbotConfig
                      { certbotExecutable = certbotExecutable,
                        certbotArguments =
                          [ "--webroot-path",
                            Text.pack configuredWebrootPath,
                            "--cert-name",
                            "configured-webroot-cert"
                          ]
                      }
              (_, stateDirectory) <-
                prepareCertbotManualTlsBindPlan
                  (runtimeAcmePlanWithCertbotConfig certbotConfig)
                  certbotConfig
              removePathForcibly stateDirectory

    it "keeps explicit non-webroot certbot authenticators from deriving webroot flags" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withCustomFakeCertbotExecutable
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "cert_name=''",
            "domain=''",
            "webroot_count=0",
            "webroot_path_count=0",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    --webroot) webroot_count=$((webroot_count + 1)); shift ;;",
            "    -w|--webroot-path) webroot_path_count=$((webroot_path_count + 1)); shift 2 ;;",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
            "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
            "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
            "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "test \"$webroot_count\" = 0",
            "test \"$webroot_path_count\" = 0",
            "if [ -z \"$cert_name\" ]; then",
            "  cert_name=\"${domain%%,*}\"",
            "fi",
            "mkdir -p \"$config_dir/live/$cert_name\"",
            "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
            "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
          ]
          $ \certbotExecutable -> do
            let standaloneConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "--authenticator",
                          "standalone",
                          "--cert-name",
                          "standalone-cert"
                        ]
                    }
                dnsConfig =
                  CertbotConfig
                    { certbotExecutable = certbotExecutable,
                      certbotArguments =
                        [ "--authenticator",
                          "dns-route53",
                          "--cert-name",
                          "dns-cert"
                        ]
                    }
            (_, standaloneStateDirectory) <-
              prepareCertbotManualTlsBindPlan
                (runtimeAcmePlanWithCertbotConfig standaloneConfig)
                standaloneConfig
            removePathForcibly standaloneStateDirectory
            (_, dnsStateDirectory) <-
              prepareCertbotManualTlsBindPlan
                (runtimeAcmePlanWithCertbotConfig dnsConfig)
                dnsConfig
            removePathForcibly dnsStateDirectory

    it "rejects empty and path-traversal certbot challenge tokens" $ do
      validAcmeHttp01ChallengeToken "" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "nested/token" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken ".." `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "token..suffix" `shouldBe` Nothing
      validAcmeHttp01ChallengeToken "loopback-token" `shouldBe` Just "loopback-token"

    it "checks registered certbot webroots for challenge files before they exist" $
      withManualTlsFiles $ \certificatePath privateKeyPath ->
        withSystemTempDirectory "harch-web-certbot-marker" $ \markerDirectory ->
          withCustomFakeCertbotExecutable
            [ "#!/bin/sh",
              "set -eu",
              "config_dir=''",
              "cert_name=''",
              "domain=''",
              "while [ \"$#\" -gt 0 ]; do",
              "  case \"$1\" in",
              "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
              "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
              "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
              "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
              "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
              "    *) shift ;;",
              "  esac",
              "done",
              "printf '%s' 'started' > " <> show (markerDirectory </> "started"),
              "sleep 1",
              "if [ -z \"$cert_name\" ]; then",
              "  cert_name=\"${domain%%,*}\"",
              "fi",
              "mkdir -p \"$config_dir/live/$cert_name\"",
              "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
              "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
            ]
            $ \certbotExecutable -> do
              prepareResultReference <- newEmptyMVar
              let certbotConfig = CertbotConfig {certbotExecutable = certbotExecutable, certbotArguments = []}
                  markerPath = markerDirectory </> "started"
                  challengeStore = AcmeChallengeStore <$> newMVar []
                  challengeRequest =
                    Wai.defaultRequest
                      { Wai.rawPathInfo = "/.well-known/acme-challenge/loopback-token",
                        Wai.requestHeaders = [("Host", "loopback.example")]
                      }
                  waitForMarker remainingAttempts = do
                    markerExists <- doesFileExist markerPath
                    if markerExists
                      then pure ()
                      else
                        if remainingAttempts > 0
                          then threadDelay 10000 >> waitForMarker (remainingAttempts - 1)
                          else expectationFailure "expected fake certbot to start before checking the registered webroot"
              _ <- forkIO $ do
                result <-
                  try
                    ( prepareCertbotManualTlsBindPlan
                        (runtimeAcmePlanWithCertbotConfig certbotConfig)
                        certbotConfig
                    ) ::
                    IO (Either SomeException (Maybe ManualTlsBindPlan, FilePath))
                putMVar prepareResultReference result
              waitForMarker (500 :: Int)
              challengeStoreValue <- challengeStore
              challengeResponse <- acmeChallengeResponseForRequest defaultRequestPolicy challengeStoreValue challengeRequest
              isNothing challengeResponse `shouldBe` True
              prepareResult <- readMVar prepareResultReference
              case prepareResult of
                Right (_, cleanupDirectory) -> removePathForcibly cleanupDirectory
                Left exception -> expectationFailure ("expected fake certbot prepare to succeed: " <> displayException exception)

    it "serves certbot webroot challenge files from the running HTTP listener while certificate acquisition is in progress" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withCustomFakeCertbotExecutable
              [ "#!/bin/sh",
                "set -eu",
                "config_dir=''",
                "cert_name=''",
                "domain=''",
                "webroot_path=''",
                "while [ \"$#\" -gt 0 ]; do",
                "  case \"$1\" in",
                "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
                "    --cert-name) cert_name=\"$2\"; shift 2 ;;",
                "    --cert-name=*) cert_name=\"${1#--cert-name=}\"; shift ;;",
                "    -d|--domain|--domains) domain=\"$2\"; shift 2 ;;",
                "    --domains=*) domain=\"${1#--domains=}\"; shift ;;",
                "    -w|--webroot-path) webroot_path=\"$2\"; shift 2 ;;",
                "    *) shift ;;",
                "  esac",
                "done",
                "if [ -z \"$cert_name\" ]; then",
                "  cert_name=\"${domain%%,*}\"",
                "fi",
                "mkdir -p \"$webroot_path/.well-known/acme-challenge\"",
                "printf '%s' 'loopback-token-response' > \"$webroot_path/.well-known/acme-challenge/loopback-token\"",
                "sleep 1",
                "mkdir -p \"$config_dir/live/$cert_name\"",
                "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\"",
                "cp " <> show privateKeyPath <> " \"$config_dir/live/$cert_name/privkey.pem\""
              ]
              $ \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  completionReference <- newIORef Nothing
                  requestObservabilityReference <- newIORef []
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          []
                      clientAddressAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "client.address",
                            Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                          }
                      peerAddressAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "network.peer.address",
                            Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                          }
                      forwardedPrefixAttribute =
                        Observability.ObservabilityAttribute
                          { Observability.attributeName = "http.request.header.x_forwarded_prefix",
                            Observability.attributeValue = Observability.TextAttribute "/app"
                          }
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListenerWithDomainsAndChallengePort challengePort "127.0.0.1" httpsPort ["ops@example.com"] ["loopback.example", "alt.example"] certbotBackend
                          ]
                      waitForChallengeResponse remainingAttempts = do
                        completionResult <- readIORef completionReference
                        case completionResult of
                          Just (Left exception) ->
                            expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
                              >> pure ByteString.empty
                          Just (Right ()) ->
                            expectationFailure "expected runServer to remain running, but it exited early"
                              >> pure ByteString.empty
                          Nothing -> do
                            responseResult <-
                              readLoopbackHttpResponseBytesWithHostResult
                                challengePort
                                "loopback.example"
                                "/.well-known/acme-challenge/loopback-token"
                            case responseResult of
                              Right responseBytes
                                | "loopback-token-response" `ByteString.isInfixOf` responseBytes ->
                                    pure responseBytes
                              Right _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForChallengeResponse (remainingAttempts - 1)
                              Left _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForChallengeResponse (remainingAttempts - 1)
                              _ ->
                                expectationFailure "expected runServer to serve certbot webroot challenge files on the HTTP listener"
                                  >> pure ByteString.empty
                      waitForPrefixedChallengeResponse remainingAttempts = do
                        completionResult <- readIORef completionReference
                        case completionResult of
                          Just (Left exception) ->
                            expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
                              >> pure ByteString.empty
                          Just (Right ()) ->
                            expectationFailure "expected runServer to remain running, but it exited early"
                              >> pure ByteString.empty
                          Nothing -> do
                            responseResult <-
                              readLoopbackHttpResponseBytesWithHostAndHeadersResult
                                challengePort
                                "loopback.example"
                                "/app/.well-known/acme-challenge/loopback-token"
                                [("X-Forwarded-Prefix", "/app")]
                            case responseResult of
                              Right responseBytes
                                | "loopback-token-response" `ByteString.isInfixOf` responseBytes ->
                                    pure responseBytes
                              Right _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForPrefixedChallengeResponse (remainingAttempts - 1)
                              Left _
                                | remainingAttempts > 0 -> do
                                    threadDelay 10000
                                    waitForPrefixedChallengeResponse (remainingAttempts - 1)
                              _ ->
                                expectationFailure "expected runServer to serve prefixed certbot webroot challenge files on the HTTP listener"
                                  >> pure ByteString.empty
                      waitForRequestObservability expectedObservability failureMessage remainingAttempts = do
                        observedValues <- readIORef requestObservabilityReference
                        case find ((== expectedObservability) . stripVolatileRequestTiming) observedValues of
                          Just requestObservabilityValue ->
                            pure requestObservabilityValue
                          Nothing ->
                            if remainingAttempts > 0
                              then threadDelay 10000 >> waitForRequestObservability expectedObservability failureMessage (remainingAttempts - 1)
                              else expectationFailure failureMessage >> pure expectedObservability
                  serverThreadId <- forkIO $ do
                    result <-
                      try
                        ( runServer
                            outputHandle
                            acmeTlsConfig
                            sampleApplication
                              { applicationRequestPolicy = defaultRequestPolicy {trustForwardedHeaders = True},
                                requestContextFromRequest = sampleRequestContextFromRequest True,
                                reportRequestObservability = \requestObservabilityValue ->
                                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
                              }
                        ) ::
                        IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  challengeResponseBytes <- waitForChallengeResponse (500 :: Int)
                  challengeResponseBytes `shouldSatisfy` ByteString.isInfixOf "loopback-token-response"
                  prefixedChallengeResponseBytes <- waitForPrefixedChallengeResponse (500 :: Int)
                  prefixedChallengeResponseBytes `shouldSatisfy` ByteString.isInfixOf "loopback-token-response"
                  let expectedChallengeRequestObservability =
                        Observability.buildRequestObservability
                          "GET"
                          "http"
                          "/.well-known/acme-challenge/loopback-token"
                          "/.well-known/acme-challenge/*"
                          200
                          Observability.BodyResponseKind
                          [clientAddressAttribute, peerAddressAttribute]
                      expectedPrefixedChallengeRequestObservability =
                        Observability.buildRequestObservability
                          "GET"
                          "http"
                          "/.well-known/acme-challenge/loopback-token"
                          "/app/.well-known/acme-challenge/*"
                          200
                          Observability.BodyResponseKind
                          [clientAddressAttribute, peerAddressAttribute, forwardedPrefixAttribute]
                  challengeRequestObservability <-
                    waitForRequestObservability
                      expectedChallengeRequestObservability
                      "expected certbot webroot challenge response to report request observability"
                      (500 :: Int)
                  stripVolatileRequestTiming challengeRequestObservability
                    `shouldBe` expectedChallengeRequestObservability
                  expectMeasuredRootRequestTiming challengeRequestObservability
                  prefixedChallengeRequestObservability <-
                    waitForRequestObservability
                      expectedPrefixedChallengeRequestObservability
                      "expected prefixed certbot webroot challenge response to report request observability"
                      (500 :: Int)
                  stripVolatileRequestTiming prefixedChallengeRequestObservability
                    `shouldBe` expectedPrefixedChallengeRequestObservability
                  expectMeasuredRootRequestTiming prefixedChallengeRequestObservability
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  killThread serverThreadId
                  waitForServerExit completionReference

    it "starts certbot-backed ACME listeners on the declared http-01 port and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \httpsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeCertbotExecutable certificatePath privateKeyPath $
              \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
                  completionReference <- newIORef Nothing
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          ["certonly", "--http-01-port", Text.pack (show challengePort)]
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListenerWithDomains "127.0.0.1" httpsPort ["ops@example.com"] ["loopback.example", "alt.example"] certbotBackend
                          ]
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle acmeTlsConfig sampleApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  firstResponseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" firstResponseText `shouldBe` True
                  threadDelay 50000
                  secondResponseText <- readLoopbackHttpsResponse httpsPort "/known"
                  Text.isInfixOf "<h1>Known</h1>" secondResponseText `shouldBe` True
                  completionResult <- readIORef completionReference
                  completionResult `shouldSatisfy` isNothing
                  killThread serverThreadId
                  waitForServerExit completionReference
                  hClose outputHandle
                  readFile outputPath
                    `shouldReturn` unlines
                      [ "HTTP Server listening at http://127.0.0.1:" <> show challengePort,
                        "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                      ]

    it "lets other HTTPS listeners reuse ACME certificates from a shared directory" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \acmeHttpsPort ->
          withUnusedLoopbackPort $ \sharedHttpsPort ->
            withManualTlsFiles $ \certificatePath privateKeyPath ->
              withFakeCertbotExecutable certificatePath privateKeyPath $
                \certbotExecutable ->
                  withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
                    withSystemTempFile
                      "harch-web-output.txt"
                      ( \_ outputHandle -> do
                          completionReference <- newIORef Nothing
                          let certbotBackend =
                                certbotHttp01BackendWithExecutable
                                  certbotExecutable
                                  ["certonly", "--http-01-port", Text.pack (show challengePort)]
                              acmeConfig =
                                AcmeConfig
                                  { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                    acmeContactEmails = ["ops@example.com"],
                                    acmeDomains = ["loopback.example", "alt.example"],
                                    acmeHttp01Port = challengePort,
                                    acmeCertificateDirectory = Just sharedDirectory,
                                    acmeCertbotConfig = certbotBackend
                                  }
                              acmeListener =
                                ListenerConfig
                                  { listenerHost = "127.0.0.1",
                                    listenerPort = acmeHttpsPort,
                                    listenerScheme = Https,
                                    listenerTls =
                                      Just
                                        TlsConfig
                                          { certificateSource = AcmeCertificateSource acmeConfig
                                          },
                                    listenerAcme = Nothing
                                  }
                              runtimeConfig =
                                serverConfigWithListeners
                                  [ httpRuntimeListener "127.0.0.1" challengePort,
                                    acmeListener,
                                    sharedHttpsListener "127.0.0.1" sharedHttpsPort sharedDirectory
                                  ]
                          serverThreadId <- forkIO $ do
                            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
                            writeIORef completionReference (Just result)
                          acmeResponseText <- waitForHttpsServerResponse completionReference acmeHttpsPort "/known"
                          Text.isInfixOf "<h1>Known</h1>" acmeResponseText `shouldBe` True
                          sharedResponseText <- waitForHttpsServerResponse completionReference sharedHttpsPort "/known"
                          Text.isInfixOf "<h1>Known</h1>" sharedResponseText `shouldBe` True
                          readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` manualTlsCertificatePem
                          readFile (sharedDirectory </> "privkey.pem") `shouldReturn` manualTlsPrivateKeyPem
                          killThread serverThreadId
                          waitForServerExit completionReference
                      )

    it "waits for shared TLS certificate files to appear before starting the HTTPS listener" $
      withUnusedLoopbackPort $ \httpsPort ->
        withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            let runtimeConfig =
                  serverConfigWithListeners
                    [sharedHttpsListener "127.0.0.1" httpsPort sharedDirectory]
            serverThreadId <- forkIO $ do
              result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            threadDelay 100000
            readIORef completionReference >>= (`shouldSatisfy` isNothing)
            writeFile (sharedDirectory </> "fullchain.pem") manualTlsCertificatePem
            writeFile (sharedDirectory </> "privkey.pem") manualTlsPrivateKeyPem
            responseText <- waitForHttpsServerResponse completionReference httpsPort "/known"
            Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
            killThread serverThreadId
            waitForServerExit completionReference

    it "fails explicitly when certbot-backed ACME listeners do not have the default http-01 port listener" $
      withUnusedLoopbackPort $ \otherPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certbotBackend =
                certbotHttp01Backend
                  ["certonly", "--cert-name", "loopback.example"]
              acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" otherPort,
                    acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when certbot-backed ACME listeners do not declare a cert name or domain" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath privateKeyPath ->
          withFakeCertbotExecutable certificatePath privateKeyPath $
            \certbotExecutable ->
              withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                let certbotBackend =
                      certbotHttp01BackendWithExecutable
                        certbotExecutable
                        ["certonly", "--http-01-port", Text.pack (show challengePort)]
                    acmeTlsConfig =
                      serverConfigWithListeners
                        [ httpRuntimeListener "127.0.0.1" challengePort,
                          acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                        ]
                runServer outputHandle acmeTlsConfig sampleApplication
                  `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains.)")

    it "fails explicitly when certbot-backed ACME listeners propagate certbot runtime failures" $
      withUnusedLoopbackPort $ \challengePort ->
        withFailingFakeCertbotExecutable $ \certbotExecutable ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            logEntriesReference <- newIORef []
            let certbotBackend =
                  certbotHttp01BackendWithExecutable
                    certbotExecutable
                    [ "certonly",
                      "--http-01-port",
                      Text.pack (show challengePort),
                      "--cert-name",
                      "loopback.example",
                      "--email",
                      "already-set@example.com",
                      "--server",
                      "https://acme-staging.example/directory"
                    ]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
                observingApplication =
                  sampleApplication
                    { reportApplicationLog = \logEntry ->
                        modifyIORef' logEntriesReference (<> [logEntry])
                    }
            runServer outputHandle acmeTlsConfig observingApplication
              `shouldThrow` ( \exception ->
                                let rendered = show (exception :: IOError)
                                 in "user error (Certbot failed for ACME listener on 127.0.0.1:5443 with exit code ExitFailure 42.\nstdout:\n\nstderr:\nfake certbot failure\n" `isPrefixOf` rendered
                                      && "Certbot state directory was preserved for inspection: " `isInfixOf` rendered
                                      && "letsencrypt.log tail:\nfake letsencrypt detail\n" `isInfixOf` rendered
                            )
            readIORef logEntriesReference
              `shouldReturn` [ "ACME certbot webroot registered for listener 127.0.0.1:5443",
                               "Launching certbot for ACME listener on 127.0.0.1:5443",
                               "Certbot failed for ACME listener on 127.0.0.1:5443 with exit code ExitFailure 42",
                               "ACME certbot webroot unregistered for listener 127.0.0.1:5443"
                             ]

    it "keeps certbot failure diagnostics useful when certbot exits without a logfile" $
      withUnusedLoopbackPort $ \challengePort ->
        withCustomFakeCertbotExecutable ["#!/bin/sh", "echo fake certbot failure without log >&2", "exit 42"] $ \certbotExecutable ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let certbotBackend =
                  certbotHttp01BackendWithExecutable
                    certbotExecutable
                    [ "certonly",
                      "--http-01-port",
                      Text.pack (show challengePort),
                      "--cert-name",
                      "loopback.example"
                    ]
                acmeTlsConfig =
                  serverConfigWithListeners
                    [ httpRuntimeListener "127.0.0.1" challengePort,
                      acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                    ]
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` ( \exception ->
                                let rendered = show (exception :: IOError)
                                 in "fake certbot failure without log" `isInfixOf` rendered
                                      && "No certbot logfile was found at " `isInfixOf` rendered
                                      && ".\n)" `isSuffixOf` rendered
                            )

    it "fails explicitly when certbot-backed ACME listeners cannot launch the certbot executable" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let certbotBackend =
                certbotHttp01BackendWithExecutable
                  "/definitely/missing/certbot"
                  ["certonly", "--http-01-port", Text.pack (show challengePort), "--domains=loopback.example,alt.example"]
              acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" challengePort,
                    acmeHttpsListenerWithContacts "127.0.0.1" 5443 [] certbotBackend
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` ( \exception ->
                              let rendered = show (exception :: IOError)
                               in "user error (Failed to launch certbot for ACME listener on 127.0.0.1:5443:" `isPrefixOf` rendered
                                    && "/definitely/missing/certbot" `isInfixOf` rendered
                          )

    it "fails explicitly when certbot-backed ACME listeners do not produce a certificate file" $
      withUnusedLoopbackPort $ \challengePort ->
        withCustomFakeCertbotExecutable
          ( fakeCertbotScriptPreamble
              <> ["mkdir -p \"$config_dir/live/$cert_name\""]
          )
          ( \certbotExecutable ->
              withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                let certbotBackend =
                      certbotHttp01BackendWithExecutable
                        certbotExecutable
                        ["certonly", "--http-01-port", Text.pack (show challengePort), "--domains=loopback.example,alt.example"]
                    acmeTlsConfig =
                      serverConfigWithListeners
                        [ httpRuntimeListener "127.0.0.1" challengePort,
                          acmeHttpsListenerWithContacts "127.0.0.1" 5443 [] certbotBackend
                        ]
                runServer outputHandle acmeTlsConfig sampleApplication
                  `shouldThrow` (\exception -> "user error (Certbot ACME certificate file does not exist: " `isPrefixOf` show (exception :: IOError))
          )

    it "fails explicitly when certbot-backed ACME listeners do not produce a private key file" $
      withUnusedLoopbackPort $ \challengePort ->
        withManualTlsFiles $ \certificatePath _ ->
          withCustomFakeCertbotExecutable
            ( fakeCertbotScriptPreamble
                <> [ "mkdir -p \"$config_dir/live/$cert_name\"",
                     "cp " <> show certificatePath <> " \"$config_dir/live/$cert_name/fullchain.pem\""
                   ]
            )
            ( \certbotExecutable ->
                withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          ["certonly", "--http-01-port", Text.pack (show challengePort), "--cert-name=loopback.example"]
                      acmeTlsConfig =
                        serverConfigWithListeners
                          [ httpRuntimeListener "127.0.0.1" challengePort,
                            acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                          ]
                  runServer outputHandle acmeTlsConfig sampleApplication
                    `shouldThrow` (\exception -> "user error (Certbot ACME private key file does not exist: " `isPrefixOf` show (exception :: IOError))
            )

    it "cleans up already-started ACME listeners when a later ACME bind fails" $
      withUnusedLoopbackPort $ \firstChallengePort ->
        withUnusedLoopbackPort $ \secondChallengePort ->
          withUnusedLoopbackPort $ \firstHttpsPort ->
            withUnusedLoopbackPort $ \blockedHttpsPort ->
              withManualTlsFiles $ \certificatePath privateKeyPath ->
                withFakeCertbotExecutable certificatePath privateKeyPath $
                  \certbotExecutable ->
                    withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                      addressInfo : _ <-
                        Socket.getAddrInfo
                          (Just (Socket.defaultHints {Socket.addrFlags = [Socket.AI_PASSIVE]}))
                          (Just "127.0.0.1")
                          (Just (show blockedHttpsPort))
                      blockingSocket <- Socket.openSocket addressInfo
                      Socket.setSocketOption blockingSocket Socket.ReuseAddr 1
                      Socket.bind blockingSocket (Socket.addrAddress addressInfo)
                      Socket.listen blockingSocket Socket.maxListenQueue
                      let certbotBackend =
                            certbotHttp01BackendWithExecutable
                              certbotExecutable
                              ["certonly", "--http-01-port", Text.pack (show firstChallengePort), "--cert-name", "loopback.example"]
                          secondCertbotBackend =
                            certbotHttp01BackendWithExecutable
                              certbotExecutable
                              ["certonly", "--http-01-port", Text.pack (show secondChallengePort), "--domains=second.example"]
                          acmeTlsConfig =
                            serverConfigWithListeners
                              [ httpRuntimeListener "127.0.0.1" firstChallengePort,
                                httpRuntimeListener "127.0.0.1" secondChallengePort,
                                acmeHttpsListener "127.0.0.1" firstHttpsPort certbotBackend,
                                acmeHttpsListenerWithContacts "127.0.0.1" blockedHttpsPort [] secondCertbotBackend
                              ]
                      (runServer outputHandle acmeTlsConfig sampleApplication `shouldThrow` anyException)
                        `finally` Socket.close blockingSocket
                      threadDelay 50000
                      readLoopbackHttpsResponseResult firstHttpsPort "/known"
                        >>= (`shouldSatisfy` either (const True) (const False))

    it "fails explicitly when certbot-backed ACME listeners declare an invalid http-01 port" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let certbotBackend =
              certbotHttp01Backend
                ["certonly", "--http-01-port", "not-a-port"]
            acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "127.0.0.1" 80,
                  acmeHttpsListener "127.0.0.1" 5443 certbotBackend
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 has an invalid certbot http-01 port: not-a-port)")

    it "fails explicitly when no supported runtime listeners are configured" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
        runServer outputHandle (serverConfigWithListeners []) sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: no runtime listeners are configured.)")

    it "fails gracefully when the configured HTTP port is already in use" $
      withOccupiedLoopbackPort $ \occupiedPort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
          let runtimeConfig =
                serverConfigWithListeners
                  [ ListenerConfig
                      { listenerHost = "127.0.0.1",
                        listenerPort = occupiedPort,
                        listenerScheme = Http,
                        listenerTls = Nothing,
                        listenerAcme = Nothing
                      }
                  ]
           in runServer outputHandle runtimeConfig sampleApplication
                `shouldThrow` isAlreadyInUseError

    it "cleans up already-started HTTP listeners when a later bind fails" $
      withUnusedLoopbackPort $ \firstPort ->
        withOccupiedLoopbackPort $ \occupiedPort ->
          withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
            let multiListenerConfig =
                  serverConfigWithListeners
                    [ ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = firstPort,
                          listenerScheme = Http,
                          listenerTls = Nothing,
                          listenerAcme = Nothing
                        },
                      ListenerConfig
                        { listenerHost = "127.0.0.1",
                          listenerPort = occupiedPort,
                          listenerScheme = Http,
                          listenerTls = Nothing,
                          listenerAcme = Nothing
                        }
                    ]
            runServer outputHandle multiListenerConfig sampleApplication
              `shouldThrow` isAlreadyInUseError
            expectLoopbackPortReusable firstPort

    it "cleans up already-started HTTP listeners when a later manual TLS bind fails" $
      withUnusedLoopbackPort $ \firstPort ->
        withOccupiedLoopbackPort $ \occupiedTlsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
              let multiListenerConfig =
                    serverConfigWithListeners
                      [ ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = firstPort,
                            listenerScheme = Http,
                            listenerTls = Nothing,
                            listenerAcme = Nothing
                          },
                        ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = occupiedTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
              runServer outputHandle multiListenerConfig sampleApplication
                `shouldThrow` isAlreadyInUseError
              expectLoopbackPortReusable firstPort

    it "cleans up already-started manual TLS listeners when a later manual TLS bind fails" $
      withUnusedLoopbackPort $ \firstTlsPort ->
        withOccupiedLoopbackPort $ \occupiedTlsPort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
              let multiListenerConfig =
                    serverConfigWithListeners
                      [ ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = firstTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                  },
                            listenerAcme = Nothing
                          },
                        ListenerConfig
                          { listenerHost = "127.0.0.1",
                            listenerPort = occupiedTlsPort,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        { certificateFile = certificatePath,
                                          privateKeyFile = privateKeyPath
                                        }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
              runServer outputHandle multiListenerConfig sampleApplication
                `shouldThrow` isAlreadyInUseError
              expectLoopbackPortReusable firstTlsPort

  describe "withLocalTestServer" $ do
    it "serves the rendered application over a real loopback HTTP listener" $
      withLocalTestServer sampleApplication $ \localTestServer -> do
        localServerHost localTestServer `shouldBe` "127.0.0.1"
        localServerPort localTestServer `shouldSatisfy` (> 0)
        localServerBaseUrl localTestServer `shouldBe` Text.pack ("http://127.0.0.1:" <> show (localServerPort localTestServer))
        responseText <- readLocalTestServerResponse localTestServer "/known"
        Text.isInfixOf "<h1>Known</h1>" responseText `shouldBe` True
        Text.isInfixOf "<nav data-navigation-region=\"primary\">" responseText `shouldBe` True

    it "serves static assets through the same loopback HTTP listener" $
      withSystemTempDirectory "harch-web-local-static" $ \tempDirectory -> do
        let assetConfig =
              StaticAssetsConfig
                { staticAssetRoots =
                    [ StaticAssetRoot
                        { staticUrlPrefix = "/assets",
                          staticDirectory = tempDirectory
                        }
                    ],
                  staticAssetContentTypes = defaultStaticAssetContentTypes,
                  staticCacheControlSeconds = Just 60
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
            assetDirectory = tempDirectory </> "styles"
            assetPath = assetDirectory </> "site.css"
        createDirectoryIfMissing True assetDirectory
        writeFile assetPath "body { color: red; }"
        withLocalTestServer staticApplication $ \localTestServer -> do
          responseText <- readLocalTestServerResponse localTestServer "/assets/styles/site.css"
          Text.isInfixOf "body { color: red; }" responseText `shouldBe` True

readLocalTestServerResponse :: LocalTestServer -> Text -> IO Text
readLocalTestServerResponse localTestServer path = do
  responseBytes <- readLocalTestServerResponseBytes localTestServer path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLocalTestServerResponseBytes :: LocalTestServer -> Text -> IO ByteString.ByteString
readLocalTestServerResponseBytes localTestServer =
  readLoopbackHttpResponseBytes (localServerPort localTestServer)

readLoopbackHttpResponse :: Int -> Text -> IO Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpsResponse :: Int -> Text -> IO Text
readLoopbackHttpsResponse port path = do
  responseResult <- readLoopbackHttpsResponseResult port path
  case responseResult of
    Right responseText -> pure responseText
    Left responseError ->
      expectationFailure ("expected curl HTTPS request to succeed: " <> responseError)
        >> pure Text.empty

readLoopbackHttpResponseBytes :: Int -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port =
  readLoopbackHttpResponseBytesWithHost port "127.0.0.1"

readLoopbackHttpResponseBytesWithHost :: Int -> Text -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytesWithHost port hostHeader path = do
  responseResult <- readLoopbackHttpResponseBytesWithHostResult port hostHeader path
  case responseResult of
    Right responseBytes -> pure responseBytes
    Left responseError ->
      ioError (userError responseError)

readLoopbackHttpResponseBytesWithHostResult :: Int -> Text -> Text -> IO (Either String ByteString.ByteString)
readLoopbackHttpResponseBytesWithHostResult port hostHeader path =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    responseResult <- try $ do
      Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      SocketByteString.sendAll clientSocket (buildHttpRequestWithHost hostHeader path)
      responseBytes <- readAllSocketChunks clientSocket
      pure (extractHttpBody responseBytes)
    Socket.close clientSocket
    pure $
      either
        (Left . displayException)
        Right
        (responseResult :: Either IOError ByteString.ByteString)

readLoopbackHttpResponseBytesWithHostAndHeadersResult :: Int -> Text -> Text -> [(Text, Text)] -> IO (Either String ByteString.ByteString)
readLoopbackHttpResponseBytesWithHostAndHeadersResult port hostHeader path headers =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    responseResult <- try $ do
      Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      SocketByteString.sendAll clientSocket (buildHttpRequestWithHostAndHeaders hostHeader path headers)
      responseBytes <- readAllSocketChunks clientSocket
      pure (extractHttpBody responseBytes)
    Socket.close clientSocket
    pure $
      either
        (Left . displayException)
        Right
        (responseResult :: Either IOError ByteString.ByteString)

readLoopbackHttpsResponseResult :: Int -> Text -> IO (Either String Text)
readLoopbackHttpsResponseResult port path = do
  let url = "https://127.0.0.1:" <> show port <> Text.unpack path
  (exitCode, stdoutText, stderrText) <-
    readProcessWithExitCode
      "curl"
      ["--silent", "--show-error", "--insecure", "--fail", "--noproxy", "*", url]
      ""
  pure $
    case exitCode of
      ExitSuccess -> Right (Text.pack stdoutText)
      ExitFailure _ -> Left stderrText

connectAndCloseLoopbackSocket :: Int -> IO ()
connectAndCloseLoopbackSocket port =
  Socket.withSocketsDo $ do
    clientSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
    Socket.connect clientSocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.close clientSocket

hasTextAttribute :: Text -> Text -> [Observability.ObservabilityAttribute] -> Bool
hasTextAttribute attributeName expectedValue =
  any
    ( \attribute ->
        Observability.attributeName attribute == attributeName
          && Observability.attributeValue attribute == Observability.TextAttribute expectedValue
    )

lookupIntAttribute :: Text -> [Observability.ObservabilityAttribute] -> Maybe Int
lookupIntAttribute expectedName =
  listToMaybe
    . mapMaybe
      ( \attribute ->
          case (Observability.attributeName attribute, Observability.attributeValue attribute) of
            (currentName, Observability.IntAttribute attributeValue)
              | currentName == expectedName -> Just attributeValue
            _ -> Nothing
      )

expectMeasuredRootRequestTiming :: Observability.RequestObservability -> Expectation
expectMeasuredRootRequestTiming requestObservabilityValue = do
  let spanAttributes =
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservabilityValue)
      metricAttributes =
        Observability.httpServerMetricAttributes
          (Observability.observabilityHttpServerMetrics requestObservabilityValue)
      expectTimingAttribute attributeName = do
        lookupIntAttribute attributeName spanAttributes
          `shouldSatisfy` maybe False (>= 0)
        lookupIntAttribute attributeName metricAttributes
          `shouldSatisfy` maybe False (>= 0)
  expectTimingAttribute "harch.request.start_monotonic_ns"
  expectTimingAttribute "harch.request.duration_ns"

expectMeasuredRequestTiming :: Observability.RequestObservability -> Expectation
expectMeasuredRequestTiming requestObservabilityValue = do
  expectMeasuredRootRequestTiming requestObservabilityValue
  let spanAttributes =
        Observability.requestSpanAttributes
          (Observability.observabilityRequestSpan requestObservabilityValue)
      metricAttributes =
        Observability.httpServerMetricAttributes
          (Observability.observabilityHttpServerMetrics requestObservabilityValue)
      expectTimingAttribute attributeName = do
        lookupIntAttribute attributeName spanAttributes
          `shouldSatisfy` maybe False (>= 0)
        lookupIntAttribute attributeName metricAttributes
          `shouldSatisfy` maybe False (>= 0)
  expectTimingAttribute "harch.phase.request-policy.start_offset_ns"
  expectTimingAttribute "harch.phase.request-policy.duration_ns"
  expectTimingAttribute "harch.phase.route-match.start_offset_ns"
  expectTimingAttribute "harch.phase.route-match.duration_ns"
  expectTimingAttribute "harch.phase.render-response.start_offset_ns"
  expectTimingAttribute "harch.phase.render-response.duration_ns"

waitForConnectionObservability :: IORef [Observability.ConnectionObservability] -> Text -> IO Observability.ConnectionObservability
waitForConnectionObservability connectionObservabilityReference expectedEventName =
  waitForObservabilityAttempts (500 :: Int)
  where
    waitForObservabilityAttempts remainingAttempts = do
      connectionObservabilityValues <- readIORef connectionObservabilityReference
      case find matchesExpectedEvent connectionObservabilityValues of
        Just connectionObservabilityValue -> pure connectionObservabilityValue
        Nothing
          | remainingAttempts > 0 -> do
              threadDelay 10000
              waitForObservabilityAttempts (remainingAttempts - 1)
          | otherwise ->
              expectationFailure ("expected connection observability for " <> Text.unpack expectedEventName)
                >> pure
                  ( Observability.buildConnectionObservability
                      "missing"
                      []
                  )

    matchesExpectedEvent connectionObservabilityValue =
      hasTextAttribute
        "harch.connection.event"
        expectedEventName
        (Observability.requestSpanAttributes (Observability.observabilityConnectionSpan connectionObservabilityValue))

waitForServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runServer to remain running, but it exited early"
            >> pure Text.empty
        Nothing -> do
          responseResult <- try (readLoopbackHttpResponse port path) :: IO (Either IOError Text)
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected runServer to accept loopback HTTP requests"
                    >> pure Text.empty

waitForHttpsServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForHttpsServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runServer to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runServer to remain running, but it exited early"
            >> pure Text.empty
        Nothing -> do
          responseResult <- readLoopbackHttpsResponseResult port path
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected runServer to accept loopback HTTPS requests"
                    >> pure Text.empty

waitForServerExit :: IORef (Maybe (Either SomeException ())) -> IO ()
waitForServerExit completionReference =
  waitForExitAttempts (500 :: Int)
  where
    waitForExitAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just _ -> pure ()
        Nothing
          | remainingAttempts > 0 -> do
              threadDelay 10000
              waitForExitAttempts (remainingAttempts - 1)
          | otherwise ->
              expectationFailure "expected runServer to stop after being signalled"

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind reservedSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- Socket.getSocketName reservedSocket
  case socketAddress of
    Socket.SockAddrInet port _ -> do
      Socket.close reservedSocket
      action (fromIntegral port)
    _ -> do
      Socket.close reservedSocket
      error "expected IPv4 loopback reservation socket"

withOccupiedLoopbackPort :: (Int -> IO a) -> IO a
withOccupiedLoopbackPort action = do
  listeningSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind listeningSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  Socket.listen listeningSocket Socket.maxListenQueue
  socketAddress <- Socket.getSocketName listeningSocket
  case socketAddress of
    Socket.SockAddrInet port _ ->
      action (fromIntegral port)
        `finally` Socket.close listeningSocket
    _ ->
      Socket.close listeningSocket
        >> error "expected IPv4 loopback listening socket"

withManualTlsFiles :: (FilePath -> FilePath -> IO a) -> IO a
withManualTlsFiles action =
  withSystemTempDirectory "harch-web-tls" $ \tempDirectory -> do
    let certificatePath = tempDirectory </> "cert.pem"
        privateKeyPath = tempDirectory </> "key.pem"
    writeFile certificatePath manualTlsCertificatePem
    writeFile privateKeyPath manualTlsPrivateKeyPem
    action certificatePath privateKeyPath

withEmptyExecutablePath :: IO a -> IO a
withEmptyExecutablePath action =
  withSystemTempDirectory "missing-executable-path" $ \tempDirectory -> do
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" tempDirectory
    action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

stripVolatileRequestTiming :: Observability.RequestObservability -> Observability.RequestObservability
stripVolatileRequestTiming requestObservability =
  requestObservability
    { Observability.observabilityRequestSpan =
        stripVolatileRequestSpanTiming (Observability.observabilityRequestSpan requestObservability),
      Observability.observabilityHttpServerMetrics =
        (Observability.observabilityHttpServerMetrics requestObservability)
          { Observability.httpServerMetricAttributes =
              filter
                (not . isVolatileRequestTimingAttribute)
                (Observability.httpServerMetricAttributes (Observability.observabilityHttpServerMetrics requestObservability))
          }
    }

stripVolatileRequestSpanTiming :: Observability.RequestSpan -> Observability.RequestSpan
stripVolatileRequestSpanTiming requestSpan =
  requestSpan
    { Observability.requestSpanAttributes =
        filter
          (not . isVolatileRequestTimingAttribute)
          (Observability.requestSpanAttributes requestSpan)
    }

isVolatileRequestTimingAttribute :: Observability.ObservabilityAttribute -> Bool
isVolatileRequestTimingAttribute attribute =
  Observability.attributeName attribute
    `elem` [ "harch.request.start_monotonic_ns",
             "harch.request.duration_ns",
             "harch.phase.request-policy.start_offset_ns",
             "harch.phase.request-policy.duration_ns",
             "harch.phase.route-match.start_offset_ns",
             "harch.phase.route-match.duration_ns",
             "harch.phase.render-response.start_offset_ns",
             "harch.phase.render-response.duration_ns"
           ]

withOtlpCollector ::
  Http.Status ->
  LazyByteString.ByteString ->
  (Text -> MVar CapturedCollectorRequest -> IO a) ->
  IO a
withOtlpCollector responseStatus responseBody action =
  withUnusedLoopbackPort $ \collectorPort -> do
    capturedRequestReference <- newEmptyMVar
    let collectorUrl = Text.pack ("http://127.0.0.1:" <> show collectorPort <> "/v1/traces")
        collectorApplication request respond = do
          requestBody <- Wai.strictRequestBody request
          putMVar
            capturedRequestReference
            CapturedCollectorRequest
              { capturedCollectorMethod = Wai.requestMethod request,
                capturedCollectorPath = Wai.rawPathInfo request,
                capturedCollectorHeaders = Wai.requestHeaders request,
                capturedCollectorBody = requestBody
              }
          respond (Wai.responseLBS responseStatus [("Content-Type", "application/json")] responseBody)
    serverThreadId <- forkIO (Warp.run collectorPort collectorApplication)
    threadDelay 50000
    action collectorUrl capturedRequestReference `finally` killThread serverThreadId

extractQuotedJsonField :: Text -> Text -> Maybe Text
extractQuotedJsonField fieldName bodyText =
  listToMaybe (extractQuotedJsonFields fieldName bodyText)

extractQuotedJsonFields :: Text -> Text -> [Text]
extractQuotedJsonFields fieldName bodyText =
  case Text.breakOn fieldPrefix bodyText of
    (_, withField)
      | Text.null withField -> []
      | otherwise ->
          let fieldValueStart = Text.drop (Text.length fieldPrefix) withField
              fieldValue = Text.takeWhile (/= '"') fieldValueStart
              remainingBody = Text.drop (Text.length fieldValue + 1) fieldValueStart
           in fieldValue : extractQuotedJsonFields fieldName remainingBody
  where
    fieldPrefix = "\"" <> fieldName <> "\":\""

extractQuotedJsonIntegerFields :: Text -> Text -> [Integer]
extractQuotedJsonIntegerFields fieldName bodyText =
  mapMaybe (readMaybe . Text.unpack) (extractQuotedJsonFields fieldName bodyText)

expectPlausibleEpochNanoTimestamps :: Text -> Expectation
expectPlausibleEpochNanoTimestamps bodyText = do
  let earliestPlausibleEpochNano = 1577836800000000000
      latestPlausibleEpochNano = 4102444800000000000
      startTimes = extractQuotedJsonIntegerFields "startTimeUnixNano" bodyText
      endTimes = extractQuotedJsonIntegerFields "endTimeUnixNano" bodyText
  startTimes `shouldSatisfy` (not . null)
  length startTimes `shouldBe` length endTimes
  mapM_
    ( \(startTimeUnixNano, endTimeUnixNano) -> do
        startTimeUnixNano `shouldSatisfy` (>= earliestPlausibleEpochNano)
        endTimeUnixNano `shouldSatisfy` (< latestPlausibleEpochNano)
        startTimeUnixNano `shouldSatisfy` (< endTimeUnixNano)
        (endTimeUnixNano - startTimeUnixNano) `shouldSatisfy` (>= 1000)
    )
    (zip startTimes endTimes)

expectLoopbackPortReusable :: Int -> IO ()
expectLoopbackPortReusable port = do
  bindResult <- try bindTemporaryListener :: IO (Either IOError ())
  case bindResult of
    Right () -> pure ()
    Left bindError ->
      expectationFailure ("expected loopback port " <> show port <> " to be reusable, but bind failed: " <> displayException bindError)
  where
    bindTemporaryListener = do
      temporarySocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
      Socket.bind temporarySocket (Socket.SockAddrInet (fromIntegral port) (Socket.tupleToHostAddress (127, 0, 0, 1)))
      Socket.listen temporarySocket Socket.maxListenQueue
      Socket.close temporarySocket

buildHttpRequestWithHost :: Text -> Text -> ByteString.ByteString
buildHttpRequestWithHost hostHeader path =
  buildHttpRequestWithHostAndHeaders hostHeader path []

buildHttpRequestWithHostAndHeaders :: Text -> Text -> [(Text, Text)] -> ByteString.ByteString
buildHttpRequestWithHostAndHeaders hostHeader path headers =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: "
      <> Text.unpack hostHeader
      <> concatMap
        (\(headerName, headerValue) -> "\r\n" <> Text.unpack headerName <> ": " <> Text.unpack headerValue)
        headers
      <> "\r\nConnection: close\r\n\r\n"

manualTlsCertificatePem :: String
manualTlsCertificatePem =
  unlines
    [ "-----BEGIN CERTIFICATE-----",
      "MIICMzCCAdmgAwIBAgIUAliSVDIFHNHzI1q+e3P+1Ah1kbkwCgYIKoZIzj0EAwIw",
      "QDEXMBUGA1UECgwOdHJ1c3RtZSB2MS4yLjExJTAjBgNVBAsMHFRlc3RpbmcgQ0Eg",
      "I2JZeVBlbjVhVnQ0MHlLaXAwIBcNMDAwMTAxMDAwMDAwWhgPMzAwMDAxMDEwMDAw",
      "MDBaMEIxFzAVBgNVBAoMDnRydXN0bWUgdjEuMi4xMScwJQYDVQQLDB5UZXN0aW5n",
      "IGNlcnQgI3JHR1p2N1VLMVQyd1hjeG8wWTATBgcqhkjOPQIBBggqhkjOPQMBBwNC",
      "AARK6NEQhfcGYBt2TRWkrktWpYdmCvYo76sciH70kYBcihzjqaKEw5dD/KbdJjmU",
      "v4pqTQEMnb8hVwKMfSYqOmqwo4GsMIGpMB0GA1UdDgQWBBR8NRVz81tKH8nCWLNI",
      "Pn7zdlXakTAMBgNVHRMBAf8EAjAAMB8GA1UdIwQYMBaAFCVFUSwlXOOm5JvKD5o1",
      "fvsmUu2bMB0GA1UdEQEB/wQTMBGHBH8AAAGCCWxvY2FsaG9zdDAOBgNVHQ8BAf8E",
      "BAMCBaAwKgYDVR0lAQH/BCAwHgYIKwYBBQUHAwIGCCsGAQUFBwMBBggrBgEFBQcD",
      "AzAKBggqhkjOPQQDAgNIADBFAiEAujBETz7z5tWMOpwL/NQFEX9LcbcuHA3+T2oa",
      "6z0Y87gCIDvX/o0KT31LKZM9LklDE11u1S63AYjY0948jEd4Jnrx",
      "-----END CERTIFICATE-----"
    ]

manualTlsPrivateKeyPem :: String
manualTlsPrivateKeyPem =
  unlines
    [ "-----BEGIN EC PRIVATE KEY-----",
      "MHcCAQEEIJ9itNr2Vm4XTUo74d26GQWuZNdRfEjN6cZqWK418T5LoAoGCCqGSM49",
      "AwEHoUQDQgAESujREIX3BmAbdk0VpK5LVqWHZgr2KO+rHIh+9JGAXIoc46mihMOX",
      "Q/ym3SY5lL+Kak0BDJ2/IVcCjH0mKjpqsA==",
      "-----END EC PRIVATE KEY-----"
    ]

readAllSocketChunks :: Socket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (_, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
   in ByteString.drop 4 withSeparator
