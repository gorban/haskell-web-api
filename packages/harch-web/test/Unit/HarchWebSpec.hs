{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWebSpec (spec) where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, newMVar, putMVar, readMVar, threadDelay)
import Control.Exception (SomeException, displayException, evaluate, finally, try)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Char (isHexDigit)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import HarchWeb
import qualified HarchWeb.Observability as Observability
import qualified Network.HTTP.Types as Http
import qualified Network.Socket as Socket
import qualified Network.Socket.ByteString as SocketByteString
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Internal as WaiInternal
import System.Directory (createDirectoryIfMissing, doesFileExist, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess, readProcessWithExitCode)
import Test.Hspec

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
        (_, DataRoute) -> "/data"
        (_, MissingRoute) -> "/404"
    )

applyTestPathPrefix :: Text -> Text -> Text
applyTestPathPrefix pathPrefix path
  | Text.null pathPrefix = path
  | path == "/" = pathPrefix
  | otherwise = pathPrefix <> path

sampleRequestContextFromRequest :: Wai.Request -> TestContext -> TestContext
sampleRequestContextFromRequest request requestContext =
  requestContext
    { requestPathPrefix =
        maybe
          ""
          normalizeTestPathPrefix
          ( lookup "X-Forwarded-Prefix" (Wai.requestHeaders request)
              >>= firstTestHeaderValue
          )
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
      shellScriptSources = ["/assets/navigation.js"]
    }

emptyStaticAssets :: StaticAssetsConfig
emptyStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [],
      staticCacheControlSeconds = Nothing
    }

defaultRequestPolicy :: RequestPolicyConfig
defaultRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing
    }

sampleApplicationWithStaticAssets :: StaticAssetsConfig -> Application TestRoute TestContext
sampleApplicationWithStaticAssets staticAssetsConfig =
  sampleApplicationWithConfig staticAssetsConfig defaultRequestPolicy

sampleApplicationWithConfig :: StaticAssetsConfig -> RequestPolicyConfig -> Application TestRoute TestContext
sampleApplicationWithConfig staticAssetsConfig requestPolicyConfig =
  Application
    { appName = "sample",
      defaultRequestContext = defaultContext,
      requestContextFromRequest = sampleRequestContextFromRequest,
      applicationStaticAssets = staticAssetsConfig,
      applicationRequestPolicy = requestPolicyConfig,
      routeCodec = sampleCodec,
      renderResponse = pure . renderSampleResponse,
      pageShell = buildPageShell sampleCodec sampleShell,
      reportRequestObservability = const (pure ()),
      reportConnectionObservability = const (pure ()),
      reportApplicationLog = const (pure ())
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
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [],
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

acmeHttpsListenerWithDomains :: Text -> Int -> [Text] -> [Text] -> AcmeChallengeBackend -> ListenerConfig
acmeHttpsListenerWithDomains =
  acmeHttpsListenerWithDomainsAndChallengePort 80

acmeHttpsListenerWithDomainsAndChallengePort :: Int -> Text -> Int -> [Text] -> [Text] -> AcmeChallengeBackend -> ListenerConfig
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
                      acmeChallengeBackend = challengeBackend
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

acmeHttpsListenerWithContacts :: Text -> Int -> [Text] -> AcmeChallengeBackend -> ListenerConfig
acmeHttpsListenerWithContacts host port contactEmails =
  acmeHttpsListenerWithDomains host port contactEmails []

acmeHttpsListener :: Text -> Int -> AcmeChallengeBackend -> ListenerConfig
acmeHttpsListener host port =
  acmeHttpsListenerWithContacts host port ["ops@example.com"]

certbotHttp01Backend :: [Text] -> AcmeChallengeBackend
certbotHttp01Backend =
  certbotHttp01BackendWithExecutable "certbot"

certbotHttp01BackendWithExecutable :: FilePath -> [Text] -> AcmeChallengeBackend
certbotHttp01BackendWithExecutable executablePath certbotArguments =
  CertbotHttp01
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
            acmeChallengeBackend = CertbotHttp01 certbotConfig
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
      "echo fake certbot failure >&2",
      "exit 42"
    ]

withFakeOpenSslExecutable :: FilePath -> FilePath -> (FilePath -> IO a) -> IO a
withFakeOpenSslExecutable _certificatePath privateKeyPath action =
  withSystemTempDirectory "fake-openssl" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "openssl"
        modulusHex = "A1B2C3D4E5F60718293A4B5C6D7E8F90"
    writeFile
      scriptPath
      ( unlines
          [ "#!/bin/sh",
            "set -eu",
            "command=\"$1\"",
            "shift",
            "case \"$command\" in",
            "  genrsa)",
            "    output=''",
            "    while [ \"$#\" -gt 0 ]; do",
            "      case \"$1\" in",
            "        -out) output=\"$2\"; shift 2 ;;",
            "        *) shift ;;",
            "      esac",
            "    done",
            "    printf '%s\\n' 'FAKE ACCOUNT KEY' > \"$output\"",
            "    ;;",
            "  rsa)",
            "    printf 'Modulus=" <> modulusHex <> "\\n'",
            "    ;;",
            "  req)",
            "    input=''",
            "    keyout=''",
            "    output=''",
            "    while [ \"$#\" -gt 0 ]; do",
            "      case \"$1\" in",
            "        -in) input=\"$2\"; shift 2 ;;",
            "        -keyout) keyout=\"$2\"; shift 2 ;;",
            "        -out) output=\"$2\"; shift 2 ;;",
            "        *) shift ;;",
            "      esac",
            "    done",
            "    if [ -n \"$keyout\" ]; then",
            "      cp " <> show privateKeyPath <> " \"$keyout\"",
            "      printf '%s\\n' 'FAKE CSR PEM' > \"$output\"",
            "    elif [ -n \"$input\" ]; then",
            "      printf 'FAKE DER' > \"$output\"",
            "    fi",
            "    ;;",
            "  dgst)",
            "    output=''",
            "    while [ \"$#\" -gt 0 ]; do",
            "      case \"$1\" in",
            "        -out) output=\"$2\"; shift 2 ;;",
            "        *) shift ;;",
            "      esac",
            "    done",
            "    printf 'fake-bytes' > \"$output\"",
            "    ;;",
            "  *)",
            "    echo unknown fake openssl command: \"$command\" >&2",
            "    exit 64",
            "    ;;",
            "esac"
          ]
      )
    callProcess "chmod" ["+x", scriptPath]
    withPrependedPathDirectory tempDirectory (action scriptPath)

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
      requestContextFromRequest = sampleRequestContextFromRequest,
      applicationStaticAssets = emptyStaticAssets,
      applicationRequestPolicy = defaultRequestPolicy,
      routeCodec = rootPathCodec,
      renderResponse = pure . PageResponse . samplePage,
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
          DataRoute -> applyTestPathPrefix (requestPathPrefix (requestContext request)) "/data"
          MissingRoute -> applyTestPathPrefix (requestPathPrefix (requestContext request)) "/404",
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
          challengeBackend = CertbotHttp01 certbotConfig
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeChallengeBackend = challengeBackend
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
                strictTransportSecurity = Just strictTransportSecurityConfig
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
      acmeChallengeBackend acmeConfig `shouldBe` challengeBackend
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
          requestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = True,
                httpsRedirectPort = Just 5443,
                strictTransportSecurity = Just strictTransportSecurityConfig
              }
          otherRequestPolicyConfig =
            RequestPolicyConfig
              { redirectHttpToHttps = False,
                httpsRedirectPort = Nothing,
                strictTransportSecurity = Just otherStrictTransportSecurityConfig
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeChallengeBackend = CertbotHttp01 certbotConfig
              }
          otherAcmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com"],
                acmeDomains = ["staging.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Just "/var/lib/harch-web/staging-certs",
                acmeChallengeBackend = InProcessHttp01
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
          staticAssetsConfig = StaticAssetsConfig {staticAssetRoots = [staticRoot], staticCacheControlSeconds = Just 3600}
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
      requestPolicyConfig `shouldBe` requestPolicyConfig
      requestPolicyConfig `shouldNotBe` otherRequestPolicyConfig
      InProcessHttp01 `shouldNotBe` CertbotHttp01 certbotConfig
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
      staticAssetsConfig `shouldNotBe` StaticAssetsConfig {staticAssetRoots = [], staticCacheControlSeconds = Nothing}
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
      show requestPolicyConfig `shouldBe` "RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True})}"
      show (CertbotHttp01 certbotConfig) `shouldBe` "CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})"
      show acmeConfig `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}"
      show manualCertificateSource `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show sharedCertificateSource `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show acmeCertificateSource `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})"
      show (TlsConfig {certificateSource = manualCertificateSource}) `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show listenerConfig `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}"
      show httpAcmeListenerConfig `shouldBe` "ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 80, listenerScheme = Http, listenerTls = Nothing, listenerAcme = AcmeConfig {acmeDirectoryUrl = \"https://acme-staging-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"staging.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Just \"/var/lib/harch-web/staging-certs\", acmeChallengeBackend = InProcessHttp01}}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show staticAssetsConfig `shouldBe` "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}"
      show tracingConfig `shouldBe` "OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}"
      show observabilityConfig `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}"
      show ManualTlsCredentials `shouldBe` "ManualTlsCredentials"
      show (AwaitCertificateFiles (Just 15)) `shouldBe` "AwaitCertificateFiles {certificateWaitTimeoutSeconds = Just 15}"
      show TracingSignal `shouldBe` "TracingSignal"
      show exporterStartup `shouldBe` "OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}"
      show observabilityPlan `shouldBe` "ObservabilityStartupPlan {startupExporters = [OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]}"
      show serverConfig `shouldBe` "ServerConfig {listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, requestPolicy = RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True})}, observability = ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}}"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 strictTransportSecurityConfig "")
      shouldBeParenthesized (showsPrec 11 requestPolicyConfig "")
      shouldBeParenthesized (showsPrec 11 (CertbotHttp01 certbotConfig) "")
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
      show [requestPolicyConfig] `shouldBe` "[RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True})}]"
      show [InProcessHttp01, CertbotHttp01 certbotConfig] `shouldBe` "[InProcessHttp01,CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})]"
      show [acmeConfig] `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}]"
      show [manualCertificateSource, sharedCertificateSource, acmeCertificateSource] `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},SharedCertificateFiles {certificateDirectory = \"/var/lib/harch-web/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})]"
      show [tlsConfig] `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})}]"
      show [listenerConfig] `shouldBe` "[ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}]"
      show [httpAcmeListenerConfig] `shouldBe` "[ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 80, listenerScheme = Http, listenerTls = Nothing, listenerAcme = AcmeConfig {acmeDirectoryUrl = \"https://acme-staging-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"staging.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Just \"/var/lib/harch-web/staging-certs\", acmeChallengeBackend = InProcessHttp01}}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig] `shouldBe` "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}]"
      show [tracingConfig] `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig] `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}]"
      show [TracingSignal, MetricsSignal] `shouldBe` "[TracingSignal,MetricsSignal]"
      show [exporterStartup] `shouldBe` "[OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityPlan] `shouldBe` "[ObservabilityStartupPlan {startupExporters = [OtlpExporterStartup {startupSignal = TracingSignal, startupEndpoint = \"http://collector:4318/v1/traces\", startupHeaders = [(\"authorization\",\"Bearer token\")]}]}]"
      show [serverConfig] `shouldBe` "[ServerConfig {listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, requestPolicy = RequestPolicyConfig {redirectHttpToHttps = True, httpsRedirectPort = Just 5443, strictTransportSecurity = Just (StrictTransportSecurityConfig {strictTransportSecurityMaxAgeSeconds = 31536000, strictTransportSecurityIncludeSubDomains = True, strictTransportSecurityPreload = True})}, observability = ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://collector:4318/v1/traces\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Nothing}}]"

  describe "public record coverage" $ do
    it "reads every exported selector from the public request, page, shell, and document records" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          navigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "primary"}
          mainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "true"}
          localTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5001, localServerBaseUrl = "http://127.0.0.1:5001"}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = "<h1>Known</h1>", pageBootstrapHooks = ["known-page"]}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentScriptSources = ["/assets/navigation.js"]}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellScriptSources = ["/assets/navigation.js"]}
          responseBodyValue = ResponseBody {responseStatus = 202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = []}
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
      documentScriptSources document `shouldBe` ["/assets/navigation.js"]
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationAttributes shell `shouldBe` [navigationAttribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` "app-main"
      shellMainAttributes shell `shouldBe` [mainAttribute]
      shellScriptSources shell `shouldBe` ["/assets/navigation.js"]
      localServerHost localTestServer `shouldBe` "127.0.0.1"
      localServerPort localTestServer `shouldBe` 5001
      localServerBaseUrl localTestServer `shouldBe` "http://127.0.0.1:5001"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      responseStatus responseBodyValue `shouldBe` 202
      responseContentType responseBodyValue `shouldBe` "application/json"
      responseBody responseBodyValue `shouldBe` "{\"route\":\"data\"}"
      responseObservabilityAttributes responseBodyValue `shouldBe` []
      responseLogEntries responseBodyValue `shouldBe` []

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
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute, navigationHref = "/404", navigationIsActive = False}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentScriptSources = ["/assets/navigation.js"]}
          otherDocument = Document {documentTitle = "Missing", documentBodyAttributes = [otherAttribute], documentNavigationAttributes = [otherNavigationAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = "other-main", documentMainAttributes = [otherMainAttribute], documentMainContent = "<h1>Missing</h1>", documentBootstrapHooks = [], documentScriptSources = []}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellScriptSources = ["/assets/navigation.js"]}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationAttributes = [otherNavigationAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = "other-main", shellMainAttributes = [otherMainAttribute], shellScriptSources = []}
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
      (resolvedNavigationItem == resolvedNavigationItem) `shouldBe` True
      (resolvedNavigationItem /= otherResolvedNavigationItem) `shouldBe` True
      show resolvedNavigationItem `shouldBe` "ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}"
      (document == document) `shouldBe` True
      (document /= otherDocument) `shouldBe` True
      show document `shouldBe` "Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigationAttributes = [HtmlAttribute {attributeName = \"data-navigation-region\", attributeValue = \"primary\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainAttributes = [HtmlAttribute {attributeName = \"data-navigation-content\", attributeValue = \"true\"}], documentMainContent = \"<h1>Known</h1>\", documentBootstrapHooks = [\"known-page\"], documentScriptSources = [\"/assets/navigation.js\"]}"
      show [document] `shouldBe` "[Document {documentTitle = \"Known\", documentBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], documentNavigationAttributes = [HtmlAttribute {attributeName = \"data-navigation-region\", attributeValue = \"primary\"}], documentNavigation = [ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}], documentMainId = \"app-main\", documentMainAttributes = [HtmlAttribute {attributeName = \"data-navigation-content\", attributeValue = \"true\"}], documentMainContent = \"<h1>Known</h1>\", documentBootstrapHooks = [\"known-page\"], documentScriptSources = [\"/assets/navigation.js\"]}]"
      (localTestServer == localTestServer) `shouldBe` True
      (localTestServer /= otherLocalTestServer) `shouldBe` True
      show localTestServer `shouldBe` "LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}"
      show [localTestServer] `shouldBe` "[LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}]"
      (shell == shell) `shouldBe` True
      (shell /= otherShell) `shouldBe` True
      show shell `shouldBe` "PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationAttributes = [HtmlAttribute {attributeName = \"data-navigation-region\", attributeValue = \"primary\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\", shellMainAttributes = [HtmlAttribute {attributeName = \"data-navigation-content\", attributeValue = \"true\"}], shellScriptSources = [\"/assets/navigation.js\"]}"
      show [shell] `shouldBe` "[PageShell {shellBodyAttributes = [HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}], shellNavigationAttributes = [HtmlAttribute {attributeName = \"data-navigation-region\", attributeValue = \"primary\"}], shellNavigationItems = [NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}], shellMainId = \"app-main\", shellMainAttributes = [HtmlAttribute {attributeName = \"data-navigation-content\", attributeValue = \"true\"}], shellScriptSources = [\"/assets/navigation.js\"]}]"
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
      show [pageResponse, pageResponseWithMetadata, bodyResponseValue] `shouldBe` "[PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),PageResponseWithMetadata (ResponseBody {responseStatus = 500, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"]}) (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", requestPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),BodyResponse (ResponseBody {responseStatus = 202, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = []})]"

    it "reads the Application fields directly without relying on higher-level helpers" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          codec = routeCodec sampleApplication

      appName sampleApplication `shouldBe` "sample"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      applicationStaticAssets sampleApplication `shouldBe` emptyStaticAssets
      parseRoute codec defaultContext "/known" `shouldBe` Just request
      parseRoute codec defaultContext "/data" `shouldBe` Just RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
      renderRoute codec request `shouldBe` "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldReturn` PageResponse (samplePage request)
      pageShell sampleApplication (samplePage request)
        `shouldBe` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"
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
            documentScriptSources = ["/assets/navigation.js"]
          }

  describe "buildPageShell" $ do
    it "renders the shared HTML document for the supplied page and shell options" $
      buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "renders bootstrap hook metadata only for pages that opt in" $
      buildPageShell
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
        `shouldBe` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"known-page,hydrate-known\"><h1>Known</h1></main></body></html>"

  describe "toWaiApplication" $ do
    it "selects request paths through the stored route parser and returns HTML pages" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["es", "known"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/es/known\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "treats an empty raw path as the root path" $ do
      response <- performWaiRequest (toWaiApplication rootPathApplication) Wai.defaultRequest
      Wai.responseStatus response `shouldBe` Http.status200
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/\" aria-current=\"page\">Known</a><a href=\"/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "normalizes forwarded root prefixes for route matching and rendered root links" $ do
      let prefixedRootRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "app")]
              }
      response <- performWaiRequest (toWaiApplication rootPathApplication) prefixedRootRequest
      Wai.responseStatus response `shouldBe` Http.status200
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/app\" aria-current=\"page\">Known</a><a href=\"/app/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "uses forwarded path prefixes for route matching and rendered navigation links" $ do
      let prefixedRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/app/known",
                Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
      response <- performWaiRequest (toWaiApplication sampleApplication) prefixedRequest
      Wai.responseStatus response `shouldBe` Http.status200
      readResponseBody response
        `shouldReturn` "<html><head><title>Known</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/app/known\" aria-current=\"page\">Known</a><a href=\"/app/404\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "renders the not-found page through the shared shell with a 404 status" $ do
      response <- performWaiRequest (toWaiApplication sampleApplication) (waiRequest ["missing"])
      Wai.responseStatus response `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      readResponseBody response
        `shouldReturn` "<html><head><title>Missing</title><script src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\">Known</a><a href=\"/404\" aria-current=\"page\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Missing</h1></main></body></html>"

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
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets (defaultRequestPolicy {redirectHttpToHttps = True}))) redirectRequest
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
                      }
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
                      }
              }
      response <- performWaiRequest (toWaiApplication (sampleApplicationWithConfig emptyStaticAssets requestPolicyConfig)) (waiRequest ["data"])
      Http.statusCode (Wai.responseStatus response) `shouldBe` 202
      lookup "Strict-Transport-Security" (Wai.responseHeaders response) `shouldBe` Nothing

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
            sampleApplication
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
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue]),
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
                             forwardedForAttribute,
                             forwardedProtoAttribute,
                             forwardedPrefixAttribute,
                             failureAttribute
                           ]
                       ]
      readIORef logEntriesReference
        `shouldReturn` [ "[client.address=\"203.0.113.10\" network.peer.address=\"127.0.0.1\" http.request.header.x_forwarded_for=\"203.0.113.10, 10.0.0.1\" http.request.header.x_forwarded_proto=\"https\" http.request.header.x_forwarded_prefix=\"/app\" url.scheme=\"https\"] Sample failure log"
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
            sampleApplication
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
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue]),
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
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue]),
                reportApplicationLog = \logEntry ->
                  modifyIORef' logEntriesReference (<> [logEntry])
              }
      response <- performWaiRequest (toWaiApplication diagnosticApplication) pageRequest
      Http.statusCode (Wai.responseStatus response) `shouldBe` 500
      Http.statusMessage (Wai.responseStatus response) `shouldBe` ""
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "text/html; charset=utf-8"
      readResponseBody response `shouldReturn` pageShell diagnosticApplication (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
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
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
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
          diagnosticApplication =
            sampleApplication
              { reportRequestObservability = \requestObservabilityValue ->
                  modifyIORef' requestObservabilityReference (<> [requestObservabilityValue])
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
                           [clientAddressAttribute, peerAddressAttribute, forwardedForAttribute]
                       ]

    it "keeps unmatched request paths in request span display names instead of collapsing them to the synthetic not-found route" $ do
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
      readIORef requestObservabilityReference
        `shouldReturn` [ Observability.buildRequestObservability
                           "GET"
                           "http"
                           "/favicon.ico"
                           "/404"
                           404
                           Observability.PageResponseKind
                           [clientAddressAttribute, peerAddressAttribute]
                       ]

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

    it "applies HSTS headers to static asset responses when the effective request scheme is HTTPS" $
      withSystemTempDirectory "harch-web-static-hsts" $ \tempDirectory -> do
        let assetDirectory = tempDirectory <> "/public"
            assetConfig =
              StaticAssetsConfig
                { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = assetDirectory}],
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
                        }
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
                  staticCacheControlSeconds = Nothing
                }
            prefixedRequest =
              Wai.defaultRequest
                { Wai.rawPathInfo = "/app/assets/app.js",
                  Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
                }
            staticApplication = sampleApplicationWithStaticAssets assetConfig
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
      show acmePlan `shouldBe` "AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeTlsEndpoint = Just (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}), acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}"
      show [acmePlan] `shouldBe` "[AcmeBindPlan {acmeEndpoint = ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}, acmeTlsEndpoint = Just (ListenerEndpoint {endpointHost = \"0.0.0.0\", endpointPort = 5444}), acmeListenerConfig = AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}}]"

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
                        acmeChallengeBackend = InProcessHttp01
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
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"exception.type\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""
        extractQuotedJsonField "traceId" requestBodyText
          `shouldSatisfy` maybe False (\traceId -> Text.length traceId == 32 && Text.all isHexDigit traceId)
        extractQuotedJsonField "spanId" requestBodyText
          `shouldSatisfy` maybe False (\spanId -> Text.length spanId == 16 && Text.all isHexDigit spanId)

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
                (Observability.buildRequestObservability "GET" "http" "/" "/" 200 Observability.BodyResponseKind [])
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
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""

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
                [acmeHttpsListener "127.0.0.1" 5443 InProcessHttp01]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime listeners do not have an HTTP port 80 challenge listener" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "0.0.0.0" 5001,
                  acmeHttpsListener "0.0.0.0" 5443 InProcessHttp01
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly when ACME runtime challenge listeners do not match the HTTPS host" $
      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
        let acmeTlsConfig =
              serverConfigWithListeners
                [ httpRuntimeListener "127.0.0.1" 80,
                  acmeHttpsListener "0.0.0.0" 5443 InProcessHttp01
                ]
        runServer outputHandle acmeTlsConfig sampleApplication
          `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 0.0.0.0:5443 requires an HTTP listener on port 80 for http-01 challenges.)")

    it "fails explicitly after resolving an exact-host ACME challenge listener without ACME domains" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
          let acmeTlsConfig =
                serverConfigWithListeners
                  [ httpRuntimeListener "127.0.0.1" challengePort,
                    acmeHttpsListenerWithDomainsAndChallengePort challengePort "127.0.0.1" 5443 ["ops@example.com"] [] InProcessHttp01
                  ]
          runServer outputHandle acmeTlsConfig sampleApplication
            `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Unsupported runtime listener startup plan: ACME listener on 127.0.0.1:5443 requires ACME domains for in-process http-01 runtime startup.)")

    it "starts in-process ACME listeners on the configured http-01 port and stays running until signalled to stop" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \acmePort ->
          withUnusedLoopbackPort $ \httpsPort ->
            withManualTlsFiles $ \certificatePath privateKeyPath ->
              withFakeOpenSslExecutable certificatePath privateKeyPath $
                \_ ->
                  withFakeAcmeServer acmePort challengePort certificatePath $
                    \directoryUrl ->
                      withSystemTempFile "harch-web-output.txt" $ \outputPath outputHandle -> do
                        completionReference <- newIORef Nothing
                        let acmeConfig =
                              AcmeConfig
                                { acmeDirectoryUrl = directoryUrl,
                                  acmeContactEmails = ["ops@example.com"],
                                  acmeDomains = ["loopback.example"],
                                  acmeHttp01Port = challengePort,
                                  acmeCertificateDirectory = Nothing,
                                  acmeChallengeBackend = InProcessHttp01
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
                            [ "HTTP Server listening at http://0.0.0.0:" <> show challengePort,
                              "HTTPS Server listening at https://127.0.0.1:" <> show httpsPort
                            ]

    it "lets shared HTTPS listeners reuse certificates issued by the in-process ACME backend" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \acmePort ->
          withUnusedLoopbackPort $ \sharedHttpsPort ->
            withManualTlsFiles $ \certificatePath privateKeyPath ->
              withFakeOpenSslExecutable certificatePath privateKeyPath $
                \_ ->
                  withFakeAcmeServer acmePort challengePort certificatePath $
                    \directoryUrl ->
                      withSystemTempDirectory "harch-web-shared-certs" $ \sharedDirectory ->
                        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                          completionReference <- newIORef Nothing
                          let acmeConfig =
                                AcmeConfig
                                  { acmeDirectoryUrl = directoryUrl,
                                    acmeContactEmails = ["ops@example.com"],
                                    acmeDomains = ["loopback.example"],
                                    acmeHttp01Port = challengePort,
                                    acmeCertificateDirectory = Just sharedDirectory,
                                    acmeChallengeBackend = InProcessHttp01
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
                          serverThreadId <- forkIO $ do
                            result <- try (runServer outputHandle runtimeConfig sampleApplication) :: IO (Either SomeException ())
                            writeIORef completionReference (Just result)
                          sharedResponseText <- waitForHttpsServerResponse completionReference sharedHttpsPort "/known"
                          Text.isInfixOf "<h1>Known</h1>" sharedResponseText `shouldBe` True
                          readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` manualTlsCertificatePem
                          readFile (sharedDirectory </> "privkey.pem") `shouldReturn` manualTlsPrivateKeyPem
                          killThread serverThreadId
                          waitForServerExit completionReference

    it "shuts down HTTP ACME producers that only publish certificates without starting HTTPS" $
      withUnusedLoopbackPort $ \challengePort ->
        withUnusedLoopbackPort $ \acmePort ->
          withManualTlsFiles $ \certificatePath privateKeyPath ->
            withFakeOpenSslExecutable certificatePath privateKeyPath $
              \_ ->
                withFakeAcmeServer acmePort challengePort certificatePath $
                  \directoryUrl ->
                    withSystemTempDirectory "harch-web-published-certs" $ \sharedDirectory ->
                      withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
                        completionReference <- newIORef Nothing
                        let acmeConfig =
                              AcmeConfig
                                { acmeDirectoryUrl = directoryUrl,
                                  acmeContactEmails = ["ops@example.com"],
                                  acmeDomains = ["loopback.example"],
                                  acmeHttp01Port = challengePort,
                                  acmeCertificateDirectory = Just sharedDirectory,
                                  acmeChallengeBackend = InProcessHttp01
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

    it "fails explicitly when in-process ACME listeners cannot launch openssl" $
      withUnusedLoopbackPort $ \challengePort ->
        withSystemTempFile "harch-web-output.txt" $ \_ outputHandle ->
          withEmptyExecutablePath $ do
            let acmeConfig =
                  AcmeConfig
                    { acmeDirectoryUrl = "http://127.0.0.1:14000/directory",
                      acmeContactEmails = ["ops@example.com"],
                      acmeDomains = ["loopback.example"],
                      acmeHttp01Port = challengePort,
                      acmeCertificateDirectory = Nothing,
                      acmeChallengeBackend = InProcessHttp01
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
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` (\exception -> "user error (Failed to launch openssl for ACME listener on 127.0.0.1:5443:" `isPrefixOf` show (exception :: IOError))

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
                    acmeChallengeBackend = InProcessHttp01
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
                    acmeChallengeBackend = InProcessHttp01
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
            "test \"$http_port\" = 80",
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
              challengeResponse <- acmeChallengeResponseForRequest challengeStoreValue challengeRequest
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
                  let certbotBackend =
                        certbotHttp01BackendWithExecutable
                          certbotExecutable
                          []
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
                  serverThreadId <- forkIO $ do
                    result <- try (runServer outputHandle acmeTlsConfig sampleApplication) :: IO (Either SomeException ())
                    writeIORef completionReference (Just result)
                  challengeResponseBytes <- waitForChallengeResponse (500 :: Int)
                  challengeResponseBytes `shouldSatisfy` ByteString.isInfixOf "loopback-token-response"
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
                    withSystemTempFile "harch-web-output.txt" $ \_ outputHandle -> do
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
                                acmeChallengeBackend = certbotBackend
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
            runServer outputHandle acmeTlsConfig sampleApplication
              `shouldThrow` (\exception -> show (exception :: IOError) == "user error (Certbot failed for ACME listener on 127.0.0.1:5443 with exit code ExitFailure 42.\nstdout:\n\nstderr:\nfake certbot failure\n)")

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

withPrependedPathDirectory :: FilePath -> IO a -> IO a
withPrependedPathDirectory pathDirectory action = do
  originalPath <- lookupEnv "PATH"
  let updatedPath =
        maybe
          pathDirectory
          (\currentPath -> pathDirectory <> ":" <> currentPath)
          originalPath
  setEnv "PATH" updatedPath
  action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

withEmptyExecutablePath :: IO a -> IO a
withEmptyExecutablePath action =
  withSystemTempDirectory "missing-executable-path" $ \tempDirectory -> do
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" tempDirectory
    action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

withFakeAcmeServer :: Int -> Int -> FilePath -> (Text -> IO a) -> IO a
withFakeAcmeServer acmePort challengePort certificatePath action = do
  challengeValidatedReference <- newIORef False
  orderFinalizedReference <- newIORef False
  certificateBytes <- ByteString.readFile certificatePath
  let baseUrl = "http://127.0.0.1:" <> show acmePort
      directoryUrl = Text.pack (baseUrl <> "/directory")
      nonceHeader = [("Replay-Nonce", "fake-nonce")]
      jsonHeaders = ("Content-Type", "application/json") : nonceHeader
      certificateHeaders = ("Content-Type", "application/pem-certificate-chain") : nonceHeader
      token = ("loopback-token" :: Text)
      challengeRequestSucceeded =
        either
          (const False)
          (const True)
          <$> readLoopbackHttpResponseBytesWithHostResult challengePort "loopback.example" "/.well-known/acme-challenge/loopback-token"
      fakeAcmeApplication request respond =
        case (Wai.requestMethod request, Wai.rawPathInfo request) of
          ("GET", "/directory") ->
            respond
              ( Wai.responseLBS
                  Http.ok200
                  jsonHeaders
                  ( LazyByteString.fromStrict . TextEncoding.encodeUtf8 . Text.pack $
                      "{\"newNonce\":\""
                        <> baseUrl
                        <> "/new-nonce\",\"newAccount\":\""
                        <> baseUrl
                        <> "/new-account\",\"newOrder\":\""
                        <> baseUrl
                        <> "/new-order\"}"
                  )
              )
          ("HEAD", "/new-nonce") ->
            respond (Wai.responseLBS Http.noContent204 nonceHeader LazyByteString.empty)
          ("POST", "/new-account") ->
            respond
              ( Wai.responseLBS
                  Http.created201
                  (("Location", ByteStringChar8.pack (baseUrl <> "/account/1")) : jsonHeaders)
                  "{}"
              )
          ("POST", "/new-order") ->
            respond
              ( Wai.responseLBS
                  Http.created201
                  (("Location", ByteStringChar8.pack (baseUrl <> "/order/1")) : jsonHeaders)
                  ( LazyByteString.fromStrict . TextEncoding.encodeUtf8 . Text.pack $
                      "{\"status\":\"pending\",\"authorizations\":[\""
                        <> baseUrl
                        <> "/authz/1\"],\"finalize\":\""
                        <> baseUrl
                        <> "/finalize/1\"}"
                  )
              )
          ("POST", "/authz/1") ->
            respond
              ( Wai.responseLBS
                  Http.ok200
                  jsonHeaders
                  ( LazyByteString.fromStrict . TextEncoding.encodeUtf8 . Text.pack $
                      "{\"identifier\":{\"type\":\"dns\",\"value\":\"loopback.example\"},\"challenges\":[{\"type\":\"http-01\",\"url\":\""
                        <> baseUrl
                        <> "/challenge/1\",\"token\":\""
                        <> Text.unpack token
                        <> "\"}]}"
                  )
              )
          ("POST", "/challenge/1") -> do
            challengeSucceeded <- challengeRequestSucceeded
            writeIORef challengeValidatedReference challengeSucceeded
            respond (Wai.responseLBS Http.ok200 jsonHeaders "{}")
          ("POST", "/order/1") -> do
            challengeValidated <- readIORef challengeValidatedReference
            orderFinalized <- readIORef orderFinalizedReference
            let orderBody
                  | challengeValidated && orderFinalized =
                      "{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}"
                  | challengeValidated =
                      "{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"
                  | otherwise =
                      "{\"status\":\"pending\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"
            respond
              ( Wai.responseLBS
                  Http.ok200
                  jsonHeaders
                  (LazyByteString.fromStrict (TextEncoding.encodeUtf8 (Text.pack orderBody)))
              )
          ("POST", "/finalize/1") ->
            writeIORef orderFinalizedReference True
              >> respond (Wai.responseLBS Http.ok200 jsonHeaders "{}")
          ("POST", "/cert/1") ->
            respond (Wai.responseLBS Http.ok200 certificateHeaders (LazyByteString.fromStrict certificateBytes))
          _ ->
            respond (Wai.responseLBS Http.notFound404 [] "not found")
  serverThreadId <- forkIO (Warp.run acmePort fakeAcmeApplication)
  threadDelay 50000
  action directoryUrl `finally` killThread serverThreadId

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
  if Text.null withField
    then Nothing
    else
      Just
        ( Text.takeWhile (/= '"') $
            Text.drop (Text.length fieldPrefix) withField
        )
  where
    fieldPrefix = "\"" <> fieldName <> "\":\""
    (_, withField) = Text.breakOn fieldPrefix bodyText

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
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: "
      <> Text.unpack hostHeader
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
