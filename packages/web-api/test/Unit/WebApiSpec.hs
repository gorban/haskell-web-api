{-# SPEC #-}

import Control.Exception (finally)
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified HarchWeb
import qualified Network.HTTP.Types as Http
import qualified Network.Wai as Wai
import qualified Network.Wai.Internal as WaiInternal
import System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess)
import WebApi (buildApp, run)
import WebApi.App (buildAppWithDatabase)
import WebApi.Config (AcmeChallengeBackend (..), AcmeConfig (..), AppConfig (..), AppEnvironmentConfig (..), AppMode (..), CertbotConfig (..), DatabaseConfig (..), ListenerConfig (..), ListenerScheme (..), ObservabilityConfig (..), OtlpExporter (..), StaticAssetRoot (..), StaticAssetsConfig (..), TlsCertificateSource (..), TlsConfig (..), committedEnvDefaults, committedRuntimeDefaults, defaultAppConfig, defaultAppEnvironmentConfig, parseAppEnvironmentConfig, parseRuntimeAppConfig)
import WebApi.Database (DatabaseEffect (..), DatabaseError (..), DatabaseSeed (..), HomePageData (..), SecondPageData (..), buildSeededDatabaseEffect, defaultDatabaseEffect, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), CallToAction (..), HomePageModel (..), NotFoundPageModel (..), SecondPageModel (..), buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase, renderPage, renderPageBody, renderPageFromRouteData, renderPageWithDatabase)
import WebApi.Postgres (PostgresCommand (..), PostgresCommandResult (..), PostgresRunnerError (..), buildPostgresDatabaseEffect, buildPostgresDatabaseEffectWithRunner, migrationStatements, runPostgresMigrations, runPostgresMigrationsWithRunner, runPostgresSeed, runPostgresSeedWithRunner, seedStatements)
import WebApi.Response (renderApiResponseFromRouteData, selectResponse, selectResponseWithDatabase)
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), RequestSurface (..), RouteSelectionError (..), defaultRequestContext, parseRoute, renderRoutePath, selectRoute)
import qualified WebApi.Route
import WebApi.RouteData (HomeRouteData (..), RouteDataResult (..), SecondRouteData (..), StatusApiData (..), selectRouteData, selectRouteDataWithDatabase)

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

frenchApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = frenchRequestContext {requestSurface = ApiSurface}
    }

frenchApiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
frenchApiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondRoute,
      HarchWeb.requestContext = frenchRequestContext {requestSurface = ApiSurface}
    }

notFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
notFoundRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = defaultRequestContext}

apiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

apiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

apiNotFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiNotFoundRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = NotFoundRoute,
      HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface}
    }

pureRouteMatcher :: Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
pureRouteMatcher = WebApi.Route.matchRoute WebApi.Route.defaultRequestContext

renderedShell :: AppConfig -> AppRoute -> IO Text
renderedShell config route = do
  let application = buildApp config
  page <- renderPage config (HarchWeb.RouteRequest {HarchWeb.requestRoute = route, HarchWeb.requestContext = defaultRequestContext})
  pure (HarchWeb.pageShell application page)

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

postgresTestConfig :: DatabaseConfig
postgresTestConfig =
  DatabaseConfig
    { databaseHost = Text.pack "db.internal",
      databasePort = 6543,
      databaseName = Text.pack "web_api_prod",
      databaseUser = Text.pack "web_api_app",
      databasePassword = Text.pack "super-secret"
    }

successfulPostgresResult :: Text -> PostgresCommandResult
successfulPostgresResult stdoutText =
  PostgresCommandResult
    { postgresExitCode = ExitSuccess,
      postgresStdout = stdoutText,
      postgresStderr = Text.empty
    }

failingPostgresResult :: Text -> PostgresCommandResult
failingPostgresResult stderrText =
  PostgresCommandResult
    { postgresExitCode = ExitFailure 1,
      postgresStdout = Text.empty,
      postgresStderr = stderrText
    }

commandSql :: PostgresCommand -> Text
commandSql command =
  case reverse (postgresArguments command) of
    sqlArgument : _ -> Text.pack sqlArgument
    [] -> Text.empty

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  previousValue <- lookupEnv key
  case maybeValue of
    Just value -> setEnv key value
    Nothing -> unsetEnv key
  let restore =
        case previousValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  action `finally` restore

withFakePsqlScriptResults :: [(Text, PostgresCommandResult)] -> (FilePath -> IO a) -> IO a
withFakePsqlScriptResults commandResults action =
  withSystemTempDirectory "fake-psql" $ \tempDirectory -> do
    originalPath <- getEnv "PATH"
    let scriptPath = tempDirectory <> "/psql"
        argsLogPath = tempDirectory <> "/psql-args.log"
        scriptBody =
          unlines
            ( [ "#!/usr/bin/env bash",
                "set -euo pipefail",
                "printf '%s\\n' \"$*\" >> \"$PSQL_ARGS_LOG\"",
                "sql=''",
                "while [ \"$#\" -gt 0 ]; do",
                "  case \"$1\" in",
                "    --command)",
                "      sql=\"$2\"",
                "      shift 2",
                "      ;;",
                "    *)",
                "      shift",
                "      ;;",
                "  esac",
                "done",
                "case \"$sql\" in"
              ]
                ++ concatMap renderCase commandResults
                ++ [ "  *)",
                     "    exit 0",
                     "    ;;",
                     "esac"
                   ]
            )
    writeFile scriptPath scriptBody
    callProcess "chmod" ["+x", scriptPath]
    withTemporaryEnvironment "PSQL_ARGS_LOG" (Just argsLogPath) $
      withTemporaryEnvironment "PATH" (Just (tempDirectory <> ":" <> originalPath)) $
        action argsLogPath
  where
    renderCase (sqlText, commandResult) =
      [ "  " <> show (Text.unpack sqlText) <> ")"
      ]
        ++ renderStdoutLines (postgresStdout commandResult)
        ++ renderStderrLines (postgresStderr commandResult)
        ++ [ "    exit " <> renderExitCode (postgresExitCode commandResult),
             "    ;;"
           ]

    renderStdoutLines stdoutText =
      case Text.unpack stdoutText of
        "" -> []
        stdoutValue -> ["    printf %s\\\\n " <> show stdoutValue]

    renderStderrLines stderrText =
      case Text.unpack stderrText of
        "" -> []
        stderrValue -> ["    printf %s\\\\n " <> show stderrValue <> " >&2"]

    renderExitCode exitCode =
      case exitCode of
        ExitSuccess -> "0"
        ExitFailure code -> show code

withFakePsqlScript :: [(Text, Text)] -> (FilePath -> IO a) -> IO a
withFakePsqlScript commandOutputs =
  withFakePsqlScriptResults
    (map toSuccessfulCommandResult commandOutputs)
  where
    toSuccessfulCommandResult (sqlText, stdoutText) =
      (sqlText, successfulPostgresResult stdoutText)

spec = do
  describe "defaultAppConfig" $ do
    it "reserves structured listener, static asset, and observability settings" $ do
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
      let serverConfig = HarchWeb.toServerConfig defaultAppConfig
      HarchWeb.listenerConfigs serverConfig `shouldBe` listenerConfigs defaultAppConfig
      HarchWeb.staticAssets serverConfig `shouldBe` staticAssets defaultAppConfig
      HarchWeb.observability serverConfig `shouldBe` observability defaultAppConfig

  describe "parseRuntimeAppConfig" $ do
    it "parses committed runtime defaults into the expected app config" $
      parseRuntimeAppConfig committedRuntimeDefaults [] []
        `shouldBe` Right defaultAppConfig

    it "fails when no listeners are configured" $
      parseRuntimeAppConfig
        [(Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test")]
        []
        []
        `shouldBe` Left (MissingConfigValue (Text.pack "LISTENER_0_HOST"))

    it "parses multiple listeners in deterministic index order" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_2_SCHEME", Text.pack "http"),
              (Text.pack "LISTENER_1_PORT", Text.pack "5002"),
              (Text.pack "LISTENER_2_PORT", Text.pack "5003"),
              (Text.pack "LISTENER_1_HOST", Text.pack "127.0.0.2"),
              (Text.pack "LISTENER_2_HOST", Text.pack "127.0.0.3"),
              (Text.pack "LISTENER_1_SCHEME", Text.pack "http")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "127.0.0.2",
                      listenerPort = 5002,
                      listenerScheme = Http,
                      listenerTls = Nothing
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "127.0.0.3",
                      listenerPort = 5003,
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

    it "requires HTTPS listeners to specify a TLS source" $
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue (Text.pack "LISTENER_0_TLS_SOURCE"))

    it "parses manual and ACME-backed HTTPS listeners distinctly" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_BAD_HOST", Text.pack "ignored-host"),
              (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
              (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "manual"),
              (Text.pack "LISTENER_0_TLS_CERTIFICATE_FILE", Text.pack "cert.pem"),
              (Text.pack "LISTENER_0_TLS_PRIVATE_KEY_FILE", Text.pack "key.pem"),
              (Text.pack "LISTENER_1_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_1_PORT", Text.pack "5444"),
              (Text.pack "LISTENER_1_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_1_TLS_SOURCE", Text.pack "acme"),
              (Text.pack "LISTENER_1_ACME_DIRECTORY_URL", Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory"),
              (Text.pack "LISTENER_1_ACME_CONTACT_EMAILS", Text.pack "ops@example.com,alerts@example.com"),
              (Text.pack "LISTENER_1_ACME_CHALLENGE_BACKEND", Text.pack "in-process-http01"),
              (Text.pack "LISTENER_2_HOST", Text.pack "0.0.0.0"),
              (Text.pack "LISTENER_2_PORT", Text.pack "5445"),
              (Text.pack "LISTENER_2_SCHEME", Text.pack "https"),
              (Text.pack "LISTENER_2_TLS_SOURCE", Text.pack "acme"),
              (Text.pack "LISTENER_2_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
              (Text.pack "LISTENER_2_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
              (Text.pack "LISTENER_2_ACME_CHALLENGE_BACKEND", Text.pack "certbot-http01"),
              (Text.pack "LISTENER_2_ACME_CERTBOT_EXECUTABLE", Text.pack "certbot"),
              (Text.pack "LISTENER_2_ACME_CERTBOT_ARGUMENTS", Text.pack "certonly,--webroot,--agree-tos")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "cert.pem",
                                    privateKeyFile = "key.pem"
                                  }
                            }
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com", Text.pack "alerts@example.com"],
                                      acmeChallengeBackend = InProcessHttp01
                                    }
                            }
                    },
                  ListenerConfig
                    { listenerHost = Text.pack "0.0.0.0",
                      listenerPort = 5445,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com"],
                                      acmeChallengeBackend =
                                        CertbotHttp01
                                          CertbotConfig
                                            { certbotExecutable = "certbot",
                                              certbotArguments = [Text.pack "certonly", Text.pack "--webroot", Text.pack "--agree-tos"]
                                            }
                                    }
                            }
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

    it "rejects invalid listener scheme and TLS source values" $ do
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "tcp")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "LISTENER_0_SCHEME") (Text.pack "tcp"))
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "0.0.0.0"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5443"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "vault")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "LISTENER_0_TLS_SOURCE") (Text.pack "vault"))

    it "parses static asset roots and cache policy into the expected config" $ do
      let committedDefaults =
            [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
              (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
              (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
              (Text.pack "LISTENER_0_SCHEME", Text.pack "http"),
              (Text.pack "STATIC_ASSET_ROOT_2_DIRECTORY", Text.pack "vendor/public"),
              (Text.pack "STATIC_ASSET_ROOT_1_URL_PREFIX", Text.pack "/assets"),
              (Text.pack "STATIC_ASSET_ROOT_2_URL_PREFIX", Text.pack "/vendor"),
              (Text.pack "STATIC_ASSET_ROOT_1_DIRECTORY", Text.pack "public"),
              (Text.pack "STATIC_CACHE_CONTROL_SECONDS", Text.pack "3600")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
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
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = Text.pack "/assets",
                            staticDirectory = "public"
                          },
                        StaticAssetRoot
                          { staticUrlPrefix = Text.pack "/vendor",
                            staticDirectory = "vendor/public"
                          }
                      ],
                    staticCacheControlSeconds = Just 3600
                  },
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "parses tracing and metrics exporters independently while preserving header order" $ do
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ (Text.pack "OTLP_TRACING_ENDPOINT", Text.pack "http://collector:4318/v1/traces"),
          (Text.pack "OTLP_TRACING_HEADERS", Text.pack "authorization=Bearer token;x-api-key=secret")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                            otlpHeaders =
                              [ (Text.pack "authorization", Text.pack "Bearer token"),
                                (Text.pack "x-api-key", Text.pack "secret")
                              ]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [(Text.pack "OTLP_TRACING_ENDPOINT", Text.pack "http://collector:4318/v1/traces")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ (Text.pack "OTLP_METRICS_ENDPOINT", Text.pack "http://collector:4318/v1/metrics"),
          (Text.pack "OTLP_METRICS_HEADERS", Text.pack "x-scope=metrics;broken-entry")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = Text.pack "http://collector:4318/v1/metrics",
                            otlpHeaders = [(Text.pack "x-scope", Text.pack "metrics")]
                          }
                  }
            }

    it "fails invalid runtime values with explicit errors" $ do
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "0"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "http")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "LISTENER_0_PORT") (Text.pack "0"))
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack ""),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "shell-script")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS") (Text.pack ""))
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "shell-script")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND") (Text.pack "shell-script"))
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "https"),
          (Text.pack "LISTENER_0_TLS_SOURCE", Text.pack "acme"),
          (Text.pack "LISTENER_0_ACME_DIRECTORY_URL", Text.pack "https://acme-v02.api.letsencrypt.org/directory"),
          (Text.pack "LISTENER_0_ACME_CONTACT_EMAILS", Text.pack "ops@example.com"),
          (Text.pack "LISTENER_0_ACME_CHALLENGE_BACKEND", Text.pack "certbot-http01"),
          (Text.pack "LISTENER_0_ACME_CERTBOT_EXECUTABLE", Text.pack "certbot")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = Text.pack "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = Text.pack "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = [Text.pack "ops@example.com"],
                                      acmeChallengeBackend =
                                        CertbotHttp01
                                          CertbotConfig
                                            { certbotExecutable = "certbot",
                                              certbotArguments = []
                                            }
                                    }
                            }
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
      parseRuntimeAppConfig
        [ (Text.pack "APP_TITLE_PREFIX", Text.pack "runtime-test"),
          (Text.pack "LISTENER_0_HOST", Text.pack "127.0.0.1"),
          (Text.pack "LISTENER_0_PORT", Text.pack "5001"),
          (Text.pack "LISTENER_0_SCHEME", Text.pack "http"),
          (Text.pack "STATIC_CACHE_CONTROL_SECONDS", Text.pack "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue (Text.pack "STATIC_CACHE_CONTROL_SECONDS") (Text.pack "-1"))
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [(Text.pack "OTLP_TRACING_HEADERS", Text.pack "authorization=Bearer token")]
        `shouldBe` Left (MissingConfigValue (Text.pack "OTLP_TRACING_ENDPOINT"))

  describe "defaultAppEnvironmentConfig" $ do
    it "keeps committed .env defaults aligned with the parsed development config" $ do
      committedEnvDefaults
        `shouldBe` [ (Text.pack "APP_MODE", Text.pack "development"),
                     (Text.pack "DATABASE_HOST", Text.pack "127.0.0.1"),
                     (Text.pack "DATABASE_PORT", Text.pack "5432"),
                     (Text.pack "DATABASE_NAME", Text.pack "web_api_dev"),
                     (Text.pack "DATABASE_USER", Text.pack "web_api"),
                     (Text.pack "DATABASE_PASSWORD", Text.pack "web_api")
                   ]
      defaultAppEnvironmentConfig
        `shouldBe` AppEnvironmentConfig
          { appMode = Development,
            databaseConfig =
              DatabaseConfig
                { databaseHost = Text.pack "127.0.0.1",
                  databasePort = 5432,
                  databaseName = Text.pack "web_api_dev",
                  databaseUser = Text.pack "web_api",
                  databasePassword = Text.pack "web_api"
                }
          }

    it "covers the new app/database config selectors and derived instances" $ do
      let productionDatabaseConfig =
            DatabaseConfig
              { databaseHost = Text.pack "db.internal",
                databasePort = 6543,
                databaseName = Text.pack "web_api_prod",
                databaseUser = Text.pack "web_api_app",
                databasePassword = Text.pack "super-secret"
              }
          productionEnvironmentConfig =
            AppEnvironmentConfig
              { appMode = Production,
                databaseConfig = productionDatabaseConfig
              }
      appMode productionEnvironmentConfig `shouldBe` Production
      databaseConfig productionEnvironmentConfig `shouldBe` productionDatabaseConfig
      databaseHost productionDatabaseConfig `shouldBe` Text.pack "db.internal"
      databasePort productionDatabaseConfig `shouldBe` 6543
      databaseName productionDatabaseConfig `shouldBe` Text.pack "web_api_prod"
      databaseUser productionDatabaseConfig `shouldBe` Text.pack "web_api_app"
      databasePassword productionDatabaseConfig `shouldBe` Text.pack "super-secret"
      Development `shouldNotBe` Test
      Test `shouldNotBe` Production
      productionDatabaseConfig `shouldBe` productionDatabaseConfig
      productionDatabaseConfig
        `shouldNotBe` productionDatabaseConfig
          { databasePassword = Text.pack "different-secret"
          }
      productionEnvironmentConfig `shouldBe` productionEnvironmentConfig
      productionEnvironmentConfig
        `shouldNotBe` productionEnvironmentConfig
          { appMode = Test
          }
      MissingConfigValue (Text.pack "DATABASE_PASSWORD")
        `shouldNotBe` InvalidConfigValue (Text.pack "DATABASE_PASSWORD") (Text.pack "missing")
      show Development `shouldBe` "Development"
      show Test `shouldBe` "Test"
      show Production `shouldBe` "Production"
      show [Development, Test, Production] `shouldBe` "[Development,Test,Production]"
      show productionDatabaseConfig
        `shouldBe` "DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}"
      show [productionDatabaseConfig]
        `shouldBe` "[DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}]"
      show productionEnvironmentConfig
        `shouldBe` "AppEnvironmentConfig {appMode = Production, databaseConfig = DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}}"
      show [productionEnvironmentConfig]
        `shouldBe` "[AppEnvironmentConfig {appMode = Production, databaseConfig = DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}}]"
      show (MissingConfigValue (Text.pack "DATABASE_PASSWORD")) `shouldBe` "MissingConfigValue \"DATABASE_PASSWORD\""
      show (InvalidConfigValue (Text.pack "APP_MODE") (Text.pack "staging")) `shouldBe` "InvalidConfigValue \"APP_MODE\" \"staging\""
      show [MissingConfigValue (Text.pack "DATABASE_PASSWORD"), InvalidConfigValue (Text.pack "APP_MODE") (Text.pack "staging")]
        `shouldBe` "[MissingConfigValue \"DATABASE_PASSWORD\",InvalidConfigValue \"APP_MODE\" \"staging\"]"

  describe "defaultDatabaseSeed" $ do
    it "defines deterministic page-facing seeded results for both locales" $
      defaultDatabaseSeed
        `shouldBe` DatabaseSeed
          { englishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = Text.pack "Server-rendered home page with stubbed content."
                  },
            frenchHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = Text.pack "Accueil cote serveur avec des donnees de developpement preconfigurees."
                  },
            englishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            frenchSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  }
          }

    it "keeps seeded database data serializable and stable for tests" $ do
      let homePageData = HomePageData {homePageDataSummary = Text.pack "Seeded home"}
          otherHomePageData = HomePageData {homePageDataSummary = Text.pack "Different home"}
          secondPageData =
            SecondPageData
              { secondPageDataSummary = Text.pack "Seeded second",
                secondPageDataHighlights = [Text.pack "One"]
              }
          otherSecondPageData =
            SecondPageData
              { secondPageDataSummary = Text.pack "Other second",
                secondPageDataHighlights = []
              }
          homeError = HomePageDataError (Text.pack "home unavailable")
          secondError = SecondPageDataError (Text.pack "second unavailable")
          seededDatabase =
            DatabaseSeed
              { englishHomePageData = Right homePageData,
                frenchHomePageData = Left homeError,
                englishSecondPageData = Right secondPageData,
                frenchSecondPageData = Left secondError
              }
      homePageData `shouldBe` homePageData
      homePageData `shouldNotBe` otherHomePageData
      secondPageData `shouldBe` secondPageData
      secondPageData `shouldNotBe` otherSecondPageData
      homeError `shouldBe` homeError
      homeError `shouldNotBe` secondError
      seededDatabase `shouldBe` seededDatabase
      seededDatabase
        `shouldNotBe` seededDatabase
          { frenchSecondPageData = Right otherSecondPageData
          }
      show (HomePageData {homePageDataSummary = Text.pack "Seeded home"})
        `shouldBe` "HomePageData {homePageDataSummary = \"Seeded home\"}"
      show (SecondPageData {secondPageDataSummary = Text.pack "Seeded second", secondPageDataHighlights = [Text.pack "One"]})
        `shouldBe` "SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}"
      show (HomePageDataError (Text.pack "home unavailable"))
        `shouldBe` "HomePageDataError \"home unavailable\""
      show (SecondPageDataError (Text.pack "second unavailable"))
        `shouldBe` "SecondPageDataError \"second unavailable\""
      show seededDatabase
        `shouldBe` "DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), frenchHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), frenchSecondPageData = Left (SecondPageDataError \"second unavailable\")}"
      show [HomePageData {homePageDataSummary = Text.pack "Seeded home"}]
        `shouldBe` "[HomePageData {homePageDataSummary = \"Seeded home\"}]"
      show [homeError, secondError]
        `shouldBe` "[HomePageDataError \"home unavailable\",SecondPageDataError \"second unavailable\"]"
      show
        [ SecondPageData
            { secondPageDataSummary = Text.pack "Seeded second",
              secondPageDataHighlights = [Text.pack "One"]
            }
        ]
        `shouldBe` "[SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}]"
      show [seededDatabase]
        `shouldBe` "[DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), frenchHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), frenchSecondPageData = Left (SecondPageDataError \"second unavailable\")}]"

  describe "buildSeededDatabaseEffect" $ do
    it "loads page-oriented seeded data for both English and French requests" $ do
      let englishEffect = buildSeededDatabaseEffect defaultDatabaseSeed
      loadHomePageData englishEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = Text.pack "Server-rendered home page with stubbed content."
            }
      loadSecondPageData englishEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadHomePageData englishEffect frenchRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = Text.pack "Accueil cote serveur avec des donnees de developpement preconfigurees."
            }
      loadSecondPageData englishEffect frenchRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }

    it "returns explicit seeded errors without collapsing page-specific failures" $ do
      let seededEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError (Text.pack "home seed unavailable")),
                  frenchHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = Text.pack "Accueil seede"
                        },
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = Text.pack "Second seed",
                          secondPageDataHighlights = [Text.pack "Known branch"]
                        },
                  frenchSecondPageData = Left (SecondPageDataError (Text.pack "second seed unavailable"))
                }
      loadHomePageData seededEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError (Text.pack "home seed unavailable"))
      loadSecondPageData seededEffect frenchRequestContext
        `shouldReturn` Left (SecondPageDataError (Text.pack "second seed unavailable"))

    it "keeps the default seeded interpreter deterministic for repeated requests" $ do
      firstHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      secondHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      firstHome `shouldBe` secondHome
      firstSecond <- loadSecondPageData defaultDatabaseEffect frenchRequestContext
      secondSecond <- loadSecondPageData defaultDatabaseEffect frenchRequestContext
      firstSecond `shouldBe` secondSecond

  describe "selectRouteData" $ do
    it "selects the same second-route domain data for page and API surfaces" $ do
      let seededDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = Text.pack "Shared domain summary.",
                          secondPageDataHighlights = [Text.pack "Shared loader", Text.pack "Shared renderer"]
                        },
                  frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
                }
      selectedRouteData <- selectRouteDataWithDatabase seededDatabaseEffect secondRequest
      selectedRouteData
        `shouldBe` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = Text.pack "Shared domain summary.",
                  secondRouteHighlights = [Text.pack "Shared loader", Text.pack "Shared renderer"]
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect apiSecondRequest `shouldReturn` selectedRouteData

    it "keeps route-data selectors and derived instances deterministic for tests" $ do
      let homeRouteData =
            HomeRouteData
              { homeRouteSummary = Text.pack "Stubbed home summary"
              }
          otherHomeRouteData =
            HomeRouteData
              { homeRouteSummary = Text.pack "Different home summary"
              }
          secondRouteData =
            SecondRouteData
              { secondRouteSummary = Text.pack "Shared domain summary",
                secondRouteHighlights = [Text.pack "Shared loader"]
              }
          statusApiData =
            StatusApiData
              { statusApiLocale = French
              }
          routeDataResult = HomeRouteDataResult homeRouteData
      homeRouteSummary homeRouteData `shouldBe` Text.pack "Stubbed home summary"
      secondRouteSummary secondRouteData `shouldBe` Text.pack "Shared domain summary"
      secondRouteHighlights secondRouteData `shouldBe` [Text.pack "Shared loader"]
      statusApiLocale statusApiData `shouldBe` French
      homeRouteData `shouldBe` homeRouteData
      homeRouteData `shouldNotBe` otherHomeRouteData
      secondRouteData `shouldNotBe` secondRouteData {secondRouteHighlights = []}
      statusApiData `shouldBe` statusApiData
      statusApiData `shouldNotBe` StatusApiData {statusApiLocale = English}
      routeDataResult `shouldBe` routeDataResult
      routeDataResult `shouldNotBe` NotFoundRouteDataResult
      show homeRouteData `shouldBe` "HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}"
      show secondRouteData
        `shouldBe` "SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}"
      show statusApiData `shouldBe` "StatusApiData {statusApiLocale = French}"
      show routeDataResult
        `shouldBe` "HomeRouteDataResult (HomeRouteData {homeRouteSummary = \"Stubbed home summary\"})"
      show (SecondRouteDataResult (Right secondRouteData))
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show (StatusApiDataResult statusApiData)
        `shouldBe` "StatusApiDataResult (StatusApiData {statusApiLocale = French})"
      show [homeRouteData] `shouldBe` "[HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}]"
      show [secondRouteData]
        `shouldBe` "[SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}]"
      show [statusApiData] `shouldBe` "[StatusApiData {statusApiLocale = French}]"
      show [NotFoundRouteDataResult] `shouldBe` "[NotFoundRouteDataResult]"

    it "selects default stubbed and status route data without extra wiring" $ do
      selectRouteData homeRequest
        `shouldReturn` HomeRouteDataResult
          HomeRouteData
            { homeRouteSummary = Text.pack "Server-rendered home page with stubbed content."
            }
      selectRouteData secondRequest
        `shouldReturn` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                  secondRouteHighlights = []
                }
          )
      selectRouteData frenchApiStatusRequest
        `shouldReturn` StatusApiDataResult
          StatusApiData
            { statusApiLocale = French
            }
      selectRouteData apiNotFoundRequest `shouldReturn` NotFoundRouteDataResult

  describe "WebApi.Postgres" $ do
    it "translates database config into psql commands for page queries" $ do
      recordedCommandsReference <- newIORef []
      let runner command = do
            modifyIORef' recordedCommandsReference (<> [command])
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf (Text.pack "route_slug = 'home'") sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf (Text.pack "locale = 'fr'") sql
                          then Text.pack "Accueil cote serveur avec des donnees de developpement preconfigurees."
                          else Text.pack "Server-rendered home page with stubbed content."
                  | Text.isInfixOf (Text.pack "SELECT summary FROM page_content WHERE route_slug = 'second'") sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf (Text.pack "locale = 'fr'") sql
                          then Text.pack "Charge depuis PostgreSQL."
                          else Text.pack "Loaded from PostgreSQL."
                  | Text.isInfixOf (Text.pack "SELECT highlight FROM page_highlights") sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf (Text.pack "locale = 'fr'") sql
                          then Text.pack "SSR rapide\nDonnees partagees"
                          else Text.pack "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult (Text.pack "unexpected query")
          postgresEffect = buildPostgresDatabaseEffectWithRunner runner postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = Text.pack "Server-rendered home page with stubbed content."
            }
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Loaded from PostgreSQL.",
              secondPageDataHighlights = [Text.pack "Fast SSR", Text.pack "Shared route data"]
            }
      loadHomePageData postgresEffect frenchRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = Text.pack "Accueil cote serveur avec des donnees de developpement preconfigurees."
            }
      loadSecondPageData postgresEffect frenchRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Charge depuis PostgreSQL.",
              secondPageDataHighlights = [Text.pack "SSR rapide", Text.pack "Donnees partagees"]
            }
      recordedCommands <- readIORef recordedCommandsReference
      recordedCommands
        `shouldBe` [ PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT summary FROM page_content WHERE route_slug = 'home' AND locale = 'en';"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       },
                     PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT summary FROM page_content WHERE route_slug = 'second' AND locale = 'en';"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       },
                     PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT highlight FROM page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       },
                     PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT summary FROM page_content WHERE route_slug = 'home' AND locale = 'fr';"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       },
                     PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT summary FROM page_content WHERE route_slug = 'second' AND locale = 'fr';"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       },
                     PostgresCommand
                       { postgresExecutable = "psql",
                         postgresArguments =
                           [ "--host",
                             "db.internal",
                             "--port",
                             "6543",
                             "--dbname",
                             "web_api_prod",
                             "--username",
                             "web_api_app",
                             "--no-password",
                             "--set",
                             "ON_ERROR_STOP=1",
                             "--tuples-only",
                             "--no-align",
                             "--quiet",
                             "--command",
                             "SELECT highlight FROM page_highlights WHERE route_slug = 'second' AND locale = 'fr' ORDER BY position ASC;"
                           ],
                         postgresEnvironment = [("PGPASSWORD", "super-secret")]
                       }
                   ]

    it "maps missing rows and command failures into database errors" $ do
      let missingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf (Text.pack "route_slug = 'home'") sql ->
                      successfulPostgresResult Text.empty
                  | otherwise ->
                      failingPostgresResult (Text.pack "relation does not exist")
          postgresEffect = buildPostgresDatabaseEffectWithRunner missingRunner postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError (Text.pack "expected exactly one row: "))
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError (Text.pack "relation does not exist"))

    it "maps scalar query failures, malformed rows, and highlight query failures into explicit errors" $ do
      let homeFailureRunner command =
            pure $
              if Text.isInfixOf (Text.pack "route_slug = 'home'") (commandSql command)
                then
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 2,
                      postgresStdout = Text.empty,
                      postgresStderr = Text.empty
                    }
                else successfulPostgresResult Text.empty
          malformedScalarRunner command =
            pure $
              if Text.isInfixOf (Text.pack "route_slug = 'home'") (commandSql command)
                then successfulPostgresResult (Text.pack "first\nsecond")
                else successfulPostgresResult Text.empty
          highlightFailureRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf (Text.pack "SELECT summary FROM page_content WHERE route_slug = 'second'") sql ->
                      successfulPostgresResult (Text.pack "Loaded from PostgreSQL.")
                  | Text.isInfixOf (Text.pack "SELECT highlight FROM page_highlights") sql ->
                      failingPostgresResult (Text.pack "highlights unavailable")
                  | otherwise ->
                      successfulPostgresResult Text.empty
      loadHomePageData (buildPostgresDatabaseEffectWithRunner homeFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError (Text.pack "psql command failed"))
      loadHomePageData (buildPostgresDatabaseEffectWithRunner malformedScalarRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError (Text.pack "expected exactly one row: first, second"))
      loadSecondPageData (buildPostgresDatabaseEffectWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (SecondPageDataError (Text.pack "highlights unavailable"))

    it "runs migrations and seed statements in order through the provided runner" $ do
      recordedCommandsReference <- newIORef []
      let runner command = modifyIORef' recordedCommandsReference (<> [command]) >> pure (successfulPostgresResult Text.empty)
      runPostgresMigrationsWithRunner runner postgresTestConfig `shouldReturn` Right ()
      runPostgresSeedWithRunner runner postgresTestConfig `shouldReturn` Right ()
      recordedCommands <- readIORef recordedCommandsReference
      map commandSql recordedCommands `shouldBe` migrationStatements <> seedStatements

    it "stops database setup when a migration or seed command fails" $ do
      case seedStatements of
        failingSeedStatement : _ -> do
          let runner command =
                pure $
                  if commandSql command == failingSeedStatement
                    then failingPostgresResult (Text.pack "seed failed")
                    else successfulPostgresResult Text.empty
          runPostgresSeedWithRunner runner postgresTestConfig
            `shouldReturn` Left
              ( PostgresCommandFailed
                  PostgresCommand
                    { postgresExecutable = "psql",
                      postgresArguments =
                        [ "--host",
                          "db.internal",
                          "--port",
                          "6543",
                          "--dbname",
                          "web_api_prod",
                          "--username",
                          "web_api_app",
                          "--no-password",
                          "--set",
                          "ON_ERROR_STOP=1",
                          "--command",
                          "DELETE FROM page_highlights;"
                        ],
                      postgresEnvironment = [("PGPASSWORD", "super-secret")]
                    }
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 1,
                      postgresStdout = Text.empty,
                      postgresStderr = Text.pack "seed failed"
                    }
              )
        [] -> expectationFailure "expected at least one seed statement"

    it "keeps postgres command, result, and error values serializable and stable" $ do
      let command =
            PostgresCommand
              { postgresExecutable = "psql",
                postgresArguments = ["--command", "SELECT 1;"],
                postgresEnvironment = [("PGPASSWORD", "secret")]
              }
          commandResult =
            PostgresCommandResult
              { postgresExitCode = ExitSuccess,
                postgresStdout = Text.pack "1",
                postgresStderr = Text.empty
              }
          failedCommandResult =
            PostgresCommandResult
              { postgresExitCode = ExitFailure 3,
                postgresStdout = Text.empty,
                postgresStderr = Text.pack "boom"
              }
          runnerError = PostgresCommandFailed command commandResult
          unexpectedRowsError = UnexpectedQueryRows (Text.pack "expected exactly one row") [Text.pack "first", Text.pack "second"]
      command `shouldBe` command
      command `shouldNotBe` command {postgresArguments = ["--command", "SELECT 2;"]}
      commandResult `shouldBe` commandResult
      commandResult `shouldNotBe` commandResult {postgresStdout = Text.pack "2"}
      runnerError `shouldBe` runnerError
      runnerError `shouldNotBe` PostgresCommandFailed command failedCommandResult
      unexpectedRowsError `shouldBe` unexpectedRowsError
      unexpectedRowsError `shouldNotBe` UnexpectedQueryRows (Text.pack "expected exactly one row") [Text.pack "first"]
      show command
        `shouldBe` "PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}"
      show commandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}"
      show failedCommandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitFailure 3, postgresStdout = \"\", postgresStderr = \"boom\"}"
      show runnerError
        `shouldBe` "PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})"
      show unexpectedRowsError
        `shouldBe` "UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"]"
      show [command]
        `shouldBe` "[PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}]"
      show [commandResult]
        `shouldBe` "[PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}]"
      show [runnerError]
        `shouldBe` "[PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = [\"--command\",\"SELECT 1;\"], postgresEnvironment = [(\"PGPASSWORD\",\"secret\")]}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})]"

    it "uses the default psql runner for effect loading and database setup when psql is on PATH"
      $ withFakePsqlScript
        [ (Text.pack "SELECT summary FROM page_content WHERE route_slug = 'home' AND locale = 'en';", Text.pack "Server-rendered home page with stubbed content."),
          (Text.pack "SELECT summary FROM page_content WHERE route_slug = 'second' AND locale = 'en';", Text.pack "Second page content with stubbed data ready for future loaders."),
          (Text.pack "SELECT highlight FROM page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;", Text.empty),
          (Text.pack "CREATE TABLE IF NOT EXISTS page_content (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));", Text.empty),
          (Text.pack "CREATE TABLE IF NOT EXISTS page_highlights (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));", Text.empty),
          (Text.pack "DELETE FROM page_highlights;", Text.empty),
          (Text.pack "DELETE FROM page_content;", Text.empty),
          (Text.pack "INSERT INTO page_content (route_slug, locale, summary) VALUES ('home', 'en', 'Server-rendered home page with stubbed content.'), ('home', 'fr', 'Accueil cote serveur avec des donnees de developpement preconfigurees.'), ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'fr', 'Second page content with stubbed data ready for future loaders.');", Text.empty)
        ]
      $ \argsLogPath -> do
        let application = buildAppWithDatabase defaultAppConfig (buildPostgresDatabaseEffect postgresTestConfig)
        HarchWeb.renderResponse application secondRequest
          `shouldReturn` HarchWeb.PageResponse
            ( HarchWeb.Page
                { HarchWeb.pageTitle = Text.pack "web-api: Second",
                  HarchWeb.pageRoute = SecondRoute,
                  HarchWeb.pageContext = defaultRequestContext,
                  HarchWeb.pageBody = Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
                }
            )
        runPostgresMigrations postgresTestConfig `shouldReturn` Right ()
        runPostgresSeed postgresTestConfig `shouldReturn` Right ()
        readFile argsLogPath
          `shouldReturn` unlines
            [ "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --tuples-only --no-align --quiet --command SELECT summary FROM page_content WHERE route_slug = 'second' AND locale = 'en';",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --tuples-only --no-align --quiet --command SELECT highlight FROM page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --command CREATE TABLE IF NOT EXISTS page_content (route_slug TEXT NOT NULL, locale TEXT NOT NULL, summary TEXT NOT NULL, PRIMARY KEY (route_slug, locale));",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --command CREATE TABLE IF NOT EXISTS page_highlights (route_slug TEXT NOT NULL, locale TEXT NOT NULL, position INTEGER NOT NULL, highlight TEXT NOT NULL, PRIMARY KEY (route_slug, locale, position));",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --command DELETE FROM page_highlights;",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --command DELETE FROM page_content;",
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --command INSERT INTO page_content (route_slug, locale, summary) VALUES ('home', 'en', 'Server-rendered home page with stubbed content.'), ('home', 'fr', 'Accueil cote serveur avec des donnees de developpement preconfigurees.'), ('second', 'en', 'Second page content with stubbed data ready for future loaders.'), ('second', 'fr', 'Second page content with stubbed data ready for future loaders.');"
            ]

    it "uses stderr from the default psql runner when a command fails"
      $ withFakePsqlScriptResults
        [ ( Text.pack "SELECT summary FROM page_content WHERE route_slug = 'home' AND locale = 'en';",
            PostgresCommandResult
              { postgresExitCode = ExitFailure 4,
                postgresStdout = Text.empty,
                postgresStderr = Text.pack "default runner failed"
              }
          )
        ]
      $ \_ ->
        loadHomePageData (buildPostgresDatabaseEffect postgresTestConfig) defaultRequestContext
          `shouldReturn` Left (HomePageDataError (Text.pack "default runner failed"))

  describe "parseAppEnvironmentConfig" $ do
    it "parses committed development defaults into the expected config" $
      parseAppEnvironmentConfig committedEnvDefaults [] []
        `shouldBe` Right defaultAppEnvironmentConfig

    it "lets .env.local override committed .env defaults" $ do
      let localOverrides =
            [ (Text.pack "APP_MODE", Text.pack "production"),
              (Text.pack "DATABASE_HOST", Text.pack "localhost"),
              (Text.pack "DATABASE_PORT", Text.pack "6432"),
              (Text.pack "DATABASE_NAME", Text.pack "web_api_local"),
              (Text.pack "DATABASE_USER", Text.pack "local_user"),
              (Text.pack "DATABASE_PASSWORD", Text.pack "local_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides []
        `shouldBe` Right
          AppEnvironmentConfig
            { appMode = Production,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = Text.pack "localhost",
                    databasePort = 6432,
                    databaseName = Text.pack "web_api_local",
                    databaseUser = Text.pack "local_user",
                    databasePassword = Text.pack "local_password"
                  }
            }

    it "lets environment variables override .env.local values" $ do
      let localOverrides =
            [ (Text.pack "APP_MODE", Text.pack "production"),
              (Text.pack "DATABASE_HOST", Text.pack "localhost"),
              (Text.pack "DATABASE_PORT", Text.pack "6432"),
              (Text.pack "DATABASE_NAME", Text.pack "web_api_local"),
              (Text.pack "DATABASE_USER", Text.pack "local_user"),
              (Text.pack "DATABASE_PASSWORD", Text.pack "local_password")
            ]
          environmentOverrides =
            [ (Text.pack "APP_MODE", Text.pack "test"),
              (Text.pack "DATABASE_PORT", Text.pack "7432"),
              (Text.pack "DATABASE_PASSWORD", Text.pack "runtime_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides environmentOverrides
        `shouldBe` Right
          AppEnvironmentConfig
            { appMode = Test,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = Text.pack "localhost",
                    databasePort = 7432,
                    databaseName = Text.pack "web_api_local",
                    databaseUser = Text.pack "local_user",
                    databasePassword = Text.pack "runtime_password"
                  }
            }

    it "fails missing required values with explicit errors" $
      parseAppEnvironmentConfig
        [ (Text.pack "APP_MODE", Text.pack "development"),
          (Text.pack "DATABASE_HOST", Text.pack "127.0.0.1"),
          (Text.pack "DATABASE_PORT", Text.pack "5432"),
          (Text.pack "DATABASE_NAME", Text.pack "web_api_dev"),
          (Text.pack "DATABASE_USER", Text.pack "web_api")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue (Text.pack "DATABASE_PASSWORD"))

    it "fails invalid port or mode values with precise errors" $ do
      parseAppEnvironmentConfig committedEnvDefaults [] [(Text.pack "APP_MODE", Text.pack "staging")]
        `shouldBe` Left (InvalidConfigValue (Text.pack "APP_MODE") (Text.pack "staging"))
      parseAppEnvironmentConfig committedEnvDefaults [] [(Text.pack "DATABASE_PORT", Text.pack "0")]
        `shouldBe` Left (InvalidConfigValue (Text.pack "DATABASE_PORT") (Text.pack "0"))

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
                requestCorrelationId = Just (Text.pack "req-456"),
                requestSurface = PageSurface
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
          homePageModel =
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
          secondPageModel =
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR", Text.pack "Progressive enhancement"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
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
      homeHeading homePageModel `shouldBe` Text.pack "Home"
      homeSummary homePageModel `shouldBe` Text.pack "Server-rendered home page with stubbed content."
      homePrimaryAction homePageModel
        `shouldBe` CallToAction
          { callToActionLabel = Text.pack "Browse the second page",
            callToActionRoute = SecondRoute,
            callToActionHref = Text.pack "/fr/second"
          }
      secondHeading secondPageModel `shouldBe` Text.pack "Second"
      secondSummary secondPageModel `shouldBe` Text.pack "Second page content with stubbed data ready for future loaders."
      secondHighlights secondPageModel `shouldBe` [Text.pack "Fast SSR", Text.pack "Progressive enhancement"]
      secondPrimaryAction secondPageModel `shouldBe` callToAction
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
                secondErrorMessage = Nothing,
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
      PageSurface `shouldBe` PageSurface
      ApiSurface `shouldBe` ApiSurface
      HomeRoute `shouldBe` HomeRoute
      SecondRoute `shouldBe` SecondRoute
      StatusApiRoute `shouldBe` StatusApiRoute
      NotFoundRoute `shouldBe` NotFoundRoute
      UnsupportedLocalePrefix (Text.pack "de") `shouldBe` UnsupportedLocalePrefix (Text.pack "de")
      UnsupportedPath (Text.pack "/missing") `shouldBe` UnsupportedPath (Text.pack "/missing")
      HomePage homePageModel `shouldBe` HomePage homePageModel
      SecondPage secondPageModel `shouldBe` SecondPage secondPageModel
      NotFoundPage notFoundPageModel `shouldBe` NotFoundPage notFoundPageModel
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show InProcessHttp01 `shouldBe` "InProcessHttp01"
      show (CertbotHttp01 certbotConfig)
        `shouldBe` "CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})"
      show
        AcmeConfig
          { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = [Text.pack "ops@example.com"],
            acmeChallengeBackend = CertbotHttp01 certbotConfig
          }
        `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}"
      show acmeCertificateSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})"
      show (TlsConfig {certificateSource = manualCertificateSource})
        `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show manualCertificateSource
        `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show (ListenerConfig {listenerHost = Text.pack "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing})
        `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show
        ( StaticAssetsConfig
            { staticAssetRoots = [staticRoot],
              staticCacheControlSeconds = Just 3600
            }
        )
        `shouldBe` "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}"
      show
        ( ObservabilityConfig
            { tracingExporter =
                Just
                  OtlpExporter
                    { otlpEndpoint = Text.pack "http://otel-collector:4318",
                      otlpHeaders = [(Text.pack "x-api-key", Text.pack "secret")]
                    },
              metricsExporter = Nothing
            }
        )
        `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"x-api-key\",\"secret\")]}), metricsExporter = Nothing}"
      show
        ( AppRequestContext
            { requestLocale = French,
              requestCorrelationId = Just (Text.pack "req-789"),
              requestSurface = PageSurface
            }
        )
        `shouldBe` "AppRequestContext {requestLocale = French, requestCorrelationId = Just \"req-789\", requestSurface = PageSurface}"
      show
        ( CallToAction
            { callToActionLabel = Text.pack "Return home",
              callToActionRoute = HomeRoute,
              callToActionHref = Text.pack "/"
            }
        )
        `shouldBe` "CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}"
      show English `shouldBe` "English"
      show French `shouldBe` "French"
      show PageSurface `shouldBe` "PageSurface"
      show ApiSurface `shouldBe` "ApiSurface"
      show (UnsupportedLocalePrefix (Text.pack "de")) `shouldBe` "UnsupportedLocalePrefix \"de\""
      show (UnsupportedPath (Text.pack "/missing")) `shouldBe` "UnsupportedPath \"/missing\""
      show homePageModel
        `shouldBe` "HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}}"
      show secondPageModel
        `shouldBe` "SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (HomePage homePageModel)
        `shouldBe` "HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}})"
      show (SecondPage secondPageModel)
        `shouldBe` "SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show notFoundPageModel
        `shouldBe` "NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (NotFoundPage notFoundPageModel)
        `shouldBe` "NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show
        ( AppConfig
            { appTitlePrefix = Text.pack "test-app",
              listenerConfigs = [ListenerConfig {listenerHost = Text.pack "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}],
              staticAssets = StaticAssetsConfig {staticAssetRoots = [staticRoot], staticCacheControlSeconds = Just 3600},
              observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
            }
        )
        `shouldBe` "AppConfig {appTitlePrefix = \"test-app\", listenerConfigs = [ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}}"

    it "covers direct equality branches across the remaining public config and page types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
              }
          otherCertbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "renew"]
              }
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
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = Text.pack "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing
              }
          secureListenerConfig =
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
          staticAssetsConfig =
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
                metricsExporter = Nothing
              }
          appConfig =
            AppConfig
              { appTitlePrefix = Text.pack "test-app",
                listenerConfigs = [listenerConfig, secureListenerConfig],
                staticAssets = staticAssetsConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just (Text.pack "req-123"),
                requestSurface = PageSurface
              }
          callToAction =
            CallToAction
              { callToActionLabel = Text.pack "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = Text.pack "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = Text.pack "Home",
                homeSummary = Text.pack "Server-rendered home page with stubbed content.",
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = Text.pack "Not Found",
                notFoundSummary = Text.pack "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` [Text.pack "certonly", Text.pack "--webroot"]
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldNotBe` otherCertbotConfig
      acmeConfig `shouldBe` acmeConfig
      acmeConfig `shouldNotBe` otherAcmeConfig
      manualCertificateSource `shouldBe` manualCertificateSource
      manualCertificateSource `shouldNotBe` acmeCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      acmeCertificateSource `shouldNotBe` AcmeCertificateSource otherAcmeConfig
      tlsConfig `shouldBe` tlsConfig
      tlsConfig `shouldNotBe` TlsConfig {certificateSource = acmeCertificateSource}
      listenerConfig `shouldBe` listenerConfig
      listenerConfig `shouldNotBe` secureListenerConfig
      staticRoot `shouldBe` staticRoot
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = Text.pack "/static", staticDirectory = "public"}
      staticAssetsConfig `shouldBe` staticAssetsConfig
      staticAssetsConfig `shouldNotBe` StaticAssetsConfig {staticAssetRoots = [], staticCacheControlSeconds = Nothing}
      exporter `shouldBe` exporter
      exporter `shouldNotBe` OtlpExporter {otlpEndpoint = Text.pack "http://other-collector:4318", otlpHeaders = []}
      observabilityConfig `shouldBe` observabilityConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      appConfig `shouldBe` appConfig
      appConfig `shouldNotBe` appConfig {listenerConfigs = [listenerConfig]}
      English `shouldNotBe` French
      requestContext `shouldBe` requestContext
      requestContext `shouldNotBe` defaultRequestContext
      callToAction `shouldBe` callToAction
      callToAction `shouldNotBe` callToAction {callToActionHref = Text.pack "/fr"}
      homePageModel `shouldBe` homePageModel
      homePageModel `shouldNotBe` homePageModel {homeHeading = Text.pack "Accueil"}
      secondPageModel `shouldBe` secondPageModel
      secondPageModel `shouldNotBe` secondPageModel {secondHighlights = [Text.pack "Different"]}
      notFoundPageModel `shouldBe` notFoundPageModel
      notFoundPageModel `shouldNotBe` notFoundPageModel {notFoundSummary = Text.pack "Missing"}
      HomePage homePageModel `shouldNotBe` SecondPage secondPageModel
      SecondPage secondPageModel `shouldNotBe` NotFoundPage notFoundPageModel
      UnsupportedLocalePrefix (Text.pack "de") `shouldNotBe` UnsupportedPath (Text.pack "/de")
      PageSurface `shouldNotBe` ApiSurface
      HomeRoute `shouldNotBe` SecondRoute
      SecondRoute `shouldNotBe` NotFoundRoute

    it "covers high-precedence show rendering for the remaining public types" $ do
      let shouldBeParenthesized rendered = do
            case rendered of
              '(' : rest ->
                case reverse rest of
                  ')' : _ -> pure ()
                  _ -> expectationFailure "expected parenthesized rendering"
              _ -> expectationFailure "expected parenthesized rendering"
          certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend = CertbotHttp01 certbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
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
          staticAssetsConfig =
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
                staticAssets = staticAssetsConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just (Text.pack "req-999"),
                requestSurface = PageSurface
              }
          callToAction =
            CallToAction
              { callToActionLabel = Text.pack "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = Text.pack "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = Text.pack "Home",
                homeSummary = Text.pack "Server-rendered home page with stubbed content.",
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = Text.pack "Not Found",
                notFoundSummary = Text.pack "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show HomeRoute `shouldBe` "HomeRoute"
      show SecondRoute `shouldBe` "SecondRoute"
      show StatusApiRoute `shouldBe` "StatusApiRoute"
      show NotFoundRoute `shouldBe` "NotFoundRoute"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 (CertbotHttp01 certbotConfig) "")
      shouldBeParenthesized (showsPrec 11 acmeConfig "")
      shouldBeParenthesized (showsPrec 11 manualCertificateSource "")
      shouldBeParenthesized (showsPrec 11 acmeCertificateSource "")
      shouldBeParenthesized (showsPrec 11 tlsConfig "")
      shouldBeParenthesized (showsPrec 11 listenerConfig "")
      shouldBeParenthesized (showsPrec 11 staticRoot "")
      shouldBeParenthesized (showsPrec 11 staticAssetsConfig "")
      shouldBeParenthesized (showsPrec 11 exporter "")
      shouldBeParenthesized (showsPrec 11 observabilityConfig "")
      shouldBeParenthesized (showsPrec 11 appConfig "")
      shouldBeParenthesized (showsPrec 11 requestContext "")
      shouldBeParenthesized (showsPrec 11 callToAction "")
      shouldBeParenthesized (showsPrec 11 homePageModel "")
      shouldBeParenthesized (showsPrec 11 secondPageModel "")
      shouldBeParenthesized (showsPrec 11 notFoundPageModel "")
      shouldBeParenthesized (showsPrec 11 (HomePage homePageModel) "")
      shouldBeParenthesized (showsPrec 11 (SecondPage secondPageModel) "")
      shouldBeParenthesized (showsPrec 11 (NotFoundPage notFoundPageModel) "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedLocalePrefix (Text.pack "de")) "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedPath (Text.pack "/missing")) "")

    it "covers derived list-show rendering for the remaining public types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = [Text.pack "certonly", Text.pack "--webroot"]
              }
          acmeConfig =
            AcmeConfig
              { acmeDirectoryUrl = Text.pack "https://acme-v02.api.letsencrypt.org/directory",
                acmeContactEmails = [Text.pack "ops@example.com"],
                acmeChallengeBackend = CertbotHttp01 certbotConfig
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
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
          staticAssetsConfig =
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
                staticAssets = staticAssetsConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = French,
                requestCorrelationId = Just (Text.pack "req-list"),
                requestSurface = PageSurface
              }
          callToAction =
            CallToAction
              { callToActionLabel = Text.pack "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = Text.pack "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = Text.pack "Home",
                homeSummary = Text.pack "Server-rendered home page with stubbed content.",
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = Text.pack "Not Found",
                notFoundSummary = Text.pack "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      Http `shouldNotBe` Https
      InProcessHttp01 `shouldNotBe` CertbotHttp01 certbotConfig
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [InProcessHttp01, CertbotHttp01 certbotConfig]
        `shouldBe` "[InProcessHttp01,CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})]"
      show [acmeConfig]
        `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})}]"
      show [manualCertificateSource, acmeCertificateSource]
        `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})]"
      show [tlsConfig]
        `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})}]"
      show [listenerConfig]
        `shouldBe` "[ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 5443, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}]"
      show [exporter]
        `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig]
        `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]})}]"
      show [appConfig]
        `shouldBe` "[AppConfig {appTitlePrefix = \"test-app\", listenerConfigs = [ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 5443, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeChallengeBackend = CertbotHttp01 (CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]})})})}], staticAssets = StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticCacheControlSeconds = Just 3600}, observability = ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]})}}]"
      show [English, French] `shouldBe` "[English,French]"
      show [PageSurface, ApiSurface] `shouldBe` "[PageSurface,ApiSurface]"
      show [requestContext]
        `shouldBe` "[AppRequestContext {requestLocale = French, requestCorrelationId = Just \"req-list\", requestSurface = PageSurface}]"
      show [callToAction]
        `shouldBe` "[CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}]"
      show [homePageModel]
        `shouldBe` "[HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [secondPageModel]
        `shouldBe` "[SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [notFoundPageModel]
        `shouldBe` "[NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [HomePage homePageModel, SecondPage secondPageModel, NotFoundPage notFoundPageModel]
        `shouldBe` "[HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})]"
      show [UnsupportedLocalePrefix (Text.pack "de"), UnsupportedPath (Text.pack "/missing")]
        `shouldBe` "[UnsupportedLocalePrefix \"de\",UnsupportedPath \"/missing\"]"
      show [HomeRoute, SecondRoute, StatusApiRoute, NotFoundRoute] `shouldBe` "[HomeRoute,SecondRoute,StatusApiRoute,NotFoundRoute]"

  describe "parseRoute" $ do
    it "maps bare and default-locale paths to the same home route" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/")) `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/en")) `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext (Text.pack "/404")) `shouldBe` Just NotFoundRoute

    it "parses API routes with the API response surface" $ do
      parseRoute defaultRequestContext (Text.pack "/api/status") `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext (Text.pack "/api/second") `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext (Text.pack "/api") `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext (Text.pack "/api/404") `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext (Text.pack "/api/missing") `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext (Text.pack "/api/status/extra") `shouldBe` Just apiNotFoundRequest

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
      parseRoute defaultRequestContext (renderRoutePath apiStatusRequest) `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext (renderRoutePath apiSecondRequest) `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext (renderRoutePath apiNotFoundRequest) `shouldBe` Just apiNotFoundRequest

    it "renders locale prefixes only for non-default locales" $ do
      renderRoutePath homeRequest `shouldBe` Text.pack "/"
      renderRoutePath frenchHomeRequest `shouldBe` Text.pack "/fr"
      renderRoutePath secondRequest `shouldBe` Text.pack "/second"
      renderRoutePath frenchSecondRequest `shouldBe` Text.pack "/fr/second"
      renderRoutePath (HarchWeb.RouteRequest {HarchWeb.requestRoute = StatusApiRoute, HarchWeb.requestContext = defaultRequestContext}) `shouldBe` Text.pack "/404"
      renderRoutePath apiStatusRequest `shouldBe` Text.pack "/api/status"
      renderRoutePath apiSecondRequest `shouldBe` Text.pack "/api/second"
      renderRoutePath apiNotFoundRequest `shouldBe` Text.pack "/api/404"
      renderRoutePath notFoundRequest `shouldBe` Text.pack "/404"

  describe "matchRoute" $ do
    it "remains available separately from HarchWeb.matchRoute" $
      WebApi.Route.matchRoute WebApi.Route.defaultRequestContext (Text.pack "/second")
        `shouldBe` HarchWeb.matchRoute WebApi.Route.routeCodec WebApi.Route.defaultRequestContext (Text.pack "/second")

    it "matches the home path" $
      pureRouteMatcher (Text.pack "/") `shouldBe` homeRequest

    it "matches the second page path" $
      pureRouteMatcher (Text.pack "/second") `shouldBe` secondRequest

    it "matches locale-prefixed paths with the merged request context" $
      pureRouteMatcher (Text.pack "/fr") `shouldBe` frenchHomeRequest

    it "matches API paths with the API response surface" $ do
      pureRouteMatcher (Text.pack "/api/status") `shouldBe` apiStatusRequest
      pureRouteMatcher (Text.pack "/api/second") `shouldBe` apiSecondRequest
      pureRouteMatcher (Text.pack "/api/missing") `shouldBe` apiNotFoundRequest

    it "falls back to the stable not-found route for unknown paths" $
      pureRouteMatcher (Text.pack "/missing") `shouldBe` notFoundRequest

  describe "renderPage" $ do
    it "selects the expected home page model" $
      renderPage defaultAppConfig homeRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"
          }

    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
          }

    it "renders selected route data without reloading it" $
      renderPageFromRouteData
        defaultAppConfig
        secondRequest
        ( SecondRouteDataResult
            ( Right
                SecondRouteData
                  { secondRouteSummary = Text.pack "Shared domain summary.",
                    secondRouteHighlights = [Text.pack "Shared loader"]
                  }
            )
        )
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Shared domain summary.</p><ul><li>Shared loader</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
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
        `shouldReturn` Text.pack "<html><head><title>test-app: Home</title></head><body data-app=\"test-app\"><nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      renderedShell config SecondRoute
        `shouldReturn` Text.pack "<html><head><title>test-app: Second</title></head><body data-app=\"test-app\"><nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      renderedShell config NotFoundRoute
        `shouldReturn` Text.pack "<html><head><title>test-app: Not Found</title></head><body data-app=\"test-app\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

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
      show defaultRequestContext `shouldBe` "AppRequestContext {requestLocale = English, requestCorrelationId = Nothing, requestSurface = PageSurface}"
      show (renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = Text.pack "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []}))))
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageContext = AppRequestContext {requestLocale = English, requestCorrelationId = Nothing, requestSurface = PageSurface}, pageBody = \"<section data-page=\\\"second\\\"><h1 data-page-title=\\\"true\\\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\\\"true\\\">No highlights yet.</p><p><a href=\\\"/\\\" data-page-link=\\\"true\\\">Return home</a></p></section>\"}"
      renderPage config secondRequest `shouldReturn` renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = Text.pack "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))

  describe "selectResponse" $ do
    it "resolves page routes to page responses that still flow through the shared shell" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      selectResponse defaultAppConfig secondRequest `shouldReturn` HarchWeb.PageResponse renderedPage

    it "resolves API-only routes to explicit status, content type, and body values" $ do
      selectResponse defaultAppConfig apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"status\":\"ok\",\"locale\":\"en\"}"
            }
      selectResponse defaultAppConfig apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
            }

    it "keeps API payload rendering locale-aware without touching page routing" $ do
      selectResponse defaultAppConfig frenchApiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"status\":\"ok\",\"locale\":\"fr\"}"
            }
      selectResponse defaultAppConfig frenchApiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
            }

    it "keeps not-found handling consistent across page and non-page responses" $ do
      renderedPage <- renderPage defaultAppConfig notFoundRequest
      selectResponse defaultAppConfig notFoundRequest `shouldReturn` HarchWeb.PageResponse renderedPage
      selectResponse defaultAppConfig apiNotFoundRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 404,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"error\":\"not-found\"}"
            }

    it "maps shared second-page load failures into explicit API error responses" $
      selectResponseWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError (Text.pack "seed unavailable")),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 503,
              HarchWeb.responseContentType = Text.pack "application/json",
              HarchWeb.responseBody = Text.pack "{\"error\":\"second-page-unavailable\"}"
            }

    it "is deterministic for repeated requests" $ do
      firstResponse <- selectResponse defaultAppConfig apiStatusRequest
      secondResponse <- selectResponse defaultAppConfig apiStatusRequest
      firstResponse `shouldBe` secondResponse

  describe "buildPageModel" $ do
    it "builds stubbed home page data with a navigation affordance" $
      buildPageModel homeRequest
        `shouldReturn` HomePage
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
        `shouldReturn` HomePage
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

    it "renders selected route data into both page models and API responses" $ do
      let selectedRouteData =
            SecondRouteDataResult
              ( Right
                  SecondRouteData
                    { secondRouteSummary = Text.pack "Shared domain summary.",
                      secondRouteHighlights = [Text.pack "Shared loader", Text.pack "Shared renderer"]
                    }
              )
      buildPageModelFromRouteData secondRequest selectedRouteData
        `shouldBe` SecondPage
          SecondPageModel
            { secondHeading = Text.pack "Second",
              secondSummary = Text.pack "Shared domain summary.",
              secondHighlights = [Text.pack "Shared loader", Text.pack "Shared renderer"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = Text.pack "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = Text.pack "/"
                  }
            }
      renderApiResponseFromRouteData selectedRouteData
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 200,
            HarchWeb.responseContentType = Text.pack "application/json",
            HarchWeb.responseBody = Text.pack "{\"summary\":\"Shared domain summary.\",\"highlights\":[\"Shared loader\",\"Shared renderer\"]}"
          }

    it "loads second-page content from the database effect when provided" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData =
                  Right
                    SecondPageData
                      { secondPageDataSummary = Text.pack "Loaded from the seeded database effect.",
                        secondPageDataHighlights = [Text.pack "Fast SSR", Text.pack "Progressive enhancement"]
                      },
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = Text.pack "Second",
              secondSummary = Text.pack "Loaded from the seeded database effect.",
              secondHighlights = [Text.pack "Fast SSR", Text.pack "Progressive enhancement"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = Text.pack "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = Text.pack "/"
                  }
            }

    it "builds an explicit error-state second page when the database effect fails" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError (Text.pack "seed unavailable")),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = Text.pack "Second",
              secondSummary = Text.pack "Second page content is temporarily unavailable.",
              secondHighlights = [],
              secondErrorMessage = Just (Text.pack "Could not load second page data."),
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = Text.pack "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = Text.pack "/"
                  }
            }

  describe "renderPageBody" $ do
    it "renders the home page heading and navigation affordance" $ do
      homePageModel <- buildPageModel homeRequest
      renderPageBody homePageModel
        `shouldBe` Text.pack "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      secondPageModel <- buildPageModel secondRequest
      renderPageBody secondPageModel
        `shouldBe` Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
      Text.isInfixOf (Text.pack "<nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\">") homeShell `shouldBe` True
      Text.isInfixOf (Text.pack "<nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\">") secondShell `shouldBe` True

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      homePageModel <- buildPageModel homeRequest
      secondPageModel <- buildPageModel secondRequest
      let homeBody = renderPageBody homePageModel
          secondBody = renderPageBody secondPageModel
      Text.isInfixOf (Text.pack "<section data-page=\"home\">") homeBody `shouldBe` True
      Text.isInfixOf (Text.pack "<section data-page=\"second\">") secondBody `shouldBe` True
      Text.isInfixOf (Text.pack "data-page-title=\"true\"") homeBody `shouldBe` True
      Text.isInfixOf (Text.pack "data-page-link=\"true\"") secondBody `shouldBe` True
      Text.isInfixOf (Text.pack "<main") homeBody `shouldBe` False
      Text.isInfixOf (Text.pack "<body") secondBody `shouldBe` False

    it "covers empty and populated highlight rendering branches" $ do
      secondPageModel <- buildPageModel secondRequest
      Text.isInfixOf (Text.pack "<p data-empty-state=\"true\">No highlights yet.</p>") (renderPageBody secondPageModel) `shouldBe` True
      renderPageBody
        ( SecondPage
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
                secondHighlights = [Text.pack "Fast SSR", Text.pack "Stable routes"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = Text.pack "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = Text.pack "/"
                    }
              }
        )
        `shouldBe` Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"

    it "renders an explicit error state when the second-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                frenchHomePageData = frenchHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError (Text.pack "seed unavailable")),
                frenchSecondPageData = frenchSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = Text.pack "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = Text.pack "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p data-error-state=\"true\">Could not load second page data.</p><p>Second page content is temporarily unavailable.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
          }

  describe "page shell integration" $ do
    it "marks the active navigation item for each routed page" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf (Text.pack "<a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a>") homeShell `shouldBe` True
      Text.isInfixOf (Text.pack "<a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a>") secondShell `shouldBe` True
      Text.isInfixOf (Text.pack "aria-current=\"page\"") notFoundShell `shouldBe` False

    it "keeps shell output identical for repeated renders of the same page input" $ do
      let application = buildApp defaultAppConfig
      page <- renderPage defaultAppConfig frenchSecondRequest
      HarchWeb.pageShell application page `shouldBe` HarchWeb.pageShell application page

    it "keeps not-found pages inside the shared shell" $
      renderedShell defaultAppConfig NotFoundRoute
        `shouldReturn` Text.pack "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` Text.pack "web-api"

    it "stores the default request context used by the WAI adapter" $
      HarchWeb.defaultRequestContext pureApplication `shouldBe` defaultRequestContext

    it "stores the configured static assets used by the WAI adapter" $
      HarchWeb.applicationStaticAssets pureApplication `shouldBe` staticAssets defaultAppConfig

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/") `shouldBe` parseRoute defaultRequestContext (Text.pack "/")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/fr") `shouldBe` parseRoute defaultRequestContext (Text.pack "/fr")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/second") `shouldBe` parseRoute defaultRequestContext (Text.pack "/second")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/api/status") `shouldBe` parseRoute defaultRequestContext (Text.pack "/api/status")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/api/second") `shouldBe` parseRoute defaultRequestContext (Text.pack "/api/second")
      HarchWeb.parseRoute codec defaultRequestContext (Text.pack "/missing") `shouldBe` Nothing
      HarchWeb.renderRoute codec homeRequest `shouldBe` renderRoutePath homeRequest
      HarchWeb.renderRoute codec frenchSecondRequest `shouldBe` renderRoutePath frenchSecondRequest
      HarchWeb.renderRoute codec secondRequest `shouldBe` renderRoutePath secondRequest
      HarchWeb.renderRoute codec apiStatusRequest `shouldBe` renderRoutePath apiStatusRequest
      HarchWeb.renderRoute codec apiSecondRequest `shouldBe` renderRoutePath apiSecondRequest
      HarchWeb.renderRoute codec apiNotFoundRequest `shouldBe` renderRoutePath apiNotFoundRequest
      HarchWeb.renderRoute codec notFoundRequest `shouldBe` renderRoutePath notFoundRequest
      HarchWeb.notFoundRequest codec defaultRequestContext `shouldBe` notFoundRequest

    it "stores the same response-selection behavior used by direct response tests" $ do
      expectedHomeResponse <- selectResponse defaultAppConfig homeRequest
      expectedSecondResponse <- selectResponse defaultAppConfig secondRequest
      expectedApiStatusResponse <- selectResponse defaultAppConfig apiStatusRequest
      expectedApiSecondResponse <- selectResponse defaultAppConfig apiSecondRequest
      expectedNotFoundResponse <- selectResponse defaultAppConfig notFoundRequest
      expectedApiNotFoundResponse <- selectResponse defaultAppConfig apiNotFoundRequest
      HarchWeb.renderResponse pureApplication homeRequest `shouldReturn` expectedHomeResponse
      HarchWeb.renderResponse pureApplication secondRequest `shouldReturn` expectedSecondResponse
      HarchWeb.renderResponse pureApplication apiStatusRequest `shouldReturn` expectedApiStatusResponse
      HarchWeb.renderResponse pureApplication apiSecondRequest `shouldReturn` expectedApiSecondResponse
      HarchWeb.renderResponse pureApplication notFoundRequest `shouldReturn` expectedNotFoundResponse
      HarchWeb.renderResponse pureApplication apiNotFoundRequest `shouldReturn` expectedApiNotFoundResponse

    it "adapts the pure application to WAI without changing rendered pages" $ do
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest [Text.pack "fr", Text.pack "second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "text/html; charset=utf-8"))
      renderedPage <- renderPage defaultAppConfig frenchSecondRequest
      readResponseBody secondResponse
        `shouldReturn` HarchWeb.pageShell pureApplication renderedPage

      apiStatusResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest [Text.pack "api", Text.pack "status"])
      Wai.responseStatus apiStatusResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiStatusResponse) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "application/json"))
      readResponseBody apiStatusResponse
        `shouldReturn` Text.pack "{\"status\":\"ok\",\"locale\":\"en\"}"

      apiSecondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest [Text.pack "api", Text.pack "second"])
      Wai.responseStatus apiSecondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiSecondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "application/json"))
      readResponseBody apiSecondResponse
        `shouldReturn` Text.pack "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"

      missingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest [Text.pack "missing"])
      Wai.responseStatus missingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "text/html; charset=utf-8"))
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      readResponseBody missingResponse
        `shouldReturn` HarchWeb.pageShell pureApplication notFoundPage

      apiMissingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest [Text.pack "api", Text.pack "missing"])
      Wai.responseStatus apiMissingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders apiMissingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 (Text.pack "application/json"))
      readResponseBody apiMissingResponse
        `shouldReturn` Text.pack "{\"error\":\"not-found\"}"

    it "is structurally complete enough to render supported and not-found shells" $ do
      homePage <- renderPage defaultAppConfig homeRequest
      secondPage <- renderPage defaultAppConfig secondRequest
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      HarchWeb.pageShell pureApplication homePage
        `shouldBe` Text.pack "<html><head><title>web-api: Home</title></head><body data-app=\"web-api\"><nav><a href=\"/\" aria-current=\"page\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication secondPage
        `shouldBe` Text.pack "<html><head><title>web-api: Second</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\" aria-current=\"page\">Second</a></nav><main id=\"app-main\"><section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"
      HarchWeb.pageShell pureApplication notFoundPage
        `shouldBe` Text.pack "<html><head><title>web-api: Not Found</title></head><body data-app=\"web-api\"><nav><a href=\"/\">Home</a><a href=\"/second\">Second</a></nav><main id=\"app-main\"><section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section></main></body></html>"

    it "can grow from page responses to API responses without changing route matching" $ do
      renderedResponse <- HarchWeb.renderResponse pureApplication apiSecondRequest
      case renderedResponse of
        HarchWeb.BodyResponse body -> HarchWeb.responseBody body `shouldBe` Text.pack "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected body response"

  describe "run" $
    it "writes startup output to the supplied handle for isolated tests" $
      withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
        run outputHandle
        hClose outputHandle
        readFile outputPath `shouldReturn` "HTTP Server listening at http://localhost:5001\n"
