{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# SPEC #-}

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, readMVar, threadDelay)
import Control.Exception (IOException, SomeException, bracket, displayException, finally, try)
import Control.Monad (forM_)
import Core.Setup.PrerequisiteConfig qualified as PrerequisiteConfig
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (toLower)
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (find, isInfixOf, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.DevSmtp qualified as DevSmtp
import HarchWeb.Email qualified as Email
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.RecoveryCode qualified as RecoveryCode
import HarchWeb.Secret qualified as Secret
import HarchWeb.Session qualified as Session
import HarchWeb.Totp qualified as Totp
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import Network.Socket qualified as NetworkSocket
import Network.Socket.ByteString qualified as SocketByteString
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Numeric (readHex)
import System.Directory (createDirectory, getCurrentDirectory, removePathForcibly, setCurrentDirectory)
import System.Environment (getEnv, getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import System.Process (callProcess)
import TestSupport.RealPostgres (containerizedPsqlScriptContents, defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, ensureDefaultPostgresAvailableScript, withContainerizedPsqlOnPath)
import Text.Read (readMaybe)
import WebApi (buildApp, run)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..), AccountStoreError (..), PendingAccount (..), RegistrationError (..), RegistrationResult (..), ResendVerificationError (..), confirmEmailVerificationAt, registerAccountAt, registerAccountAtWithPasswordHasher, registerAccountWithIdentityAt, resendEmailVerificationAt)
import WebApi.AccountPages (AccountWorkflow (..), LoginForm (..), MfaEnrollmentForm (..), RegistrationForm (..), VerificationForm (..), emptyRegistrationForm, handleAccountAction, mfaEnrollmentFailureDiagnostics, renderLoginPage, renderLoginRegion, renderLogoutPage, renderLogoutRegion, renderMfaEnrollmentPage, renderMfaEnrollmentRegion, renderRegistrationPage, renderRegistrationRegion, renderVerificationPage, renderVerificationRegion)
import WebApi.App (buildAppWithDatabase, buildRuntimeAccountWorkflow, buildRuntimeApp, buildRuntimeAppWithDatabaseBuilder, runWithConfig, unavailableAccountWorkflow)
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.App.Shell (buildAppPageShell, buildAppPageShellConfig)
import WebApi.AppEffect qualified as AppEffect
import WebApi.Config (AcmeConfig (..), AppConfig (..), AppEnvironmentConfig (..), AppEnvironmentConfigLoadError (..), AppMode (..), AppStartupConfig (..), AppStartupConfigLoadError (..), CertbotConfig (..), CorsPolicyConfig (..), DatabaseConfig (..), ListenerConfig (..), ListenerScheme (..), ObservabilityConfig (..), OtlpExporter (..), RequestPolicyConfig (..), ResponseSecurityHeadersConfig (..), SmtpDeliveryConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), StrictTransportSecurityConfig (..), TlsCertificateSource (..), TlsConfig (..), TlsStartupMode (..), committedEnvDefaults, committedRuntimeDefaults, defaultAppConfig, defaultAppEnvironmentConfig, defaultAppStartupConfig, defaultCorsPolicyConfig, defaultResponseSecurityHeadersConfig, defaultStaticAssetContentTypes, loadAppEnvironmentConfig, loadAppEnvironmentConfigWithFiles, loadAppStartupConfig, loadAppStartupConfigWithFiles, parseAppEnvironmentConfig, parseAppStartupConfig, parseRuntimeAppConfig)
import WebApi.Database (DatabaseEffect (..), DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), HomePageData (..), SecondPageData (..), buildSeededDatabaseEffect, defaultDatabaseEffect, defaultDatabaseSeed)
import WebApi.DatabaseSetup (DatabaseSetupCommand (..), DatabaseSetupError (..), loadDatabaseSetupConfig, parseDatabaseSetupCommand, parseDatabaseSetupConfig, renderDatabaseSetupError, runDatabaseSetupArgs, runDatabaseSetupArgsWith, runDatabaseSetupCommand, runDatabaseSetupCommandWith)
import WebApi.Login (AccountCredential (..), AccountCredentialStore (..), AccountCredentialStoreError (..), LoginIdentifier (..), PasswordLoginResult (..), beginPasswordLoginWithIdentifier)
import WebApi.Mfa (MfaStore (..), MfaStoreError (..), StoredTotpEnrollment (..))
import WebApi.MfaEnrollment (MfaEnrollmentError (..))
import WebApi.Page (AppPageModel (..), CallToAction (..), HomePageModel (..), NotFoundPageModel (..), ProfilePageModel (..), SecondPageModel (..), SpacesPageModel (..), buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase, renderPage, renderPageBody, renderPageFromRouteData, renderPageWithDatabase)
import WebApi.PageShell qualified as LegacyPageShell
import WebApi.Postgres (PostgresCommand (..), PostgresCommandResult (..), PostgresRunnerError (..), buildPostgresDatabaseEffect, buildPostgresDatabaseEffectWithRunner, buildRuntimePostgresAccountProfileStore, buildRuntimePostgresAccountProfileStoreWithRunner, buildRuntimePostgresAccountStore, buildRuntimePostgresAccountStoreWithRunner, buildRuntimePostgresDatabaseEffectWithRunner, decodeRuntimeQueryValue, migrationStatementsFor, renderRuntimeConnectionErrorMessage, renderRuntimeResultErrorMessage, runPostgresMigrations, runPostgresMigrationsForRuntime, runPostgresMigrationsWithRunner, runPostgresMigrationsWithRunnerForRuntime, runPostgresSeed, runPostgresSeedWithRunner, runRuntimeParameterizedRowsQuery, runRuntimeRowsQuery, runRuntimeScalarQuery, seedStatements)
import WebApi.Response (renderApiResponseFromRouteData, selectResponse, selectResponseWithDatabase)
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), RequestSurface (..), RouteMetadata (..), RouteSelectionError (..), defaultRequestContext, parseRoute, renderRoutePath, routeMetadata, selectRoute)
import WebApi.Route qualified
import WebApi.RouteData (HomeRouteData (..), RouteDataResult (..), RouteDataSelection (..), SecondRouteData (..), StatusApiData (..), selectRouteData, selectRouteDataSelectionWithDatabase, selectRouteDataWithDatabase)
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..))
import WebApi.SetupConfig (AppSetupConfig (..), AppSetupConfigLoadError (..), SetupAutostartConfig (..), committedSetupDefaults, defaultAppSetupConfig, defaultSetupAutostartConfig, loadAppSetupConfig, loadAppSetupConfigWithFiles, parseAppSetupConfig)
import WebApi.SetupPlan (AppPrerequisitePlan (..), ContainerAutostartPlan (..), ContainerRuntime (..), DatabasePrerequisitePlan (..), TcpEndpoint (..), TracingEndpointParseError (..), TracingPrerequisitePlan (..), checkTcpEndpointReachable, checkTcpEndpointReachableWithTimeout, checkTracingEndpointReachable, defaultContainerAutostartPlan, parseTracingEndpoint, planAppPrerequisites, toSetupPrerequisiteConfig)

pureApplication :: HarchWeb.Application AppRoute AppRequestContext
pureApplication = buildApp defaultAppConfig

equalValues :: (Eq value) => value -> value -> Bool
equalValues = (==)
{-# NOINLINE equalValues #-}

renderedValue :: (Show value) => value -> String
renderedValue = show
{-# NOINLINE renderedValue #-}

trustedForwardedApplication :: HarchWeb.Application AppRoute AppRequestContext
trustedForwardedApplication =
  buildApp
    defaultAppConfig
      { requestPolicy =
          (requestPolicy defaultAppConfig)
            { trustForwardedHeaders = True
            }
      }

navigationAppConfig :: AppConfig
navigationAppConfig =
  defaultAppConfig
    { staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
    }

homeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
homeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = defaultRequestContext}

secondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
secondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = defaultRequestContext}

spacesRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spacesRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SpacesRoute, HarchWeb.requestContext = defaultRequestContext}

profileRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
profileRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = ProfileRoute, HarchWeb.requestContext = defaultRequestContext}

spanishRequestContext :: AppRequestContext
spanishRequestContext = defaultRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = True}

assertSameProfilePageModel :: ProfilePageModel -> ProfilePageModel -> Expectation
assertSameProfilePageModel actual expected =
  if ProfilePage actual == ProfilePage expected
    then pure ()
    else expectationFailure "expected equal profile page models"
{-# NOINLINE assertSameProfilePageModel #-}

explicitEnglishRequestContext :: AppRequestContext
explicitEnglishRequestContext = defaultRequestContext {requestLocaleIsExplicit = True}

prefixedRequestContext :: AppRequestContext
prefixedRequestContext = defaultRequestContext {requestPathPrefix = "/app"}

prefixedSpanishRequestContext :: AppRequestContext
prefixedSpanishRequestContext = spanishRequestContext {requestPathPrefix = "/app"}

spanishHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = spanishRequestContext}

spanishSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = spanishRequestContext}

spanishSpacesRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishSpacesRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SpacesRoute, HarchWeb.requestContext = spanishRequestContext}

prefixedHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedSpanishSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedSpanishSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedSpanishRequestContext}

prefixedApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = prefixedRequestContext {requestSurface = ApiSurface}
    }

spanishApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = spanishRequestContext {requestSurface = ApiSurface}
    }

spanishApiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishApiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondRoute,
      HarchWeb.requestContext = spanishRequestContext {requestSurface = ApiSurface}
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
  renderedShellForRequest
    config
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = defaultRequestContext
      }

renderedShellForRequest :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO Text
renderedShellForRequest config routeRequest = do
  let application = buildApp config
  page <- renderPage config routeRequest
  pure (HarchWeb.renderDocument (HarchWeb.pageShell application page))

data CapturedOtlpRequest = CapturedOtlpRequest
  { capturedOtlpMethod :: ByteString.ByteString,
    capturedOtlpPath :: ByteString.ByteString,
    capturedOtlpHeaders :: [(ByteString.ByteString, ByteString.ByteString)],
    capturedOtlpBody :: ByteString.ByteString
  }

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

postgresTestConfig :: DatabaseConfig
postgresTestConfig =
  DatabaseConfig
    { databaseHost = "db.internal",
      databasePort = 6543,
      databaseName = "web_api_prod",
      databaseUser = "web_api_app",
      databasePassword = "super-secret"
    }

testPasswordHashingPolicy :: Password.PasswordHashingPolicy
testPasswordHashingPolicy =
  fromMaybe (error "Expected valid test password hashing policy") (Password.mkPasswordHashingPolicy 1 8 1)

requiredAccountId :: Text -> Account.AccountId
requiredAccountId value =
  fromMaybe (error "Expected a valid account id") (Account.mkAccountId value)

requiredEmailAddress :: Text -> Email.EmailAddress
requiredEmailAddress value =
  fromMaybe (error "Expected a valid email address") (Email.mkEmailAddress value)

requiredVerificationToken :: Text -> Account.EmailVerificationToken
requiredVerificationToken value =
  fromMaybe (error "Expected a valid verification token") (Account.mkEmailVerificationToken value)

isUnavailable :: Text -> AccountStoreError -> Bool
isUnavailable expectedError = \case
  AccountStoreUnavailable actualError -> actualError == expectedError
  AccountStoreCorruptData _ -> False

isCorrupt :: Text -> AccountStoreError -> Bool
isCorrupt expectedError = \case
  AccountStoreUnavailable _ -> False
  AccountStoreCorruptData actualError -> actualError == expectedError

assertAccountStoreError :: IO (Either AccountStoreError value) -> (AccountStoreError -> Bool) -> IO ()
assertAccountStoreError action matchesError = do
  result <- action
  case result of
    Left storeError | matchesError storeError -> pure ()
    _ -> expectationFailure "expected matching account-store error"

assertAccountStoreSuccess :: IO (Either AccountStoreError value) -> (value -> Bool) -> IO ()
assertAccountStoreSuccess action matchesValue = do
  result <- action
  case result of
    Right value | matchesValue value -> pure ()
    _ -> expectationFailure "expected matching account-store success"

assertEmailVerificationResult :: IO (Either AccountStoreError Account.EmailVerificationValidation) -> (Either AccountStoreError Account.EmailVerificationValidation -> Bool) -> IO ()
assertEmailVerificationResult action matchesResult = do
  result <- action
  if matchesResult result
    then pure ()
    else expectationFailure "unexpected email verification result"

assertRegistrationResult :: IO (Either RegistrationError RegistrationResult) -> (Either RegistrationError RegistrationResult -> Bool) -> IO ()
assertRegistrationResult action matchesResult = do
  result <- action
  if matchesResult result
    then pure ()
    else expectationFailure "unexpected registration result"

actionHasStatusAndFocus :: Int -> Maybe Text -> Text -> Maybe HarchWeb.ClientActionResponse -> Bool
actionHasStatusAndFocus expectedStatus expectedFocus expectedMessage = \case
  Just actionResponse ->
    forceShowValue actionResponse
      && HarchWeb.clientActionStatus actionResponse == expectedStatus
      && HarchWeb.clientActionFocusId actionResponse == expectedFocus
      && case HarchWeb.clientActionPatches actionResponse of
        [patch] ->
          HarchWeb.regionPatchId patch `elem` ["registration-region", "verification-region", "mfa-enrollment-region", "login-region", "logout-region"]
            && expectedMessage `Text.isInfixOf` HarchWeb.regionPatchHtml patch
        _ -> False
  Nothing -> False

forceShowValue :: (Show value) => value -> Bool
forceShowValue = foldr seq True . show

awaitDevSmtpEmail :: DevSmtp.DevSmtpServer -> Text -> IO (Maybe DevSmtp.DevSmtpEmail)
awaitDevSmtpEmail server recipient = go (100 :: Int)
  where
    go remaining = do
      received <- DevSmtp.takeLatestDevSmtpEmailTo server recipient
      case received of
        Just email -> pure (Just email)
        Nothing
          | remaining > 0 -> threadDelay 10000 >> go (remaining - 1)
          | otherwise -> pure Nothing

metadataFields :: RouteMetadata -> (Maybe Text, Text, Text, [Text])
metadataFields metadata =
  ( routePageSegment metadata,
    routePageSuffix metadata,
    routePageTitle metadata,
    routeEnhancementHooks metadata
  )

migrationPostgresTestConfig :: DatabaseConfig
migrationPostgresTestConfig =
  postgresTestConfig
    { databaseUser = "web_api_owner",
      databasePassword = "owner-secret"
    }

setupMigrationPostgresTestConfig :: DatabaseConfig
setupMigrationPostgresTestConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_dev",
      databaseUser = "web_api_owner",
      databasePassword = "owner-secret"
    }

runtimeSetupPostgresTestConfig :: DatabaseConfig
runtimeSetupPostgresTestConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_dev",
      databaseUser = "web_api_runtime",
      databasePassword = "runtime-secret"
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

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  action `finally` setCurrentDirectory previousDirectory

withUnreadableFile :: FilePath -> String -> IO a -> IO a
withUnreadableFile filePath _fileContents action = do
  createDirectory filePath
  action `finally` removePathForcibly filePath

withClearedAppEnvironment :: IO a -> IO a
withClearedAppEnvironment =
  withTemporaryEnvironment "APP_MODE" Nothing
    . withTemporaryEnvironment "DATABASE_HOST" Nothing
    . withTemporaryEnvironment "DATABASE_PORT" Nothing
    . withTemporaryEnvironment "DATABASE_NAME" Nothing
    . withTemporaryEnvironment "DATABASE_USER" Nothing
    . withTemporaryEnvironment "DATABASE_PASSWORD" Nothing

withClearedEnvironmentPrefixes :: [String] -> IO a -> IO a
withClearedEnvironmentPrefixes prefixes action = do
  environment <- getEnvironment
  let matchingKeys =
        [ key
        | (key, _) <- environment,
          any (`isPrefixOf` key) prefixes
        ]
  foldr (`withTemporaryEnvironment` Nothing) action matchingKeys

withClearedRuntimeEnvironment :: IO a -> IO a
withClearedRuntimeEnvironment =
  withClearedEnvironmentPrefixes
    [ "APP_TITLE_PREFIX",
      "LISTENER_",
      "STATIC_ASSET_CONTENT_TYPE_",
      "STATIC_ASSET_ROOT_",
      "STATIC_CACHE_CONTROL_SECONDS",
      "REDIRECT_HTTP_TO_HTTPS",
      "HTTPS_REDIRECT_PORT",
      "HSTS_",
      "CORS_",
      "CONTENT_SECURITY_POLICY",
      "X_CONTENT_TYPE_OPTIONS_NOSNIFF",
      "X_XSS_PROTECTION",
      "REFERRER_POLICY",
      "PERMISSIONS_POLICY",
      "X_FRAME_OPTIONS",
      "OTLP_TRACING_",
      "OTLP_METRICS_"
    ]

withClearedSetupEnvironment :: IO a -> IO a
withClearedSetupEnvironment =
  withClearedEnvironmentPrefixes
    [ "SETUP_AUTOSTART_",
      "WEB_API_MIGRATION_DATABASE_"
    ]

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

withListeningTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withListeningTcpEndpoint action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ ->
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
        `finally` close listenerSocket
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback test socket"

withUnusedTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withUnusedTcpEndpoint action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"

withDefaultRuntimePortUnavailable :: IO a -> IO a
withDefaultRuntimePortUnavailable action = do
  reservedSocketResult <- try (socket AF_INET Stream defaultProtocol >>= reserveDefaultRuntimePort) :: IO (Either IOError NetworkSocket.Socket)
  case reservedSocketResult of
    Left bindError
      | isAlreadyInUseError bindError -> action
      | otherwise -> ioError bindError
    Right reservedSocket ->
      action `finally` close reservedSocket
  where
    reserveDefaultRuntimePort reservedSocket = do
      bind reservedSocket (SockAddrInet 5001 (tupleToHostAddress (127, 0, 0, 1)))
      listen reservedSocket 1
      pure reservedSocket

withOtlpCaptureServer ::
  Http.Status ->
  ByteString.ByteString ->
  (Text -> MVar CapturedOtlpRequest -> IO a) ->
  IO a
withOtlpCaptureServer responseStatus responseBody action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ -> do
      capturedRequestReference <- newEmptyMVar
      let collectorUrl = Text.pack ("http://127.0.0.1:" <> show (fromIntegral port :: Int) <> "/v1/traces")
      serverThreadId <-
        forkIO $
          ( do
              (clientSocket, _) <- NetworkSocket.accept listenerSocket
              capturedRequest <- readCapturedHttpRequest clientSocket
              putMVar capturedRequestReference capturedRequest
              SocketByteString.sendAll clientSocket (buildHttpResponse responseStatus responseBody)
              close clientSocket
          )
            `finally` close listenerSocket
      action collectorUrl capturedRequestReference `finally` killThread serverThreadId
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback OTLP capture socket"

readCapturedHttpRequest :: NetworkSocket.Socket -> IO CapturedOtlpRequest
readCapturedHttpRequest clientSocket = do
  requestBytes <- readHttpRequestBytes clientSocket
  let (headerBytes, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes
      requestBody = ByteString.drop 4 withSeparator
      headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)
      requestLine =
        case headerLines of
          line : _ -> line
          [] -> ByteString.empty
      (requestMethod, requestPath) =
        case ByteStringChar8.words requestLine of
          method : path : _ -> (method, path)
          _ -> (ByteString.empty, ByteString.empty)
  pure
    CapturedOtlpRequest
      { capturedOtlpMethod = requestMethod,
        capturedOtlpPath = requestPath,
        capturedOtlpHeaders = mapMaybe parseCapturedHeader (drop 1 headerLines),
        capturedOtlpBody = requestBody
      }

readHttpRequestBytes :: NetworkSocket.Socket -> IO ByteString.ByteString
readHttpRequestBytes clientSocket =
  readRequestChunks ByteString.empty Nothing
  where
    readRequestChunks accumulatedRequest knownContentLength = do
      chunk <- SocketByteString.recv clientSocket 4096
      let accumulatedRequest' = accumulatedRequest <> chunk
          contentLength =
            case knownContentLength of
              Just value -> Just value
              Nothing -> parseHttpContentLength accumulatedRequest'
      case contentLength of
        Just bodyLength
          | ByteString.length (extractHttpBody accumulatedRequest') >= bodyLength ->
              pure accumulatedRequest'
        _ ->
          if ByteString.null chunk
            then pure accumulatedRequest'
            else readRequestChunks accumulatedRequest' contentLength

parseHttpContentLength :: ByteString.ByteString -> Maybe Int
parseHttpContentLength requestBytes =
  case ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes of
    (_, withSeparator)
      | ByteString.null withSeparator -> Nothing
    (headerBytes, _) ->
      lookup "content-length" (mapMaybe parseCapturedHeader (drop 1 headerLines)) >>= readMaybe . ByteStringChar8.unpack
      where
        headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)

parseCapturedHeader :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString)
parseCapturedHeader headerLine =
  case ByteStringChar8.break (== ':') headerLine of
    (headerName, withSeparator)
      | ByteString.null withSeparator -> Nothing
      | otherwise ->
          Just
            ( ByteStringChar8.map toLower headerName,
              ByteStringChar8.dropWhile (== ' ') (stripHeaderLineEnd (ByteString.drop 1 withSeparator))
            )

stripHeaderLineEnd :: ByteString.ByteString -> ByteString.ByteString
stripHeaderLineEnd =
  ByteStringChar8.filter (/= '\r')

buildHttpResponse :: Http.Status -> ByteString.ByteString -> ByteString.ByteString
buildHttpResponse responseStatus responseBody =
  ByteStringChar8.pack $
    "HTTP/1.1 "
      <> show (Http.statusCode responseStatus)
      <> " "
      <> ByteStringChar8.unpack (Http.statusMessage responseStatus)
      <> "\r\nContent-Type: application/json\r\nContent-Length: "
      <> show (ByteString.length responseBody)
      <> "\r\nConnection: close\r\n\r\n"
      <> ByteStringChar8.unpack responseBody

readLoopbackHttpResponse :: Int -> Text -> IO Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpResponseBytes :: Int -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port path = do
  clientSocket <- socket AF_INET Stream defaultProtocol
  connect clientSocket
  SocketByteString.sendAll clientSocket (buildHttpRequest path)
  responseBytes <- readAllSocketChunks clientSocket
  close clientSocket
  pure (extractHttpBody responseBytes)
  where
    connect clientSocket =
      NetworkSocket.connect clientSocket (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))

waitForRuntimeServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForRuntimeServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runtime server to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runtime server to remain running, but it exited early"
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
                  expectationFailure "expected runtime server to accept loopback HTTP requests"
                    >> pure Text.empty

waitForRuntimeServerExit :: IORef (Maybe (Either SomeException ())) -> IO ()
waitForRuntimeServerExit completionReference =
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
              expectationFailure "expected runtime server to stop after being signalled"

buildHttpRequest :: Text -> ByteString.ByteString
buildHttpRequest path =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"

readAllSocketChunks :: NetworkSocket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (headers, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
      responseBody = ByteString.drop 4 withSeparator
   in if ByteStringChar8.isInfixOf "Transfer-Encoding: chunked" headers
        then decodeChunkedBody responseBody
        else responseBody

decodeChunkedBody :: ByteString.ByteString -> ByteString.ByteString
decodeChunkedBody chunkedBytes =
  case ByteStringChar8.breakSubstring "\r\n" chunkedBytes of
    (chunkSizeHex, withSizeSeparator)
      | ByteString.null withSizeSeparator ->
          chunkedBytes
      | otherwise ->
          case readHex (ByteStringChar8.unpack chunkSizeHex) of
            [(chunkSize, "")]
              | chunkSize == (0 :: Int) ->
                  ByteString.empty
              | otherwise ->
                  let chunkPayload = ByteString.drop 2 withSizeSeparator
                      (chunk, withChunkSuffix) = ByteString.splitAt chunkSize chunkPayload
                   in chunk <> decodeChunkedBody (ByteString.drop 2 withChunkSuffix)
            _ ->
              chunkedBytes

spec = do
  describe "defaultAppConfig" $ do
    it "reserves structured listener, static asset, and observability settings" $ do
      defaultAppConfig
        `shouldBe` AppConfig
          { appTitlePrefix = "web-api",
            listenerConfigs =
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
            requestPolicy =
              RequestPolicyConfig
                { redirectHttpToHttps = False,
                  httpsRedirectPort = Nothing,
                  strictTransportSecurity = Nothing,
                  trustForwardedHeaders = False,
                  corsPolicy = defaultCorsPolicyConfig,
                  responseSecurityHeaders = defaultResponseSecurityHeadersConfig
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
      HarchWeb.requestPolicy serverConfig `shouldBe` requestPolicy defaultAppConfig
      HarchWeb.observability serverConfig `shouldBe` observability defaultAppConfig

  describe "parseRuntimeAppConfig" $ do
    it "parses committed runtime defaults into the expected app config" $
      parseRuntimeAppConfig committedRuntimeDefaults [] []
        `shouldBe` Right defaultAppConfig

    it "fails when no listeners are configured" $
      parseRuntimeAppConfig
        [("APP_TITLE_PREFIX", "runtime-test")]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_HOST")

    it "parses multiple listeners in deterministic index order" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_2_SCHEME", "http"),
              ("LISTENER_1_PORT", "5002"),
              ("LISTENER_2_PORT", "5003"),
              ("LISTENER_1_HOST", "127.0.0.2"),
              ("LISTENER_2_HOST", "127.0.0.3"),
              ("LISTENER_1_SCHEME", "http")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.2",
                      listenerPort = 5002,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.3",
                      listenerPort = 5003,
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
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "requires HTTPS listeners to specify a TLS source" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_0_TLS_SOURCE")

    it "defaults redirects on and records the HTTPS port when one HTTP and one manual HTTPS listener are configured" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
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
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses shared HTTPS directories and ACME certificate publishing directories" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_ACME_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/shared-certs",
                                    sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just "/var/lib/web-api/shared-certs",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }

    it "defaults ACME publish directories and shared TLS reuse directories to .tls paths" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = ".tls/example.com",
                                    sharedCertificateStartupMode = AwaitCertificateFiles Nothing
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--cert-name,prod/example")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/prod/example",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--cert-name", "prod/example"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "one.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "acme"),
          ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_1_ACME_DOMAINS", "two.example.com"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5445"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "shared")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "LISTENER_2_TLS_CERTIFICATE_DIRECTORY")

    it "parses explicit shared TLS wait and fail-fast startup modes" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5444"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_1_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/preprovisioned-certs")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/shared-certs",
                                    sharedCertificateStartupMode = AwaitCertificateFiles (Just 15)
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = "/var/lib/web-api/preprovisioned-certs",
                                    sharedCertificateStartupMode = RequireCertificateFiles
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy = requestPolicy defaultAppConfig
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-fail-fast"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "15")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "15")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "shared-wait"),
          ("LISTENER_0_TLS_CERTIFICATE_DIRECTORY", "/var/lib/web-api/shared-certs"),
          ("LISTENER_0_TLS_SHARED_WAIT_SECONDS", "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SHARED_WAIT_SECONDS" "-1")

    it "defaults production ACME directory URLs and redirects on for HTTP ACME producers plus shared HTTPS listener plans" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "8080"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "shared-wait"),
          ("LISTENER_1_TLS_SHARED_WAIT_SECONDS", "120")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 8080,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme =
                        Just
                          AcmeConfig
                            { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                              acmeContactEmails = ["ops@example.com"],
                              acmeDomains = ["example.com", "www.example.com"],
                              acmeHttp01Port = 8080,
                              acmeCertificateDirectory = Just ".tls/example.com",
                              acmeCertbotConfig =
                                CertbotConfig
                                  { certbotExecutable = "certbot",
                                    certbotArguments = []
                                  }
                            }
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                SharedCertificateFiles
                                  { certificateDirectory = ".tls/example.com",
                                    sharedCertificateStartupMode = AwaitCertificateFiles (Just 120)
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses manual and ACME-backed HTTPS listeners distinctly" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_BAD_HOST", "ignored-host"),
              ("LISTENER_0_HOST", "0.0.0.0"),
              ("LISTENER_0_PORT", "5443"),
              ("LISTENER_0_SCHEME", "https"),
              ("LISTENER_0_TLS_SOURCE", "manual"),
              ("LISTENER_0_TLS_CERTIFICATE_FILE", "cert.pem"),
              ("LISTENER_0_TLS_PRIVATE_KEY_FILE", "key.pem"),
              ("LISTENER_1_HOST", "0.0.0.0"),
              ("LISTENER_1_PORT", "5444"),
              ("LISTENER_1_SCHEME", "https"),
              ("LISTENER_1_TLS_SOURCE", "acme"),
              ("LISTENER_1_ACME_DIRECTORY_URL", "https://acme-staging-v02.api.letsencrypt.org/directory"),
              ("LISTENER_1_ACME_CONTACT_EMAILS", "ops@example.com,alerts@example.com"),
              ("LISTENER_1_ACME_DOMAINS", "example.com,www.example.com"),
              ("LISTENER_2_HOST", "0.0.0.0"),
              ("LISTENER_2_PORT", "5445"),
              ("LISTENER_2_SCHEME", "https"),
              ("LISTENER_2_TLS_SOURCE", "acme"),
              ("LISTENER_2_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
              ("LISTENER_2_ACME_CONTACT_EMAILS", "ops@example.com"),
              ("LISTENER_2_ACME_DOMAINS", "example.com"),
              ("LISTENER_2_ACME_CERTBOT_ARGUMENTS", "certonly,--webroot,--agree-tos")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "0.0.0.0",
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
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5444,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "0.0.0.0",
                      listenerPort = 5445,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = ["certonly", "--webroot", "--agree-tos"]
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "rejects invalid listener scheme and TLS source values" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "tcp")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_SCHEME" "tcp")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "0.0.0.0"),
          ("LISTENER_0_PORT", "5443"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "vault")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_TLS_SOURCE" "vault")

    it "parses static asset roots and cache policy into the expected config" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_ROOT_2_DIRECTORY", "vendor/public"),
              ("STATIC_ASSET_ROOT_1_URL_PREFIX", "/assets"),
              ("STATIC_ASSET_ROOT_2_URL_PREFIX", "/vendor"),
              ("STATIC_ASSET_ROOT_1_DIRECTORY", "public"),
              ("STATIC_CACHE_CONTROL_SECONDS", "3600")
            ]
      parseRuntimeAppConfig committedDefaults [] []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
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
                  { staticAssetRoots =
                      [ StaticAssetRoot
                          { staticUrlPrefix = "/assets",
                            staticDirectory = "public"
                          },
                        StaticAssetRoot
                          { staticUrlPrefix = "/vendor",
                            staticDirectory = "vendor/public"
                          }
                      ],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }

    it "parses numbered static asset content type entries including extensionless opt-in" $ do
      let committedDefaults =
            [ ("APP_TITLE_PREFIX", "runtime-test"),
              ("LISTENER_0_HOST", "127.0.0.1"),
              ("LISTENER_0_PORT", "5001"),
              ("LISTENER_0_SCHEME", "http"),
              ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm"),
              ("STATIC_ASSET_CONTENT_TYPE_2_EXTENSION", ""),
              ("STATIC_ASSET_CONTENT_TYPE_2_MIME_TYPE", "application/octet-stream")
            ]
      fmap (staticAssetContentTypes . staticAssets) (parseRuntimeAppConfig committedDefaults [] [])
        `shouldBe` Right
          [ (".wasm", "application/wasm"),
            ("", "application/octet-stream")
          ]

    it "parses redirect and HSTS request policy values for TLS-offload deployments" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "true"),
          ("HSTS_MAX_AGE_SECONDS", "31536000"),
          ("HSTS_INCLUDE_SUBDOMAINS", "true"),
          ("HSTS_PRELOAD", "true")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
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
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses explicit false redirect and HSTS flags without changing the default policy shape" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("REDIRECT_HTTP_TO_HTTPS", "false"),
          ("HSTS_MAX_AGE_SECONDS", "86400"),
          ("HSTS_INCLUDE_SUBDOMAINS", "false"),
          ("HSTS_PRELOAD", "false")
        ]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
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
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "defaults optional HSTS booleans to false when only max-age is configured" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_MAX_AGE_SECONDS", "86400")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
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
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses trusted forwarded-header mode explicitly when enabled" $
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("TRUST_FORWARDED_HEADERS", "true")]
        `shouldBe` Right
          defaultAppConfig
            { requestPolicy =
                (requestPolicy defaultAppConfig)
                  { trustForwardedHeaders = True
                  }
            }

    it "parses CORS and response security policy overrides" $
      fmap
        requestPolicy
        ( parseRuntimeAppConfig
            committedRuntimeDefaults
            []
            [ ("CORS_ALLOWED_ORIGINS", "https://app.example.com, https://admin.example.com"),
              ("CORS_ALLOWED_METHODS", "GET, HEAD"),
              ("CORS_ALLOWED_HEADERS", "Content-Type, X-Requested-With"),
              ("CORS_MAX_AGE_SECONDS", "600"),
              ("CONTENT_SECURITY_POLICY", "default-src 'self'; connect-src 'self' https://collector.example.com"),
              ("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "false"),
              ("X_XSS_PROTECTION", "0"),
              ("REFERRER_POLICY", "no-referrer"),
              ("PERMISSIONS_POLICY", "camera=()"),
              ("X_FRAME_OPTIONS", "SAMEORIGIN")
            ]
        )
        `shouldBe` Right
          ( (requestPolicy defaultAppConfig)
              { corsPolicy =
                  CorsPolicyConfig
                    { corsAllowedOrigins = ["https://app.example.com", "https://admin.example.com"],
                      corsAllowedMethods = ["GET", "HEAD"],
                      corsAllowedHeaders = ["Content-Type", "X-Requested-With"],
                      corsMaxAgeSeconds = Just 600
                    },
                responseSecurityHeaders =
                  ResponseSecurityHeadersConfig
                    { contentSecurityPolicy = Just "default-src 'self'; connect-src 'self' https://collector.example.com",
                      contentTypeOptionsNoSniff = False,
                      xssProtection = Just "0",
                      referrerPolicy = Just "no-referrer",
                      permissionsPolicy = Just "camera=()",
                      frameOptions = Just "SAMEORIGIN"
                    }
              }
          )

    it "lets REDIRECT_HTTP_TO_HTTPS=false disable the listener-aware default for dual listeners" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "5443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "key.pem")
        ]
        []
        [("REDIRECT_HTTP_TO_HTTPS", "false")]
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
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
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = False,
                    httpsRedirectPort = Just 5443,
                    strictTransportSecurity = Nothing,
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "keeps redirects on but leaves the redirect port implicit when multiple HTTPS ports exist" $
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("LISTENER_1_HOST", "127.0.0.1"),
          ("LISTENER_1_PORT", "443"),
          ("LISTENER_1_SCHEME", "https"),
          ("LISTENER_1_TLS_SOURCE", "manual"),
          ("LISTENER_1_TLS_CERTIFICATE_FILE", "https-443-cert.pem"),
          ("LISTENER_1_TLS_PRIVATE_KEY_FILE", "https-443-key.pem"),
          ("LISTENER_2_HOST", "127.0.0.1"),
          ("LISTENER_2_PORT", "5443"),
          ("LISTENER_2_SCHEME", "https"),
          ("LISTENER_2_TLS_SOURCE", "manual"),
          ("LISTENER_2_TLS_CERTIFICATE_FILE", "https-5443-cert.pem"),
          ("LISTENER_2_TLS_PRIVATE_KEY_FILE", "https-5443-key.pem")
        ]
        []
        []
        `shouldBe` Right
          defaultAppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Http,
                      listenerTls = Nothing,
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "https-443-cert.pem",
                                    privateKeyFile = "https-443-key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    },
                  ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5443,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                ManualCertificateFiles
                                  { certificateFile = "https-5443-cert.pem",
                                    privateKeyFile = "https-5443-key.pem"
                                  }
                            },
                      listenerAcme = Nothing
                    }
                ],
              requestPolicy =
                RequestPolicyConfig
                  { redirectHttpToHttps = True,
                    httpsRedirectPort = Nothing,
                    strictTransportSecurity = Nothing,
                    trustForwardedHeaders = False,
                    corsPolicy = defaultCorsPolicyConfig,
                    responseSecurityHeaders = defaultResponseSecurityHeadersConfig
                  }
            }

    it "parses tracing and metrics exporters independently while preserving header order" $ do
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token;x-api-key=secret")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders =
                              [ ("authorization", "Bearer token"),
                                ("x-api-key", "secret")
                              ]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://collector:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = [("authorization", "Bearer token")]
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENABLED", "true")]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter =
                      Just
                        OtlpExporter
                          { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                            otlpHeaders = []
                          },
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_TRACING_ENABLED", "false"),
          ("OTLP_TRACING_ENDPOINT", "http://collector:4318/v1/traces"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          defaultAppConfig
            { observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("OTLP_METRICS_ENDPOINT", "http://collector:4318/v1/metrics"),
          ("OTLP_METRICS_HEADERS", "x-scope=metrics;broken-entry")
        ]
        `shouldBe` Left
          ( InvalidConfigValue
              "OTLP_METRICS_HEADERS"
              "x-scope=metrics;broken-entry"
          )

    it "fails invalid runtime values with explicit errors" $ do
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "0"),
          ("LISTENER_0_SCHEME", "http")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_PORT" "0")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", ""),
          ("LISTENER_0_ACME_DOMAINS", "")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_CONTACT_EMAILS" "")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_DOMAINS" "")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_DIRECTORY_URL", "https://acme-v02.api.letsencrypt.org/directory"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_CHALLENGE_BACKEND", "shell-script")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_ACME_CHALLENGE_BACKEND" "shell-script")
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com"),
          ("LISTENER_0_ACME_CERTBOT_EXECUTABLE", "certbot")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com"),
          ("LISTENER_0_ACME_DOMAINS", "example.com,www.example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = ["example.com", "www.example.com"],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/example.com",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "https"),
          ("LISTENER_0_TLS_SOURCE", "acme"),
          ("LISTENER_0_ACME_CONTACT_EMAILS", "ops@example.com")
        ]
        []
        []
        `shouldBe` Right
          AppConfig
            { appTitlePrefix = "runtime-test",
              listenerConfigs =
                [ ListenerConfig
                    { listenerHost = "127.0.0.1",
                      listenerPort = 5001,
                      listenerScheme = Https,
                      listenerTls =
                        Just
                          TlsConfig
                            { certificateSource =
                                AcmeCertificateSource
                                  AcmeConfig
                                    { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                                      acmeContactEmails = ["ops@example.com"],
                                      acmeDomains = [],
                                      acmeHttp01Port = 80,
                                      acmeCertificateDirectory = Just ".tls/listener-0",
                                      acmeCertbotConfig =
                                        CertbotConfig
                                          { certbotExecutable = "certbot",
                                            certbotArguments = []
                                          }
                                    }
                            },
                      listenerAcme = Nothing
                    }
                ],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Nothing
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability =
                ObservabilityConfig
                  { tracingExporter = Nothing,
                    metricsExporter = Nothing
                  }
            }
      parseRuntimeAppConfig
        [ ("APP_TITLE_PREFIX", "runtime-test"),
          ("LISTENER_0_HOST", "127.0.0.1"),
          ("LISTENER_0_PORT", "5001"),
          ("LISTENER_0_SCHEME", "http"),
          ("STATIC_CACHE_CONTROL_SECONDS", "-1")
        ]
        []
        []
        `shouldBe` Left (InvalidConfigValue "STATIC_CACHE_CONTROL_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", "wasm"),
          ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "application/wasm")
        ]
        `shouldBe` Left (InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_EXTENSION" "wasm")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("STATIC_ASSET_CONTENT_TYPE_1_EXTENSION", ".wasm"),
          ("STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE", "")
        ]
        `shouldBe` Left (InvalidConfigValue "STATIC_ASSET_CONTENT_TYPE_1_MIME_TYPE" "")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_HEADERS", "authorization=Bearer token")]
        `shouldBe` Left (MissingConfigValue "OTLP_TRACING_ENDPOINT")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("OTLP_TRACING_ENABLED", "maybe")]
        `shouldBe` Left (InvalidConfigValue "OTLP_TRACING_ENABLED" "maybe")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("REDIRECT_HTTP_TO_HTTPS", "maybe")]
        `shouldBe` Left (InvalidConfigValue "REDIRECT_HTTP_TO_HTTPS" "maybe")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_INCLUDE_SUBDOMAINS", "true")]
        `shouldBe` Left (MissingConfigValue "HSTS_MAX_AGE_SECONDS")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [ ("HSTS_MAX_AGE_SECONDS", "31536000"),
          ("HSTS_PRELOAD", "sometimes")
        ]
        `shouldBe` Left (InvalidConfigValue "HSTS_PRELOAD" "sometimes")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("HSTS_MAX_AGE_SECONDS", "-1")]
        `shouldBe` Left (InvalidConfigValue "HSTS_MAX_AGE_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CORS_ALLOWED_ORIGINS", " , ")]
        `shouldBe` Left (InvalidConfigValue "CORS_ALLOWED_ORIGINS" " , ")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CORS_MAX_AGE_SECONDS", "-1")]
        `shouldBe` Left (InvalidConfigValue "CORS_MAX_AGE_SECONDS" "-1")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("CONTENT_SECURITY_POLICY", "")]
        `shouldBe` Left (InvalidConfigValue "CONTENT_SECURITY_POLICY" "")
      parseRuntimeAppConfig
        committedRuntimeDefaults
        []
        [("X_CONTENT_TYPE_OPTIONS_NOSNIFF", "maybe")]
        `shouldBe` Left (InvalidConfigValue "X_CONTENT_TYPE_OPTIONS_NOSNIFF" "maybe")

  describe "defaultAppEnvironmentConfig" $ do
    it "keeps committed .env defaults aligned with the parsed development config" $ do
      committedEnvDefaults
        `shouldBe` [ ("APP_MODE", "development"),
                     ("DATABASE_HOST", "127.0.0.1"),
                     ("DATABASE_PORT", "5432"),
                     ("DATABASE_NAME", "web_api_dev"),
                     ("DATABASE_USER", "web_api_runtime"),
                     ("DATABASE_PASSWORD", "web_api"),
                     ("SMTP_HOST", "127.0.0.1"),
                     ("SMTP_PORT", "5025"),
                     ("SMTP_HELO_NAME", "localhost"),
                     ("SMTP_USER", "test@localhost"),
                     ("SMTP_PASSWORD", "password"),
                     ("EMAIL_FROM", "noreply@localhost"),
                     ("PUBLIC_BASE_URL", "http://127.0.0.1:5001"),
                     ("TOTP_ENCRYPTION_KEY", "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                   ]
      smtpDeliveryHost (smtpDeliveryConfig defaultAppEnvironmentConfig) `shouldBe` "127.0.0.1"
      smtpDeliveryPort (smtpDeliveryConfig defaultAppEnvironmentConfig) `shouldBe` 5025
      publicBaseUrl defaultAppEnvironmentConfig `shouldBe` "http://127.0.0.1:5001"
      runtimeMarker <- lookupEnv "PATH"
      let dynamicSmtpPort = if isNothing runtimeMarker then 2526 else 2527
      smtpDeliveryConfig defaultAppEnvironmentConfig
        /= (smtpDeliveryConfig defaultAppEnvironmentConfig)
          { smtpDeliveryPort = dynamicSmtpPort
          }
          `shouldBe` True
      let dynamicSmtpConfig =
            (smtpDeliveryConfig defaultAppEnvironmentConfig)
              { smtpDeliveryPort = dynamicSmtpPort
              }
          dynamicEnvironmentConfig = defaultAppEnvironmentConfig {smtpDeliveryConfig = dynamicSmtpConfig}
      show dynamicEnvironmentConfig
        `shouldBe` ( "AppEnvironmentConfig {appMode = Development, databaseConfig = DatabaseConfig {databaseHost = \"127.0.0.1\", databasePort = 5432, databaseName = \"web_api_dev\", databaseUser = \"web_api_runtime\", databasePassword = \"web_api\"}, smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = "
                       <> show dynamicSmtpPort
                       <> ", smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = \"password\"}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}"
                   )

    it "builds localized verification URLs and delivers through the native loopback SMTP server" $
      bracket DevSmtp.startDevSmtpServer DevSmtp.stopDevSmtpServer $ \server -> do
        let environmentConfig =
              defaultAppEnvironmentConfig
                { publicBaseUrl = "https://accounts.example.test/",
                  smtpDeliveryConfig =
                    (smtpDeliveryConfig defaultAppEnvironmentConfig)
                      { smtpDeliveryPort = fromIntegral (DevSmtp.devSmtpPort server)
                      }
                }
            workflow = buildRuntimeAccountWorkflow environmentConfig
            baseUrlWithoutTrailingSlash = "https://accounts.example.test:" <> Text.pack (show (DevSmtp.devSmtpPort server))
            untrimmedWorkflow = buildRuntimeAccountWorkflow (environmentConfig {publicBaseUrl = baseUrlWithoutTrailingSlash})
            token = requiredVerificationToken (Text.replicate 43 "a")
            recipient = requiredEmailAddress "person@example.test"
        accountWorkflowVerificationUrl workflow (defaultRequestContext {requestLocale = Spanish}) token
          `shouldBe` "https://accounts.example.test/es/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        Text.unpack (accountWorkflowVerificationUrl workflow defaultRequestContext token)
          `shouldBe` "https://accounts.example.test/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        accountWorkflowVerificationUrl untrimmedWorkflow defaultRequestContext token
          `shouldBe` baseUrlWithoutTrailingSlash
          <> "/verify?token=aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        accountWorkflowStore workflow `seq` pure ()
        accountWorkflowPasswordHasher workflow `seq` pure ()
        accountWorkflowMfaStore workflow `seq` pure ()
        accountWorkflowCredentialStore workflow `seq` pure ()
        accountWorkflowSessionStore workflow `seq` pure ()
        accountWorkflowProfileStore workflow `seq` pure ()
        accountWorkflowTotpEncryptionKey workflow `seq` pure ()
        accountWorkflowClock workflow >>= (`shouldSatisfy` (> 0))
        accountWorkflowTotpClock workflow >>= (`shouldSatisfy` (> 0))
        case Email.mkEmailMessage (Email.EmailMessageInput recipient "Verification test" "Hello") of
          Nothing -> expectationFailure "expected a valid SMTP test message"
          Just message -> Email.deliverEmail (accountWorkflowEmailDelivery workflow) message
        awaitDevSmtpEmail server "person@example.test"
          >>= \case
            Just received ->
              "Subject: Verification test"
                `ByteString.isInfixOf` DevSmtp.devSmtpRawMessage received
                `shouldBe` True
            Nothing -> expectationFailure "expected the loopback SMTP server to receive the message"
        let runtimeApplication = buildRuntimeApp defaultAppConfig environmentConfig
        HarchWeb.renderResponse
          runtimeApplication
          (HarchWeb.RouteRequest StatusApiRoute (defaultRequestContext {requestSurface = ApiSurface}))
          >>= (`shouldSatisfy` \case HarchWeb.BodyResponse _ -> True; _ -> False)
        HarchWeb.reportConnectionObservability
          runtimeApplication
          (Observability.buildConnectionObservability "CONNECTION runtime-account-workflow-test" [])

    it "rejects invalid SMTP runtime delivery configurations" $ do
      let recipient = requiredEmailAddress "person@example.test"
          invalidSenderWorkflow =
            buildRuntimeAccountWorkflow
              defaultAppEnvironmentConfig
                { smtpDeliveryConfig = (smtpDeliveryConfig defaultAppEnvironmentConfig) {smtpDeliverySender = "not-an-email"}
                }
          invalidHeloWorkflow =
            buildRuntimeAccountWorkflow
              defaultAppEnvironmentConfig
                { smtpDeliveryConfig = (smtpDeliveryConfig defaultAppEnvironmentConfig) {smtpDeliveryHeloName = "bad\nhelo"}
                }
      case Email.mkEmailMessage (Email.EmailMessageInput recipient "Verification test" "Hello") of
        Nothing -> expectationFailure "expected a valid SMTP test message"
        Just message ->
          forM_ [invalidSenderWorkflow, invalidHeloWorkflow] $ \invalidWorkflow ->
            (try (Email.deliverEmail (accountWorkflowEmailDelivery invalidWorkflow) message) :: IO (Either IOException ()))
              >>= \case
                Left errorValue -> displayException errorValue `shouldContain` "SMTP delivery configuration is invalid"
                Right () -> expectationFailure "invalid SMTP configuration unexpectedly delivered email"

    it "covers the new app/database config selectors and derived instances" $ do
      let productionDatabaseConfig =
            DatabaseConfig
              { databaseHost = "db.internal",
                databasePort = 6543,
                databaseName = "web_api_prod",
                databaseUser = "web_api_app",
                databasePassword = "super-secret"
              }
          productionEnvironmentConfig =
            defaultAppEnvironmentConfig
              { appMode = Production,
                databaseConfig = productionDatabaseConfig
              }
      appMode productionEnvironmentConfig `shouldBe` Production
      databaseConfig productionEnvironmentConfig `shouldBe` productionDatabaseConfig
      databaseHost productionDatabaseConfig `shouldBe` "db.internal"
      databasePort productionDatabaseConfig `shouldBe` 6543
      databaseName productionDatabaseConfig `shouldBe` "web_api_prod"
      databaseUser productionDatabaseConfig `shouldBe` "web_api_app"
      databasePassword productionDatabaseConfig `shouldBe` "super-secret"
      Development `shouldNotBe` Test
      Test `shouldNotBe` Production
      productionDatabaseConfig `shouldBe` productionDatabaseConfig
      productionDatabaseConfig
        `shouldNotBe` productionDatabaseConfig
          { databasePassword = "different-secret"
          }
      productionEnvironmentConfig `shouldBe` productionEnvironmentConfig
      productionEnvironmentConfig
        `shouldNotBe` productionEnvironmentConfig
          { appMode = Test
          }
      MissingConfigValue "DATABASE_PASSWORD"
        `shouldNotBe` InvalidConfigValue "DATABASE_PASSWORD" "missing"
      show Development `shouldBe` "Development"
      show Test `shouldBe` "Test"
      show Production `shouldBe` "Production"
      show [Development, Test, Production] `shouldBe` "[Development,Test,Production]"
      show productionDatabaseConfig
        `shouldBe` "DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}"
      show [productionDatabaseConfig]
        `shouldBe` "[DatabaseConfig {databaseHost = \"db.internal\", databasePort = 6543, databaseName = \"web_api_prod\", databaseUser = \"web_api_app\", databasePassword = \"super-secret\"}]"
      show productionEnvironmentConfig
        `shouldContain` "smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = 5025, smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = \"password\"}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}"
      show [productionEnvironmentConfig]
        `shouldContain` "smtpDeliveryConfig = SmtpDeliveryConfig {smtpDeliveryHost = \"127.0.0.1\", smtpDeliveryPort = 5025, smtpDeliveryHeloName = \"localhost\", smtpDeliverySender = \"noreply@localhost\", smtpDeliveryUsername = \"test@localhost\", smtpDeliveryPassword = \"password\"}, publicBaseUrl = \"http://127.0.0.1:5001\", totpEncryptionKey = <redacted>}]"
      show (MissingConfigValue "DATABASE_PASSWORD") `shouldBe` "MissingConfigValue \"DATABASE_PASSWORD\""
      show (InvalidConfigValue "APP_MODE" "staging") `shouldBe` "InvalidConfigValue \"APP_MODE\" \"staging\""
      show [MissingConfigValue "DATABASE_PASSWORD", InvalidConfigValue "APP_MODE" "staging"]
        `shouldBe` "[MissingConfigValue \"DATABASE_PASSWORD\",InvalidConfigValue \"APP_MODE\" \"staging\"]"

  describe "defaultDatabaseSeed" $ do
    it "defines deterministic page-facing seeded results for both locales" $
      defaultDatabaseSeed
        `shouldBe` DatabaseSeed
          { englishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            spanishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                  },
            englishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            spanishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  }
          }

    it "keeps seeded database data serializable and stable for tests" $ do
      let homePageData = HomePageData {homePageDataSummary = "Seeded home"}
          otherHomePageData = HomePageData {homePageDataSummary = "Different home"}
          secondPageData =
            SecondPageData
              { secondPageDataSummary = "Seeded second",
                secondPageDataHighlights = ["One"]
              }
          otherSecondPageData =
            SecondPageData
              { secondPageDataSummary = "Other second",
                secondPageDataHighlights = []
              }
          homeError = HomePageDataError "home unavailable"
          secondError = SecondPageDataError "second unavailable"
          databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          databaseResult =
            DatabaseResult
              { databaseResultValue = Right homePageData,
                databaseResultOperations = [databaseOperation]
              }
          seededDatabase =
            DatabaseSeed
              { englishHomePageData = Right homePageData,
                spanishHomePageData = Left homeError,
                englishSecondPageData = Right secondPageData,
                spanishSecondPageData = Left secondError
              }
      homePageData `shouldBe` homePageData
      homePageData `shouldNotBe` otherHomePageData
      secondPageData `shouldBe` secondPageData
      secondPageData `shouldNotBe` otherSecondPageData
      homeError `shouldBe` homeError
      homeError `shouldNotBe` secondError
      databaseOperation `shouldBe` databaseOperation
      databaseOperation `shouldNotBe` databaseOperation {databaseOperationName = "load-home-page-summary"}
      databaseResult `shouldBe` databaseResult
      databaseResult
        `shouldNotBe` databaseResult
          { databaseResultOperations = []
          }
      seededDatabase `shouldBe` seededDatabase
      seededDatabase
        `shouldNotBe` seededDatabase
          { spanishSecondPageData = Right otherSecondPageData
          }
      show (HomePageData {homePageDataSummary = "Seeded home"})
        `shouldBe` "HomePageData {homePageDataSummary = \"Seeded home\"}"
      show (SecondPageData {secondPageDataSummary = "Seeded second", secondPageDataHighlights = ["One"]})
        `shouldBe` "SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}"
      show (HomePageDataError "home unavailable")
        `shouldBe` "HomePageDataError \"home unavailable\""
      show (SecondPageDataError "second unavailable")
        `shouldBe` "SecondPageDataError \"second unavailable\""
      show databaseOperation
        `shouldBe` "DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}"
      show databaseResult
        `shouldBe` "DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show seededDatabase
        `shouldBe` "DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), spanishHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}"
      show [HomePageData {homePageDataSummary = "Seeded home"}]
        `shouldBe` "[HomePageData {homePageDataSummary = \"Seeded home\"}]"
      show [homeError, secondError]
        `shouldBe` "[HomePageDataError \"home unavailable\",SecondPageDataError \"second unavailable\"]"
      show [databaseOperation]
        `shouldBe` "[DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]"
      show [databaseResult]
        `shouldBe` "[DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      show
        [ SecondPageData
            { secondPageDataSummary = "Seeded second",
              secondPageDataHighlights = ["One"]
            }
        ]
        `shouldBe` "[SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}]"
      show [seededDatabase]
        `shouldBe` "[DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), spanishHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}]"

  describe "buildSeededDatabaseEffect" $ do
    it "loads page-oriented seeded data for both English and Spanish requests" $ do
      let englishEffect = buildSeededDatabaseEffect defaultDatabaseSeed
      loadHomePageDataWithObservability englishEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations = []
          }
      loadHomePageData englishEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageData englishEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadHomePageData englishEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageData englishEffect spanishRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadSecondPageDataWithObservability englishEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            databaseResultOperations = []
          }

    it "returns explicit seeded errors without collapsing page-specific failures" $ do
      let seededEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  spanishHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Inicio sembrado"
                        },
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Second seed",
                          secondPageDataHighlights = ["Known branch"]
                        },
                  spanishSecondPageData = Left (SecondPageDataError "second seed unavailable")
                }
      loadHomePageData seededEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "home seed unavailable")
      loadSecondPageData seededEffect spanishRequestContext
        `shouldReturn` Left (SecondPageDataError "second seed unavailable")
      loadSecondPageDataWithObservability seededEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "second seed unavailable"),
            databaseResultOperations = []
          }

    it "keeps the default seeded interpreter deterministic for repeated requests" $ do
      firstHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      secondHome <- loadHomePageData defaultDatabaseEffect defaultRequestContext
      firstHome `shouldBe` secondHome
      firstSecond <- loadSecondPageData defaultDatabaseEffect spanishRequestContext
      secondSecond <- loadSecondPageData defaultDatabaseEffect spanishRequestContext
      firstSecond `shouldBe` secondSecond

  describe "selectRouteData" $ do
    it "selects the same second-route domain data for page and API surfaces" $ do
      let seededDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Shared domain summary.",
                          secondPageDataHighlights = ["Shared loader", "Shared renderer"]
                        },
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectedRouteData <- selectRouteDataWithDatabase seededDatabaseEffect secondRequest
      selectedRouteData
        `shouldBe` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Shared domain summary.",
                  secondRouteHighlights = ["Shared loader", "Shared renderer"]
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect apiSecondRequest `shouldReturn` selectedRouteData

    it "keeps route-data selections deterministic while exposing database operations separately" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          routeDataSelection =
            RouteDataSelection
              { routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Shared domain summary", secondRouteHighlights = []})),
                routeDataDatabaseOperations = [databaseOperation]
              }
      routeDataSelection `shouldBe` routeDataSelection
      routeDataSelection
        `shouldNotBe` routeDataSelection
          { routeDataDatabaseOperations = []
          }
      show routeDataSelection
        `shouldBe` "RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show [routeDataSelection]
        `shouldBe` "[RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      selectRouteDataSelectionWithDatabase (buildSeededDatabaseEffect defaultDatabaseSeed) secondRequest
        `shouldReturn` RouteDataSelection
          { routeDataResult =
              SecondRouteDataResult
                ( Right
                    SecondRouteData
                      { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                        secondRouteHighlights = []
                      }
                ),
            routeDataDatabaseOperations = []
          }

    it "loads home-route data from the database effect and preserves explicit failures" $ do
      let seededDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Loaded from the seeded database effect."
                        },
                  spanishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectRouteDataWithDatabase seededDatabaseEffect homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Loaded from the seeded database effect."
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect spanishHomeRequest
        `shouldReturn` HomeRouteDataResult
          (Left (HomePageDataError "home seed unavailable"))

    it "preserves home-route database operations alongside selected data" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-home-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          observedHomeEffect =
            defaultDatabaseEffect
              { loadHomePageDataWithObservability =
                  \_ ->
                    pure
                      DatabaseResult
                        { databaseResultValue = Right (HomePageData "Observed home summary."),
                          databaseResultOperations = [databaseOperation]
                        }
              }
      selectRouteDataSelectionWithDatabase observedHomeEffect homeRequest
        `shouldReturn` RouteDataSelection
          { routeDataResult = HomeRouteDataResult (Right (HomeRouteData "Observed home summary.")),
            routeDataDatabaseOperations = [databaseOperation]
          }

    it "keeps route-data selectors and derived instances deterministic for tests" $ do
      let homeRouteData =
            HomeRouteData
              { homeRouteSummary = "Stubbed home summary"
              }
          otherHomeRouteData =
            HomeRouteData
              { homeRouteSummary = "Different home summary"
              }
          secondRouteData =
            SecondRouteData
              { secondRouteSummary = "Shared domain summary",
                secondRouteHighlights = ["Shared loader"]
              }
          statusApiData =
            StatusApiData
              { statusApiLocale = Spanish
              }
          routeDataResult = HomeRouteDataResult (Right homeRouteData)
      homeRouteSummary homeRouteData `shouldBe` "Stubbed home summary"
      secondRouteSummary secondRouteData `shouldBe` "Shared domain summary"
      secondRouteHighlights secondRouteData `shouldBe` ["Shared loader"]
      statusApiLocale statusApiData `shouldBe` Spanish
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
      show statusApiData `shouldBe` "StatusApiData {statusApiLocale = Spanish}"
      show routeDataResult
        `shouldBe` "HomeRouteDataResult (Right (HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}))"
      show (SecondRouteDataResult (Right secondRouteData))
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show (StatusApiDataResult statusApiData)
        `shouldBe` "StatusApiDataResult (StatusApiData {statusApiLocale = Spanish})"
      show SpacesRouteDataResult `shouldBe` "SpacesRouteDataResult"
      show [homeRouteData] `shouldBe` "[HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}]"
      show [secondRouteData]
        `shouldBe` "[SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}]"
      show [statusApiData] `shouldBe` "[StatusApiData {statusApiLocale = Spanish}]"
      show [SpacesRouteDataResult] `shouldBe` "[SpacesRouteDataResult]"
      show [NotFoundRouteDataResult] `shouldBe` "[NotFoundRouteDataResult]"

    it "selects default stubbed and status route data without extra wiring" $ do
      selectRouteData homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Server-rendered home page with stubbed content."
                }
          )
      selectRouteData secondRequest
        `shouldReturn` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                  secondRouteHighlights = []
                }
          )
      selectRouteData spacesRequest `shouldReturn` SpacesRouteDataResult
      selectRouteDataSelectionWithDatabase (buildSeededDatabaseEffect defaultDatabaseSeed) spacesRequest
        `shouldReturn` RouteDataSelection SpacesRouteDataResult []
      selectRouteData spanishApiStatusRequest
        `shouldReturn` StatusApiDataResult
          StatusApiData
            { statusApiLocale = Spanish
            }
      selectRouteData apiNotFoundRequest `shouldReturn` NotFoundRouteDataResult

  describe "WebApi.Account" $ do
    it "persists only a password hash and verification digest before delivering a localized verification email" $ do
      pendingAccountsReference <- newIORef []
      deliveredMessagesReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> do
                  modifyIORef' pendingAccountsReference (<> [pendingAccount])
                  pure (Right True),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          emailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          emailAddress = requiredEmailAddress "person@example.test"
      registrationResult <-
        registerAccountAt
          testPasswordHashingPolicy
          accountStore
          emailDelivery
          Email.EmailSpanish
          (\token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token)
          100
          200
          emailAddress
          (Password.mkPassword "correct horse battery staple")
      pendingAccounts <- readIORef pendingAccountsReference
      deliveredMessages <- readIORef deliveredMessagesReference
      createdAccountId <-
        case registrationResult of
          Right (RegistrationCreated accountId) -> pure accountId
          _ -> expectationFailure "expected a created registration" >> pure (requiredAccountId "unreachable")
      case (pendingAccounts, deliveredMessages) of
        ([pendingAccount], [message]) -> do
          createdAccountId `shouldBe` pendingAccountId pendingAccount
          pendingAccountEmail pendingAccount `shouldBe` emailAddress
          pendingAccountCreatedAtNanoseconds pendingAccount `shouldBe` 100
          Account.storedVerificationAccountId (pendingAccountVerification pendingAccount) `shouldBe` pendingAccountId pendingAccount
          Account.storedVerificationEmail (pendingAccountVerification pendingAccount) `shouldBe` emailAddress
          Account.storedVerificationExpiresAtNanoseconds (pendingAccountVerification pendingAccount) `shouldBe` 300
          Account.emailVerificationTokenDigestText (Account.storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) `shouldSatisfy` (not . Text.null)
          Password.verifyPassword (Password.mkPassword "correct horse battery staple") (pendingAccountPasswordHash pendingAccount) `shouldBe` True
          Email.emailMessageRecipient message `shouldBe` emailAddress
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electronico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electronico:\nhttps://account.example.test/es/verify?token="
        _ -> expectationFailure "expected exactly one pending account and verification email"

    it "persists typed account identity without changing verification delivery" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right True),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          username = fromMaybe (error "expected username") (Username.mkUsername "person_01")
          emailAddress = requiredEmailAddress "person@example.test"
      assertRegistrationResult
        ( registerAccountWithIdentityAt
            testPasswordHashingPolicy
            accountStore
            (Email.EmailDelivery (\_ -> pure ()))
            Email.EmailEnglish
            (const "https://account.example.test/verify")
            100
            200
            (Just username)
            (Just "Person Example")
            emailAddress
            (Password.mkPassword "correct horse battery staple")
        )
        (\case Right (RegistrationCreated _) -> True; _ -> False)
      pendingAccounts <- readIORef pendingAccountsReference
      case pendingAccounts of
        [pendingAccount] -> do
          pendingAccountUsername pendingAccount `shouldBe` Just username
          pendingAccountDisplayName pendingAccount `shouldBe` Just "Person Example"
        _ -> expectationFailure "expected exactly one pending account"

    it "covers password-hashing failures and account-workflow value representations" $ do
      let accountStore =
            AccountStore
              { createPendingAccount = \_ -> error "password hashing should stop before persistence",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          emailDelivery = Email.EmailDelivery (\_ -> error "password hashing should stop before delivery")
          emailAddress = requiredEmailAddress "person@example.test"
          accountId = requiredAccountId "account_01"
      assertRegistrationResult
        (registerAccountAtWithPasswordHasher (\_ _ -> pure Nothing) testPasswordHashingPolicy accountStore emailDelivery Email.EmailEnglish (const "https://account.example.test/verify") 100 200 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Left RegistrationPasswordHashingFailed -> True; _ -> False)
      pendingAccountsReference <- newIORef []
      let successfulStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right True),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
      assertRegistrationResult
        (registerAccountAtWithPasswordHasher Password.hashPassword testPasswordHashingPolicy successfulStore (Email.EmailDelivery (\_ -> pure ())) Email.EmailEnglish (const "https://account.example.test/verify") 100 200 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Right (RegistrationCreated _) -> True; _ -> False)
      readIORef pendingAccountsReference >>= \case
        [pendingAccount] -> do
          pendingAccountUsername pendingAccount `shouldBe` Nothing
          pendingAccountDisplayName pendingAccount `shouldBe` Nothing
        _ -> expectationFailure "expected one pending account"
      Account.accountIdText accountId `shouldBe` "account_01"
      equalValues (AccountStoreUnavailable "database unavailable") (AccountStoreUnavailable "database unavailable") `shouldBe` True
      equalValues (AccountStoreCorruptData "malformed account") (AccountStoreCorruptData "malformed account") `shouldBe` True
      equalValues (AccountStoreUnavailable "database unavailable") (AccountStoreCorruptData "database unavailable") `shouldBe` False
      renderedValue (AccountStoreUnavailable "database unavailable") `shouldBe` "AccountStoreUnavailable \"database unavailable\""
      renderedValue (AccountStoreCorruptData "malformed account") `shouldBe` "AccountStoreCorruptData \"malformed account\""
      equalValues ResendVerificationNoLongerPending ResendVerificationNoLongerPending `shouldBe` True
      equalValues (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) `shouldBe` True
      equalValues (ResendVerificationDeliveryFailed "SMTP unavailable") (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` True
      equalValues ResendVerificationClockOverflow ResendVerificationClockOverflow `shouldBe` True
      equalValues (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` False
      equalValues ResendVerificationClockOverflow ResendVerificationNoLongerPending `shouldBe` False
      renderedValue (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")) `shouldBe` "ResendVerificationStoreError (AccountStoreUnavailable \"database unavailable\")"
      renderedValue (ResendVerificationDeliveryFailed "SMTP unavailable") `shouldBe` "ResendVerificationDeliveryFailed \"SMTP unavailable\""
      renderedValue ResendVerificationClockOverflow `shouldBe` "ResendVerificationClockOverflow"
      renderedValue ResendVerificationNoLongerPending `shouldBe` "ResendVerificationNoLongerPending"
      expectAll
        ( ((AccountStoreUnavailable "database unavailable" /= AccountStoreCorruptData "database unavailable") `shouldBe` True)
            :| [ show [AccountStoreUnavailable "database unavailable"] `shouldBe` "[AccountStoreUnavailable \"database unavailable\"]",
                 (ResendVerificationStoreError (AccountStoreUnavailable "database unavailable") /= ResendVerificationDeliveryFailed "database unavailable") `shouldBe` True,
                 show [ResendVerificationStoreError (AccountStoreUnavailable "database unavailable")] `shouldBe` "[ResendVerificationStoreError (AccountStoreUnavailable \"database unavailable\")]"
               ]
        )

    it "does not send an email when registration is already present or persistence fails" $ do
      deliveredMessagesReference <- newIORef []
      let emailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          emailAddress = requiredEmailAddress "person@example.test"
          existingStore =
            AccountStore
              { createPendingAccount = \_ -> pure (Right False),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          unavailableStore = existingStore {createPendingAccount = \_ -> pure (Left (AccountStoreUnavailable "database unavailable"))}
      assertRegistrationResult
        (registerAccountAt testPasswordHashingPolicy existingStore emailDelivery Email.EmailEnglish (const "https://account.example.test/verify") 100 200 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Right RegistrationAlreadyRegistered -> True; _ -> False)
      assertRegistrationResult
        (registerAccountAt testPasswordHashingPolicy unavailableStore emailDelivery Email.EmailEnglish (const "https://account.example.test/verify") 100 200 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Left (RegistrationStoreError storeError) -> isUnavailable "database unavailable" storeError; _ -> False)
      readIORef deliveredMessagesReference `shouldReturn` []

    it "rotates a pending account's verification token before resending its localized email" $ do
      storedVerificationReference <- newIORef Nothing
      deliveredMessagesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          pendingProfile = AccountProfile accountId emailAddress Nothing Nothing False
          verifiedProfile = AccountProfile accountId emailAddress Nothing Nothing True
          successfulStore =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \verification -> writeIORef storedVerificationReference (Just verification) >> pure (Right True),
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          unavailableStore = successfulStore {replaceEmailVerification = \_ -> pure (Left (AccountStoreUnavailable "database unavailable"))}
          noLongerPendingStore = successfulStore {replaceEmailVerification = \_ -> pure (Right False)}
          delivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message]))
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          resend store emailDelivery profile now lifetime =
            resendEmailVerificationAt store emailDelivery Email.EmailSpanish (\token -> "https://account.example.test/es/verify?token=" <> Account.emailVerificationTokenText token) now lifetime profile
      resend successfulStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Right () -> True; _ -> False)
      storedVerification <- readIORef storedVerificationReference
      deliveredMessages <- readIORef deliveredMessagesReference
      case (storedVerification, deliveredMessages) of
        (Just verification, [message]) -> do
          Account.storedVerificationAccountId verification `shouldBe` accountId
          Account.storedVerificationEmail verification `shouldBe` emailAddress
          Account.storedVerificationExpiresAtNanoseconds verification `shouldBe` 300
          Email.emailMessageRecipient message `shouldBe` emailAddress
          Email.emailMessageSubject message `shouldBe` "Verifica tu correo electronico"
          Email.emailMessageBody message `shouldSatisfy` Text.isPrefixOf "Abre este enlace para verificar tu correo electronico:\nhttps://account.example.test/es/verify?token="
        _ -> expectationFailure "expected a rotated verification and one email"
      resend unavailableStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left (ResendVerificationStoreError storeError) -> isUnavailable "database unavailable" storeError; _ -> False)
      resend noLongerPendingStore delivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left ResendVerificationNoLongerPending -> True; _ -> False)
      resend successfulStore failingDelivery pendingProfile 100 200 >>= (`shouldSatisfy` \case Left (ResendVerificationDeliveryFailed detail) -> "SMTP unavailable" `Text.isInfixOf` detail; _ -> False)
      resend successfulStore delivery pendingProfile maxBound 1 >>= (`shouldSatisfy` \case Left ResendVerificationClockOverflow -> True; _ -> False)
      resend successfulStore delivery verifiedProfile 100 200 >>= (`shouldSatisfy` \case Left ResendVerificationNoLongerPending -> True; _ -> False)

    it "reports delivery failures after the pending account has been stored and rejects overflowing expiry calculations" $ do
      pendingAccountsReference <- newIORef []
      let accountStore =
            AccountStore
              { createPendingAccount = \pendingAccount -> modifyIORef' pendingAccountsReference (<> [pendingAccount]) >> pure (Right True),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> error "unexpected verification lookup",
                consumeEmailVerification = \_ _ -> error "unexpected verification consumption"
              }
          failingDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP unavailable"))
          emailAddress = requiredEmailAddress "person@example.test"
      assertRegistrationResult
        (registerAccountAt testPasswordHashingPolicy accountStore failingDelivery Email.EmailEnglish (const "https://account.example.test/verify") 100 200 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Left (RegistrationDeliveryFailed message) -> "SMTP unavailable" `Text.isInfixOf` message; _ -> False)
      length <$> readIORef pendingAccountsReference `shouldReturn` 1
      assertRegistrationResult
        (registerAccountAt testPasswordHashingPolicy accountStore failingDelivery Email.EmailEnglish (const "https://account.example.test/verify") maxBound 1 emailAddress (Password.mkPassword "correct horse battery staple"))
        (\case Left RegistrationClockOverflow -> True; _ -> False)

    it "validates and atomically consumes a matching verification token" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          accountStore =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \digest ->
                  if digest == Account.emailVerificationTokenDigest token
                    then pure (Right (Just storedVerification))
                    else error "unexpected token digest",
                consumeEmailVerification = \digest now ->
                  if digest == Account.emailVerificationTokenDigest token && now == 499
                    then pure (Right (Just accountId))
                    else error "unexpected verification consumption"
              }
      confirmationResult <- confirmEmailVerificationAt accountStore 499 token
      case confirmationResult of
        Right (Account.EmailVerificationAccepted actualAccountId actualEmailAddress) -> do
          actualAccountId `shouldBe` accountId
          actualEmailAddress `shouldBe` emailAddress
        _ -> expectationFailure "expected accepted email verification"

    it "handles missing, expired, raced, corrupt, and unavailable verification records" $ do
      let accountId = requiredAccountId "account_01"
          otherAccountId = requiredAccountId "account_02"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          storeWith lookupResult consumptionResult =
            AccountStore
              { createPendingAccount = \_ -> error "unexpected account creation",
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure lookupResult,
                consumeEmailVerification = \_ _ -> pure consumptionResult
              }
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Left (AccountStoreUnavailable "lookup unavailable")) (Right Nothing)) 499 token)
        (\case Left storeError -> isUnavailable "lookup unavailable" storeError; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right Nothing) (Right Nothing)) 499 token)
        (\case Right Account.EmailVerificationRejected -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right Nothing)) 500 token)
        (\case Right Account.EmailVerificationExpired -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Left (AccountStoreUnavailable "consume unavailable"))) 499 token)
        (\case Left storeError -> isUnavailable "consume unavailable" storeError; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right Nothing)) 499 token)
        (\case Right Account.EmailVerificationRejected -> True; _ -> False)
      assertEmailVerificationResult
        (confirmEmailVerificationAt (storeWith (Right (Just storedVerification)) (Right (Just otherAccountId))) 499 token)
        (\case Left storeError -> isCorrupt "email verification was consumed for a different account" storeError; _ -> False)

  describe "WebApi.AppEffect" $ do
    it "composes application services, IO, and typed failures through one boundary" $ do
      let services = AppEffect.AppServices unavailableAccountWorkflow
          successfulAction :: AppEffect.AppM Text Int
          successfulAction = AppEffect.liftAppIO (pure 42)
          failureDiagnostics = AppEffect.FailureDiagnostics "sample.failure" "SampleFailure" ["private detail"]
          failure = AppEffect.AppFailure "safe failure" failureDiagnostics
          failingAction :: AppEffect.AppM Text ()
          failingAction = AppEffect.throwAppFailure failure
      AppEffect.runAppM services successfulAction
        >>= \case
          Right 42 -> pure ()
          _ -> expectationFailure "expected the applicative application action to return 42"
      AppEffect.runAppM services failingAction
        >>= \case
          Left actualFailure -> do
            AppEffect.appFailurePublic actualFailure `shouldBe` "safe failure"
            AppEffect.failureCode (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` "sample.failure"
            AppEffect.failureType (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` "SampleFailure"
            AppEffect.failureLogEntries (AppEffect.appFailureDiagnostics actualFailure) `shouldBe` ["private detail"]
          Right () -> expectationFailure "expected a typed application failure"

  describe "WebApi.AccountPages" $ do
    it "keeps account routes fully server-rendered and query-aware" $ do
      let registrationRequest = HarchWeb.RouteRequest RegistrationRoute defaultRequestContext
          verificationRequest = HarchWeb.RouteRequest EmailVerificationRoute (defaultRequestContext {requestQueryParameters = [("token", "prefilled-token")]})
          mfaRequest = HarchWeb.RouteRequest MfaEnrollmentRoute (defaultRequestContext {requestQueryParameters = [("account", "account_01")]})
          loginRequest = HarchWeb.RouteRequest LoginRoute defaultRequestContext
          logoutRequest = HarchWeb.RouteRequest LogoutRoute defaultRequestContext
          profileRequestValue = HarchWeb.RouteRequest ProfileRoute defaultRequestContext
      selectRouteData registrationRequest `shouldReturn` RegistrationRouteDataResult
      selectRouteData verificationRequest `shouldReturn` EmailVerificationRouteDataResult
      selectRouteData mfaRequest `shouldReturn` MfaEnrollmentRouteDataResult
      selectRouteData loginRequest `shouldReturn` LoginRouteDataResult
      selectRouteData logoutRequest `shouldReturn` LogoutRouteDataResult
      selectRouteData profileRequestValue `shouldReturn` ProfileRouteDataResult
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect registrationRequest
        `shouldReturn` RouteDataSelection RegistrationRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect verificationRequest
        `shouldReturn` RouteDataSelection EmailVerificationRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect mfaRequest
        `shouldReturn` RouteDataSelection MfaEnrollmentRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect loginRequest
        `shouldReturn` RouteDataSelection LoginRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect logoutRequest
        `shouldReturn` RouteDataSelection LogoutRouteDataResult []
      selectRouteDataSelectionWithDatabase defaultDatabaseEffect profileRequestValue
        `shouldReturn` RouteDataSelection ProfileRouteDataResult []
      buildPageModelFromRouteData registrationRequest RegistrationRouteDataResult
        `shouldBe` RegistrationPage "/register" emptyRegistrationForm
      buildPageModelFromRouteData verificationRequest EmailVerificationRouteDataResult
        `shouldBe` EmailVerificationPage "/verify" (VerificationForm "prefilled-token" Nothing False)
      buildPageModelFromRouteData (HarchWeb.RouteRequest EmailVerificationRoute defaultRequestContext) EmailVerificationRouteDataResult
        `shouldBe` EmailVerificationPage "/verify" (VerificationForm Text.empty Nothing False)
      buildPageModelFromRouteData mfaRequest MfaEnrollmentRouteDataResult
        `shouldBe` MfaEnrollmentPage "/mfa" (MfaEnrollmentForm "account_01" Nothing [] Nothing False)
      buildPageModelFromRouteData (HarchWeb.RouteRequest MfaEnrollmentRoute defaultRequestContext) MfaEnrollmentRouteDataResult
        `shouldBe` MfaEnrollmentPage "/mfa" (MfaEnrollmentForm Text.empty Nothing [] Nothing False)
      buildPageModelFromRouteData loginRequest LoginRouteDataResult
        `shouldBe` LoginPage "/login" (LoginForm Text.empty Nothing False)
      buildPageModelFromRouteData logoutRequest LogoutRouteDataResult
        `shouldBe` LogoutPage "/logout"
      buildPageModelFromRouteData profileRequestValue ProfileRouteDataResult
        `shouldBe` ProfilePage
          SignedOutProfilePage
            { profileHeading = "Profile",
              profileSummary = "Sign in to view and manage your profile.",
              profileSignInAction = CallToAction "Sign in" LoginRoute "/login",
              profileRegistrationAction = CallToAction "Create account" RegistrationRoute "/register"
            }
      let spanishProfileRequest = HarchWeb.RouteRequest ProfileRoute spanishRequestContext
          spanishProfileModel =
            SignedOutProfilePage
              { profileHeading = "Perfil",
                profileSummary = "Inicia sesión para ver y administrar tu perfil.",
                profileSignInAction = CallToAction "Iniciar sesión" LoginRoute "/es/login",
                profileRegistrationAction = CallToAction "Crear cuenta" RegistrationRoute "/es/register"
              }
      buildPageModelFromRouteData spanishProfileRequest ProfileRouteDataResult
        `shouldBe` ProfilePage spanishProfileModel
      let spanishProfileModelCopy =
            SignedOutProfilePage
              { profileHeading = "Perfil",
                profileSummary = "Inicia sesión para ver y administrar tu perfil.",
                profileSignInAction = CallToAction "Iniciar sesión" LoginRoute "/es/login",
                profileRegistrationAction = CallToAction "Crear cuenta" RegistrationRoute "/es/register"
              }
      assertSameProfilePageModel spanishProfileModel spanishProfileModelCopy
      show (ProfilePage spanishProfileModel)
        `shouldSatisfy` (Text.isPrefixOf "SignedOutProfilePage" . Text.pack)
      renderPageFromRouteData defaultAppConfig verificationRequest EmailVerificationRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Verify email"
            && "data-page=\"email-verification\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "value=\"prefilled-token\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig registrationRequest RegistrationRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Create account"
            && "data-page=\"registration\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig mfaRequest MfaEnrollmentRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Set up authenticator"
            && "data-page=\"mfa-enrollment\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig loginRequest LoginRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Sign in"
            && "data-page=\"login\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig logoutRequest LogoutRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Sign out"
            && "data-page=\"logout\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      renderPageFromRouteData defaultAppConfig profileRequestValue ProfileRouteDataResult
        `shouldSatisfy` \page ->
          HarchWeb.pageTitle page == "web-api: Profile"
            && "data-page=\"profile\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "href=\"/login\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
            && "href=\"/register\"" `Text.isInfixOf` HarchWeb.renderHtml (HarchWeb.pageBody page)
      HarchWeb.renderResponse pureApplication registrationRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"registration\""
          _ -> expectationFailure "expected a registration page response"
      HarchWeb.renderResponse pureApplication verificationRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"email-verification\""
          _ -> expectationFailure "expected an email-verification page response"
      HarchWeb.renderResponse pureApplication mfaRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"mfa-enrollment\""
          _ -> expectationFailure "expected an MFA-enrollment page response"
      HarchWeb.renderResponse pureApplication loginRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"login\""
          _ -> expectationFailure "expected a login page response"
      HarchWeb.renderResponse pureApplication logoutRequest
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"logout\""
          _ -> expectationFailure "expected a logout page response"
      HarchWeb.renderResponse pureApplication profileRequestValue
        >>= \case
          HarchWeb.PageResponse page -> HarchWeb.renderHtml (HarchWeb.pageBody page) `shouldSatisfy` Text.isInfixOf "data-page=\"profile\""
          _ -> expectationFailure "expected a profile page response"
      let runtimeApplication = buildRuntimeAppWithDatabaseBuilder defaultAppConfig (const defaultDatabaseEffect) defaultAppEnvironmentConfig
      HarchWeb.handleClientAction
        runtimeApplication
        HarchWeb.ClientActionRequest
          { HarchWeb.clientActionMethod = "POST",
            HarchWeb.clientActionPath = "/register",
            HarchWeb.clientActionFields = [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")],
            HarchWeb.clientActionCsrfToken = Nothing,
            HarchWeb.clientActionContext = defaultRequestContext
          }
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable")

    it "renders complete SSR registration and verification forms with escaped values" $ do
      if emptyRegistrationForm == RegistrationForm Text.empty Text.empty Text.empty Nothing False then pure () else expectationFailure "expected empty registration form"
      if RegistrationForm "person_01" "person@example.test" "Person Example" Nothing False /= RegistrationForm "other_01" "other@example.test" "Other Example" Nothing False then pure () else expectationFailure "registration forms must compare identity values"
      if VerificationForm "token" Nothing False /= VerificationForm "token" (Just "error") True then pure () else expectationFailure "verification forms must compare their state"
      if MfaEnrollmentForm "account_01" Nothing [] Nothing False /= MfaEnrollmentForm "account_01" Nothing [] (Just "error") True then pure () else expectationFailure "MFA forms must compare their state"
      if LoginForm "person@example.test" Nothing False /= LoginForm "other@example.test" Nothing False then pure () else expectationFailure "login forms must compare their email values"
      show (RegistrationPage "/register" (RegistrationForm "person_01" "person@example.test" "Person Example" Nothing False))
        `shouldBe` "RegistrationPage \"/register\" (RegistrationForm {registrationFormUsername = \"person_01\", registrationFormEmail = \"person@example.test\", registrationFormDisplayName = \"Person Example\", registrationFormMessage = Nothing, registrationFormIsError = False})"
      show (EmailVerificationPage "/verify" (VerificationForm "token" (Just "ready") False))
        `shouldBe` "EmailVerificationPage \"/verify\" (VerificationForm {verificationFormToken = \"token\", verificationFormMessage = Just \"ready\", verificationFormIsError = False})"
      show (MfaEnrollmentPage "/mfa" (MfaEnrollmentForm "account_01" Nothing [] Nothing False))
        `shouldBe` "MfaEnrollmentPage \"/mfa\" \"account_01\" Nothing False"
      show (LoginPage "/login" (LoginForm "person@example.test" Nothing False))
        `shouldBe` "LoginPage \"/login\" \"person@example.test\" Nothing False"
      show (LogoutPage "/logout") `shouldBe` "LogoutPage \"/logout\""
      renderRegistrationPage Spanish "/es/register" (RegistrationForm "person_01\" onclick=\"bad" "person@example.test\" onclick=\"bad" "Person & Example" (Just "Ready <now>") False)
        `shouldSatisfy` \html ->
          "Nombre de usuario" `Text.isInfixOf` html
            && "Nombre para mostrar (opcional)" `Text.isInfixOf` html
            && "person_01&quot; onclick=&quot;bad" `Text.isInfixOf` html
            && "person@example.test&quot; onclick=&quot;bad" `Text.isInfixOf` html
            && "Person &amp; Example" `Text.isInfixOf` html
            && "Ready &lt;now&gt;" `Text.isInfixOf` html
      renderRegistrationRegion English "/register" (RegistrationForm Text.empty Text.empty Text.empty (Just "No") True)
        `shouldSatisfy` Text.isInfixOf "data-error-state=\"true\""
      renderVerificationPage English "/verify" (VerificationForm "token&value" Nothing False)
        `shouldSatisfy` \html ->
          "<section data-page=\"email-verification\">" `Text.isPrefixOf` html
            && "value=\"token&amp;value\"" `Text.isInfixOf` html
      renderVerificationPage Spanish "/es/verify" (VerificationForm Text.empty Nothing False)
        `shouldSatisfy` Text.isInfixOf "Verifica tu direccion de correo"
      renderVerificationRegion English "/verify" (VerificationForm Text.empty Nothing False)
        `shouldSatisfy` (not . Text.isInfixOf "data-account-message")
      renderRegistrationRegion English "/register" (RegistrationForm "'>&" "'>&" "'>&" Nothing False)
        `shouldSatisfy` \html -> "&#39;&gt;&amp;" `Text.isInfixOf` html
      let spanishMfaPage = renderMfaEnrollmentPage Spanish "/es/mfa" (MfaEnrollmentForm "account_01" (Just "SECRET&VALUE") ["CODE-ONE"] (Just "Ready <now>") False)
      spanishMfaPage
        `shouldSatisfy` \html -> "data-harch-control" `Text.isInfixOf` html && "SECRET&amp;VALUE" `Text.isInfixOf` html && "Ready &lt;now&gt;" `Text.isInfixOf` html && "action=\"/es/mfa\"" `Text.isInfixOf` html
      mapM_
        (\label -> spanishMfaPage `shouldSatisfy` Text.isInfixOf label)
        [ "Configura tu autenticador",
          "Iniciar registro del autenticador",
          "Codigo del autenticador",
          "Confirmar autenticador",
          "Codigos de recuperacion",
          "Guarda estos codigos. No se mostraran de nuevo."
        ]
      renderMfaEnrollmentRegion English "/mfa" (MfaEnrollmentForm "account_01" Nothing ["CODE-ONE"] Nothing False)
        `shouldSatisfy` Text.isInfixOf "data-recovery-codes=\"true\""
      let spanishLoginPage = renderLoginPage Spanish "/es/login" (LoginForm "person@example.test\" onclick=\"bad" (Just "Ready <now>") False)
      spanishLoginPage
        `shouldSatisfy` \html -> "data-page=\"login\"" `Text.isInfixOf` html && "action=\"/es/login\"" `Text.isInfixOf` html && "autocomplete=\"username\"" `Text.isInfixOf` html && "person@example.test&quot; onclick=&quot;bad" `Text.isInfixOf` html && "Ready &lt;now&gt;" `Text.isInfixOf` html
      mapM_
        (\label -> spanishLoginPage `shouldSatisfy` Text.isInfixOf label)
        [ "Iniciar sesion",
          "Direccion de correo o nombre de usuario",
          "Contrasena",
          "Metodo de verificacion",
          "Codigo del autenticador",
          "Codigo de recuperacion",
          "Codigo de verificacion"
        ]
      renderLoginRegion English "/login" (LoginForm Text.empty Nothing False)
        `shouldSatisfy` (not . Text.isInfixOf "data-account-message")
      renderLogoutPage English "/logout" `shouldSatisfy` Text.isInfixOf "data-harch-control"
      Text.length (renderLogoutPage English "/logout") `shouldSatisfy` (> 0)
      let spanishLogoutPage = renderLogoutPage Spanish "/es/logout"
      spanishLogoutPage `shouldSatisfy` Text.isInfixOf "Cerrar sesion"
      spanishLogoutPage `shouldSatisfy` Text.isInfixOf ">Cerrar sesion</button>"
      renderLogoutRegion English "/logout" (Just "Signed <out>") True
        `shouldSatisfy` \html -> "data-error-state=\"true\"" `Text.isInfixOf` html && "Signed &lt;out&gt;" `Text.isInfixOf` html
      pageEnhancementHooks RegistrationRoute `shouldBe` []
      pageEnhancementHooks EmailVerificationRoute `shouldBe` []
      pageEnhancementHooks MfaEnrollmentRoute `shouldBe` []
      pageEnhancementHooks LoginRoute `shouldBe` []
      pageEnhancementHooks LogoutRoute `shouldBe` []

    it "captures registration actions before deferred behavior and patches the localized region" $ do
      deliveredMessagesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          createdStore =
            AccountStore
              { createPendingAccount = \pendingAccount ->
                  do
                    pendingAccountUsername pendingAccount `shouldBe` Just (fromMaybe (error "expected username") (Username.mkUsername "person_01"))
                    pendingAccountDisplayName pendingAccount `shouldSatisfy` (`elem` [Nothing, Just "Person Example"])
                    pendingAccountEmail pendingAccount `shouldBe` emailAddress
                    pure (Right True),
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure (Right (Just storedVerification)),
                consumeEmailVerification = \_ _ -> pure (Right (Just accountId))
              }
          workflow =
            AccountWorkflow
              { accountWorkflowStore = createdStore,
                accountWorkflowEmailDelivery = Email.EmailDelivery (\message -> modifyIORef' deliveredMessagesReference (<> [message])),
                accountWorkflowPasswordHasher = Password.hashPassword,
                accountWorkflowClock = pure 100,
                accountWorkflowMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow,
                accountWorkflowCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow,
                accountWorkflowSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow,
                accountWorkflowProfileStore = accountWorkflowProfileStore unavailableAccountWorkflow,
                accountWorkflowTotpEncryptionKey = accountWorkflowTotpEncryptionKey unavailableAccountWorkflow,
                accountWorkflowTotpClock = pure 0,
                accountWorkflowVerificationUrl = \requestContext verificationToken ->
                  "https://account.example.test"
                    <> renderRoutePath
                      HarchWeb.RouteRequest
                        { HarchWeb.requestRoute = EmailVerificationRoute,
                          HarchWeb.requestContext = requestContext
                        }
                    <> "?token="
                    <> Account.emailVerificationTokenText verificationToken
              }
          request method path fields locale =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = method,
                HarchWeb.clientActionPath = path,
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext {requestLocale = locale}
              }
      handleAccountAction workflow (request "GET" "/register" [] English) `shouldReturn` Nothing
      handleAccountAction workflow (request "POST" "/missing" [] English) `shouldReturn` Nothing
      invalidMfaResult <- handleAccountAction workflow (request "POST" "/mfa" [("intent", "start")] English)
      invalidMfaResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "mfa-account") "The enrollment link is invalid"
      spanishInvalidMfaResult <- handleAccountAction workflow (request "POST" "/es/mfa" [("intent", "start")] Spanish)
      spanishInvalidMfaResult `shouldSatisfy` \case
        Just response -> HarchWeb.clientActionStatus response == 422 && any (Text.isInfixOf "action=\"/es/mfa\"" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      invalidUsernameResult <- handleAccountAction workflow (request "POST" "/register" [("username", "no!"), ("email", "person@example.test"), ("password", "correct horse battery staple")] English)
      invalidUsernameResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "Use a username"
      spanishInvalidUsernameResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "no!"), ("email", "person@example.test"), ("password", "correct horse battery staple")] Spanish)
      spanishInvalidUsernameResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-username") "Usa un nombre de usuario"
      invalidEmailResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "not-an-email"), ("password", "correct horse battery staple")] English)
      invalidEmailResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-email") "Enter a valid email address."
      spanishInvalidEmailResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "not-an-email"), ("password", "correct horse battery staple")] Spanish)
      spanishInvalidEmailResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-email") "Introduce una direccion"
      invalidPasswordResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "short")] English)
      invalidPasswordResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-password") "Use a password with at least 12 characters."
      spanishInvalidPasswordResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "person@example.test"), ("password", "short")] Spanish)
      spanishInvalidPasswordResult `shouldSatisfy` actionHasStatusAndFocus 422 (Just "registration-password") "Usa una contrasena"
      emptyDisplayNameResult <- handleAccountAction workflow (request "POST" "/register" [("username", "person_01"), ("email", "person@example.test"), ("displayName", ""), ("password", "correct horse battery staple")] English)
      emptyDisplayNameResult `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Check your inbox"
      createdResult <- handleAccountAction workflow (request "POST" "/es/register" [("username", "person_01"), ("email", "person@example.test"), ("displayName", "Person Example"), ("password", "correct horse battery staple")] Spanish)
      createdResult `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Revisa tu bandeja de entrada"
      deliveredMessages <- readIORef deliveredMessagesReference
      deliveredMessages `shouldSatisfy` \case
        [_, message] -> "https://account.example.test/es/verify?token=" `Text.isInfixOf` Email.emailMessageBody message
        _ -> False
      unconfiguredAction <-
        HarchWeb.handleClientAction
          pureApplication
          HarchWeb.ClientActionRequest
            { HarchWeb.clientActionMethod = "POST",
              HarchWeb.clientActionPath = "/register",
              HarchWeb.clientActionFields = [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")],
              HarchWeb.clientActionCsrfToken = Nothing,
              HarchWeb.clientActionContext = defaultRequestContext
            }
      unconfiguredAction `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      let unconfiguredStore = accountWorkflowStore unavailableAccountWorkflow
      assertAccountStoreError
        (createPendingAccount unconfiguredStore (error "the unavailable store must ignore pending-account input"))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (replaceEmailVerification unconfiguredStore (error "the unavailable store must ignore verification input"))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (findEmailVerification unconfiguredStore (Account.emailVerificationTokenDigest token))
        (isUnavailable "account persistence is not configured")
      assertAccountStoreError
        (consumeEmailVerification unconfiguredStore (Account.emailVerificationTokenDigest token) 0)
        (isUnavailable "account persistence is not configured")
      let unconfiguredMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow
          assertMfaUnavailable action = do
            result <- action
            case result of
              Left (MfaStoreUnavailable "MFA persistence is not configured") -> pure ()
              _ -> expectationFailure "expected unavailable MFA persistence"
      assertMfaUnavailable (loadTotpEnrollment unconfiguredMfaStore accountId)
      assertMfaUnavailable (saveUnconfirmedTotpEnrollment unconfiguredMfaStore accountId "secret" 0)
      assertMfaUnavailable (confirmTotpEnrollment unconfiguredMfaStore accountId ("hash" :| []) 0)
      assertMfaUnavailable (loadUnusedRecoveryCodeHashes unconfiguredMfaStore accountId)
      assertMfaUnavailable (consumeRecoveryCodeHash unconfiguredMfaStore accountId "hash" 0)
      let unconfiguredCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow
      findAccountCredentialByEmail unconfiguredCredentialStore (requiredEmailAddress "person@example.test")
        >>= \case
          Left (AccountCredentialStoreUnavailable "account credentials are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account credentials"
      findAccountCredentialByUsername unconfiguredCredentialStore (fromMaybe (error "expected valid username") (Username.mkUsername "person_01"))
        >>= \case
          Left (AccountCredentialStoreUnavailable "account credentials are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account credentials"
      let unconfiguredSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow
          assertSessionUnavailable :: IO (Either AccountSessionStoreError value) -> Expectation
          assertSessionUnavailable action =
            action >>= \case
              Left AccountSessionStoreUnavailable -> pure ()
              _ -> expectationFailure "expected unavailable account sessions"
      assertSessionUnavailable (saveAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input"))
      assertSessionUnavailable (loadAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input"))
      assertSessionUnavailable (invalidateAccountSession unconfiguredSessionStore (error "unavailable session store must ignore input"))
      findAccountProfile (accountWorkflowProfileStore unavailableAccountWorkflow) accountId
        >>= \case
          Left (AccountStoreUnavailable "account profiles are not configured") -> pure ()
          _ -> expectationFailure "expected unavailable account profiles"
      accountWorkflowPasswordHasher unavailableAccountWorkflow `seq` pure ()
      accountWorkflowTotpEncryptionKey unavailableAccountWorkflow `seq` pure ()
      accountWorkflowTotpClock unavailableAccountWorkflow `shouldReturn` 0
      unavailableDelivery <-
        try (Email.deliverEmail (accountWorkflowEmailDelivery unavailableAccountWorkflow) (error "the unavailable delivery must ignore messages")) :: IO (Either IOException ())
      unavailableDelivery `shouldSatisfy` \case Left errorMessage -> "email delivery is not configured" `isInfixOf` displayException errorMessage; Right _ -> False
      accountWorkflowVerificationUrl unavailableAccountWorkflow defaultRequestContext token `shouldBe` "https://invalid.example.test/verify"

    it "returns opaque registration failures and accepts, rejects, or expires verification actions" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          storedVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token
          request path fields =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = path,
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext
              }
          workflowFor accountStore now emailDelivery =
            AccountWorkflow
              { accountWorkflowStore = accountStore,
                accountWorkflowEmailDelivery = emailDelivery,
                accountWorkflowPasswordHasher = Password.hashPassword,
                accountWorkflowClock = pure now,
                accountWorkflowMfaStore = accountWorkflowMfaStore unavailableAccountWorkflow,
                accountWorkflowCredentialStore = accountWorkflowCredentialStore unavailableAccountWorkflow,
                accountWorkflowSessionStore = accountWorkflowSessionStore unavailableAccountWorkflow,
                accountWorkflowProfileStore = accountWorkflowProfileStore unavailableAccountWorkflow,
                accountWorkflowTotpEncryptionKey = accountWorkflowTotpEncryptionKey unavailableAccountWorkflow,
                accountWorkflowTotpClock = pure 0,
                accountWorkflowVerificationUrl = \_ verificationToken -> "https://account.example.test/verify?token=" <> Account.emailVerificationTokenText verificationToken
              }
          store createResult lookupResult consumeResult =
            AccountStore
              { createPendingAccount = \_ -> pure createResult,
                replaceEmailVerification = \_ -> error "unexpected verification replacement",
                findEmailVerification = \_ -> pure lookupResult,
                consumeEmailVerification = \_ _ -> pure consumeResult
              }
          validRegistration = [("username", "person_01"), ("email", "person@example.test"), ("password", "correct horse battery staple")]
          validToken = [("token", Account.emailVerificationTokenText token)]
          delivery = Email.EmailDelivery (\message -> Email.emailMessageSubject message `shouldBe` "Verify your email address")
          spanishAction path fields =
            (request path fields)
              { HarchWeb.clientActionPath = "/es" <> path,
                HarchWeb.clientActionContext = defaultRequestContext {requestLocale = Spanish}
              }
      alreadyRegistered <- handleAccountAction (workflowFor (store (Right False) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      alreadyRegistered `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "If that address can register"
      spanishAlreadyRegistered <- handleAccountAction (workflowFor (store (Right False) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/register" validRegistration)
      spanishAlreadyRegistered `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Si esa direccion"
      createdEnglish <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      createdEnglish `shouldSatisfy` actionHasStatusAndFocus 202 Nothing "Check your inbox"
      unavailableRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreUnavailable "down")) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      unavailableRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishUnavailableRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreUnavailable "down")) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/register" validRegistration)
      spanishUnavailableRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      corruptRegistration <- handleAccountAction (workflowFor (store (Left (AccountStoreCorruptData "bad")) (Right Nothing) (Right Nothing)) 100 delivery) (request "/register" validRegistration)
      corruptRegistration `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      corruptRegistration
        `shouldSatisfy` maybe
          False
          ( \response ->
              not (any (Text.isInfixOf "bad" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response))
                && any (Text.isInfixOf "bad") (HarchWeb.clientActionLogEntries response)
                && any (\attribute -> Observability.attributeName attribute == "app.failure.code" && Observability.attributeValue attribute == Observability.TextAttribute "account.registration.store") (HarchWeb.clientActionObservabilityAttributes response)
          )
      deliveryFailure <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 (Email.EmailDelivery (\_ -> ioError (userError "mail down")))) (request "/register" validRegistration)
      deliveryFailure `shouldSatisfy` actionHasStatusAndFocus 502 (Just "registration-email") "could not send"
      spanishDeliveryFailure <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 (Email.EmailDelivery (\_ -> ioError (userError "mail down")))) (spanishAction "/register" validRegistration)
      spanishDeliveryFailure `shouldSatisfy` actionHasStatusAndFocus 502 (Just "registration-email") "No pudimos enviar"
      passwordHashingFailure <-
        handleAccountAction
          ( (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordHasher = \_ _ -> pure Nothing
              }
          )
          (request "/register" validRegistration)
      passwordHashingFailure `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishPasswordHashingFailure <-
        handleAccountAction
          ( (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery)
              { accountWorkflowPasswordHasher = \_ _ -> pure Nothing
              }
          )
          (spanishAction "/register" validRegistration)
      spanishPasswordHashingFailure `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      clockOverflow <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) maxBound delivery) (request "/register" validRegistration)
      clockOverflow `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "temporarily unavailable"
      spanishClockOverflow <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) maxBound delivery) (spanishAction "/register" validRegistration)
      spanishClockOverflow `shouldSatisfy` actionHasStatusAndFocus 503 (Just "registration-email") "no esta disponible"
      invalidVerification <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery) (request "/verify" [("token", "invalid")])
      invalidVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "link is invalid"
      spanishInvalidVerification <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery) (spanishAction "/verify" [("token", "invalid")])
      spanishInvalidVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "enlace de verificacion no es valido"
      missingVerification <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 100 delivery) (request "/verify" [])
      missingVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "link is invalid"
      acceptedVerification <- handleAccountAction (workflowFor (store (Right True) (Right (Just storedVerification)) (Right (Just accountId))) 499 delivery) (request "/verify" validToken)
      acceptedVerification `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "email address is verified"
      spanishAcceptedVerification <- handleAccountAction (workflowFor (store (Right True) (Right (Just storedVerification)) (Right (Just accountId))) 499 delivery) (spanishAction "/verify" validToken)
      spanishAcceptedVerification `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "direccion de correo esta verificada"
      expiredVerification <- handleAccountAction (workflowFor (store (Right True) (Right (Just storedVerification)) (Right Nothing)) 500 delivery) (request "/verify" validToken)
      expiredVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "has expired"
      spanishExpiredVerification <- handleAccountAction (workflowFor (store (Right True) (Right (Just storedVerification)) (Right Nothing)) 500 delivery) (spanishAction "/verify" validToken)
      spanishExpiredVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "ha caducado"
      rejectedVerification <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 499 delivery) (request "/verify" validToken)
      rejectedVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "invalid or has already been used"
      spanishRejectedVerification <- handleAccountAction (workflowFor (store (Right True) (Right Nothing) (Right Nothing)) 499 delivery) (spanishAction "/verify" validToken)
      spanishRejectedVerification `shouldSatisfy` actionHasStatusAndFocus 422 (Just "verification-token") "no es valido o ya se ha utilizado"
      unavailableVerification <- handleAccountAction (workflowFor (store (Right True) (Left (AccountStoreUnavailable "down")) (Right Nothing)) 499 delivery) (request "/verify" validToken)
      unavailableVerification `shouldSatisfy` actionHasStatusAndFocus 503 (Just "verification-token") "temporarily unavailable"
      spanishUnavailableVerification <- handleAccountAction (workflowFor (store (Right True) (Left (AccountStoreUnavailable "down")) (Right Nothing)) 499 delivery) (spanishAction "/verify" validToken)
      spanishUnavailableVerification `shouldSatisfy` actionHasStatusAndFocus 503 (Just "verification-token") "no esta disponible"

    it "issues a cookie only after password and TOTP verification, and revokes it on logout" $ do
      savedSessionsReference <- newIORef []
      invalidatedSessionsReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          password = Password.mkPassword "correct horse battery staple"
          passwordHash = fromMaybe (error "expected test password hash") (Password.hashPasswordWithSalt Password.defaultPasswordHashingPolicy (ByteString.replicate 16 7) password)
          totpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret = fromMaybe (error "expected encrypted TOTP secret") (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (ByteString.replicate 12 7) (TextEncoding.encodeUtf8 (Totp.renderTotpSecret totpSecret)))
          credentialStore = AccountCredentialStore (\email -> (email `shouldBe` emailAddress) >> pure (Right (Just (AccountCredential accountId passwordHash True)))) (\_ -> pure (error "unexpected username credential lookup"))
          mfaStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment save"),
                loadTotpEnrollment = \account -> (account `shouldBe` accountId) >> pure (Right (Just (StoredTotpEnrollment encryptedTotpSecret (Just 1)))),
                confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment confirmation"),
                loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption")
              }
          sessionStore =
            AccountSessionStore
              { saveAccountSession = \session -> modifyIORef' savedSessionsReference (<> [session]) >> pure (Right True),
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \session -> modifyIORef' invalidatedSessionsReference (<> [session]) >> pure (Right True)
              }
          workflow =
            unavailableAccountWorkflow
              { accountWorkflowCredentialStore = credentialStore,
                accountWorkflowMfaStore = mfaStore,
                accountWorkflowSessionStore = sessionStore,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = pure 123456
              }
          loginRequest fields =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = "/login",
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext
              }
          loginFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", Totp.totpCodeText (Totp.totpCode 123456 totpSecret))]
      invalidEmail <- handleAccountAction workflow (loginRequest [("email", "not an identifier!")])
      invalidEmail `shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "valid email address"
      loginResult <- handleAccountAction workflow (loginRequest loginFields)
      case loginResult of
        Nothing -> expectationFailure "expected login action response"
        Just response -> do
          forceShowValue response `shouldBe` True
          HarchWeb.clientActionStatus response `shouldBe` 200
          HarchWeb.clientActionFocusId response `shouldBe` Nothing
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
      savedSessions <- readIORef savedSessionsReference
      length savedSessions `shouldBe` 1
      loggedInSession <-
        case savedSessions of
          [session] -> pure session
          _ -> expectationFailure "expected exactly one saved session" >> pure (error "unreachable")
      Session.sessionPrincipal loggedInSession `shouldBe` accountId
      Session.sessionIssuedAtNanoseconds loggedInSession `shouldBe` 500
      let logoutRequest =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = "/logout",
                HarchWeb.clientActionFields = [],
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext {requestSessionId = Just (Session.sessionId loggedInSession)}
              }
      logoutResult <- handleAccountAction workflow logoutRequest
      case logoutResult of
        Nothing -> expectationFailure "expected logout action response"
        Just response -> do
          forceShowValue response `shouldBe` True
          HarchWeb.clientActionStatus response `shouldBe` 200
          HarchWeb.clientActionHeaders response `shouldSatisfy` any (Text.isInfixOf "Max-Age=0" . TextEncoding.decodeUtf8 . snd)
      readIORef invalidatedSessionsReference `shouldReturn` [Session.sessionId loggedInSession]

    it "validates every login state and keeps logout revocation failures visible" $ do
      let accountId = requiredAccountId "account_02"
          password = Password.mkPassword "correct horse battery staple"
          passwordHash = fromMaybe (error "expected test password hash") (Password.hashPasswordWithSalt Password.defaultPasswordHashingPolicy (ByteString.replicate 16 8) password)
          confirmedCredential = AccountCredential accountId passwordHash True
          unverifiedCredential = AccountCredential accountId passwordHash False
          totpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret = fromMaybe (error "expected encrypted TOTP secret") (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (ByteString.replicate 12 8) (TextEncoding.encodeUtf8 (Totp.renderTotpSecret totpSecret)))
          confirmedEnrollment = StoredTotpEnrollment encryptedTotpSecret (Just 1)
          loginRequest requestContext fields =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = "/login",
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = requestContext
              }
          spanishLoginRequest fields =
            (loginRequest spanishRequestContext fields)
              { HarchWeb.clientActionPath = "/es/login"
              }
          logoutRequest requestContext =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = "/logout",
                HarchWeb.clientActionFields = [],
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = requestContext
              }
          workflowFor credentialResult enrollmentResult sessionSaveResult invalidationResult =
            unavailableAccountWorkflow
              { accountWorkflowCredentialStore = AccountCredentialStore (\_ -> pure credentialResult) (\receivedUsername -> receivedUsername `seq` pure credentialResult),
                accountWorkflowMfaStore =
                  MfaStore
                    { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment save"),
                      loadTotpEnrollment = \_ -> pure enrollmentResult,
                      confirmTotpEnrollment = \_ _ _ -> pure (error "unexpected enrollment confirmation"),
                      loadUnusedRecoveryCodeHashes = \_ -> pure (Right []),
                      consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption")
                    },
                accountWorkflowSessionStore =
                  AccountSessionStore
                    { saveAccountSession = \_ -> pure sessionSaveResult,
                      loadAccountSession = \_ -> pure (Right Nothing),
                      invalidateAccountSession = \_ -> pure invalidationResult
                    },
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = pure 123456
              }
          validCode = Totp.totpCodeText (Totp.totpCode 123456 totpSecret)
          invalidCode = Text.take 5 validCode <> if Text.drop 5 validCode == "0" then "1" else "0"
          validFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          usernameFields = [("email", ""), ("username", "person_01"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          emailUsernameFields = [("email", "person_01"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", validCode)]
          validWorkflow = workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Right True) (Right True)
          recoveryCode = fromMaybe (error "expected a valid recovery code") (RecoveryCode.mkRecoveryCode "0123456789ABCDEF0123")
          recoveryHash = fromMaybe (error "expected a recovery-code hash") (RecoveryCode.hashRecoveryCodeWithSalt testPasswordHashingPolicy "0123456789abcdef" recoveryCode)
          recoveryMfaStore =
            (accountWorkflowMfaStore validWorkflow)
              { loadUnusedRecoveryCodeHashes = \receivedAccountId -> do
                  receivedAccountId `shouldBe` accountId
                  pure (Right [RecoveryCode.recoveryCodeHashText recoveryHash]),
                consumeRecoveryCodeHash = \receivedAccountId receivedHash receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedHash `shouldBe` RecoveryCode.recoveryCodeHashText recoveryHash
                  receivedNow `shouldBe` 500
                  pure (Right True)
              }
          recoveryWorkflow = validWorkflow {accountWorkflowMfaStore = recoveryMfaStore}
          recoveryFields = [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "recovery"), ("code", RecoveryCode.recoveryCodeText recoveryCode)]
          unavailableSession = workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Left AccountSessionStoreUnavailable) (Right True)
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "not an identifier!")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "valid email address")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "not an identifier!")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-email") "nombre de usuario valido")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "short"), ("proof", "totp"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-password") "Enter your password")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "short"), ("proof", "totp"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-password") "Introduce tu contrasena")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "unknown"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Enter a valid authenticator")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "unknown"), ("code", validCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Introduce un codigo")
      handleAccountAction (workflowFor (Right (Just unverifiedCredential)) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Verify your email address")
      handleAccountAction (workflowFor (Right (Just unverifiedCredential)) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Verifica tu direccion")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Enroll your authenticator")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 403 Nothing "Registra tu autenticador")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", invalidCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Sign-in was rejected")
      handleAccountAction validWorkflow (spanishLoginRequest [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "totp"), ("code", invalidCode)])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "inicio de sesion fue rechazado")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext [("email", "person@example.test"), ("password", "correct horse battery staple"), ("proof", "recovery"), ("code", "0123456789ABCDEF0123")])
        >>= (`shouldSatisfy` actionHasStatusAndFocus 422 (Just "login-code") "Sign-in was rejected")
      handleAccountAction recoveryWorkflow (loginRequest defaultRequestContext recoveryFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext usernameFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      handleAccountAction validWorkflow (loginRequest defaultRequestContext emailUsernameFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed in")
      usernameLoginResult <-
        beginPasswordLoginWithIdentifier
          (accountWorkflowCredentialStore validWorkflow)
          (accountWorkflowMfaStore validWorkflow)
          (LoginUsername (fromMaybe (error "expected valid username") (Username.mkUsername "person_01")))
          password
      case usernameLoginResult of
        PasswordLoginMfaRequired receivedAccountId -> receivedAccountId `shouldBe` accountId
        _ -> expectationFailure "expected MFA to be required for a valid username login"
      handleAccountAction (workflowFor (Left (AccountCredentialStoreUnavailable "down")) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Left (AccountCredentialStoreCorruptData "bad credential")) (Right Nothing) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Left (AccountCredentialStoreUnavailable "down")) (Right Nothing) (Right True) (Right True)) (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "no esta disponible")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Left (MfaStoreUnavailable "down")) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-code") "temporarily unavailable")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right (Just (StoredTotpEnrollment "not-encrypted" (Just 1)))) (Right True) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-code") "temporarily unavailable")
      handleAccountAction unavailableSession (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction (workflowFor (Right (Just confirmedCredential)) (Right (Just confirmedEnrollment)) (Left AccountSessionStoreCorruptData) (Right True)) (loginRequest defaultRequestContext validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "temporarily unavailable")
      handleAccountAction unavailableSession (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 (Just "login-email") "no esta disponible")
      handleAccountAction validWorkflow (spanishLoginRequest validFields)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has iniciado sesion")
      handleAccountAction validWorkflow (logoutRequest defaultRequestContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "You are signed out")
      handleAccountAction validWorkflow ((logoutRequest spanishRequestContext) {HarchWeb.clientActionPath = "/es/logout"})
        >>= (`shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has cerrado sesion")
      let sessionId = fromMaybe (error "expected valid session id") (Session.mkSessionId "0123456789ABCDEF0123456789ABCDEF0123456789ABC")
          sessionContext = defaultRequestContext {requestSessionId = Just sessionId}
      handleAccountAction (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreUnavailable)) (logoutRequest sessionContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "Sign-out is temporarily unavailable")
      handleAccountAction (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreCorruptData)) (logoutRequest sessionContext)
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "Sign-out is temporarily unavailable")
      handleAccountAction
        (workflowFor (Right Nothing) (Right Nothing) (Right True) (Left AccountSessionStoreUnavailable))
        ((logoutRequest (spanishRequestContext {requestSessionId = Just sessionId})) {HarchWeb.clientActionPath = "/es/logout"})
        >>= (`shouldSatisfy` actionHasStatusAndFocus 503 Nothing "no esta disponible")
      logoutSuccess <- handleAccountAction validWorkflow (logoutRequest sessionContext)
      case logoutSuccess of
        Just response -> do
          forceShowValue response `shouldBe` True
          HarchWeb.clientActionHeaders response `shouldSatisfy` any ((== "Set-Cookie") . fst)
        Nothing -> expectationFailure "expected a logout action response"
      spanishLogoutSuccess <- handleAccountAction validWorkflow ((logoutRequest (spanishRequestContext {requestSessionId = Just sessionId})) {HarchWeb.clientActionPath = "/es/logout"})
      spanishLogoutSuccess `shouldSatisfy` actionHasStatusAndFocus 200 Nothing "Has cerrado sesion"

    it "captures a complete authenticator enrollment and returns recovery codes in one patch" $ do
      encryptedSecretReference <- newIORef Nothing
      confirmationHashesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          mfaStore =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \receivedAccountId encryptedSecret receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedNow `shouldBe` 500
                  writeIORef encryptedSecretReference (Just encryptedSecret)
                  pure (Right True),
                loadTotpEnrollment = \receivedAccountId -> do
                  receivedAccountId `shouldBe` accountId
                  fmap (Right . fmap (`StoredTotpEnrollment` Nothing)) (readIORef encryptedSecretReference),
                confirmTotpEnrollment = \receivedAccountId hashes receivedNow -> do
                  receivedAccountId `shouldBe` accountId
                  receivedNow `shouldBe` 500
                  writeIORef confirmationHashesReference (toList hashes)
                  pure (Right True),
                loadUnusedRecoveryCodeHashes = \_ -> pure (error "unexpected recovery-code lookup"),
                consumeRecoveryCodeHash = \_ _ _ -> pure (error "unexpected recovery-code consumption")
              }
          workflow =
            unavailableAccountWorkflow
              { accountWorkflowMfaStore = mfaStore,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = pure 123456
              }
          request path actionContext fields =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = path,
                HarchWeb.clientActionFields = ("account", Account.accountIdText accountId) : fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = actionContext
              }
      started <- handleAccountAction workflow (request "/mfa" defaultRequestContext [("intent", "start")])
      started `shouldSatisfy` \case
        Just response -> HarchWeb.clientActionStatus response == 200 && HarchWeb.clientActionFocusId response == Just "mfa-code"
        Nothing -> False
      secret <-
        case started of
          Just response ->
            case HarchWeb.clientActionPatches response of
              [patch] ->
                let html = HarchWeb.regionPatchHtml patch
                 in case Text.stripPrefix "<code>" (snd (Text.breakOn "<code>" html)) of
                      Just secretWithSuffix -> pure (Text.takeWhile (/= '<') secretWithSuffix)
                      Nothing -> expectationFailure "expected an enrollment secret" >> pure Text.empty
              _ -> expectationFailure "expected one enrollment patch" >> pure Text.empty
          Nothing -> expectationFailure "expected enrollment action" >> pure Text.empty
      totpSecret <- maybe (expectationFailure "expected a valid enrollment secret" >> pure (error "unreachable")) pure (Totp.mkTotpSecret secret)
      confirmed <- handleAccountAction workflow (request "/mfa" defaultRequestContext [("intent", "confirm"), ("code", Totp.totpCodeText (Totp.totpCode 123456 totpSecret))])
      confirmed `shouldSatisfy` \case
        Just response -> HarchWeb.clientActionStatus response == 200 && any (Text.isInfixOf "data-recovery-codes=\"true\"" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      confirmationHashes <- readIORef confirmationHashesReference
      length confirmationHashes `shouldBe` 8
      spanishStarted <- handleAccountAction workflow (request "/es/mfa" (defaultRequestContext {requestLocale = Spanish}) [("intent", "start")])
      spanishStarted `shouldSatisfy` \case
        Just response -> HarchWeb.clientActionStatus response == 200 && HarchWeb.clientActionFocusId response == Just "mfa-code" && any (Text.isInfixOf "Agrega este secreto" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False
      spanishSecret <-
        case spanishStarted of
          Just response ->
            case HarchWeb.clientActionPatches response of
              [patch] ->
                let html = HarchWeb.regionPatchHtml patch
                 in case Text.stripPrefix "<code>" (snd (Text.breakOn "<code>" html)) of
                      Just secretWithSuffix -> pure (Text.takeWhile (/= '<') secretWithSuffix)
                      Nothing -> expectationFailure "expected a Spanish enrollment secret" >> pure Text.empty
              _ -> expectationFailure "expected one Spanish enrollment patch" >> pure Text.empty
          Nothing -> expectationFailure "expected a Spanish enrollment action" >> pure Text.empty
      spanishTotpSecret <- maybe (expectationFailure "expected a valid Spanish enrollment secret" >> pure (error "unreachable")) pure (Totp.mkTotpSecret spanishSecret)
      spanishConfirmed <- handleAccountAction workflow (request "/es/mfa" (defaultRequestContext {requestLocale = Spanish}) [("intent", "confirm"), ("code", Totp.totpCodeText (Totp.totpCode 123456 spanishTotpSecret))])
      spanishConfirmed `shouldSatisfy` \case
        Just response -> HarchWeb.clientActionStatus response == 200 && isNothing (HarchWeb.clientActionFocusId response) && any (Text.isInfixOf "Autenticador registrado" . HarchWeb.regionPatchHtml) (HarchWeb.clientActionPatches response)
        Nothing -> False

    it "returns every MFA enrollment action error as a localized region patch" $ do
      let accountId = requiredAccountId "account_01"
          request fields =
            HarchWeb.ClientActionRequest
              { HarchWeb.clientActionMethod = "POST",
                HarchWeb.clientActionPath = "/mfa",
                HarchWeb.clientActionFields = ("account", Account.accountIdText accountId) : fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext
              }
          spanishRequest fields =
            (request fields)
              { HarchWeb.clientActionPath = "/es/mfa",
                HarchWeb.clientActionContext = defaultRequestContext {requestLocale = Spanish}
              }
          workflowFor mfaStore =
            unavailableAccountWorkflow
              { accountWorkflowMfaStore = mfaStore,
                accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
                accountWorkflowClock = pure 500,
                accountWorkflowTotpClock = pure 123456
              }
          mfaStoreFor saveResult loadResult confirmationResult =
            MfaStore
              { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure saveResult,
                loadTotpEnrollment = \_ -> pure loadResult,
                confirmTotpEnrollment = \_ _ _ -> pure confirmationResult,
                loadUnusedRecoveryCodeHashes = \_ -> error "unexpected recovery-code lookup",
                consumeRecoveryCodeHash = \_ _ _ -> error "unexpected recovery-code consumption"
              }
          validTotpSecret = fromMaybe (error "expected TOTP secret") (Totp.mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
          encryptedTotpSecret =
            fromMaybe
              (error "expected encrypted TOTP secret")
              (Secret.encryptSecretWithNonce (totpEncryptionKey defaultAppEnvironmentConfig) (ByteString.replicate 12 3) (TextEncoding.encodeUtf8 (Totp.renderTotpSecret validTotpSecret)))
          expect mfaStore fields status focusId message = do
            actionResult <- handleAccountAction (workflowFor mfaStore) (request fields)
            actionResult `shouldSatisfy` actionHasStatusAndFocus status focusId message
          expectSpanish mfaStore fields status focusId message = do
            actionResult <- handleAccountAction (workflowFor mfaStore) (spanishRequest fields)
            actionResult `shouldSatisfy` actionHasStatusAndFocus status focusId message
      expect (mfaStoreFor (Right False) (Right Nothing) (Right False)) [("intent", "start")] 422 (Just "mfa-account") "Verify your email address"
      expect (mfaStoreFor (Left (MfaStoreUnavailable "down")) (Right Nothing) (Right False)) [("intent", "start")] 503 (Just "mfa-account") "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm")] 422 (Just "mfa-code") "Enter a six-digit authenticator code"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Start a new authenticator enrollment"
      expect (mfaStoreFor (Right True) (Left (MfaStoreCorruptData "bad enrollment")) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "temporarily unavailable"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100)))) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "That enrollment can no longer be confirmed"
      expect (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "other")] 422 (Just "mfa-account") "Choose an enrollment action"
      expectSpanish (mfaStoreFor (Right False) (Right Nothing) (Right False)) [("intent", "start")] 422 (Just "mfa-account") "Verifica tu direccion de correo"
      expectSpanish (mfaStoreFor (Left (MfaStoreUnavailable "down")) (Right Nothing) (Right False)) [("intent", "start")] 503 (Just "mfa-account") "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm")] 422 (Just "mfa-code") "Introduce un codigo de autenticador"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Inicia un nuevo registro"
      expectSpanish (mfaStoreFor (Right True) (Left (MfaStoreCorruptData "bad enrollment")) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" Nothing))) (Right False)) [("intent", "confirm"), ("code", "123456")] 503 (Just "mfa-code") "no esta disponible temporalmente"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment "not-an-envelope" (Just 100)))) (Right False)) [("intent", "confirm"), ("code", "123456")] 422 (Just "mfa-code") "Ese registro ya no se puede confirmar"
      expectSpanish (mfaStoreFor (Right True) (Right Nothing) (Right False)) [("intent", "other")] 422 (Just "mfa-account") "Elige una accion de registro"
      expect (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment encryptedTotpSecret Nothing))) (Right False)) [("intent", "confirm"), ("code", "000000")] 422 (Just "mfa-code") "That authenticator code is invalid"
      expectSpanish (mfaStoreFor (Right True) (Right (Just (StoredTotpEnrollment encryptedTotpSecret Nothing))) (Right False)) [("intent", "confirm"), ("code", "000000")] 422 (Just "mfa-code") "Ese codigo de autenticador no es valido"
      forM_
        [ (MfaEnrollmentRecoveryCodeHashingFailed, "RecoveryCodeHashingError", "recovery-code hashing failed"),
          (MfaEnrollmentEncryptionFailed, "TotpEncryptionError", "TOTP secret encryption failed")
        ]
        $ \(failureValue, expectedType, expectedDetail) ->
          case mfaEnrollmentFailureDiagnostics "confirm" failureValue of
            Nothing -> expectationFailure "expected infrastructure diagnostics for the MFA failure"
            Just diagnostics -> do
              AppEffect.failureCode diagnostics `shouldBe` "account.mfa.confirm"
              AppEffect.failureType diagnostics `shouldBe` expectedType
              AppEffect.failureLogEntries diagnostics `shouldSatisfy` any (Text.isInfixOf expectedDetail)

  describe "WebApi.Postgres" $ do
    it "uses bound parameters for pending account and verification persistence" $ do
      recordedQueriesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          username = fromMaybe (error "Expected username") (Username.mkUsername "person_01")
          token = requiredVerificationToken (Text.replicate 43 "a")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Just username,
                pendingAccountDisplayName = Just "Person Example",
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          runner config sql parameters =
            config `seq` do
              modifyIORef' recordedQueriesReference (<> [(sql, parameters)])
              pure $
                if "INSERT INTO web_api.accounts" `Text.isInfixOf` sql
                  then Right [["account_01"]]
                  else
                    if "SELECT account_id, email_normalized" `Text.isInfixOf` sql
                      then Right [["account_01", "person@example.test", "500"]]
                      else
                        if "DELETE FROM web_api.email_verifications" `Text.isInfixOf` sql
                          then Right [["account_01"]]
                          else Left "unexpected query"
          accountStore = buildRuntimePostgresAccountStoreWithRunner runner postgresTestConfig
      assertAccountStoreSuccess (createPendingAccount accountStore pendingAccount) id
      assertAccountStoreSuccess
        (findEmailVerification accountStore (Account.emailVerificationTokenDigest token))
        (\case Just storedVerification -> storedVerification == pendingAccountVerification pendingAccount; Nothing -> False)
      assertAccountStoreSuccess
        (replaceEmailVerification accountStore (pendingAccountVerification pendingAccount))
        id
      assertAccountStoreSuccess
        (consumeEmailVerification accountStore (Account.emailVerificationTokenDigest token) 499)
        (\case Just consumedAccountId -> consumedAccountId == accountId; Nothing -> False)
      recordedQueries <- readIORef recordedQueriesReference
      let queryText = Text.intercalate "\n" (map fst recordedQueries)
          parameterText = Text.intercalate "\n" (concatMap snd recordedQueries)
      Text.isInfixOf (Password.passwordHashText passwordHash) queryText `shouldBe` False
      Text.isInfixOf (Account.emailVerificationTokenDigestText (Account.emailVerificationTokenDigest token)) queryText `shouldBe` False
      Text.isInfixOf (Password.passwordHashText passwordHash) parameterText `shouldBe` True
      Text.isInfixOf (Account.emailVerificationTokenDigestText (Account.emailVerificationTokenDigest token)) parameterText `shouldBe` True
      Text.isInfixOf (Username.usernameText username) parameterText `shouldBe` True
      Text.isInfixOf "Person Example" parameterText `shouldBe` True

    it "maps malformed account-store query results to application-owned errors" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Nothing,
                pendingAccountDisplayName = Nothing,
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          storeFor result = buildRuntimePostgresAccountStoreWithRunner (\_ _ _ -> pure result) postgresTestConfig
      assertAccountStoreError (createPendingAccount (storeFor (Left "connection failed")) pendingAccount) (isUnavailable "connection failed")
      assertAccountStoreSuccess (createPendingAccount (storeFor (Right [])) pendingAccount) not
      assertAccountStoreError (createPendingAccount (storeFor (Right [["other_account"]])) pendingAccount) (isCorrupt "unexpected pending-account result: [[\"other_account\"]]")
      assertAccountStoreError (replaceEmailVerification (storeFor (Left "connection failed")) (pendingAccountVerification pendingAccount)) (isUnavailable "connection failed")
      assertAccountStoreSuccess (replaceEmailVerification (storeFor (Right [])) (pendingAccountVerification pendingAccount)) not
      assertAccountStoreError (replaceEmailVerification (storeFor (Right [["other_account"]])) (pendingAccountVerification pendingAccount)) (isCorrupt "unexpected email-verification replacement result: [[\"other_account\"]]")
      assertAccountStoreError (findEmailVerification (storeFor (Left "connection failed")) (Account.emailVerificationTokenDigest token)) (isUnavailable "connection failed")
      assertAccountStoreSuccess (findEmailVerification (storeFor (Right [])) (Account.emailVerificationTokenDigest token)) (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (findEmailVerification (storeFor (Right [["invalid id", "person@example.test", "500"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid account id")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01", "invalid email", "500"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid email address")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01", "person@example.test", "invalid"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid expiry")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "unexpected email-verification result: [[\"account_01\"]]")
      assertAccountStoreError (consumeEmailVerification (storeFor (Right [["invalid id"]])) (Account.emailVerificationTokenDigest token) 499) (isCorrupt "email verification was consumed for an invalid account id")
      assertAccountStoreError (consumeEmailVerification (storeFor (Left "connection failed")) (Account.emailVerificationTokenDigest token) 499) (isUnavailable "connection failed")
      assertAccountStoreSuccess (consumeEmailVerification (storeFor (Right [])) (Account.emailVerificationTokenDigest token) 499) (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (consumeEmailVerification (storeFor (Right [["account_01", "extra"]])) (Account.emailVerificationTokenDigest token) 499) (isCorrupt "unexpected email-verification consumption result: [[\"account_01\",\"extra\"]]")

    it "loads safe account profiles and rejects malformed profile rows" $ do
      let accountId = requiredAccountId "account_01"
          profileStoreFor result = buildRuntimePostgresAccountProfileStoreWithRunner (\_ _ _ -> pure result) postgresTestConfig
          username = fromMaybe (error "expected username") (Username.mkUsername "person_01")
          expectedProfile = AccountProfile accountId (requiredEmailAddress "person@example.test") (Just username) (Just "Person Example") True
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "person_01", "Person Example", "500"]])) accountId)
        ( \case
            Just profile ->
              accountProfileId profile == accountProfileId expectedProfile
                && accountProfileEmail profile == accountProfileEmail expectedProfile
                && accountProfileUsername profile == accountProfileUsername expectedProfile
                && accountProfileDisplayName profile == accountProfileDisplayName expectedProfile
                && accountProfileEmailVerified profile == accountProfileEmailVerified expectedProfile
            Nothing -> False
        )
      accountProfileId expectedProfile `shouldBe` accountId
      accountProfileEmail expectedProfile `shouldBe` requiredEmailAddress "person@example.test"
      accountProfileUsername expectedProfile `shouldBe` Just username
      accountProfileDisplayName expectedProfile `shouldBe` Just "Person Example"
      accountProfileEmailVerified expectedProfile `shouldBe` True
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "", "", ""]])) accountId)
        (\case Just profile -> not (accountProfileEmailVerified profile) && isNothing (accountProfileUsername profile) && isNothing (accountProfileDisplayName profile); Nothing -> False)
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [])) accountId)
        (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (findAccountProfile (profileStoreFor (Left "connection failed")) accountId) (isUnavailable "connection failed")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["invalid id", "person@example.test", "", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid account id")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01", "invalid email", "", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid email address")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "invalid username", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid username")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_02", "person@example.test", "", "", ""]])) accountId) (isCorrupt "account profile lookup returned a different account id")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01"]])) accountId) (isCorrupt "unexpected account profile lookup result: [[\"account_01\"]]")
      buildRuntimePostgresAccountProfileStore postgresTestConfig `seq` pure ()

    it "executes the native account-profile adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      assertAccountStoreSuccess
        ( findAccountProfile
            (buildRuntimePostgresAccountProfileStore defaultRealPostgresConfig)
            (requiredAccountId "profile_lookup_missing_01")
        )
        (\case Nothing -> True; Just _ -> False)

    it "translates database config into psql commands for page queries" $ do
      recordedCommandsReference <- newIORef []
      let runner command = do
            modifyIORef' recordedCommandsReference (<> [command])
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "SSR rápido\nDatos compartidos"
                          else "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner runner postgresTestConfig
      loadHomePageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageData postgresEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageData postgresEffect spanishRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Charge depuis PostgreSQL.",
              secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
            }
      loadSecondPageDataWithObservability postgresEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      recordedCommands <- readIORef recordedCommandsReference
      let expectedQueryCommand sql =
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
                    Text.unpack sql
                  ],
                postgresEnvironment = [("PGPASSWORD", "super-secret")]
              }
      recordedCommands
        `shouldBe` map
          expectedQueryCommand
          [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'es';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;"
          ]

    it "maps missing rows and command failures into database errors" $ do
      let missingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult Text.empty
                  | otherwise ->
                      failingPostgresResult "relation does not exist"
          postgresEffect = buildPostgresDatabaseEffectWithRunner missingRunner postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: ")
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "relation does not exist")
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "relation does not exist"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "maps scalar query failures, malformed rows, and highlight query failures into explicit errors" $ do
      let homeFailureRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 2,
                      postgresStdout = Text.empty,
                      postgresStderr = Text.empty
                    }
                else successfulPostgresResult Text.empty
          malformedScalarRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then successfulPostgresResult "first\nsecond"
                else successfulPostgresResult Text.empty
          highlightFailureRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      successfulPostgresResult Text.empty
      loadHomePageData (buildPostgresDatabaseEffectWithRunner homeFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "psql command failed")
      loadHomePageData (buildPostgresDatabaseEffectWithRunner malformedScalarRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: first, second")
      loadSecondPageData (buildPostgresDatabaseEffectWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageDataWithObservability (buildPostgresDatabaseEffectWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "translates database config into runtime SQL queries for page queries" $ do
      recordedScalarQueriesReference <- newIORef []
      recordedRowsQueriesReference <- newIORef []
      let scalarRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedScalarQueriesReference (<> [sql])
            pure $
              case sql of
                queryText
                  | Text.isInfixOf "route_slug = 'home'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'es'" queryText
                          then "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'es'" queryText
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | otherwise ->
                      Left "unexpected query"
          rowsRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedRowsQueriesReference (<> [sql])
            pure $
              if Text.isInfixOf "locale = 'es'" sql
                then Right ["SSR rápido", "Datos compartidos"]
                else Right ["Fast SSR", "Shared route data"]
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageData postgresEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageDataWithObservability postgresEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      readIORef recordedScalarQueriesReference
        `shouldReturn` [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'es';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';"
                       ]
      readIORef recordedRowsQueriesReference
        `shouldReturn` [ "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
                         "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;"
                       ]

    it "maps runtime query failures into explicit database errors" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'home'" sql
                then Left "connection refused"
                else Right "Loaded from PostgreSQL."
          rowsRunner _ _ =
            pure (Left "highlights unavailable")
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageData postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "connection refused")
      loadSecondPageData postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "maps runtime second-page summary failures without attempting highlight queries" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'second'" sql
                then Left "summary unavailable"
                else Right "Server-rendered home page with stubbed content."
          rowsRunner _ _ =
            error "expected runtime highlight query to be skipped when the second-page summary fails"
          postgresEffect =
            buildRuntimePostgresDatabaseEffectWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadSecondPageDataWithObservability postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "summary unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "covers runtime libpq helper decoding branches" $ do
      decodeRuntimeQueryValue Nothing
        `shouldBe` Left "unexpected NULL column value"
      decodeRuntimeQueryValue (Just (ByteString.pack [115, 115, 114, 255]))
        `shouldBe` Right (Text.pack ['s', 's', 'r', '\xfffd'])
      renderRuntimeConnectionErrorMessage Nothing
        `shouldBe` "libpq connection failed"
      renderRuntimeConnectionErrorMessage (Just (ByteString.pack [32, 114, 117, 110, 255, 10]))
        `shouldBe` Text.pack ['r', 'u', 'n', '\xfffd']
      renderRuntimeResultErrorMessage Nothing
        `shouldBe` "libpq query failed"
      renderRuntimeResultErrorMessage (Just (ByteString.pack [32, 113, 117, 101, 114, 121, 255, 10]))
        `shouldBe` Text.pack ['q', 'u', 'e', 'r', 'y', '\xfffd']

    it "runs direct runtime libpq queries and surfaces malformed-row, syntax, and connection failures explicitly" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()

      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT 'Loaded from PostgreSQL.'::text;"
        `shouldReturn` Right "Loaded from PostgreSQL."
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('Fast SSR'::text), ('Shared route data'::text)) AS runtime_rows(value);"
        `shouldReturn` Right ["Fast SSR", "Shared route data"]
      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('first'::text), ('second'::text)) AS runtime_rows(value);"
        `shouldReturn` Left "expected exactly one row: first, second"
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT NULL::text;"
        `shouldReturn` Left "unexpected NULL column value"
      runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT $1::text, $2::text;" ["first value", "second value"]
        `shouldReturn` Right [["first value", "second value"]]
      runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT NULL::text;" []
        `shouldReturn` Left "unexpected NULL column value"

      accountId <- Account.generateAccountId
      token <- Account.generateEmailVerificationToken
      let emailAddress = requiredEmailAddress (Account.accountIdText accountId <> "@example.test")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Nothing,
                pendingAccountDisplayName = Nothing,
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          accountStore = buildRuntimePostgresAccountStore defaultRealPostgresConfig
      assertAccountStoreSuccess (createPendingAccount accountStore pendingAccount) id
      assertAccountStoreSuccess
        (findEmailVerification accountStore (Account.emailVerificationTokenDigest token))
        (\case Just storedVerification -> storedVerification == pendingAccountVerification pendingAccount; Nothing -> False)
      assertAccountStoreSuccess
        (consumeEmailVerification accountStore (Account.emailVerificationTokenDigest token) 499)
        (\case Just consumedAccountId -> consumedAccountId == accountId; Nothing -> False)

      syntaxResult <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT FROM"
      syntaxResult
        `shouldSatisfy` \case
          Left runtimeError ->
            Text.isInfixOf "syntax error" runtimeError
          Right rows ->
            error ("expected syntax failure, got rows: " <> show rows)

      parameterSyntaxResult <- runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT FROM" []
      parameterSyntaxResult
        `shouldSatisfy` \case
          Left runtimeError -> Text.isInfixOf "syntax error" runtimeError
          Right rows -> error ("expected parameterized syntax failure, got rows: " <> show rows)

      withUnusedTcpEndpoint $ \unusedEndpoint -> do
        refusedResult <-
          runRuntimeScalarQuery
            defaultRealPostgresConfig
              { databasePort = tcpEndpointPort unusedEndpoint
              }
            "SELECT 1::text;"
        refusedResult
          `shouldSatisfy` \case
            Left runtimeError ->
              not (Text.null runtimeError)
                && not (Text.isInfixOf "posix_spawnp" runtimeError)
            Right value ->
              error ("expected connection failure, got value: " <> show value)
        parameterRefusedResult <-
          runRuntimeParameterizedRowsQuery
            defaultRealPostgresConfig
              { databasePort = tcpEndpointPort unusedEndpoint
              }
            "SELECT $1::text;"
            ["value"]
        parameterRefusedResult
          `shouldSatisfy` \case
            Left runtimeError -> not (Text.null runtimeError)
            Right rows -> error ("expected parameterized connection failure, got rows: " <> show rows)

    it "runs migrations and seed statements in order through the provided runner" $ do
      recordedCommandsReference <- newIORef []
      let runner command = modifyIORef' recordedCommandsReference (<> [command]) >> pure (successfulPostgresResult Text.empty)
      runPostgresMigrationsWithRunnerForRuntime runner migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
      runPostgresSeedWithRunner runner postgresTestConfig `shouldReturn` Right ()
      recordedCommands <- readIORef recordedCommandsReference
      map commandSql recordedCommands `shouldBe` migrationStatementsFor migrationPostgresTestConfig postgresTestConfig <> seedStatements

    it "creates account verification, MFA, and opaque-session storage without persisting raw bearer secrets" $ do
      migrationStatementsFor migrationPostgresTestConfig postgresTestConfig
        `shouldSatisfy` \statements ->
          all
            (`elem` statements)
            [ "CREATE TABLE IF NOT EXISTS web_api.accounts (account_id TEXT PRIMARY KEY, email_normalized TEXT NOT NULL UNIQUE, username TEXT, display_name TEXT, password_hash TEXT NOT NULL, email_verified_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
              "ALTER TABLE web_api.accounts ADD COLUMN IF NOT EXISTS username TEXT;",
              "ALTER TABLE web_api.accounts ADD COLUMN IF NOT EXISTS display_name TEXT;",
              "CREATE UNIQUE INDEX IF NOT EXISTS accounts_username_lower_unique ON web_api.accounts (lower(username)) WHERE username IS NOT NULL;",
              "CREATE TABLE IF NOT EXISTS web_api.email_verifications (token_digest TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL);",
              "CREATE TABLE IF NOT EXISTS web_api.account_totp (account_id TEXT PRIMARY KEY REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, encrypted_secret BYTEA NOT NULL, confirmed_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
              "CREATE TABLE IF NOT EXISTS web_api.account_recovery_codes (account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, code_hash TEXT NOT NULL UNIQUE, created_at_nanoseconds BIGINT NOT NULL, used_at_nanoseconds BIGINT, PRIMARY KEY (account_id, code_hash));",
              "CREATE TABLE IF NOT EXISTS web_api.account_sessions (session_id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);",
              "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.accounts TO \"" <> databaseUser postgresTestConfig <> "\";",
              "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.email_verifications TO \"" <> databaseUser postgresTestConfig <> "\";",
              "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.account_totp TO \"" <> databaseUser postgresTestConfig <> "\";",
              "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.account_recovery_codes TO \"" <> databaseUser postgresTestConfig <> "\";",
              "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.account_sessions TO \"" <> databaseUser postgresTestConfig <> "\";"
            ]

    it "keeps the legacy same-config migration wrappers on the runtime-config path"
      $ withFakePsqlScript
        (fmap (,Text.empty) (migrationStatementsFor postgresTestConfig postgresTestConfig))
      $ \argsLogPath -> do
        recordedCommandsReference <- newIORef []
        let runner command = modifyIORef' recordedCommandsReference (<> [command]) >> pure (successfulPostgresResult Text.empty)
        runPostgresMigrationsWithRunner runner postgresTestConfig `shouldReturn` Right ()
        map commandSql
          <$> readIORef recordedCommandsReference
            `shouldReturn` migrationStatementsFor postgresTestConfig postgresTestConfig
        runPostgresMigrations postgresTestConfig `shouldReturn` Right ()
        let renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            (fmap (renderMutationLogEntry postgresTestConfig) (migrationStatementsFor postgresTestConfig postgresTestConfig))

    it "stops database setup when a migration or seed command fails" $ do
      case seedStatements of
        failingSeedStatement : _ -> do
          let runner command =
                pure $
                  if commandSql command == failingSeedStatement
                    then failingPostgresResult "seed failed"
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
                          "DELETE FROM web_api.page_highlights;"
                        ],
                      postgresEnvironment = [("PGPASSWORD", "super-secret")]
                    }
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 1,
                      postgresStdout = Text.empty,
                      postgresStderr = "seed failed"
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
                postgresStdout = "1",
                postgresStderr = Text.empty
              }
          failedCommandResult =
            PostgresCommandResult
              { postgresExitCode = ExitFailure 3,
                postgresStdout = Text.empty,
                postgresStderr = "boom"
              }
          runnerError = PostgresCommandFailed command commandResult
          unexpectedRowsError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
      command `shouldBe` command
      command `shouldNotBe` command {postgresArguments = ["--command", "SELECT 2;"]}
      commandResult `shouldBe` commandResult
      commandResult `shouldNotBe` commandResult {postgresStdout = "2"}
      runnerError `shouldBe` runnerError
      runnerError `shouldNotBe` PostgresCommandFailed command failedCommandResult
      unexpectedRowsError `shouldBe` unexpectedRowsError
      unexpectedRowsError `shouldNotBe` UnexpectedQueryRows "expected exactly one row" ["first"]
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
        ( [ ("SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';", "Server-rendered home page with stubbed content."),
            ("SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';", "Second page content with stubbed data ready for future loaders."),
            ("SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;", Text.empty)
          ]
            <> fmap (,Text.empty) (migrationStatementsFor migrationPostgresTestConfig postgresTestConfig <> seedStatements)
        )
      $ \argsLogPath -> do
        let application = buildAppWithDatabase defaultAppConfig (buildPostgresDatabaseEffect postgresTestConfig)
        fmap stripVolatileDatabaseTimingResponse (HarchWeb.renderResponse application secondRequest)
          `shouldReturn` HarchWeb.PageResponseWithMetadata
            HarchWeb.ResponseBody
              { HarchWeb.responseStatus = 200,
                HarchWeb.responseContentType = "text/html; charset=utf-8",
                HarchWeb.responseBody = "",
                HarchWeb.responseObservabilityAttributes =
                  [ Observability.ObservabilityAttribute
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
                      { Observability.attributeName = "db.system",
                        Observability.attributeValue = Observability.TextAttribute "postgresql"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.operation.name",
                        Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                      },
                    Observability.ObservabilityAttribute
                      { Observability.attributeName = "db.query.template",
                        Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                      }
                  ],
                HarchWeb.responseLogEntries = []
              }
            ( HarchWeb.Page
                { HarchWeb.pageTitle = "web-api: Second",
                  HarchWeb.pageRoute = SecondRoute,
                  HarchWeb.pageContext = defaultRequestContext,
                  HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
                  HarchWeb.pageBootstrapHooks = ["second-page"]
                }
            )
        runPostgresMigrationsForRuntime migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
        runPostgresSeed postgresTestConfig `shouldReturn` Right ()
        let renderQueryLogEntry sql =
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --tuples-only --no-align --quiet --command "
                <> Text.unpack sql
            renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            ( [ renderQueryLogEntry "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                renderQueryLogEntry "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;"
              ]
                <> fmap (renderMutationLogEntry migrationPostgresTestConfig) (migrationStatementsFor migrationPostgresTestConfig postgresTestConfig)
                <> fmap (renderMutationLogEntry postgresTestConfig) seedStatements
            )

    it "uses stderr from the default psql runner when a command fails"
      $ withFakePsqlScriptResults
        [ ( "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            PostgresCommandResult
              { postgresExitCode = ExitFailure 4,
                postgresStdout = Text.empty,
                postgresStderr = "default runner failed"
              }
          )
        ]
      $ \_ ->
        loadHomePageData (buildPostgresDatabaseEffect postgresTestConfig) defaultRequestContext
          `shouldReturn` Left (HomePageDataError "default runner failed")

    it "prefers a runtime that is already running the named postgres container in the containerized psql wrapper" $ do
      containerizedPsqlScriptContents `shouldContain'` "database_endpoint_is_reachable()"
      containerizedPsqlScriptContents `shouldContain'` "host_psql_path=\"${WEB_API_REAL_PSQL_PATH:-}\""
      containerizedPsqlScriptContents `shouldContain'` "if [ -n \"$host_psql_path\" ] && [ -x \"$host_psql_path\" ] && database_endpoint_is_reachable; then"
      containerizedPsqlScriptContents `shouldContain'` "runtime_with_running_container()"
      containerizedPsqlScriptContents `shouldContain'` "for candidate in docker podman; do"
      containerizedPsqlScriptContents `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      containerizedPsqlScriptContents `shouldContain'` "exec \"$runtime\" exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\""

    it "prefers a runtime that is already running the named postgres container before trying to start or create one" $ do
      ensureDefaultPostgresAvailableScript `shouldContain'` "database_endpoint_is_reachable()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "host_psql_is_available()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "owner_is_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "ensure_owner_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "if database_endpoint_is_reachable && host_psql_is_available; then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "runtime_with_running_container()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "for candidate in docker podman; do"
      ensureDefaultPostgresAvailableScript `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "\"$runtime\" start web-api-postgres >/dev/null 2>&1 && return 0"

    it "loads seeded page data through the concrete postgres adapter against real PostgreSQL" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        let postgresEffect = buildPostgresDatabaseEffect defaultRealPostgresConfig
        loadHomePageData postgresEffect defaultRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Server-rendered home page with stubbed content."
              }
        loadSecondPageData postgresEffect defaultRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                secondPageDataHighlights = []
              }
        loadHomePageData postgresEffect spanishRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
              }
        loadSecondPageData postgresEffect spanishRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                secondPageDataHighlights = []
              }

  describe "parseAppEnvironmentConfig" $ do
    it "parses committed development defaults into the expected config" $
      parseAppEnvironmentConfig committedEnvDefaults [] []
        `shouldBe` Right defaultAppEnvironmentConfig

    it "lets .env.local override committed .env defaults" $ do
      let localOverrides =
            [ ("APP_MODE", "production"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides []
        `shouldBe` Right
          defaultAppEnvironmentConfig
            { appMode = Production,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 6432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "local_password"
                  }
            }

    it "lets environment variables override .env.local values" $ do
      let localOverrides =
            [ ("APP_MODE", "production"),
              ("DATABASE_HOST", "localhost"),
              ("DATABASE_PORT", "6432"),
              ("DATABASE_NAME", "web_api_local"),
              ("DATABASE_USER", "local_user"),
              ("DATABASE_PASSWORD", "local_password")
            ]
          environmentOverrides =
            [ ("APP_MODE", "test"),
              ("DATABASE_PORT", "7432"),
              ("DATABASE_PASSWORD", "runtime_password")
            ]
      parseAppEnvironmentConfig committedEnvDefaults localOverrides environmentOverrides
        `shouldBe` Right
          defaultAppEnvironmentConfig
            { appMode = Test,
              databaseConfig =
                DatabaseConfig
                  { databaseHost = "localhost",
                    databasePort = 7432,
                    databaseName = "web_api_local",
                    databaseUser = "local_user",
                    databasePassword = "runtime_password"
                  }
            }

    it "fails missing required values with explicit errors" $
      parseAppEnvironmentConfig
        [ ("APP_MODE", "development"),
          ("DATABASE_HOST", "127.0.0.1"),
          ("DATABASE_PORT", "5432"),
          ("DATABASE_NAME", "web_api_dev"),
          ("DATABASE_USER", "web_api_runtime")
        ]
        []
        []
        `shouldBe` Left (MissingConfigValue "DATABASE_PASSWORD")

    it "fails invalid port or mode values with precise errors" $ do
      parseAppEnvironmentConfig committedEnvDefaults [] [("APP_MODE", "staging")]
        `shouldBe` Left (InvalidConfigValue "APP_MODE" "staging")
      parseAppEnvironmentConfig committedEnvDefaults [] [("DATABASE_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "DATABASE_PORT" "0")
      parseAppEnvironmentConfig committedEnvDefaults [] [("SMTP_PORT", "65536")]
        `shouldBe` Left (InvalidConfigValue "SMTP_PORT" "65536")
      parseAppEnvironmentConfig committedEnvDefaults [] [("SMTP_PORT", "not-a-port")]
        `shouldBe` Left (InvalidConfigValue "SMTP_PORT" "not-a-port")
      parseAppEnvironmentConfig committedEnvDefaults [] [("TOTP_ENCRYPTION_KEY", "not-a-key")]
        `shouldBe` Left (InvalidConfigValue "TOTP_ENCRYPTION_KEY" "not-a-key")

  describe "loadAppEnvironmentConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers" $
      withSystemTempDirectory "app-environment-config" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Right
              defaultAppEnvironmentConfig
                { appMode = Test,
                  databaseConfig =
                    DatabaseConfig
                      { databaseHost = "db.shared",
                        databasePort = 7432,
                        databaseName = "shared_db",
                        databaseUser = "shared_user",
                        databasePassword = "local_password"
                      }
                }

    it "lets process environment override .env.local values" $
      withSystemTempDirectory "app-environment-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withTemporaryEnvironment "APP_MODE" (Just "production") $
            withTemporaryEnvironment "DATABASE_PORT" (Just "8432") $
              withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime_password") $ do
                let envPath = tempDirectory <> "/.env"
                    envLocalPath = tempDirectory <> "/.env.local"
                writeFile envPath "APP_MODE=development\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
                writeFile envLocalPath "APP_MODE=test\nDATABASE_PORT=7432\nDATABASE_PASSWORD=local_password\n"
                loadAppEnvironmentConfigWithFiles envPath envLocalPath
                  `shouldReturn` Right
                    defaultAppEnvironmentConfig
                      { appMode = Production,
                        databaseConfig =
                          DatabaseConfig
                            { databaseHost = "db.shared",
                              databasePort = 8432,
                              databaseName = "shared_db",
                              databaseUser = "shared_user",
                              databasePassword = "runtime_password"
                            }
                      }

    it "reports invalid override files with the failing path" $
      withSystemTempDirectory "app-environment-config-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_HOST\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentOverridesFileError envPath (InvalidConfigOverridesLine 1 "DATABASE_HOST"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-environment-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          withUnreadableFile envLocalPath "APP_MODE=test\n" $ do
            result <- loadAppEnvironmentConfigWithFiles envPath envLocalPath
            result `shouldSatisfy` \case
              Left
                (AppEnvironmentOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                  | failingPath == envLocalPath ->
                      not (Text.null errorMessage)
              _ -> False

    it "reports parse errors after both files load successfully" $
      withSystemTempDirectory "app-environment-config-parse-error" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          let envPath = tempDirectory <> "/.env"
              envLocalPath = tempDirectory <> "/.env.local"
          writeFile envPath "DATABASE_PORT=0\n"
          loadAppEnvironmentConfigWithFiles envPath envLocalPath
            `shouldReturn` Left
              (AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0"))

  describe "loadAppEnvironmentConfig" $
    it "loads the default .env file names from the current directory" $
      withSystemTempDirectory "app-environment-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $ do
          writeFile (tempDirectory <> "/.env") "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nDATABASE_NAME=shared_db\nDATABASE_USER=shared_user\nDATABASE_PASSWORD=shared_password\n"
          writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nDATABASE_PASSWORD=local_password\n"
          withCurrentDirectory tempDirectory $
            loadAppEnvironmentConfig
              `shouldReturn` Right
                defaultAppEnvironmentConfig
                  { appMode = Test,
                    databaseConfig =
                      DatabaseConfig
                        { databaseHost = "db.shared",
                          databasePort = 6432,
                          databaseName = "shared_db",
                          databaseUser = "shared_user",
                          databasePassword = "local_password"
                        }
                  }

  describe "AppEnvironmentConfigLoadError" $
    it "keeps load-error equality and rendering deterministic" $ do
      let fileLoadError = AppEnvironmentOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppEnvironmentConfigParseError (InvalidConfigValue "DATABASE_PORT" "0")
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppEnvironmentOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppEnvironmentConfigParseError (InvalidConfigValue \"DATABASE_PORT\" \"0\")]"

  describe "parseAppStartupConfig" $
    it "parses committed environment and runtime defaults into the expected startup config" $ do
      defaultAppStartupConfig
        `shouldBe` AppStartupConfig
          { startupEnvironmentConfig = defaultAppEnvironmentConfig,
            startupAppConfig = defaultAppConfig
          }
      parseAppStartupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Right defaultAppStartupConfig

  describe "loadAppStartupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for runtime startup" $
      withSystemTempDirectory "app-startup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            writeFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
            loadAppStartupConfigWithFiles envPath envLocalPath
              `shouldReturn` Right
                AppStartupConfig
                  { startupEnvironmentConfig =
                      defaultAppEnvironmentConfig
                        { appMode = Production,
                          databaseConfig =
                            DatabaseConfig
                              { databaseHost = "db.shared",
                                databasePort = 6432,
                                databaseName = "web_api_dev",
                                databaseUser = "web_api_runtime",
                                databasePassword = "local_password"
                              }
                        },
                    startupAppConfig =
                      defaultAppConfig
                        { appTitlePrefix = "web-api-local",
                          listenerConfigs =
                            [ ListenerConfig
                                { listenerHost = "127.0.0.1",
                                  listenerPort = 7443,
                                  listenerScheme = Http,
                                  listenerTls = Nothing,
                                  listenerAcme = Nothing
                                }
                            ]
                        }
                  }

    it "lets process environment override .env.local values for runtime startup" $
      withSystemTempDirectory "app-startup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
              withTemporaryEnvironment "LISTENER_0_HOST" (Just "0.0.0.0") $
                withTemporaryEnvironment "LISTENER_0_PORT" (Just "80") $ do
                  let envPath = tempDirectory <> "/.env"
                      envLocalPath = tempDirectory <> "/.env.local"
                  writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_HOST=127.0.0.1\nLISTENER_0_PORT=5443\n"
                  writeFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\nLISTENER_0_PORT=7443\n"
                  loadAppStartupConfigWithFiles envPath envLocalPath
                    `shouldReturn` Right
                      AppStartupConfig
                        { startupEnvironmentConfig =
                            defaultAppEnvironmentConfig
                              { appMode = Production,
                                databaseConfig =
                                  DatabaseConfig
                                    { databaseHost = "db.shared",
                                      databasePort = 6432,
                                      databaseName = "web_api_dev",
                                      databaseUser = "web_api_runtime",
                                      databasePassword = "local_password"
                                    }
                              },
                          startupAppConfig =
                            defaultAppConfig
                              { appTitlePrefix = "web-api-runtime",
                                listenerConfigs =
                                  [ ListenerConfig
                                      { listenerHost = "0.0.0.0",
                                        listenerPort = 80,
                                        listenerScheme = Http,
                                        listenerTls = Nothing,
                                        listenerAcme = Nothing
                                      }
                                  ]
                              }
                        }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-startup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let brokenEnvPath = tempDirectory <> "/broken.env"
                envLocalPath = tempDirectory <> "/.env.local"
                invalidEnvPath = tempDirectory <> "/invalid.env"
            writeFile brokenEnvPath "APP_TITLE_PREFIX\n"
            loadAppStartupConfigWithFiles brokenEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "APP_TITLE_PREFIX"))
            writeFile invalidEnvPath "LISTENER_0_PORT=0\n"
            loadAppStartupConfigWithFiles invalidEnvPath envLocalPath
              `shouldReturn` Left
                (AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-startup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            let envPath = tempDirectory <> "/.env"
                envLocalPath = tempDirectory <> "/.env.local"
            writeFile envPath "APP_MODE=production\nDATABASE_HOST=db.shared\nDATABASE_PORT=6432\nAPP_TITLE_PREFIX=web-api-shared\nLISTENER_0_PORT=5443\n"
            withUnreadableFile envLocalPath "DATABASE_PASSWORD=local_password\nAPP_TITLE_PREFIX=web-api-local\n" $ do
              result <- loadAppStartupConfigWithFiles envPath envLocalPath
              result `shouldSatisfy` \case
                Left
                  (AppStartupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                    | failingPath == envLocalPath ->
                        not (Text.null errorMessage)
                _ -> False

  describe "loadAppStartupConfig" $
    it "loads the default .env file names for runtime startup from the current directory" $
      withSystemTempDirectory "app-startup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $ do
            writeFile (tempDirectory <> "/.env") "APP_MODE=production\nAPP_TITLE_PREFIX=web-api-shared\n"
            writeFile (tempDirectory <> "/.env.local") "APP_MODE=test\nLISTENER_0_PORT=6001\n"
            withCurrentDirectory tempDirectory $
              loadAppStartupConfig
                `shouldReturn` Right
                  defaultAppStartupConfig
                    { startupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { appMode = Test
                          },
                      startupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-shared",
                            listenerConfigs =
                              [ ListenerConfig
                                  { listenerHost = "127.0.0.1",
                                    listenerPort = 6001,
                                    listenerScheme = Http,
                                    listenerTls = Nothing,
                                    listenerAcme = Nothing
                                  }
                              ]
                          }
                    }

  describe "AppStartupConfig and AppStartupConfigLoadError" $
    it "keep equality and rendering deterministic" $ do
      let startupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                startupAppConfig = defaultAppConfig {appTitlePrefix = "web-api-test"}
              }
          differentStartupConfig =
            AppStartupConfig
              { startupEnvironmentConfig = defaultAppEnvironmentConfig,
                startupAppConfig = defaultAppConfig
              }
          fileLoadError = AppStartupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppStartupConfigParseError (InvalidConfigValue "LISTENER_0_PORT" "0")
      startupConfig `shouldBe` startupConfig
      startupConfig `shouldNotBe` differentStartupConfig
      show startupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [startupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppStartupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")]"

  describe "parseAppSetupConfig" $ do
    it "parses committed runtime and setup defaults into the expected setup config" $ do
      committedSetupDefaults
        `shouldBe` [ ("SETUP_AUTOSTART_DATABASE", "true"),
                     ("SETUP_AUTOSTART_JAEGER", "false")
                   ]
      defaultSetupAutostartConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      defaultAppSetupConfig
        `shouldBe` AppSetupConfig
          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
            setupAppConfig = defaultAppConfig,
            setupMigrationDatabaseConfig = Nothing,
            setupAutostartConfig = defaultSetupAutostartConfig
          }
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig
      parseAppSetupConfig (committedEnvDefaults <> committedRuntimeDefaults) [] []
        `shouldBe` Right defaultAppSetupConfig

    it "lets setup booleans follow the same layered precedence as runtime config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        [ ("APP_TITLE_PREFIX", "setup-local"),
          ("SETUP_AUTOSTART_DATABASE", "yes")
        ]
        [("SETUP_AUTOSTART_JAEGER", "1")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
                defaultAppConfig
                  { appTitlePrefix = "setup-local"
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig =
                SetupAutostartConfig
                  { setupAutostartDatabase = True,
                    setupAutostartJaeger = True
                  }
            }

    it "lets OTLP_TRACING_ENABLED use the default local endpoint while still flowing into setup config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [ ("OTLP_TRACING_ENABLED", "true"),
          ("OTLP_TRACING_HEADERS", "authorization=Bearer token")
        ]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig = defaultAppEnvironmentConfig,
              setupAppConfig =
                defaultAppConfig
                  { observability =
                      ObservabilityConfig
                        { tracingExporter =
                            Just
                              OtlpExporter
                                { otlpEndpoint = "http://127.0.0.1:4318/v1/traces",
                                  otlpHeaders = [("authorization", "Bearer token")]
                                },
                          metricsExporter = Nothing
                        }
                  },
              setupMigrationDatabaseConfig = Nothing,
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "parses optional migration-owner credentials separately from the runtime database config" $
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        [ ("DATABASE_USER", "web_api_runtime"),
          ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        [("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")]
        `shouldBe` Right
          AppSetupConfig
            { setupEnvironmentConfig =
                defaultAppEnvironmentConfig
                  { databaseConfig =
                      DatabaseConfig
                        { databaseHost = "127.0.0.1",
                          databasePort = 5432,
                          databaseName = "web_api_dev",
                          databaseUser = "web_api_runtime",
                          databasePassword = "web_api"
                        }
                  },
              setupAppConfig = defaultAppConfig,
              setupMigrationDatabaseConfig =
                Just
                  DatabaseConfig
                    { databaseHost = "127.0.0.1",
                      databasePort = 5432,
                      databaseName = "web_api_dev",
                      databaseUser = "web_api_owner",
                      databasePassword = "owner-secret"
                    },
              setupAutostartConfig = defaultSetupAutostartConfig
            }

    it "fails invalid runtime, setup, or partial migration config values explicitly" $ do
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("LISTENER_0_PORT", "0")]
        `shouldBe` Left (InvalidConfigValue "LISTENER_0_PORT" "0")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("SETUP_AUTOSTART_DATABASE", "sometimes")]
        `shouldBe` Left (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "sometimes")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1")]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PORT")
      parseAppSetupConfig
        (committedEnvDefaults <> committedRuntimeDefaults <> committedSetupDefaults)
        []
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadAppSetupConfigWithFiles" $ do
    it "loads the documented .env then .env.local layers for setup config" $
      withSystemTempDirectory "app-setup-config" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              writeFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                      setupAppConfig =
                        defaultAppConfig
                          { appTitlePrefix = "web-api-local"
                          },
                      setupMigrationDatabaseConfig = Nothing,
                      setupAutostartConfig =
                        SetupAutostartConfig
                          { setupAutostartDatabase = True,
                            setupAutostartJaeger = True
                          }
                    }

    it "lets process environment override .env.local values for setup config" $
      withSystemTempDirectory "app-setup-config-env" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $
              withTemporaryEnvironment "APP_TITLE_PREFIX" (Just "web-api-runtime") $
                withTemporaryEnvironment "SETUP_AUTOSTART_DATABASE" (Just "false") $
                  withTemporaryEnvironment "SETUP_AUTOSTART_JAEGER" (Just "true") $ do
                    let envPath = tempDirectory <> "/.env"
                        envLocalPath = tempDirectory <> "/.env.local"
                    writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
                    writeFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=no\n"
                    loadAppSetupConfigWithFiles envPath envLocalPath
                      `shouldReturn` Right
                        AppSetupConfig
                          { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                            setupAppConfig =
                              defaultAppConfig
                                { appTitlePrefix = "web-api-runtime"
                                },
                            setupMigrationDatabaseConfig = Nothing,
                            setupAutostartConfig =
                              SetupAutostartConfig
                                { setupAutostartDatabase = False,
                                  setupAutostartJaeger = True
                                }
                          }

    it "loads optional migration-owner credentials from the same file layers without replacing runtime credentials" $
      withSystemTempDirectory "app-setup-config-migration" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile
                envPath
                ( unlines
                    [ "DATABASE_USER=web_api_runtime",
                      "WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1",
                      "WEB_API_MIGRATION_DATABASE_PORT=5432",
                      "WEB_API_MIGRATION_DATABASE_NAME=web_api_dev",
                      "WEB_API_MIGRATION_DATABASE_USER=web_api_owner"
                    ]
                )
              writeFile envLocalPath "WEB_API_MIGRATION_DATABASE_PASSWORD=owner-secret\n"
              loadAppSetupConfigWithFiles envPath envLocalPath
                `shouldReturn` Right
                  AppSetupConfig
                    { setupEnvironmentConfig =
                        defaultAppEnvironmentConfig
                          { databaseConfig =
                              DatabaseConfig
                                { databaseHost = "127.0.0.1",
                                  databasePort = 5432,
                                  databaseName = "web_api_dev",
                                  databaseUser = "web_api_runtime",
                                  databasePassword = "web_api"
                                }
                          },
                      setupAppConfig = defaultAppConfig,
                      setupMigrationDatabaseConfig =
                        Just
                          DatabaseConfig
                            { databaseHost = "127.0.0.1",
                              databasePort = 5432,
                              databaseName = "web_api_dev",
                              databaseUser = "web_api_owner",
                              databasePassword = "owner-secret"
                            },
                      setupAutostartConfig = defaultSetupAutostartConfig
                    }

    it "reports invalid override files or parse failures with explicit errors" $
      withSystemTempDirectory "app-setup-config-errors" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let brokenEnvPath = tempDirectory <> "/broken.env"
                  envLocalPath = tempDirectory <> "/.env.local"
                  invalidEnvPath = tempDirectory <> "/invalid.env"
              writeFile brokenEnvPath "SETUP_AUTOSTART_DATABASE\n"
              loadAppSetupConfigWithFiles brokenEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupOverridesFileError brokenEnvPath (InvalidConfigOverridesLine 1 "SETUP_AUTOSTART_DATABASE"))
              writeFile invalidEnvPath "SETUP_AUTOSTART_JAEGER=maybe\n"
              loadAppSetupConfigWithFiles invalidEnvPath envLocalPath
                `shouldReturn` Left
                  (AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_JAEGER" "maybe"))

    it "reports unreadable override files with the failing path" $
      withSystemTempDirectory "app-setup-config-unreadable" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              let envPath = tempDirectory <> "/.env"
                  envLocalPath = tempDirectory <> "/.env.local"
              writeFile envPath "APP_TITLE_PREFIX=web-api-shared\nSETUP_AUTOSTART_DATABASE=true\n"
              withUnreadableFile envLocalPath "APP_TITLE_PREFIX=web-api-local\nSETUP_AUTOSTART_JAEGER=yes\n" $ do
                result <- loadAppSetupConfigWithFiles envPath envLocalPath
                result `shouldSatisfy` \case
                  Left
                    (AppSetupOverridesFileError failingPath (UnreadableConfigOverridesFile errorMessage))
                      | failingPath == envLocalPath ->
                          not (Text.null errorMessage)
                  _ -> False

  describe "loadAppSetupConfig" $
    it "loads the default .env file names for setup config from the current directory" $
      withSystemTempDirectory "app-setup-config-current-directory" $ \tempDirectory ->
        withClearedAppEnvironment $
          withClearedRuntimeEnvironment $
            withClearedSetupEnvironment $ do
              writeFile (tempDirectory <> "/.env") "SETUP_AUTOSTART_DATABASE=true\n"
              writeFile (tempDirectory <> "/.env.local") "APP_TITLE_PREFIX=web-api-dev\nSETUP_AUTOSTART_JAEGER=true\n"
              withCurrentDirectory tempDirectory $
                loadAppSetupConfig
                  `shouldReturn` Right
                    AppSetupConfig
                      { setupEnvironmentConfig = defaultAppEnvironmentConfig,
                        setupAppConfig =
                          defaultAppConfig
                            { appTitlePrefix = "web-api-dev"
                            },
                        setupMigrationDatabaseConfig = Nothing,
                        setupAutostartConfig =
                          SetupAutostartConfig
                            { setupAutostartDatabase = True,
                              setupAutostartJaeger = True
                            }
                      }

  describe "AppSetupConfig and AppSetupConfigLoadError" $
    it "keep selectors, equality, and rendering deterministic" $ do
      let setupConfig =
            AppSetupConfig
              { setupEnvironmentConfig = defaultAppEnvironmentConfig {appMode = Test},
                setupAppConfig = defaultAppConfig {appTitlePrefix = "setup-app"},
                setupMigrationDatabaseConfig =
                  Just
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret"
                      },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = False
                    }
              }
          fileLoadError = AppSetupOverridesFileError ".env" (InvalidConfigOverridesLine 1 "BROKEN")
          parseLoadError = AppSetupConfigParseError (InvalidConfigValue "SETUP_AUTOSTART_DATABASE" "maybe")
      setupEnvironmentConfig setupConfig `shouldBe` defaultAppEnvironmentConfig {appMode = Test}
      setupAppConfig setupConfig `shouldBe` defaultAppConfig {appTitlePrefix = "setup-app"}
      setupMigrationDatabaseConfig setupConfig
        `shouldBe` Just
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret"
            }
      setupAutostartConfig setupConfig
        `shouldBe` SetupAutostartConfig
          { setupAutostartDatabase = True,
            setupAutostartJaeger = False
          }
      setupAutostartDatabase (setupAutostartConfig setupConfig) `shouldBe` True
      setupAutostartJaeger (setupAutostartConfig setupConfig) `shouldBe` False
      defaultSetupAutostartConfig `shouldBe` defaultSetupAutostartConfig
      defaultSetupAutostartConfig
        `shouldNotBe` SetupAutostartConfig
          { setupAutostartDatabase = False,
            setupAutostartJaeger = False
          }
      show defaultSetupAutostartConfig
        `shouldBe` "SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}"
      showsPrec 11 defaultSetupAutostartConfig ""
        `shouldBe` "(SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False})"
      show [defaultSetupAutostartConfig]
        `shouldBe` "[SetupAutostartConfig {setupAutostartDatabase = True, setupAutostartJaeger = False}]"
      setupConfig `shouldBe` setupConfig
      setupConfig
        `shouldNotBe` setupConfig
          { setupAutostartConfig =
              SetupAutostartConfig
                { setupAutostartDatabase = False,
                  setupAutostartJaeger = False
                }
          }
      show setupConfig
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      showsPrec 11 setupConfig ""
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [setupConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      fileLoadError `shouldBe` fileLoadError
      fileLoadError `shouldNotBe` parseLoadError
      show fileLoadError
        `shouldBe` "AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\")"
      show parseLoadError
        `shouldBe` "AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")"
      show [fileLoadError, parseLoadError]
        `shouldBe` "[AppSetupOverridesFileError \".env\" (InvalidConfigOverridesLine 1 \"BROKEN\"),AppSetupConfigParseError (InvalidConfigValue \"SETUP_AUTOSTART_DATABASE\" \"maybe\")]"

  describe "planAppPrerequisites" $ do
    it "preserves runtime database identity fields in the shared setup prerequisite config" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret"
                          }
                    }
              }
          prerequisiteConfig = toSetupPrerequisiteConfig setupConfig
      PrerequisiteConfig.setupDatabaseEndpoint prerequisiteConfig
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      PrerequisiteConfig.setupDatabaseName prerequisiteConfig `shouldBe` "web_api_build"
      PrerequisiteConfig.setupDatabaseUser prerequisiteConfig `shouldBe` "web_api_runtime"
      PrerequisiteConfig.setupDatabasePassword prerequisiteConfig `shouldBe` "secret"

    it "always plans the configured database reachability check and skips disabled autostarts" $ do
      let setupConfig =
            defaultAppSetupConfig
              { setupEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig =
                        DatabaseConfig
                          { databaseHost = "db.internal",
                            databasePort = 6543,
                            databaseName = "web_api_build",
                            databaseUser = "web_api_runtime",
                            databasePassword = "secret"
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = False
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "db.internal",
                        tcpEndpointPort = 6543
                      },
                  databaseAutostartPlan = Nothing
                },
            tracingPrerequisitePlan = Nothing
          }

    it "still plans tracing reachability when tracing is configured but Jaeger autostart stays disabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = []
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  defaultSetupAutostartConfig
                    { setupAutostartDatabase = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Nothing
                  }
          }

    it "plans podman-then-docker autostart for database and tracing when enabled" $ do
      let tracing =
            OtlpExporter
              { otlpEndpoint = "http://127.0.0.1:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          setupConfig =
            defaultAppSetupConfig
              { setupAppConfig =
                  defaultAppConfig
                    { observability =
                        ObservabilityConfig
                          { tracingExporter = Just tracing,
                            metricsExporter = Nothing
                          }
                    },
                setupAutostartConfig =
                  SetupAutostartConfig
                    { setupAutostartDatabase = True,
                      setupAutostartJaeger = True
                    }
              }
      planAppPrerequisites setupConfig
        `shouldBe` AppPrerequisitePlan
          { databasePrerequisitePlan =
              DatabasePrerequisitePlan
                { databaseCheckEndpoint =
                    TcpEndpoint
                      { tcpEndpointHost = "127.0.0.1",
                        tcpEndpointPort = 5432
                      },
                  databaseAutostartPlan = Just defaultContainerAutostartPlan
                },
            tracingPrerequisitePlan =
              Just
                TracingPrerequisitePlan
                  { tracingCheckEndpoint = "http://127.0.0.1:4318",
                    tracingAutostartPlan = Just defaultContainerAutostartPlan
                  }
          }

    it "keeps planner model selectors, equality, and rendering deterministic" $ do
      let databaseEndpoint =
            TcpEndpoint
              { tcpEndpointHost = "db.internal",
                tcpEndpointPort = 6543
              }
          databasePlan =
            DatabasePrerequisitePlan
              { databaseCheckEndpoint = databaseEndpoint,
                databaseAutostartPlan = Just defaultContainerAutostartPlan
              }
          tracingPlan =
            TracingPrerequisitePlan
              { tracingCheckEndpoint = "http://127.0.0.1:4318",
                tracingAutostartPlan = Nothing
              }
          appPlan =
            AppPrerequisitePlan
              { databasePrerequisitePlan = databasePlan,
                tracingPrerequisitePlan = Just tracingPlan
              }
      PodmanRuntime `shouldBe` PodmanRuntime
      PodmanRuntime `shouldNotBe` DockerRuntime
      show PodmanRuntime `shouldBe` "PodmanRuntime"
      show [PodmanRuntime, DockerRuntime] `shouldBe` "[PodmanRuntime,DockerRuntime]"
      autostartRuntimes defaultContainerAutostartPlan
        `shouldBe` [PodmanRuntime, DockerRuntime]
      defaultContainerAutostartPlan `shouldBe` defaultContainerAutostartPlan
      defaultContainerAutostartPlan
        `shouldNotBe` ContainerAutostartPlan {autostartRuntimes = [DockerRuntime]}
      show defaultContainerAutostartPlan
        `shouldBe` "ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}"
      databaseEndpoint `shouldBe` databaseEndpoint
      databaseEndpoint
        `shouldNotBe` TcpEndpoint
          { tcpEndpointHost = "db.other",
            tcpEndpointPort = 6543
          }
      show databaseEndpoint
        `shouldBe` "TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}"
      showsPrec 11 databaseEndpoint ""
        `shouldBe` "(TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543})"
      show [databaseEndpoint]
        `shouldBe` "[TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}]"
      databaseCheckEndpoint databasePlan
        `shouldBe` TcpEndpoint
          { tcpEndpointHost = "db.internal",
            tcpEndpointPort = 6543
          }
      databasePlan `shouldBe` databasePlan
      databasePlan
        `shouldNotBe` databasePlan
          { databaseAutostartPlan = Nothing
          }
      show databasePlan
        `shouldBe` "DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}"
      showsPrec 11 databasePlan ""
        `shouldBe` "(DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})})"
      databaseAutostartPlan databasePlan `shouldBe` Just defaultContainerAutostartPlan
      tracingPlan `shouldBe` tracingPlan
      tracingPlan
        `shouldNotBe` tracingPlan
          { tracingCheckEndpoint = "http://127.0.0.1:9999"
          }
      tracingCheckEndpoint tracingPlan `shouldBe` "http://127.0.0.1:4318"
      tracingAutostartPlan tracingPlan `shouldBe` Nothing
      show tracingPlan
        `shouldBe` "TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}"
      showsPrec 11 tracingPlan ""
        `shouldBe` "(TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})"
      databasePrerequisitePlan appPlan `shouldBe` databasePlan
      tracingPrerequisitePlan appPlan `shouldBe` Just tracingPlan
      appPlan `shouldBe` appPlan
      appPlan
        `shouldNotBe` appPlan
          { tracingPrerequisitePlan = Nothing
          }
      show appPlan
        `shouldBe` "AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}"
      showsPrec 11 appPlan ""
        `shouldBe` "(AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})})"
      show [defaultContainerAutostartPlan]
        `shouldBe` "[ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]}]"
      show [databasePlan]
        `shouldBe` "[DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}]"
      show [tracingPlan]
        `shouldBe` "[TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing}]"
      show [appPlan]
        `shouldBe` "[AppPrerequisitePlan {databasePrerequisitePlan = DatabasePrerequisitePlan {databaseCheckEndpoint = TcpEndpoint {tcpEndpointHost = \"db.internal\", tcpEndpointPort = 6543}, databaseAutostartPlan = Just (ContainerAutostartPlan {autostartRuntimes = [PodmanRuntime,DockerRuntime]})}, tracingPrerequisitePlan = Just (TracingPrerequisitePlan {tracingCheckEndpoint = \"http://127.0.0.1:4318\", tracingAutostartPlan = Nothing})}]"

  describe "parseTracingEndpoint" $ do
    it "parses supported tracing URLs into TCP endpoints" $ do
      parseTracingEndpoint "http://collector:4318/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector",
              tcpEndpointPort = 4318
            }
      parseTracingEndpoint "https://collector.example/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector.example",
              tcpEndpointPort = 443
            }
      parseTracingEndpoint "http://[::1]:4318/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "::1",
              tcpEndpointPort = 4318
            }
      parseTracingEndpoint "https://[::1]/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "::1",
              tcpEndpointPort = 443
            }
      parseTracingEndpoint "https://collector/v1/traces"
        `shouldBe` Right
          TcpEndpoint
            { tcpEndpointHost = "collector",
              tcpEndpointPort = 443
            }

    it "rejects malformed or unsupported tracing endpoints explicitly" $ do
      parseTracingEndpoint "://collector:4318/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "://collector:4318/v1/traces")
      parseTracingEndpoint "collector:4318/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "collector:4318/v1/traces")
      parseTracingEndpoint "grpc://collector:4317"
        `shouldBe` Left (UnsupportedTracingEndpointScheme "grpc")
      parseTracingEndpoint "http:///v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://:4318/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://collector:not-a-port/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointPort "not-a-port")
      parseTracingEndpoint "http://collector:0/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointPort "0")
      parseTracingEndpoint "http://[::1/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://[]:4318/v1/traces"
        `shouldBe` Left MissingTracingEndpointHost
      parseTracingEndpoint "http://[::1]suffix/v1/traces"
        `shouldBe` Left (InvalidTracingEndpointFormat "suffix")

    it "keeps parse error equality and rendering deterministic" $ do
      let parseError = InvalidTracingEndpointPort "not-a-port"
      parseError `shouldBe` InvalidTracingEndpointPort "not-a-port"
      parseError `shouldNotBe` MissingTracingEndpointHost
      show parseError `shouldBe` "InvalidTracingEndpointPort \"not-a-port\""
      show [parseError] `shouldBe` "[InvalidTracingEndpointPort \"not-a-port\"]"

  describe "checkTcpEndpointReachable" $ do
    it "reports True for a reachable local TCP listener" $
      withListeningTcpEndpoint $ \tcpEndpoint ->
        checkTcpEndpointReachable tcpEndpoint
          `shouldReturn` True

    it "reports False once the TCP listener is gone" $ do
      closedEndpoint <- withListeningTcpEndpoint pure
      checkTcpEndpointReachable closedEndpoint
        `shouldReturn` False

    it "reports False for invalid resolver inputs or immediate timeout cutoffs" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        checkTcpEndpointReachableWithTimeout
          1000000
          TcpEndpoint
            { tcpEndpointHost = tcpEndpointHost tcpEndpoint,
              tcpEndpointPort = -1
            }
          `shouldReturn` False
        checkTcpEndpointReachableWithTimeout 0 tcpEndpoint
          `shouldReturn` False

  describe "checkTracingEndpointReachable" $ do
    it "checks supported tracing endpoints by their parsed TCP host and port" $
      withListeningTcpEndpoint $ \tcpEndpoint -> do
        let endpoint =
              "http://"
                <> tcpEndpointHost tcpEndpoint
                <> ":"
                <> Text.pack (show (tcpEndpointPort tcpEndpoint))
                <> "/v1/traces"
        checkTracingEndpointReachable endpoint
          `shouldReturn` Right True

    it "returns parse errors instead of silently treating malformed tracing endpoints as unreachable" $
      checkTracingEndpointReachable "collector:4318/v1/traces"
        `shouldReturn` Left (InvalidTracingEndpointFormat "collector:4318/v1/traces")

  describe "parseDatabaseSetupCommand" $ do
    it "accepts migrate, seed, and migrate-and-seed" $ do
      parseDatabaseSetupCommand ["migrate"] `shouldBe` Right MigrateDatabase
      parseDatabaseSetupCommand ["seed"] `shouldBe` Right SeedDatabase
      parseDatabaseSetupCommand ["migrate-and-seed"] `shouldBe` Right MigrateAndSeedDatabase

    it "rejects unsupported command lines with explicit guidance" $ do
      parseDatabaseSetupCommand ["deploy"]
        `shouldBe` Left (InvalidDatabaseSetupCommand ["deploy"])
      renderDatabaseSetupError (InvalidDatabaseSetupCommand ["deploy"])
        `shouldBe` "Unsupported database setup command: deploy\nExpected one of: migrate, seed, migrate-and-seed"

    it "keeps command and error values stable" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          configSetupError = DatabaseSetupConfigLoadError loadError
          runtimeConfigSetupError = DatabaseSetupRuntimeConfigLoadError runtimeLoadError
          migrationSetupError = DatabaseSetupMigrationError (UnexpectedQueryRows "expected exactly one row" ["first", "second"])
          seedSetupError = DatabaseSetupSeedError (UnexpectedQueryRows "expected exactly one row" ["seed"])
      MigrateDatabase `shouldBe` MigrateDatabase
      MigrateDatabase `shouldNotBe` SeedDatabase
      show MigrateDatabase `shouldBe` "MigrateDatabase"
      show SeedDatabase `shouldBe` "SeedDatabase"
      show MigrateAndSeedDatabase `shouldBe` "MigrateAndSeedDatabase"
      show [MigrateDatabase, SeedDatabase, MigrateAndSeedDatabase]
        `shouldBe` "[MigrateDatabase,SeedDatabase,MigrateAndSeedDatabase]"
      configSetupError `shouldBe` configSetupError
      configSetupError `shouldNotBe` migrationSetupError
      runtimeConfigSetupError `shouldBe` runtimeConfigSetupError
      runtimeConfigSetupError `shouldNotBe` configSetupError
      seedSetupError `shouldBe` seedSetupError
      show configSetupError
        `shouldBe` "DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")"
      show runtimeConfigSetupError
        `shouldBe` "DatabaseSetupRuntimeConfigLoadError (MissingConfigValue \"DATABASE_PASSWORD\")"
      show migrationSetupError
        `shouldBe` "DatabaseSetupMigrationError (UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"])"
      show seedSetupError
        `shouldBe` "DatabaseSetupSeedError (UnexpectedQueryRows \"expected exactly one row\" [\"seed\"])"
      show [configSetupError]
        `shouldBe` "[DatabaseSetupConfigLoadError (InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\")]"

    it "renders load, migration, and seed failures explicitly" $ do
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          runtimeLoadError = MissingConfigValue "DATABASE_PASSWORD"
          migrationRunnerError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
          seedRunnerError = UnexpectedQueryRows "expected exactly one row" ["seed"]
      renderDatabaseSetupError (DatabaseSetupConfigLoadError loadError)
        `shouldBe` "Failed to load database setup config: InvalidConfigValue \"WEB_API_MIGRATION_DATABASE_PORT\" \"0\""
      renderDatabaseSetupError (DatabaseSetupRuntimeConfigLoadError runtimeLoadError)
        `shouldBe` "Failed to load runtime database config: MissingConfigValue \"DATABASE_PASSWORD\""
      renderDatabaseSetupError (DatabaseSetupMigrationError migrationRunnerError)
        `shouldBe` "Failed to apply database migrations: UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"]"
      renderDatabaseSetupError (DatabaseSetupSeedError seedRunnerError)
        `shouldBe` "Failed to apply database seed data: UnexpectedQueryRows \"expected exactly one row\" [\"seed\"]"

  describe "parseDatabaseSetupConfig" $ do
    it "reads owner-level migration credentials from dedicated environment variables" $
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Right
          DatabaseConfig
            { databaseHost = "127.0.0.1",
              databasePort = 5432,
              databaseName = "web_api_dev",
              databaseUser = "web_api_owner",
              databasePassword = "owner-secret"
            }

    it "fails missing or invalid migration environment values explicitly" $ do
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "5432"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner")
        ]
        `shouldBe` Left (MissingConfigValue "WEB_API_MIGRATION_DATABASE_PASSWORD")
      parseDatabaseSetupConfig
        [ ("WEB_API_MIGRATION_DATABASE_HOST", "127.0.0.1"),
          ("WEB_API_MIGRATION_DATABASE_PORT", "0"),
          ("WEB_API_MIGRATION_DATABASE_NAME", "web_api_dev"),
          ("WEB_API_MIGRATION_DATABASE_USER", "web_api_owner"),
          ("WEB_API_MIGRATION_DATABASE_PASSWORD", "owner-secret")
        ]
        `shouldBe` Left (InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0")

  describe "loadDatabaseSetupConfig" $
    it "reads dedicated migration credentials from the process environment" $
      withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1") $
        withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432") $
          withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev") $
            withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner") $
              withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret") $
                loadDatabaseSetupConfig
                  `shouldReturn` Right
                    DatabaseConfig
                      { databaseHost = "127.0.0.1",
                        databasePort = 5432,
                        databaseName = "web_api_dev",
                        databaseUser = "web_api_owner",
                        databasePassword = "owner-secret"
                      }

  describe "runDatabaseSetupCommand"
    $ it "uses the default migration environment loader and postgres runners for single-command setup"
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
    $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
    $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
    $ withFakePsqlScript
      (fmap (,Text.empty) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig <> seedStatements))
    $ \argsLogPath -> do
      runDatabaseSetupCommand MigrateDatabase `shouldReturn` Right ()
      runDatabaseSetupCommand SeedDatabase `shouldReturn` Right ()
      let renderMutationLogEntry databaseConfig sql =
            "--host "
              <> Text.unpack (databaseHost databaseConfig)
              <> " --port "
              <> show (databasePort databaseConfig)
              <> " --dbname "
              <> Text.unpack (databaseName databaseConfig)
              <> " --username "
              <> Text.unpack (databaseUser databaseConfig)
              <> " --no-password --set ON_ERROR_STOP=1 --command "
              <> Text.unpack sql
      readFile argsLogPath
        `shouldReturn` unlines
          ( fmap (renderMutationLogEntry setupMigrationPostgresTestConfig) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig)
              <> fmap (renderMutationLogEntry setupMigrationPostgresTestConfig) seedStatements
          )

  describe "runDatabaseSetupCommandWith" $ do
    it "returns configuration load errors before running any commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
          unexpectedRuntimeLoader =
            modifyIORef' recordedStepsReference (<> ["runtime-loader"])
              >> pure (Right postgresTestConfig)
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["runner"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Left loadError))
        unexpectedRuntimeLoader
        unexpectedMigrationRunner
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "returns runtime configuration load errors before running database commands" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let loadError = MissingConfigValue "DATABASE_PASSWORD"
          unexpectedMigrationRunner _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Right ())
          unexpectedSeedRunner _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Left loadError))
        unexpectedMigrationRunner
        unexpectedSeedRunner
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupRuntimeConfigLoadError loadError)
      readIORef recordedStepsReference `shouldReturn` []

    it "runs migrations and seed data in order with the loaded database config" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let recordMigrationStep migrationDatabaseConfig runtimeDatabaseConfig =
            modifyIORef'
              recordedStepsReference
              (<> ["migrate:" <> databaseUser migrationDatabaseConfig <> "->" <> databaseUser runtimeDatabaseConfig <> ":" <> databaseName runtimeDatabaseConfig])
              >> pure (Right ())
          recordSeedStep databaseRuntimeConfig =
            modifyIORef' recordedStepsReference (<> ["seed:" <> databaseUser databaseRuntimeConfig <> ":" <> databaseName databaseRuntimeConfig])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        recordMigrationStep
        recordSeedStep
        MigrateAndSeedDatabase
        `shouldReturn` Right ()
      readIORef recordedStepsReference
        `shouldReturn` ["migrate:web_api_owner->web_api_app:web_api_prod", "seed:web_api_owner:web_api_prod"]

    it "maps single-command migration failures explicitly" $ do
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Left migrationError))
        (\_ -> pure (Right ()))
        MigrateDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)

    it "maps single-command seed failures explicitly" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        SeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

    it "stops after the first migration failure and preserves the runner error" $ do
      recordedStepsReference <- newIORef ([] :: [Text])
      let migrationError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken"], postgresEnvironment = []})
              (failingPostgresResult "migration failed")
          failingMigrations _ _ =
            modifyIORef' recordedStepsReference (<> ["migrate"])
              >> pure (Left migrationError)
          unexpectedSeed _ =
            modifyIORef' recordedStepsReference (<> ["seed"])
              >> pure (Right ())
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        failingMigrations
        unexpectedSeed
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupMigrationError migrationError)
      readIORef recordedStepsReference `shouldReturn` ["migrate"]

    it "maps migrate-and-seed seed failures explicitly after successful migrations" $ do
      let seedError =
            PostgresCommandFailed
              (PostgresCommand {postgresExecutable = "psql", postgresArguments = ["--command", "broken-seed"], postgresEnvironment = []})
              (failingPostgresResult "seed failed")
      runDatabaseSetupCommandWith
        (pure (Right migrationPostgresTestConfig))
        (pure (Right postgresTestConfig))
        (\_ _ -> pure (Right ()))
        (\_ -> pure (Left seedError))
        MigrateAndSeedDatabase
        `shouldReturn` Left (DatabaseSetupSeedError seedError)

  describe "runDatabaseSetupArgsWith" $ do
    it "prints a success message for completed setup commands" $
      withSystemTempFile "database-setup-stdout.txt" $ \outputPath outputHandle -> do
        runDatabaseSetupArgsWith
          (pure (Right migrationPostgresTestConfig))
          (pure (Right postgresTestConfig))
          (\_ _ -> pure (Right ()))
          (\_ -> pure (Right ()))
          outputHandle
          ["seed"]
        hClose outputHandle
        readFile outputPath `shouldReturn` "Applied database seed data.\n"

    it "throws an explicit user error for unsupported command lines" $
      withSystemTempFile "database-setup-invalid-stdout.txt" $ \_ outputHandle -> do
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Right migrationPostgresTestConfig))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["deploy"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Unsupported database setup command: deploy"
          Right () ->
            expectationFailure "expected invalid database setup command to raise an exception"

    it "throws an explicit user error when setup returns a failure" $
      withSystemTempFile "database-setup-error-stdout.txt" $ \_ outputHandle -> do
        let loadError = InvalidConfigValue "WEB_API_MIGRATION_DATABASE_PORT" "0"
        result <-
          try
            ( runDatabaseSetupArgsWith
                (pure (Left loadError))
                (pure (Right postgresTestConfig))
                (\_ _ -> pure (Right ()))
                (\_ -> pure (Right ()))
                outputHandle
                ["migrate"]
            ) ::
            IO (Either IOException ())
        hClose outputHandle
        case result of
          Left exception ->
            displayException exception
              `shouldContain` "Failed to load database setup config"
          Right () ->
            expectationFailure "expected database setup failure to raise an exception"

  describe "runDatabaseSetupArgs"
    $ it "uses the default migration environment loader and postgres runners for migrate and migrate-and-seed output"
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_USER" (Just "web_api_owner")
    $ withTemporaryEnvironment "WEB_API_MIGRATION_DATABASE_PASSWORD" (Just "owner-secret")
    $ withTemporaryEnvironment "DATABASE_HOST" (Just "127.0.0.1")
    $ withTemporaryEnvironment "DATABASE_PORT" (Just "5432")
    $ withTemporaryEnvironment "DATABASE_NAME" (Just "web_api_dev")
    $ withTemporaryEnvironment "DATABASE_USER" (Just "web_api_runtime")
    $ withTemporaryEnvironment "DATABASE_PASSWORD" (Just "runtime-secret")
    $ withFakePsqlScript
      (fmap (,Text.empty) (migrationStatementsFor setupMigrationPostgresTestConfig runtimeSetupPostgresTestConfig <> seedStatements))
    $ \_ ->
      withSystemTempFile "database-setup-args-migrate.txt" $ \migrateOutputPath migrateOutputHandle -> do
        runDatabaseSetupArgs migrateOutputHandle ["migrate"]
        hClose migrateOutputHandle
        readFile migrateOutputPath `shouldReturn` "Applied database migrations.\n"
        withSystemTempFile "database-setup-args-migrate-and-seed.txt" $ \migrateAndSeedOutputPath migrateAndSeedOutputHandle -> do
          runDatabaseSetupArgs migrateAndSeedOutputHandle ["migrate-and-seed"]
          hClose migrateAndSeedOutputHandle
          readFile migrateAndSeedOutputPath `shouldReturn` "Applied database migrations and seed data.\n"

  describe "config model values" $ do
    it "can represent manual, shared, and certbot-backed ACME certificates plus exporter endpoints" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          tlsSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("x-api-key", "secret")]
              }
      TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
        `shouldBe` TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = "cert.pem", privateKeyFile = "key.pem"}}
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show tlsSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})"
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
              { acmeDirectoryUrl = "https://acme-staging-v02.api.letsencrypt.org/directory",
                acmeContactEmails = ["ops@example.com", "alerts@example.com"],
                acmeDomains = ["example.com", "www.example.com"],
                acmeHttp01Port = 80,
                acmeCertificateDirectory = Nothing,
                acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = []
                    }
              }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          tlsConfig = TlsConfig {certificateSource = manualCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just "req-456",
                requestSurface = PageSurface,
                requestPathPrefix = "",
                requestQueryParameters = [],
                requestSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/es"
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction =
                  CallToAction
                    { callToActionLabel = "Browse the second page",
                      callToActionRoute = SecondRoute,
                      callToActionHref = "/es/second"
                    }
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Progressive enhancement"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
      case manualCertificateSource of
        source@ManualCertificateFiles {} -> do
          certificateFile source `shouldBe` "cert.pem"
          privateKeyFile source `shouldBe` "key.pem"
        AcmeCertificateSource _ -> expectationFailure "expected manual certificate files"
        SharedCertificateFiles {} -> expectationFailure "expected manual certificate files"
      acmeDirectoryUrl inProcessAcmeConfig `shouldBe` "https://acme-staging-v02.api.letsencrypt.org/directory"
      acmeContactEmails inProcessAcmeConfig `shouldBe` ["ops@example.com", "alerts@example.com"]
      acmeDomains inProcessAcmeConfig `shouldBe` ["example.com", "www.example.com"]
      acmeHttp01Port inProcessAcmeConfig `shouldBe` 80
      acmeCertificateDirectory inProcessAcmeConfig `shouldBe` Nothing
      acmeCertbotConfig inProcessAcmeConfig
        `shouldBe` CertbotConfig
          { certbotExecutable = "certbot",
            certbotArguments = []
          }
      case sharedCertificateSource of
        SharedCertificateFiles {certificateDirectory = sharedDirectory, sharedCertificateStartupMode = startupMode} -> do
          sharedDirectory `shouldBe` "/var/lib/web-api/shared-certs"
          startupMode `shouldBe` AwaitCertificateFiles Nothing
        _ ->
          expectationFailure "expected shared certificate files"
      certificateSource tlsConfig `shouldBe` manualCertificateSource
      listenerHost listenerConfig `shouldBe` "0.0.0.0"
      listenerPort listenerConfig `shouldBe` 5443
      listenerScheme listenerConfig `shouldBe` Https
      listenerTls listenerConfig `shouldBe` Just tlsConfig
      staticUrlPrefix staticRoot `shouldBe` "/assets"
      staticDirectory staticRoot `shouldBe` "public"
      staticAssetRoots staticConfig `shouldBe` [staticRoot]
      staticCacheControlSeconds staticConfig `shouldBe` Just 3600
      otlpEndpoint exporter `shouldBe` "http://otel-collector:4318"
      otlpHeaders exporter `shouldBe` [("authorization", "Bearer token")]
      tracingExporter observabilityConfig `shouldBe` Just exporter
      metricsExporter observabilityConfig `shouldBe` Just exporter
      appTitlePrefix appConfig `shouldBe` "test-app"
      listenerConfigs appConfig `shouldBe` [listenerConfig]
      staticAssets appConfig `shouldBe` staticConfig
      observability appConfig `shouldBe` observabilityConfig
      requestLocale requestContext `shouldBe` Spanish
      requestCorrelationId requestContext `shouldBe` Just "req-456"
      callToActionLabel callToAction `shouldBe` "Return home"
      callToActionRoute callToAction `shouldBe` HomeRoute
      callToActionHref callToAction `shouldBe` "/es"
      homeHeading homePageModel `shouldBe` "Home"
      homeSummary homePageModel `shouldBe` "Server-rendered home page with stubbed content."
      homePrimaryAction homePageModel
        `shouldBe` CallToAction
          { callToActionLabel = "Browse the second page",
            callToActionRoute = SecondRoute,
            callToActionHref = "/es/second"
          }
      secondHeading secondPageModel `shouldBe` "Second"
      secondSummary secondPageModel `shouldBe` "Second page content with stubbed data ready for future loaders."
      secondHighlights secondPageModel `shouldBe` ["Fast SSR", "Progressive enhancement"]
      secondPrimaryAction secondPageModel `shouldBe` callToAction
      notFoundHeading notFoundPageModel `shouldBe` "Not Found"
      notFoundSummary notFoundPageModel `shouldBe` "The requested page could not be found."
      notFoundPrimaryAction notFoundPageModel `shouldBe` callToAction

    it "directly exercises the remaining derived eq and show instances" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource =
            AcmeCertificateSource
              AcmeConfig
                { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
                  acmeContactEmails = ["ops@example.com"],
                  acmeDomains = ["example.com", "www.example.com"],
                  acmeHttp01Port = 80,
                  acmeCertificateDirectory = Nothing,
                  acmeCertbotConfig = certbotConfig
                }
          sharedCertificateSource =
            SharedCertificateFiles
              { certificateDirectory = "/var/lib/web-api/shared-certs",
                sharedCertificateStartupMode = AwaitCertificateFiles Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction =
                  CallToAction
                    { callToActionLabel = "Browse the second page",
                      callToActionRoute = SecondRoute,
                      callToActionHref = "/second"
                    }
              }
      Http `shouldBe` Http
      Https `shouldBe` Https
      certbotConfig `shouldBe` certbotConfig
      certbotConfig `shouldBe` certbotConfig
      TlsConfig {certificateSource = manualCertificateSource}
        `shouldBe` TlsConfig {certificateSource = manualCertificateSource}
      sharedCertificateSource `shouldBe` sharedCertificateSource
      acmeCertificateSource `shouldBe` acmeCertificateSource
      staticRoot `shouldBe` staticRoot
      English `shouldBe` English
      Spanish `shouldBe` Spanish
      PageSurface `shouldBe` PageSurface
      ApiSurface `shouldBe` ApiSurface
      HomeRoute `shouldBe` HomeRoute
      SecondRoute `shouldBe` SecondRoute
      StatusApiRoute `shouldBe` StatusApiRoute
      NotFoundRoute `shouldBe` NotFoundRoute
      UnsupportedLocalePrefix "de" `shouldBe` UnsupportedLocalePrefix "de"
      UnsupportedPath "/missing" `shouldBe` UnsupportedPath "/missing"
      HomePage homePageModel `shouldBe` HomePage homePageModel
      SecondPage secondPageModel `shouldBe` SecondPage secondPageModel
      NotFoundPage notFoundPageModel `shouldBe` NotFoundPage notFoundPageModel
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show certbotConfig
        `shouldBe` "CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}"
      show
        AcmeConfig
          { acmeDirectoryUrl = "https://acme-v02.api.letsencrypt.org/directory",
            acmeContactEmails = ["ops@example.com"],
            acmeDomains = ["example.com", "www.example.com"],
            acmeHttp01Port = 80,
            acmeCertificateDirectory = Nothing,
            acmeCertbotConfig = certbotConfig
          }
        `shouldBe` "AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}"
      show acmeCertificateSource
        `shouldBe` "AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})"
      show (TlsConfig {certificateSource = manualCertificateSource})
        `shouldBe` "TlsConfig {certificateSource = ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}}"
      show sharedCertificateSource
        `shouldBe` "SharedCertificateFiles {certificateDirectory = \"/var/lib/web-api/shared-certs\", sharedCertificateStartupMode = AwaitCertificateFiles {certificateWaitTimeoutSeconds = Nothing}}"
      show manualCertificateSource
        `shouldBe` "ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"}"
      show (ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing})
        `shouldBe` "ListenerConfig {listenerHost = \"127.0.0.1\", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing}"
      show staticRoot `shouldBe` "StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}"
      show
        ( StaticAssetsConfig
            { staticAssetRoots = [staticRoot],
              staticAssetContentTypes = defaultStaticAssetContentTypes,
              staticCacheControlSeconds = Just 3600
            }
        )
        `shouldBe` ( "StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}"
                   )
      show
        ( ObservabilityConfig
            { tracingExporter =
                Just
                  OtlpExporter
                    { otlpEndpoint = "http://otel-collector:4318",
                      otlpHeaders = [("x-api-key", "secret")]
                    },
              metricsExporter = Nothing
            }
        )
        `shouldBe` "ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"x-api-key\",\"secret\")]}), metricsExporter = Nothing}"
      show
        ( AppRequestContext
            { requestLocale = Spanish,
              requestLocaleIsExplicit = False,
              requestCorrelationId = Just "req-789",
              requestSurface = PageSurface,
              requestPathPrefix = "",
              requestQueryParameters = [],
              requestSessionId = Nothing
            }
        )
        `shouldBe` "AppRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = False, requestCorrelationId = Just \"req-789\", requestSurface = PageSurface, requestPathPrefix = \"\", requestQueryParameters = [], requestSessionId = Nothing}"
      show
        ( CallToAction
            { callToActionLabel = "Return home",
              callToActionRoute = HomeRoute,
              callToActionHref = "/"
            }
        )
        `shouldBe` "CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}"
      show English `shouldBe` "English"
      show Spanish `shouldBe` "Spanish"
      show PageSurface `shouldBe` "PageSurface"
      show ApiSurface `shouldBe` "ApiSurface"
      show (UnsupportedLocalePrefix "de") `shouldBe` "UnsupportedLocalePrefix \"de\""
      show (UnsupportedPath "/missing") `shouldBe` "UnsupportedPath \"/missing\""
      show homePageModel
        `shouldBe` "HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}}"
      show secondPageModel
        `shouldBe` "SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (HomePage homePageModel)
        `shouldBe` "HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Browse the second page\", callToActionRoute = SecondRoute, callToActionHref = \"/second\"}})"
      show (SecondPage secondPageModel)
        `shouldBe` "SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show notFoundPageModel
        `shouldBe` "NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}"
      show (NotFoundPage notFoundPageModel)
        `shouldBe` "NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})"
      show
        ( AppConfig
            { appTitlePrefix = "test-app",
              listenerConfigs = [ListenerConfig {listenerHost = "127.0.0.1", listenerPort = 5001, listenerScheme = Http, listenerTls = Nothing, listenerAcme = Nothing}],
              staticAssets =
                StaticAssetsConfig
                  { staticAssetRoots = [staticRoot],
                    staticAssetContentTypes = defaultStaticAssetContentTypes,
                    staticCacheControlSeconds = Just 3600
                  },
              requestPolicy = requestPolicy defaultAppConfig,
              observability = ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
            }
        )
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)

    it "covers direct equality branches across the remaining public config and page types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
              }
          otherCertbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["renew"]
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
                acmeCertificateDirectory = Just "/var/lib/web-api/staging-certs",
                acmeCertbotConfig = otherCertbotConfig
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
              { listenerHost = "127.0.0.1",
                listenerPort = 5001,
                listenerScheme = Http,
                listenerTls = Nothing,
                listenerAcme = Nothing
              }
          secureListenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Nothing
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig, secureListenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just "req-123",
                requestSurface = PageSurface,
                requestPathPrefix = "",
                requestQueryParameters = [],
                requestSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          spacesPageModel =
            SpacesPageModel
              { spacesHeading = "Site under construction",
                spacesSummary = "Follow this space."
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      certbotExecutable certbotConfig `shouldBe` "certbot"
      certbotArguments certbotConfig `shouldBe` ["certonly", "--webroot"]
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
      staticRoot `shouldNotBe` StaticAssetRoot {staticUrlPrefix = "/static", staticDirectory = "public"}
      staticAssetsConfig `shouldBe` staticAssetsConfig
      staticAssetsConfig
        `shouldNotBe` StaticAssetsConfig
          { staticAssetRoots = [],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
      exporter `shouldBe` exporter
      exporter `shouldNotBe` OtlpExporter {otlpEndpoint = "http://other-collector:4318", otlpHeaders = []}
      observabilityConfig `shouldBe` observabilityConfig
      observabilityConfig `shouldNotBe` ObservabilityConfig {tracingExporter = Nothing, metricsExporter = Nothing}
      appConfig `shouldBe` appConfig
      appConfig `shouldNotBe` appConfig {listenerConfigs = [listenerConfig]}
      English `shouldNotBe` Spanish
      requestContext `shouldBe` requestContext
      requestContext `shouldNotBe` defaultRequestContext
      callToAction `shouldBe` callToAction
      callToAction `shouldNotBe` callToAction {callToActionHref = "/es"}
      homePageModel `shouldBe` homePageModel
      homePageModel `shouldNotBe` homePageModel {homeHeading = "Inicio"}
      secondPageModel `shouldBe` secondPageModel
      secondPageModel `shouldNotBe` secondPageModel {secondHighlights = ["Different"]}
      spacesPageModel `shouldBe` spacesPageModel
      spacesPageModel `shouldNotBe` spacesPageModel {spacesSummary = "Different"}
      notFoundPageModel `shouldBe` notFoundPageModel
      notFoundPageModel `shouldNotBe` notFoundPageModel {notFoundSummary = "Missing"}
      HomePage homePageModel `shouldNotBe` SecondPage secondPageModel
      SecondPage secondPageModel `shouldNotBe` NotFoundPage notFoundPageModel
      SpacesPage spacesPageModel `shouldNotBe` HomePage homePageModel
      UnsupportedLocalePrefix "de" `shouldNotBe` UnsupportedPath "/de"
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
                certbotArguments = ["certonly", "--webroot"]
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
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just "req-999",
                requestSurface = PageSurface,
                requestPathPrefix = "",
                requestQueryParameters = [],
                requestSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          spacesPageModel =
            SpacesPageModel
              { spacesHeading = "Site under construction",
                spacesSummary = "Follow this space."
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      show Http `shouldBe` "Http"
      show Https `shouldBe` "Https"
      show HomeRoute `shouldBe` "HomeRoute"
      show SecondRoute `shouldBe` "SecondRoute"
      show SpacesRoute `shouldBe` "SpacesRoute"
      show StatusApiRoute `shouldBe` "StatusApiRoute"
      show NotFoundRoute `shouldBe` "NotFoundRoute"
      show spacesPageModel
        `shouldBe` "SpacesPageModel {spacesHeading = \"Site under construction\", spacesSummary = \"Follow this space.\"}"
      show [spacesPageModel]
        `shouldBe` "[SpacesPageModel {spacesHeading = \"Site under construction\", spacesSummary = \"Follow this space.\"}]"
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
      shouldBeParenthesized (showsPrec 11 certbotConfig "")
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
      shouldBeParenthesized (showsPrec 11 spacesPageModel "")
      shouldBeParenthesized (showsPrec 11 notFoundPageModel "")
      shouldBeParenthesized (showsPrec 11 (HomePage homePageModel) "")
      shouldBeParenthesized (showsPrec 11 (SecondPage secondPageModel) "")
      shouldBeParenthesized (showsPrec 11 (SpacesPage spacesPageModel) "")
      shouldBeParenthesized (showsPrec 11 (NotFoundPage notFoundPageModel) "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedLocalePrefix "de") "")
      shouldBeParenthesized (showsPrec 11 (UnsupportedPath "/missing") "")

    it "covers derived list-show rendering for the remaining public types" $ do
      let certbotConfig =
            CertbotConfig
              { certbotExecutable = "certbot",
                certbotArguments = ["certonly", "--webroot"]
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
          manualCertificateSource =
            ManualCertificateFiles
              { certificateFile = "cert.pem",
                privateKeyFile = "key.pem"
              }
          acmeCertificateSource = AcmeCertificateSource acmeConfig
          tlsConfig = TlsConfig {certificateSource = acmeCertificateSource}
          listenerConfig =
            ListenerConfig
              { listenerHost = "0.0.0.0",
                listenerPort = 5443,
                listenerScheme = Https,
                listenerTls = Just tlsConfig,
                listenerAcme = Nothing
              }
          staticRoot =
            StaticAssetRoot
              { staticUrlPrefix = "/assets",
                staticDirectory = "public"
              }
          staticAssetsConfig =
            StaticAssetsConfig
              { staticAssetRoots = [staticRoot],
                staticAssetContentTypes = defaultStaticAssetContentTypes,
                staticCacheControlSeconds = Just 3600
              }
          exporter =
            OtlpExporter
              { otlpEndpoint = "http://otel-collector:4318",
                otlpHeaders = [("authorization", "Bearer token")]
              }
          observabilityConfig =
            ObservabilityConfig
              { tracingExporter = Just exporter,
                metricsExporter = Just exporter
              }
          appConfig =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = [listenerConfig],
                staticAssets = staticAssetsConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observabilityConfig
              }
          requestContext =
            AppRequestContext
              { requestLocale = Spanish,
                requestLocaleIsExplicit = False,
                requestCorrelationId = Just "req-list",
                requestSurface = PageSurface,
                requestPathPrefix = "",
                requestQueryParameters = [],
                requestSessionId = Nothing
              }
          callToAction =
            CallToAction
              { callToActionLabel = "Return home",
                callToActionRoute = HomeRoute,
                callToActionHref = "/"
              }
          homePageModel =
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Server-rendered home page with stubbed content.",
                homeErrorMessage = Nothing,
                homePrimaryAction = callToAction
              }
          secondPageModel =
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR"],
                secondErrorMessage = Nothing,
                secondPrimaryAction = callToAction
              }
          notFoundPageModel =
            NotFoundPageModel
              { notFoundHeading = "Not Found",
                notFoundSummary = "The requested page could not be found.",
                notFoundPrimaryAction = callToAction
              }
      Http `shouldNotBe` Https
      show [Http, Https] `shouldBe` "[Http,Https]"
      show [certbotConfig] `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [certbotConfig]
        `shouldBe` "[CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}]"
      show [acmeConfig]
        `shouldBe` "[AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}}]"
      show [manualCertificateSource, acmeCertificateSource]
        `shouldBe` "[ManualCertificateFiles {certificateFile = \"cert.pem\", privateKeyFile = \"key.pem\"},AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})]"
      show [tlsConfig]
        `shouldBe` "[TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})}]"
      show [listenerConfig]
        `shouldBe` "[ListenerConfig {listenerHost = \"0.0.0.0\", listenerPort = 5443, listenerScheme = Https, listenerTls = Just (TlsConfig {certificateSource = AcmeCertificateSource (AcmeConfig {acmeDirectoryUrl = \"https://acme-v02.api.letsencrypt.org/directory\", acmeContactEmails = [\"ops@example.com\"], acmeDomains = [\"example.com\",\"www.example.com\"], acmeHttp01Port = 80, acmeCertificateDirectory = Nothing, acmeCertbotConfig = CertbotConfig {certbotExecutable = \"certbot\", certbotArguments = [\"certonly\",\"--webroot\"]}})})}]"
      show [staticRoot] `shouldBe` "[StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}]"
      show [staticAssetsConfig]
        `shouldBe` ( "[StaticAssetsConfig {staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = \"/assets\", staticDirectory = \"public\"}], staticAssetContentTypes = "
                       <> show defaultStaticAssetContentTypes
                       <> ", staticCacheControlSeconds = Just 3600}]"
                   )
      show [exporter]
        `shouldBe` "[OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}]"
      show [observabilityConfig]
        `shouldBe` "[ObservabilityConfig {tracingExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]}), metricsExporter = Just (OtlpExporter {otlpEndpoint = \"http://otel-collector:4318\", otlpHeaders = [(\"authorization\",\"Bearer token\")]})}]"
      show [appConfig]
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show [English, Spanish] `shouldBe` "[English,Spanish]"
      show [PageSurface, ApiSurface] `shouldBe` "[PageSurface,ApiSurface]"
      show [requestContext]
        `shouldBe` "[AppRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = False, requestCorrelationId = Just \"req-list\", requestSurface = PageSurface, requestPathPrefix = \"\", requestQueryParameters = [], requestSessionId = Nothing}]"
      show [callToAction]
        `shouldBe` "[CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}]"
      show [homePageModel]
        `shouldBe` "[HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [secondPageModel]
        `shouldBe` "[SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [notFoundPageModel]
        `shouldBe` "[NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}]"
      show [HomePage homePageModel, SecondPage secondPageModel, NotFoundPage notFoundPageModel]
        `shouldBe` "[HomePage (HomePageModel {homeHeading = \"Home\", homeSummary = \"Server-rendered home page with stubbed content.\", homeErrorMessage = Nothing, homePrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),SecondPage (SecondPageModel {secondHeading = \"Second\", secondSummary = \"Second page content with stubbed data ready for future loaders.\", secondHighlights = [\"Fast SSR\"], secondErrorMessage = Nothing, secondPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}}),NotFoundPage (NotFoundPageModel {notFoundHeading = \"Not Found\", notFoundSummary = \"The requested page could not be found.\", notFoundPrimaryAction = CallToAction {callToActionLabel = \"Return home\", callToActionRoute = HomeRoute, callToActionHref = \"/\"}})]"
      show [UnsupportedLocalePrefix "de", UnsupportedPath "/missing"]
        `shouldBe` "[UnsupportedLocalePrefix \"de\",UnsupportedPath \"/missing\"]"
      show [HomeRoute, SecondRoute, RegistrationRoute, EmailVerificationRoute, MfaEnrollmentRoute, LoginRoute, LogoutRoute, ProfileRoute, StatusApiRoute, NotFoundRoute] `shouldBe` "[HomeRoute,SecondRoute,RegistrationRoute,EmailVerificationRoute,MfaEnrollmentRoute,LoginRoute,LogoutRoute,ProfileRoute,StatusApiRoute,NotFoundRoute]"

  describe "parseRoute" $ do
    it "maps bare and default-locale paths to the same home route" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/en") `shouldBe` Just HomeRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/404") `shouldBe` Just NotFoundRoute

    it "parses API routes with the API response surface" $ do
      parseRoute defaultRequestContext "/api/status" `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext "/api/status?fresh=1"
        `shouldBe` Just apiStatusRequest {HarchWeb.requestContext = defaultRequestContext {requestSurface = ApiSurface, requestQueryParameters = [("fresh", "1")]}}
      parseRoute defaultRequestContext "/api/second" `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext "/api" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/404" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/missing" `shouldBe` Just apiNotFoundRequest
      parseRoute defaultRequestContext "/api/status/extra" `shouldBe` Just apiNotFoundRequest

    it "parses the second page path" $
      parseRoute defaultRequestContext "/second" `shouldBe` Just secondRequest

    it "parses the app-home spaces path with its typed locale" $ do
      parseRoute defaultRequestContext "/spaces" `shouldBe` Just spacesRequest
      parseRoute defaultRequestContext "/es/spaces" `shouldBe` Just spanishSpacesRequest

    it "parses SSR account routes and preserves email-verification query values" $ do
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/register") `shouldBe` Just RegistrationRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/mfa") `shouldBe` Just MfaEnrollmentRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/login") `shouldBe` Just LoginRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/logout") `shouldBe` Just LogoutRoute
      fmap HarchWeb.requestRoute (parseRoute defaultRequestContext "/profile") `shouldBe` Just ProfileRoute
      parseRoute defaultRequestContext "/verify?token=opaque-token"
        `shouldBe` Just
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = EmailVerificationRoute,
              HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("token", "opaque-token")]}
            }
      parseRoute defaultRequestContext "/verify?=ignored&token=opaque-token&flag"
        `shouldBe` Just
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = EmailVerificationRoute,
              HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("token", "opaque-token"), ("flag", "")]}
            }

    it "keeps supported path routes available when raw query strings are present" $
      parseRoute defaultRequestContext "/second?utm=demo"
        `shouldBe` Just secondRequest {HarchWeb.requestContext = defaultRequestContext {requestQueryParameters = [("utm", "demo")]}}

    it "lets explicit locale prefixes override the incoming request context" $ do
      parseRoute defaultRequestContext "/es/second" `shouldBe` Just spanishSecondRequest
      parseRoute spanishRequestContext "/en/second" `shouldBe` Just (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)

    it "returns an unsupported-route representation for unknown paths" $
      parseRoute defaultRequestContext "/missing" `shouldBe` Nothing

    it "fails unsupported locale prefixes with a precise route-selection error" $ do
      selectRoute defaultRequestContext "/de" `shouldBe` Left (UnsupportedLocalePrefix "de")
      selectRoute defaultRequestContext "/de/second" `shouldBe` Left (UnsupportedLocalePrefix "de")

    it "rejects paths that do not start with a slash" $
      selectRoute defaultRequestContext "second" `shouldBe` Left (UnsupportedPath "second")

    it "rejects unsupported multi-segment paths" $
      selectRoute defaultRequestContext "/es/second/extra" `shouldBe` Left (UnsupportedPath "/es/second/extra")

    it "rejects unsupported single-segment non-locale paths" $
      selectRoute defaultRequestContext "/missing" `shouldBe` Left (UnsupportedPath "/missing")

    it "rejects unsupported query-bearing paths after separating the route path" $
      selectRoute defaultRequestContext "/missing?utm=demo" `shouldBe` Left (UnsupportedPath "/missing")

    it "rejects locale-prefixed paths whose trailing segment is unsupported" $ do
      selectRoute defaultRequestContext "/es/missing" `shouldBe` Left (UnsupportedPath "/es/missing")
      selectRoute defaultRequestContext "/other/second" `shouldBe` Left (UnsupportedPath "/other/second")

    it "merges middleware-supplied and path-derived request inputs deterministically" $ do
      let middlewareContext =
            defaultRequestContext
              { requestLocale = English,
                requestCorrelationId = Just "req-123"
              }
      parseRoute middlewareContext "/es"
        `shouldBe` Just (HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = middlewareContext {requestLocale = Spanish, requestLocaleIsExplicit = True}})

    it "rejects invalid trailing slashes while keeping the root path valid" $ do
      parseRoute defaultRequestContext "/" `shouldBe` Just homeRequest
      parseRoute defaultRequestContext "/second/" `shouldBe` Nothing
      selectRoute defaultRequestContext "/second/" `shouldBe` Left (UnsupportedPath "/second/")

  describe "renderRoutePath" $ do
    it "round-trips known routes through the parser" $ do
      parseRoute defaultRequestContext (renderRoutePath homeRequest) `shouldBe` Just homeRequest
      parseRoute defaultRequestContext (renderRoutePath secondRequest) `shouldBe` Just secondRequest
      parseRoute defaultRequestContext (renderRoutePath spanishSecondRequest) `shouldBe` Just spanishSecondRequest
      parseRoute defaultRequestContext (renderRoutePath spacesRequest) `shouldBe` Just spacesRequest
      parseRoute defaultRequestContext (renderRoutePath profileRequest) `shouldBe` Just profileRequest
      parseRoute defaultRequestContext (renderRoutePath spanishSpacesRequest) `shouldBe` Just spanishSpacesRequest
      parseRoute defaultRequestContext (renderRoutePath (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)) `shouldBe` Just (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext)
      parseRoute defaultRequestContext (renderRoutePath apiStatusRequest) `shouldBe` Just apiStatusRequest
      parseRoute defaultRequestContext (renderRoutePath apiSecondRequest) `shouldBe` Just apiSecondRequest
      parseRoute defaultRequestContext (renderRoutePath apiNotFoundRequest) `shouldBe` Just apiNotFoundRequest

    it "renders default and explicit locale prefixes" $ do
      renderRoutePath homeRequest `shouldBe` "/"
      renderRoutePath spanishHomeRequest `shouldBe` "/es"
      renderRoutePath secondRequest `shouldBe` "/second"
      renderRoutePath spanishSecondRequest `shouldBe` "/es/second"
      renderRoutePath spacesRequest `shouldBe` "/spaces"
      renderRoutePath spanishSpacesRequest `shouldBe` "/es/spaces"
      renderRoutePath (HarchWeb.RouteRequest HomeRoute explicitEnglishRequestContext) `shouldBe` "/en"
      renderRoutePath (HarchWeb.RouteRequest SecondRoute explicitEnglishRequestContext) `shouldBe` "/en/second"
      renderRoutePath (HarchWeb.RouteRequest RegistrationRoute defaultRequestContext) `shouldBe` "/register"
      renderRoutePath (HarchWeb.RouteRequest EmailVerificationRoute spanishRequestContext) `shouldBe` "/es/verify"
      renderRoutePath (HarchWeb.RouteRequest LoginRoute defaultRequestContext) `shouldBe` "/login"
      renderRoutePath (HarchWeb.RouteRequest LogoutRoute spanishRequestContext) `shouldBe` "/es/logout"
      renderRoutePath (HarchWeb.RouteRequest ProfileRoute spanishRequestContext) `shouldBe` "/es/profile"
      renderRoutePath (HarchWeb.RouteRequest {HarchWeb.requestRoute = StatusApiRoute, HarchWeb.requestContext = defaultRequestContext}) `shouldBe` "/404"
      renderRoutePath apiStatusRequest `shouldBe` "/api/status"
      renderRoutePath apiSecondRequest `shouldBe` "/api/second"
      renderRoutePath apiNotFoundRequest `shouldBe` "/api/404"
      renderRoutePath notFoundRequest `shouldBe` "/404"

    it "prepends the forwarded request path prefix to page and API routes" $ do
      renderRoutePath prefixedHomeRequest `shouldBe` "/app"
      renderRoutePath prefixedSpanishSecondRequest `shouldBe` "/app/es/second"
      renderRoutePath prefixedApiStatusRequest `shouldBe` "/app/api/status"

  describe "matchRoute" $ do
    it "remains available separately from HarchWeb.matchRoute" $
      WebApi.Route.matchRoute WebApi.Route.defaultRequestContext "/second"
        `shouldBe` HarchWeb.matchRoute WebApi.Route.routeCodec WebApi.Route.defaultRequestContext "/second"

    it "matches the home path" $
      pureRouteMatcher "/" `shouldBe` homeRequest

    it "matches the second page path" $
      pureRouteMatcher "/second" `shouldBe` secondRequest

    it "matches the app-home spaces path" $
      pureRouteMatcher "/spaces" `shouldBe` spacesRequest

    it "matches locale-prefixed paths with the merged request context" $
      pureRouteMatcher "/es" `shouldBe` spanishHomeRequest

    it "matches API paths with the API response surface" $ do
      pureRouteMatcher "/api/status" `shouldBe` apiStatusRequest
      pureRouteMatcher "/api/second" `shouldBe` apiSecondRequest
      pureRouteMatcher "/api/missing" `shouldBe` apiNotFoundRequest

    it "falls back to the stable not-found route for unknown paths" $
      pureRouteMatcher "/missing" `shouldBe` notFoundRequest

  describe "renderPage" $ do
    it "selects the expected home page model" $
      renderPage defaultAppConfig homeRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "renders the app-home spaces page entirely on the server" $
      renderPage defaultAppConfig spacesRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Spaces",
            HarchWeb.pageRoute = SpacesRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Site under construction</h1><p>Follow this space.</p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "renders selected route data without reloading it" $
      renderPageFromRouteData
        defaultAppConfig
        secondRequest
        ( SecondRouteDataResult
            ( Right
                SecondRouteData
                  { secondRouteSummary = "Shared domain summary.",
                    secondRouteHighlights = ["Shared loader"]
                  }
            )
        )
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Shared domain summary.</p><ul><li>Shared loader</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "keeps shared layout data consistent across all routes" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      homeShell <- renderedShell config HomeRoute
      secondShell <- renderedShell config SecondRoute
      notFoundShell <- renderedShell config NotFoundRoute
      Text.isInfixOf "<title>test-app: Home</title>" homeShell `shouldBe` True
      Text.isInfixOf "<title>test-app: Second</title>" secondShell `shouldBe` True
      Text.isInfixOf "data-bootstrap-hooks=\"second-page\"" secondShell `shouldBe` True
      Text.isInfixOf "<title>test-app: Not Found</title>" notFoundShell `shouldBe` True
      Text.isInfixOf "<script nonce=\"development-render-nonce\">" homeShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" homeShell `shouldBe` True

    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      show config
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show defaultRequestContext `shouldBe` "AppRequestContext {requestLocale = English, requestLocaleIsExplicit = False, requestCorrelationId = Nothing, requestSurface = PageSurface, requestPathPrefix = \"\", requestQueryParameters = [], requestSessionId = Nothing}"
      show (renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []}))))
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageContext = AppRequestContext {requestLocale = English, requestLocaleIsExplicit = False, requestCorrelationId = Nothing, requestSurface = PageSurface, requestPathPrefix = \"\", requestQueryParameters = [], requestSessionId = Nothing}, pageBody = \"<section data-page=\\\"second\\\"><h1 data-page-title=\\\"true\\\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\\\"true\\\">No highlights yet.</p><p><a href=\\\"/\\\" data-page-link=\\\"true\\\">Return home</a></p></section>\", pageBootstrapHooks = [\"second-page\"]}"
      renderPage config secondRequest `shouldReturn` renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))

  describe "selectResponse" $ do
    it "resolves page routes to page responses that still flow through the shared shell" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      selectResponse defaultAppConfig secondRequest `shouldReturn` HarchWeb.PageResponse renderedPage

    it "resolves API-only routes to explicit status, content type, and body values" $ do
      selectResponse defaultAppConfig apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }
      selectResponse defaultAppConfig apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "keeps API payload rendering locale-aware without touching page routing" $ do
      selectResponse defaultAppConfig spanishApiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"es\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }
      selectResponse defaultAppConfig spanishApiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "attaches safe database operation observability to postgres-backed page and API responses" $ do
      let postgresRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'home'" sql ->
                      successfulPostgresResult "Loaded home summary."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner postgresRunner postgresTestConfig
      let renderedSecondPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              ( SecondRouteDataResult
                  ( Right
                      SecondRouteData
                        { secondRouteSummary = "Loaded second summary.",
                          secondRouteHighlights = ["Fast SSR", "Shared route data"]
                        }
                  )
              )
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect secondRequest)
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
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
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries = []
            }
          renderedSecondPage
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest)
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"summary\":\"Loaded second summary.\",\"highlights\":[\"Fast SSR\",\"Shared route data\"]}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
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
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries = []
            }

    it "keeps not-found handling consistent across page and non-page responses" $ do
      renderedPage <- renderPage defaultAppConfig notFoundRequest
      selectResponse defaultAppConfig notFoundRequest `shouldReturn` HarchWeb.PageResponse renderedPage
      selectResponse defaultAppConfig apiNotFoundRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 404,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"not-found\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
            }

    it "maps shared second-page load failures into explicit API error responses" $
      selectResponseWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        apiSecondRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "error.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.failure.code",
                      Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "api"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page api response: SecondPageDataError \"seed unavailable\""]
            }

    it "omits volatile database timing fields when a database effect reports untimed operations" $ do
      let untimedOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          untimedDatabaseEffect =
            defaultDatabaseEffect
              { loadSecondPageDataWithObservability =
                  \_ ->
                    pure
                      DatabaseResult
                        { databaseResultValue =
                            Right
                              SecondPageData
                                { secondPageDataSummary = "Untimed summary.",
                                  secondPageDataHighlights = []
                                },
                          databaseResultOperations = [untimedOperation]
                        }
              }
      response <- selectResponseWithDatabase defaultAppConfig untimedDatabaseEffect apiSecondRequest
      case response of
        HarchWeb.BodyResponse responseBody ->
          HarchWeb.responseObservabilityAttributes responseBody
            `shouldBe` [ Observability.ObservabilityAttribute
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
                           }
                       ]
        _ ->
          expectationFailure "expected API response body for untimed database operation"

    it "adds safe database operation details to postgres-backed failure diagnostics" $ do
      let failingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded second summary."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresDatabaseEffectWithRunner failingRunner postgresTestConfig
      fmap stripVolatileDatabaseTimingResponse (selectResponseWithDatabase defaultAppConfig postgresEffect apiSecondRequest)
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 503,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "error.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.failure.code",
                      Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "api"
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
                    { Observability.attributeName = "db.system",
                      Observability.attributeValue = Observability.TextAttribute "postgresql"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.operation.name",
                      Observability.attributeValue = Observability.TextAttribute "load-second-page-highlights"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "db.query.template",
                      Observability.attributeValue = Observability.TextAttribute "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
                    }
                ],
              HarchWeb.responseLogEntries =
                [ "Database failure while rendering required second-page api response after database operations [load-second-page-summary (SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;), load-second-page-highlights (SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;)]: SecondPageDataError \"highlights unavailable\""
                ]
            }

    it "preserves unexpected database error constructors in API diagnostics" $
      renderApiResponseFromRouteData (SecondRouteDataResult (Left (HomePageDataError "wrong loader")))
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 503,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"error\":\"second-page-unavailable\"}",
            HarchWeb.responseObservabilityAttributes =
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "error.type",
                    Observability.attributeValue = Observability.TextAttribute "HomePageDataError"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.failure.code",
                    Observability.attributeValue = Observability.TextAttribute "database.home-page-data"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.route",
                    Observability.attributeValue = Observability.TextAttribute "/second"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "app.surface",
                    Observability.attributeValue = Observability.TextAttribute "api"
                  }
              ],
            HarchWeb.responseLogEntries =
              ["Database failure while rendering required second-page api response: HomePageDataError \"wrong loader\""]
          }

    it "maps required second-page failures into explicit HTML 500 responses" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
          renderedPage =
            renderPageFromRouteData
              defaultAppConfig
              secondRequest
              (SecondRouteDataResult (Left (SecondPageDataError "seed unavailable")))
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect secondRequest
        `shouldReturn` HarchWeb.PageResponseWithMetadata
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 500,
              HarchWeb.responseContentType = "text/html; charset=utf-8",
              HarchWeb.responseBody = "",
              HarchWeb.responseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "error.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.failure.code",
                      Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "page"
                    }
                ],
              HarchWeb.responseLogEntries =
                ["Database failure while rendering required second-page page response: SecondPageDataError \"seed unavailable\""]
            }
          renderedPage

    it "redirects root requests before a home-page database failure can be observed" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` (HarchWeb.redirectResponse 302 "/spaces" :: HarchWeb.Response AppRoute AppRequestContext)

    it "keeps locale and forwarded path prefixes in root redirect locations" $ do
      let prefixedSpanishRequest =
            HarchWeb.RouteRequest
              HomeRoute
              spanishRequestContext {requestPathPrefix = "/app"}
      selectResponse defaultAppConfig spanishHomeRequest
        `shouldReturn` (HarchWeb.redirectResponse 302 "/es/spaces" :: HarchWeb.Response AppRoute AppRequestContext)
      selectResponse defaultAppConfig prefixedSpanishRequest
        `shouldReturn` (HarchWeb.redirectResponse 302 "/app/es/spaces" :: HarchWeb.Response AppRoute AppRequestContext)

    it "keeps routes without required database data on their existing responses" $ do
      let failingDatabaseEffect =
            buildSeededDatabaseEffect
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = Left (SecondPageDataError "seed unavailable")
                }
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect homeRequest
        `shouldReturn` (HarchWeb.redirectResponse 302 "/spaces" :: HarchWeb.Response AppRoute AppRequestContext)
      selectResponseWithDatabase defaultAppConfig failingDatabaseEffect apiStatusRequest
        `shouldReturn` HarchWeb.BodyResponse
          HarchWeb.ResponseBody
            { HarchWeb.responseStatus = 200,
              HarchWeb.responseContentType = "application/json",
              HarchWeb.responseBody = "{\"status\":\"ok\",\"locale\":\"en\"}",
              HarchWeb.responseObservabilityAttributes = [],
              HarchWeb.responseLogEntries = []
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
            { homeHeading = "Home",
              homeSummary = "Server-rendered home page with stubbed content.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }

    it "keeps locale-aware action paths in stubbed page data" $
      buildPageModel spanishHomeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Ver la segunda página",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/es/second"
                  }
            }

    it "localizes Spanish second-page return actions" $
      buildPageModel spanishSecondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Second page content with stubbed data ready for future loaders.",
              secondHighlights = [],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Volver al inicio",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/es"
                  }
            }

    it "ports the spaces placeholder with its source-app English and Spanish copy" $ do
      buildPageModel spacesRequest
        `shouldReturn` SpacesPage
          SpacesPageModel
            { spacesHeading = "Site under construction",
              spacesSummary = "Follow this space."
            }
      buildPageModel spanishSpacesRequest
        `shouldReturn` SpacesPage
          SpacesPageModel
            { spacesHeading = "Sitio en construcción",
              spacesSummary = "Sigan este espacio."
            }

    it "builds explicit home-page error state when the database effect fails" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        homeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Home page content is temporarily unavailable.",
              homeErrorMessage = Just "Could not load home page data.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }

    it "renders selected route data into both page models and API responses" $ do
      let selectedRouteData =
            SecondRouteDataResult
              ( Right
                  SecondRouteData
                    { secondRouteSummary = "Shared domain summary.",
                      secondRouteHighlights = ["Shared loader", "Shared renderer"]
                    }
              )
      buildPageModelFromRouteData secondRequest selectedRouteData
        `shouldBe` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Shared domain summary.",
              secondHighlights = ["Shared loader", "Shared renderer"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }
      renderApiResponseFromRouteData selectedRouteData
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = 200,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"summary\":\"Shared domain summary.\",\"highlights\":[\"Shared loader\",\"Shared renderer\"]}",
            HarchWeb.responseObservabilityAttributes = [],
            HarchWeb.responseLogEntries = []
          }

    it "loads second-page content from the database effect when provided" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData =
                  Right
                    SecondPageData
                      { secondPageDataSummary = "Loaded from the seeded database effect.",
                        secondPageDataHighlights = ["Fast SSR", "Progressive enhancement"]
                      },
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Loaded from the seeded database effect.",
              secondHighlights = ["Fast SSR", "Progressive enhancement"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }

    it "builds an explicit error-state second page when the database effect fails" $
      buildPageModelWithDatabase
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Second page content is temporarily unavailable.",
              secondHighlights = [],
              secondErrorMessage = Just "Could not load second page data.",
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }

  describe "renderPageBody" $ do
    it "renders the home page heading and navigation affordance" $ do
      homePageModel <- buildPageModel homeRequest
      renderPageBody homePageModel
        `shouldBe` "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p>Server-rendered home page with stubbed content.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"
      renderPageBody (RegistrationPage "/register" emptyRegistrationForm)
        `shouldSatisfy` Text.isInfixOf "data-page=\"registration\""
      let pendingProfile =
            renderPageBody
              ( ProfilePage
                  ( PendingProfilePage
                      "Profile"
                      "Verify your email address before continuing."
                      "person@example.test"
                      (Just "person_01")
                      (Just "Person Example")
                      "/profile"
                      "Resend verification email"
                      (CallToAction "Sign out" LogoutRoute "/logout")
                  )
              )
      pendingProfile
        `shouldSatisfy` \html -> Text.isInfixOf "data-profile-resend=\"true\"" html && not (Text.isInfixOf "data-error-state=\"true\"" html)
      let anonymousProfile =
            renderPageBody
              ( ProfilePage
                  ( AuthenticatedProfilePage
                      "Profile"
                      "Signed in."
                      "person@example.test"
                      Nothing
                      Nothing
                      (CallToAction "Sign out" LogoutRoute "/logout")
                  )
              )
      anonymousProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-username"
      anonymousProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-display-name"

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      secondPageModel <- buildPageModel secondRequest
      renderPageBody secondPageModel
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
      Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a><a href=\"/register\" data-page-link=\"true\">Create account</a><a href=\"/login\" data-page-link=\"true\">Sign in</a><a href=\"/profile\" data-page-link=\"true\">Profile</a></nav><main id=\"app-main\" data-navigation-content=\"true\">" homeShell `shouldBe` True
      Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\">Home</a><a href=\"/second\" data-page-link=\"true\" aria-current=\"page\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a><a href=\"/register\" data-page-link=\"true\">Create account</a><a href=\"/login\" data-page-link=\"true\">Sign in</a><a href=\"/profile\" data-page-link=\"true\">Profile</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "renders the app-home spaces surface without requiring client code" $ do
      spacesPageModel <- buildPageModel spacesRequest
      renderPageBody spacesPageModel
        `shouldBe` "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Site under construction</h1><p>Follow this space.</p></section>"
      spanishSpacesPageModel <- buildPageModel spanishSpacesRequest
      renderPageBody spanishSpacesPageModel
        `shouldBe` "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Sitio en construcción</h1><p>Sigan este espacio.</p></section>"

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      homePageModel <- buildPageModel homeRequest
      secondPageModel <- buildPageModel secondRequest
      let homeBody = renderPageBody homePageModel
          secondBody = renderPageBody secondPageModel
      Text.isInfixOf "<section data-page=\"home\">" homeBody `shouldBe` True
      Text.isInfixOf "<section data-page=\"second\">" secondBody `shouldBe` True
      Text.isInfixOf "data-page-title=\"true\"" homeBody `shouldBe` True
      Text.isInfixOf "data-page-link=\"true\"" secondBody `shouldBe` True
      Text.isInfixOf "<main" homeBody `shouldBe` False
      Text.isInfixOf "<body" secondBody `shouldBe` False

    it "covers empty and populated highlight rendering branches" $ do
      secondPageModel <- buildPageModel secondRequest
      Text.isInfixOf "<p data-empty-state=\"true\">No highlights yet.</p>" (renderPageBody secondPageModel) `shouldBe` True
      renderPageBody
        ( SecondPage
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Stable routes"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
        )
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"

    it "renders an explicit error state when the second-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p data-error-state=\"true\">Could not load second page data.</p><p>Second page content is temporarily unavailable.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "renders an explicit error state when the home-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededDatabaseEffect
            DatabaseSeed
              { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        homeRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Home",
            HarchWeb.pageRoute = HomeRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"home\"><h1 data-page-title=\"true\">Home</h1><p data-error-state=\"true\">Could not load home page data.</p><p>Home page content is temporarily unavailable.</p><p><a href=\"/second\" data-page-link=\"true\">Browse the second page</a></p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

  describe "page shell integration" $ do
    it "keeps every page route's path, title, and enhancements in one metadata table" $
      map (metadataFields . routeMetadata) [HomeRoute, SecondRoute, SpacesRoute, RegistrationRoute, EmailVerificationRoute, MfaEnrollmentRoute, LoginRoute, LogoutRoute, ProfileRoute, NotFoundRoute, StatusApiRoute]
        `shouldBe` [ (Nothing, "", "Home", []),
                     (Just "second", "/second", "Second", ["second-page"]),
                     (Just "spaces", "/spaces", "Spaces", []),
                     (Just "register", "/register", "Create account", []),
                     (Just "verify", "/verify", "Verify email", []),
                     (Just "mfa", "/mfa", "Set up authenticator", []),
                     (Just "login", "/login", "Sign in", []),
                     (Just "logout", "/logout", "Sign out", []),
                     (Just "profile", "/profile", "Profile", []),
                     (Just "404", "/404", "Not Found", []),
                     (Nothing, "/404", "Not Found", [])
                   ]

    it "keeps client-only enhancement hooks in the app seam instead of page rendering" $ do
      pageEnhancementHooks HomeRoute `shouldBe` []
      pageEnhancementHooks SecondRoute `shouldBe` ["second-page"]
      pageEnhancementHooks SpacesRoute `shouldBe` []
      pageEnhancementHooks StatusApiRoute `shouldBe` []
      pageEnhancementHooks NotFoundRoute `shouldBe` []

    it "marks the active navigation item for each routed page" $ do
      homeShell <- renderedShell defaultAppConfig HomeRoute
      secondShell <- renderedShell defaultAppConfig SecondRoute
      spacesShell <- renderedShell defaultAppConfig SpacesRoute
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\" aria-current=\"page\">Home</a><a href=\"/second\" data-page-link=\"true\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a>" homeShell `shouldBe` True
      Text.isInfixOf "<a href=\"/\" data-page-link=\"true\">Home</a><a href=\"/second\" data-page-link=\"true\" aria-current=\"page\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a>" secondShell `shouldBe` True
      Text.isInfixOf "<a href=\"/spaces\" data-page-link=\"true\" aria-current=\"page\">Spaces</a>" spacesShell `shouldBe` True
      Text.isInfixOf "aria-current=\"page\"" notFoundShell `shouldBe` False

    it "emits deterministic navigation hooks and script references when assets are configured" $ do
      let rootMountedConfig =
            navigationAppConfig
              { staticAssets =
                  StaticAssetsConfig
                    { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/", staticDirectory = "public"}],
                      staticAssetContentTypes = defaultStaticAssetContentTypes,
                      staticCacheControlSeconds = Nothing
                    }
              }
      homeShellWithoutAssets <- renderedShell defaultAppConfig HomeRoute
      homeShell <- renderedShell navigationAppConfig HomeRoute
      secondShell <- renderedShell navigationAppConfig SecondRoute
      rootMountedShell <- renderedShell rootMountedConfig HomeRoute
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" homeShellWithoutAssets `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" homeShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" rootMountedShell `shouldBe` True
      Text.isInfixOf "<nav data-navigation-region=\"primary\">" homeShell `shouldBe` True
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\">" homeShell `shouldBe` True
      Text.isInfixOf "data-bootstrap-hooks" homeShell `shouldBe` False
      Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "renders navigation and script hrefs under the forwarded request path prefix" $ do
      prefixedShell <- renderedShellForRequest navigationAppConfig prefixedSecondRequest
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\">Home</a><a href=\"/app/second\" data-page-link=\"true\" aria-current=\"page\">Second</a>" prefixedShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/app/assets/navigation.js\" defer></script>" prefixedShell `shouldBe` True

    it "serves the bundled navigation asset through configured static roots" $ do
      response <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "navigation.js"])
      Wai.responseStatus response `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders response) `shouldBe` Just "application/javascript; charset=utf-8"
      responseBody <- readResponseBody response
      Text.isInfixOf "data-page-link" responseBody `shouldBe` True
      Text.isInfixOf "popstate" responseBody `shouldBe` True

    it "serves bundled style, font, and resource assets through configured static roots" $ do
      stylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "styles", "app.css"])
      Wai.responseStatus stylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders stylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      stylesheetBody <- readResponseBody stylesheetResponse
      Text.isInfixOf "font-family: system-ui, sans-serif;" stylesheetBody `shouldBe` True

      fontStylesheetResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "fonts", "font-faces.css"])
      Wai.responseStatus fontStylesheetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders fontStylesheetResponse) `shouldBe` Just "text/css; charset=utf-8"
      fontStylesheetBody <- readResponseBody fontStylesheetResponse
      Text.isInfixOf "@font-face" fontStylesheetBody `shouldBe` True

      faviconResponse <- performWaiRequest (HarchWeb.toWaiApplication (buildApp navigationAppConfig)) (waiRequest ["assets", "resources", "favicon.svg"])
      Wai.responseStatus faviconResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders faviconResponse) `shouldBe` Just "image/svg+xml"
      faviconBody <- readResponseBody faviconResponse
      Text.isInfixOf "<svg" faviconBody `shouldBe` True

    it "keeps shell output identical for repeated renders of the same page input" $ do
      let application = buildApp defaultAppConfig
      page <- renderPage defaultAppConfig spanishSecondRequest
      HarchWeb.pageShell application page `shouldBe` HarchWeb.pageShell application page

    it "keeps the legacy page-shell shim aligned with the app shell seam" $ do
      renderedPage <- renderPage defaultAppConfig secondRequest
      LegacyPageShell.buildAppPageShell defaultAppConfig renderedPage
        `shouldBe` buildAppPageShell defaultAppConfig renderedPage
      navigationRenderedPage <- renderPage navigationAppConfig secondRequest
      LegacyPageShell.buildAppPageShell navigationAppConfig navigationRenderedPage
        `shouldBe` buildAppPageShell navigationAppConfig navigationRenderedPage

    it "keeps the shell configuration seam aligned with the rendered shell entry point" $ do
      renderedPage <- renderPage navigationAppConfig secondRequest
      let shellConfig = buildAppPageShellConfig navigationAppConfig renderedPage
      HarchWeb.shellNavigationItems shellConfig `shouldBe` []
      HarchWeb.shellRuntimeDescriptors shellConfig `shouldBe` []

    it "keeps not-found pages inside the shared shell" $ do
      notFoundShell <- renderedShell defaultAppConfig NotFoundRoute
      Text.isInfixOf "<title>web-api: Not Found</title>" notFoundShell `shouldBe` True
      Text.isInfixOf "data-page=\"not-found\"" notFoundShell `shouldBe` True

  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` "web-api"

    it "stores the default request context used by the WAI adapter" $
      HarchWeb.defaultRequestContext pureApplication `shouldBe` defaultRequestContext

    it "ignores forwarded path prefixes by default in the request context used by the WAI adapter" $ do
      let forwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "app, /ignored")]
              }
          emptyForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", ", ")]
              }
          sessionToken = Text.replicate 43 "a"
          cookieRequest =
            (waiRequest ["logout"])
              { Wai.requestHeaders = [("Cookie", TextEncoding.encodeUtf8 ("theme=dark; __Host-harch-session=" <> sessionToken))]
              }
          invalidCookieRequest =
            (waiRequest ["logout"])
              { Wai.requestHeaders = [("Cookie", ByteString.pack [255])]
              }
      HarchWeb.requestContextFromRequest pureApplication forwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext
      HarchWeb.requestContextFromRequest pureApplication emptyForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext
      HarchWeb.requestContextFromRequest pureApplication cookieRequest defaultRequestContext
        `shouldBe` defaultRequestContext {requestSessionId = Session.mkSessionId sessionToken}
      HarchWeb.requestContextFromRequest pureApplication invalidCookieRequest defaultRequestContext
        `shouldBe` defaultRequestContext

    it "derives normalized forwarded path prefixes when forwarded headers are trusted" $ do
      let forwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "app, /ignored")]
              }
          emptyForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", ", ")]
              }
      HarchWeb.requestContextFromRequest trustedForwardedApplication forwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext {requestPathPrefix = "/app"}
      HarchWeb.requestContextFromRequest trustedForwardedApplication emptyForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext

    it "stores the configured static assets used by the WAI adapter" $
      HarchWeb.applicationStaticAssets pureApplication `shouldBe` staticAssets defaultAppConfig

    it "keeps pure-app observability and log reporters as no-ops" $ do
      HarchWeb.reportRequestObservability
        pureApplication
        ( Observability.buildRequestObservability
            "GET"
            "http"
            "/"
            "/"
            200
            Observability.PageResponseKind
            []
        )
      HarchWeb.reportConnectionObservability
        pureApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog pureApplication "ignored"

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec defaultRequestContext "/" `shouldBe` parseRoute defaultRequestContext "/"
      HarchWeb.parseRoute codec defaultRequestContext "/es" `shouldBe` parseRoute defaultRequestContext "/es"
      HarchWeb.parseRoute codec defaultRequestContext "/second" `shouldBe` parseRoute defaultRequestContext "/second"
      HarchWeb.parseRoute codec defaultRequestContext "/api/status" `shouldBe` parseRoute defaultRequestContext "/api/status"
      HarchWeb.parseRoute codec defaultRequestContext "/api/second" `shouldBe` parseRoute defaultRequestContext "/api/second"
      HarchWeb.parseRoute codec defaultRequestContext "/missing" `shouldBe` Nothing
      HarchWeb.renderRoute codec homeRequest `shouldBe` renderRoutePath homeRequest
      HarchWeb.renderRoute codec spanishSecondRequest `shouldBe` renderRoutePath spanishSecondRequest
      HarchWeb.renderRoute codec secondRequest `shouldBe` renderRoutePath secondRequest
      HarchWeb.renderRoute codec apiStatusRequest `shouldBe` renderRoutePath apiStatusRequest
      HarchWeb.renderRoute codec apiSecondRequest `shouldBe` renderRoutePath apiSecondRequest
      HarchWeb.renderRoute codec apiNotFoundRequest `shouldBe` renderRoutePath apiNotFoundRequest
      HarchWeb.renderRoute codec notFoundRequest `shouldBe` renderRoutePath notFoundRequest
      HarchWeb.notFoundRequest codec defaultRequestContext `shouldBe` notFoundRequest

    it "stores the same response-selection behavior used by direct response tests" $ do
      expectedHomeResponse <- selectResponse defaultAppConfig homeRequest
      expectedSecondResponse <- selectResponse defaultAppConfig secondRequest
      expectedSpacesResponse <- selectResponse defaultAppConfig spacesRequest
      expectedApiStatusResponse <- selectResponse defaultAppConfig apiStatusRequest
      expectedApiSecondResponse <- selectResponse defaultAppConfig apiSecondRequest
      expectedNotFoundResponse <- selectResponse defaultAppConfig notFoundRequest
      expectedApiNotFoundResponse <- selectResponse defaultAppConfig apiNotFoundRequest
      HarchWeb.renderResponse pureApplication homeRequest `shouldReturn` expectedHomeResponse
      HarchWeb.renderResponse pureApplication secondRequest `shouldReturn` expectedSecondResponse
      HarchWeb.renderResponse pureApplication spacesRequest `shouldReturn` expectedSpacesResponse
      HarchWeb.renderResponse pureApplication apiStatusRequest `shouldReturn` expectedApiStatusResponse
      HarchWeb.renderResponse pureApplication apiSecondRequest `shouldReturn` expectedApiSecondResponse
      HarchWeb.renderResponse pureApplication notFoundRequest `shouldReturn` expectedNotFoundResponse
      HarchWeb.renderResponse pureApplication apiNotFoundRequest `shouldReturn` expectedApiNotFoundResponse

    it "adapts the pure application to WAI without changing rendered pages" $ do
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["es", "second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      renderedSecondResponse <- readResponseBody secondResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Second</h1>" renderedSecondResponse `shouldBe` True
      Text.isInfixOf "<script nonce=\"" renderedSecondResponse `shouldBe` True

      spacesResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["spaces"])
      Wai.responseStatus spacesResponse `shouldBe` Http.status200
      renderedSpacesResponse <- readResponseBody spacesResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Site under construction</h1>" renderedSpacesResponse `shouldBe` True

      apiStatusResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "status"])
      Wai.responseStatus apiStatusResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiStatusResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiStatusResponse
        `shouldReturn` "{\"status\":\"ok\",\"locale\":\"en\"}"

      apiSecondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "second"])
      Wai.responseStatus apiSecondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiSecondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiSecondResponse
        `shouldReturn` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"

      missingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["missing"])
      Wai.responseStatus missingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      renderedMissingResponse <- readResponseBody missingResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Not Found</h1>" renderedMissingResponse `shouldBe` True

      apiMissingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "missing"])
      Wai.responseStatus apiMissingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders apiMissingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiMissingResponse
        `shouldReturn` "{\"error\":\"not-found\"}"

    it "emits second-page observability through the WAI facade" $ do
      requestObservabilityReference <- newIORef Nothing
      let observingApplication =
            pureApplication
              { HarchWeb.reportRequestObservability =
                  writeIORef requestObservabilityReference . Just
              }
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication observingApplication) (waiRequest ["second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      responseBody <- readResponseBody secondResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Second</h1>" responseBody `shouldBe` True
      Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True
      maybeRequestObservability <- readIORef requestObservabilityReference
      case maybeRequestObservability of
        Nothing ->
          expectationFailure "expected request observability to be reported for /second"
        Just requestObservability -> do
          let requestSpan = Observability.observabilityRequestSpan requestObservability
              requestAttributes = Observability.requestSpanAttributes requestSpan
          Observability.requestSpanDisplayName requestSpan `shouldBe` "GET /second"
          lookupTextObservabilityAttribute "url.path" requestAttributes `shouldBe` Just "/second"
          lookupTextObservabilityAttribute "http.route" requestAttributes `shouldBe` Just "/second"

    it "adapts forwarded path prefixes through the WAI facade for pages and static assets" $ do
      let prefixedPageRequest =
            (waiRequest ["app", "second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedAssetRequest =
            (waiRequest ["app", "assets", "navigation.js"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedApplication =
            buildApp
              navigationAppConfig
                { requestPolicy =
                    (requestPolicy navigationAppConfig)
                      { trustForwardedHeaders = True
                      }
                }
      pageResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedPageRequest
      Wai.responseStatus pageResponse `shouldBe` Http.status200
      pageBody <- readResponseBody pageResponse
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\">Home</a><a href=\"/app/second\" data-page-link=\"true\" aria-current=\"page\">Second</a>" pageBody `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/app/assets/navigation.js\" defer></script>" pageBody `shouldBe` True

      assetResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedAssetRequest
      Wai.responseStatus assetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders assetResponse) `shouldBe` Just "application/javascript; charset=utf-8"

    it "returns HTTP 500 for required page failures while keeping unaffected routes unchanged" $ do
      let failingApplication =
            buildAppWithDatabase
              defaultAppConfig
              ( buildSeededDatabaseEffect
                  DatabaseSeed
                    { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                      spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                      englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                      spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                    }
              )
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest ["second"])
      Wai.responseStatus secondResponse `shouldBe` Http.internalServerError500
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      secondResponseBody <- readResponseBody secondResponse
      secondResponseBody `shouldSatisfy` Text.isInfixOf "Second page content is temporarily unavailable."

      homeResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest [])
      Wai.responseStatus homeResponse `shouldBe` Http.status302
      lookup Http.hLocation (Wai.responseHeaders homeResponse) `shouldBe` Just "/spaces"

    it "is structurally complete enough to render supported and not-found shells" $ do
      homePage <- renderPage defaultAppConfig homeRequest
      secondPage <- renderPage defaultAppConfig secondRequest
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      HarchWeb.documentTitle (HarchWeb.pageShell pureApplication homePage) `shouldBe` "web-api: Home"
      Text.isInfixOf "Browse the second page" (HarchWeb.renderHtml (HarchWeb.documentMainContent (HarchWeb.pageShell pureApplication homePage)))
        `shouldBe` True
      HarchWeb.documentTitle (HarchWeb.pageShell pureApplication secondPage) `shouldBe` "web-api: Second"
      HarchWeb.documentBootstrapHooks (HarchWeb.pageShell pureApplication secondPage) `shouldBe` ["second-page"]
      HarchWeb.documentTitle (HarchWeb.pageShell pureApplication notFoundPage) `shouldBe` "web-api: Not Found"
      Text.isInfixOf "The requested page could not be found." (HarchWeb.renderHtml (HarchWeb.documentMainContent (HarchWeb.pageShell pureApplication notFoundPage)))
        `shouldBe` True

    it "can grow from page responses to API responses without changing route matching" $ do
      renderedResponse <- HarchWeb.renderResponse pureApplication apiSecondRequest
      case renderedResponse of
        HarchWeb.BodyResponse body -> HarchWeb.responseBody body `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected body response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected body response"
        HarchWeb.RedirectResponse _ _ -> expectationFailure "expected body response"
        HarchWeb.ClientActionBodyResponse _ -> expectationFailure "expected body response"
        HarchWeb.EventStreamResponse _ _ -> expectationFailure "expected body response"

  describe "buildRuntimeApp" $ do
    it "builds the runtime database effect from the environment config" $ do
      let runtimeEnvironmentConfig =
            defaultAppEnvironmentConfig
              { databaseConfig =
                  postgresTestConfig
                    { databaseName = "runtime_db",
                      databaseUser = "runtime_user"
                    }
              }
          runtimeApplication =
            buildRuntimeAppWithDatabaseBuilder
              defaultAppConfig
              ( \databaseRuntimeConfig ->
                  buildSeededDatabaseEffect
                    defaultDatabaseSeed
                      { englishSecondPageData =
                          Right
                            SecondPageData
                              { secondPageDataSummary =
                                  "runtime:" <> databaseName databaseRuntimeConfig <> ":" <> databaseUser databaseRuntimeConfig,
                                secondPageDataHighlights = ["configured-from-environment"]
                              }
                      }
              )
              runtimeEnvironmentConfig
      runtimeResponse <- HarchWeb.renderResponse runtimeApplication apiSecondRequest
      case runtimeResponse of
        HarchWeb.BodyResponse body ->
          HarchWeb.responseBody body
            `shouldBe` "{\"summary\":\"runtime:runtime_db:runtime_user\",\"highlights\":[\"configured-from-environment\"]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected body response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected body response"
        HarchWeb.RedirectResponse _ _ -> expectationFailure "expected body response"
        HarchWeb.ClientActionBodyResponse _ -> expectationFailure "expected body response"
        HarchWeb.EventStreamResponse _ _ -> expectationFailure "expected body response"
      HarchWeb.reportRequestObservability
        runtimeApplication
        ( Observability.buildRequestObservability
            "GET"
            "http"
            "/second"
            "/second"
            500
            Observability.BodyResponseKind
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "exception.type",
                  Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                }
            ]
        )
      HarchWeb.reportConnectionObservability
        runtimeApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog runtimeApplication "runtime failure detail"

    it "exports runtime request observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability "GET" "http" "/api/status" "/api/status" 200 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"service.name\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"web-api\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /api/status\""
        requestBodyText `shouldSatisfy` (not . Text.isInfixOf "\"STATUS_CODE_ERROR\"")

    it "keeps runtime request reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability "GET" "http" "/api/second" "/api/second" 500 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

    it "exports runtime connection observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
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
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION insecure-connection-denied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""

    it "keeps runtime connection reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultDatabaseEffect)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
          ( Observability.buildConnectionObservability
              "CONNECTION client-closed-connection-prematurely"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  }
              ]
          )
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

  describe "run" $ do
    it "starts the runtime server from an explicit environment and app config" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Http,
                            listenerTls = Nothing,
                            listenerAcme = Nothing
                          }
                      ]
                  }
          serverThreadId <- forkIO $ do
            result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
          responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForRuntimeServerExit completionReference
          hClose outputHandle
          readFile outputPath
            `shouldReturn` unlines
              [ "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
              ]

    it "surfaces listener bind failures through the default app config" $
      withDefaultRuntimePortUnavailable $
        withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle ->
          runWithConfig outputHandle defaultAppConfig defaultAppEnvironmentConfig
            `shouldThrow` isAlreadyInUseError

    it "serves database-backed runtime routes from the supplied environment config" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            let runtimeAppConfig =
                  defaultAppConfig
                    { listenerConfigs =
                        [ ListenerConfig
                            { listenerHost = tcpEndpointHost unusedEndpoint,
                              listenerPort = tcpEndpointPort unusedEndpoint,
                              listenerScheme = Http,
                              listenerTls = Nothing,
                              listenerAcme = Nothing
                            }
                        ]
                    }
                runtimeEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig = defaultMigrationPostgresConfig
                    }
            serverThreadId <- forkIO $ do
              result <- try (runWithConfig outputHandle runtimeAppConfig runtimeEnvironmentConfig) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/second"
            responseText `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
            completionResult <- readIORef completionReference
            completionResult `shouldSatisfy` isNothing
            killThread serverThreadId
            waitForRuntimeServerExit completionReference
            hClose outputHandle

    it "announces parsed HTTPS listener configs before surfacing manual TLS startup failures" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        { certificateFile = "/tmp/missing-cert.pem",
                                          privateKeyFile = "/tmp/missing-key.pem"
                                        }
                                  },
                            listenerAcme = Nothing
                          }
                      ]
                  }
          result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either IOException ())
          hClose outputHandle
          case result of
            Left exception ->
              displayException exception
                `shouldContain` "Manual TLS certificate file does not exist: /tmp/missing-cert.pem"
            Right () ->
              expectationFailure "expected runWithConfig to fail when manual TLS files are missing"
          readFile outputPath
            `shouldReturn` ("Parsed listener config: https://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint) <> "\n")

    it "writes startup output to the supplied handle for isolated tests and serves real requests" $
      withClearedAppEnvironment $
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempDirectory "web-api-run" $ \tempDirectory ->
            withCurrentDirectory tempDirectory $ do
              writeFile ".env" ("LISTENER_0_PORT=" <> show (tcpEndpointPort unusedEndpoint) <> "\n")
              withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
                completionReference <- newIORef Nothing
                serverThreadId <- forkIO $ do
                  result <- try (run outputHandle) :: IO (Either SomeException ())
                  writeIORef completionReference (Just result)
                responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
                responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                completionResult <- readIORef completionReference
                completionResult `shouldSatisfy` isNothing
                killThread serverThreadId
                waitForRuntimeServerExit completionReference
                hClose outputHandle
                readFile outputPath
                  `shouldReturn` unlines
                    [ "Loaded config file: ./.env",
                      "Config file missing: ./.env.local",
                      "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                      "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
                    ]

    it "fails explicitly when the layered runtime startup config is invalid" $
      withClearedAppEnvironment $
        withSystemTempDirectory "web-api-run-invalid" $ \tempDirectory ->
          withCurrentDirectory tempDirectory $ do
            writeFile ".env" "LISTENER_0_PORT=0\n"
            result <-
              ( try $
                  withSystemTempFile "web-api-output.txt" $ \_ outputHandle -> do
                    run outputHandle
                    hClose outputHandle
              ) ::
                IO (Either IOException ())
            case result of
              Left exception ->
                displayException exception
                  `shouldContain` "Failed to load app startup config: AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
              Right () ->
                expectationFailure "expected run to fail on invalid runtime startup config"

stripVolatileDatabaseTimingResponse :: HarchWeb.Response route context -> HarchWeb.Response route context
stripVolatileDatabaseTimingResponse response =
  case response of
    HarchWeb.PageResponse page -> HarchWeb.PageResponse page
    HarchWeb.PageResponseWithMetadata responseBody page ->
      HarchWeb.PageResponseWithMetadata (stripVolatileDatabaseTimingResponseBody responseBody) page
    HarchWeb.BodyResponse responseBody ->
      HarchWeb.BodyResponse (stripVolatileDatabaseTimingResponseBody responseBody)
    HarchWeb.RedirectResponse responseBody location ->
      HarchWeb.RedirectResponse (stripVolatileDatabaseTimingResponseBody responseBody) location
    HarchWeb.ClientActionBodyResponse actionResponse ->
      HarchWeb.ClientActionBodyResponse actionResponse
    HarchWeb.EventStreamResponse responseBody eventSource ->
      HarchWeb.EventStreamResponse (stripVolatileDatabaseTimingResponseBody responseBody) eventSource

stripVolatileDatabaseTimingResponseBody :: HarchWeb.ResponseBody -> HarchWeb.ResponseBody
stripVolatileDatabaseTimingResponseBody responseBody =
  responseBody
    { HarchWeb.responseObservabilityAttributes =
        filter
          (not . isVolatileDatabaseTimingAttribute)
          (HarchWeb.responseObservabilityAttributes responseBody)
    }

isVolatileDatabaseTimingAttribute :: Observability.ObservabilityAttribute -> Bool
isVolatileDatabaseTimingAttribute attribute =
  Observability.attributeName attribute
    `elem` [ "db.operation.start_monotonic_ns",
             "db.operation.duration_ns"
           ]

lookupTextObservabilityAttribute :: Text -> [Observability.ObservabilityAttribute] -> Maybe Text
lookupTextObservabilityAttribute attributeName attributes =
  find ((== attributeName) . Observability.attributeName) attributes >>= attributeTextValue
  where
    attributeTextValue attribute =
      case Observability.attributeValue attribute of
        Observability.TextAttribute value -> Just value
        Observability.IntAttribute _ -> Nothing
