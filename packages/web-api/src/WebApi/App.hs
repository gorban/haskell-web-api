{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module WebApi.App
  ( buildAppWithDatabase,
    buildAppWithDatabaseAndAccountWorkflow,
    buildApp,
    buildRuntimeAccountWorkflow,
    buildRuntimeApp,
    buildRuntimeAppWithDatabaseBuilder,
    run,
    runWithConfig,
    runtimeRequestObservabilityReporter,
    unavailableAccountWorkflow,
  )
where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_, forever, unless)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.IO qualified as TextIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb qualified
import HarchWeb.Account qualified as HarchAccount
import HarchWeb.Action (decodeAction)
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.Site qualified as Site
import Network.HTTP.Client qualified as HttpClient
import Numeric.Natural (Natural)
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush, stderr)
import System.IO.Unsafe (unsafePerformIO)
import WebApi.Account (AccountProfileStore (..), AccountStore (..), AccountStoreError (..))
import WebApi.AccountPages (AccountAction, accountActions, handleAccountAction)
import WebApi.Api.Endpoints (secondApiRouteDefinition, statusApiRouteDefinition)
import WebApi.App.Shell (buildAppPageShellConfig)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    AppMode (..),
    AppStartupConfig (..),
    AppStartupConfigLoadError,
    DatabaseConfig,
    ListenerConfig (..),
    ListenerScheme (..),
    SmtpDeliveryConfig (..),
    defaultAppEnvironmentConfig,
    loadAppStartupConfig,
  )
import WebApi.Database (PageRepository, defaultPageRepository)
import WebApi.Login (AccountCredentialStore (..), AccountCredentialStoreError (..), LoginAttemptStore (..), LoginAttemptStoreError (..))
import WebApi.Mfa (MfaStore (..), MfaStoreError (..))
import WebApi.Postgres.AccountRepository
  ( buildRuntimePostgresAccountCredentialStore,
    buildRuntimePostgresAccountProfileStore,
    buildRuntimePostgresAccountStore,
  )
import WebApi.Postgres.LoginAttemptRepository (buildRuntimePostgresLoginAttemptStore)
import WebApi.Postgres.MfaEnrollmentSessionRepository (buildRuntimePostgresMfaEnrollmentSessionStore)
import WebApi.Postgres.MfaRepository (buildRuntimePostgresMfaStore)
import WebApi.Postgres.Runtime (buildRuntimePostgresPageRepository)
import WebApi.Postgres.SessionRepository (buildRuntimePostgresAccountSessionStore)
import WebApi.Response (selectResponseWithDatabaseAndAccountWorkflow)
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
    defaultRequestContext,
    renderRoutePath,
    requestContextFromWaiRequest,
    routeCodec,
  )
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..), MfaEnrollmentSessionStore (..), MfaEnrollmentSessionStoreError (..))

buildAppWithDatabase ::
  AppConfig ->
  PageRepository ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabase config pageRepository =
  buildAppWithDatabaseAndAccountWorkflow config pageRepository unavailableAccountWorkflow

buildAppWithDatabaseAndAccountWorkflow ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow =
  buildAppWithDatabaseAndReporters config pageRepository accountWorkflow ignoreRequestObservability ignoreConnectionObservability ignoreApplicationLog

buildAppWithDatabaseAndReporters ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  (Observability.RequestObservability -> IO ()) ->
  (Observability.ConnectionObservability -> IO ()) ->
  (Text.Text -> IO ()) ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildAppWithDatabaseAndReporters config pageRepository !accountWorkflow requestObservabilityReporter connectionObservabilityReporter applicationLogReporter =
  Site.buildSiteApplication
    ( ( Site.simpleSite
          "web-api"
          defaultRequestContext
          routeCodec
          (buildAppPageShellConfig config)
          appNavigationRoutes
          (buildAppRouteDefinition config pageRepository accountWorkflow)
      )
        { Site.siteRequestContextFromRequest =
            requestContextFromWaiRequest (HarchWeb.forwardedHeaderTrust (requestPolicy config)),
          Site.siteStaticAssets = staticAssets config,
          Site.siteNavigationRuntimePathPrefix = requestPathPrefix,
          Site.siteRequestPolicy = requestPolicy config,
          Site.siteDecodeClientAction = decodeAction accountActions,
          Site.siteReportRequestObservability = requestObservabilityReporter,
          Site.siteReportConnectionObservability = connectionObservabilityReporter,
          Site.siteReportApplicationLog = applicationLogReporter,
          Site.siteHandleClientAction = handleAccountAction accountWorkflow
        }
    )

buildApp :: AppConfig -> HarchWeb.Application AppRoute AccountAction AppRequestContext
buildApp config =
  buildAppWithDatabase config defaultPageRepository

appNavigationRoutes :: [AppRoute]
appNavigationRoutes =
  [HomeRoute, SecondRoute, SpacesRoute, RegistrationRoute, LoginRoute, ProfileRoute]

buildAppRouteDefinition ::
  AppConfig ->
  PageRepository ->
  AccountWorkflow ->
  AppRoute ->
  Site.RouteDefinition AppRoute AppRequestContext
buildAppRouteDefinition config pageRepository accountWorkflow route =
  case route of
    StatusApiRoute -> statusApiRouteDefinition
    SecondApiRoute -> secondApiRouteDefinition pageRepository
    _ ->
      Site.RouteDefinition
        { Site.routeNavigationLabel = routeNavigationLabel route,
          Site.routeMethods = HarchWeb.routeMethodPolicyMethods (HarchWeb.routeMethods routeCodec route),
          Site.routeResponse =
            \_ -> selectResponseWithDatabaseAndAccountWorkflow config pageRepository accountWorkflow
        }

routeNavigationLabel :: AppRoute -> Maybe Text.Text
routeNavigationLabel route = lookup route navigationLabels
  where
    navigationLabels =
      [ (HomeRoute, "Home"),
        (SecondRoute, "Second"),
        (SpacesRoute, "Spaces"),
        (RegistrationRoute, "Create account"),
        (LoginRoute, "Sign in"),
        (ProfileRoute, "Profile")
      ]

buildRuntimeApp ::
  AppConfig ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildRuntimeApp config environmentConfig =
  let databaseConfiguration = databaseConfig environmentConfig
      pageRepository = buildRuntimePostgresPageRepository databaseConfiguration
      accountWorkflow = buildRuntimeAccountWorkflow environmentConfig
   in buildAppWithDatabaseAndReporters
        (withPublicBaseUrlRedirectAuthority environmentConfig config)
        pageRepository
        accountWorkflow
        (runtimeRequestObservabilityReporter (appMode environmentConfig) config)
        (runtimeConnectionObservabilityReporter (appMode environmentConfig) config)
        runtimeApplicationLogReporter

buildRuntimeAppWithDatabaseBuilder ::
  AppConfig ->
  (DatabaseConfig -> PageRepository) ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildRuntimeAppWithDatabaseBuilder config buildPageRepository environmentConfig =
  let pageRepository = buildPageRepository (databaseConfig environmentConfig)
   in buildAppWithDatabaseAndReporters
        (withPublicBaseUrlRedirectAuthority environmentConfig config)
        pageRepository
        unavailableAccountWorkflow
        (runtimeRequestObservabilityReporter (appMode environmentConfig) config)
        (runtimeConnectionObservabilityReporter (appMode environmentConfig) config)
        runtimeApplicationLogReporter

buildRuntimeAccountWorkflow :: AppEnvironmentConfig -> AccountWorkflow
buildRuntimeAccountWorkflow !environmentConfig =
  let databaseConfiguration = databaseConfig environmentConfig
   in AccountWorkflow
        { accountWorkflowStore = buildRuntimePostgresAccountStore databaseConfiguration,
          accountWorkflowEmailDelivery = runtimeEmailDelivery (smtpDeliveryConfig environmentConfig),
          accountWorkflowPasswordHasher = Password.hashPassword,
          accountWorkflowClock = getMonotonicTimeNSec,
          accountWorkflowMfaStore = buildRuntimePostgresMfaStore databaseConfiguration,
          accountWorkflowCredentialStore = buildRuntimePostgresAccountCredentialStore databaseConfiguration,
          accountWorkflowLoginAttemptStore = buildRuntimePostgresLoginAttemptStore databaseConfiguration,
          accountWorkflowSessionStore = buildRuntimePostgresAccountSessionStore databaseConfiguration,
          accountWorkflowMfaEnrollmentSessionStore = buildRuntimePostgresMfaEnrollmentSessionStore databaseConfiguration,
          accountWorkflowProfileStore = buildRuntimePostgresAccountProfileStore databaseConfiguration,
          accountWorkflowTotpEncryptionKey = totpEncryptionKey environmentConfig,
          accountWorkflowTotpClock = floor <$> getPOSIXTime,
          accountWorkflowVerificationUrl = runtimeVerificationUrl (publicBaseUrl environmentConfig)
        }

runtimeEmailDelivery :: SmtpDeliveryConfig -> Email.EmailDelivery
runtimeEmailDelivery smtpConfig =
  case Email.mkEmailAddress (smtpDeliverySender smtpConfig) of
    Just sender ->
      case Email.mkSmtpConfig
        Email.SmtpConfigInput
          { Email.smtpInputHost = Email.smtpServerHost (smtpDeliveryHost smtpConfig),
            Email.smtpInputPort = fromIntegral (smtpDeliveryPort smtpConfig),
            Email.smtpInputHeloName = Email.smtpServerHeloName (smtpDeliveryHeloName smtpConfig),
            Email.smtpInputEnvelopeSender = sender,
            Email.smtpInputAuthentication =
              Just
                ( Email.smtpAuthentication
                    (Email.smtpLoginUsername (smtpDeliveryUsername smtpConfig))
                    (Email.smtpLoginPassword (smtpDeliveryPassword smtpConfig))
                )
          } of
        Just configuredSmtp -> Email.EmailDelivery (Email.deliverSmtpEmail configuredSmtp)
        Nothing -> unavailableEmailDelivery
    Nothing -> unavailableEmailDelivery
  where
    unavailableEmailDelivery = Email.EmailDelivery (\_ -> ioError (userError "SMTP delivery configuration is invalid"))

runtimeVerificationUrl :: Text.Text -> AppRequestContext -> HarchAccount.EmailVerificationToken -> Text.Text
runtimeVerificationUrl baseUrl requestContext verificationToken =
  trimTrailingSlash baseUrl
    <> renderRoutePath (HarchWeb.RouteRequest EmailVerificationRoute requestContext)
    <> "?token="
    <> HarchAccount.emailVerificationTokenText verificationToken

trimTrailingSlash :: Text.Text -> Text.Text
trimTrailingSlash value =
  case Text.unsnoc value of
    Just (prefix, '/') -> prefix
    _ -> value

-- | The HTTPS-upgrade redirect must never echo a client-supplied @Host@
-- header into its target (see 'HarchWeb.httpsRedirectAuthority'). Every
-- web-api deployment already declares a canonical @PUBLIC_BASE_URL@ (used
-- for email links), including a TLS-offloading deployment whose own
-- listeners are HTTP-only and so cannot supply
-- 'WebApi.Config.defaultHttpsRedirectAuthority''s listener-derived guess.
-- Prefer the host parsed from that required setting, falling back to the
-- config-derived guess only if @PUBLIC_BASE_URL@ is malformed.
withPublicBaseUrlRedirectAuthority :: AppEnvironmentConfig -> AppConfig -> AppConfig
withPublicBaseUrlRedirectAuthority !environmentConfig config =
  config
    { requestPolicy =
        (requestPolicy config)
          { HarchWeb.httpsRedirectAuthority =
              authorityFromPublicBaseUrl (publicBaseUrl environmentConfig)
                <|> HarchWeb.httpsRedirectAuthority (requestPolicy config)
          }
    }

authorityFromPublicBaseUrl :: Text.Text -> Maybe ByteString.ByteString
authorityFromPublicBaseUrl baseUrl =
  case Text.stripPrefix "https://" baseUrl <|> Text.stripPrefix "http://" baseUrl of
    Nothing -> Nothing
    Just afterScheme ->
      let authority = Text.takeWhile (\character -> character /= '/' && character /= '?' && character /= '#') afterScheme
          host = Text.takeWhile (/= ':') authority
       in if Text.null host then Nothing else Just (TextEncoding.encodeUtf8 host)

runWithConfig :: Handle -> AppConfig -> AppEnvironmentConfig -> IO ()
runWithConfig outputHandle appConfig !environmentConfig = do
  announceParsedListenerConfigs outputHandle appConfig
  HarchWeb.runServer outputHandle appConfig (buildRuntimeApp appConfig environmentConfig)

run :: Handle -> IO ()
run outputHandle = do
  configFileStatuses <- loadDefaultStartupConfigFileStatuses
  either throwStartupLoadError (runLoadedStartupConfig outputHandle configFileStatuses) =<< loadAppStartupConfig

throwStartupLoadError :: AppStartupConfigLoadError -> IO ()
throwStartupLoadError loadError =
  ioError (userError ("Failed to load app startup config: " <> show loadError))

runLoadedStartupConfig :: Handle -> [(FilePath, Bool)] -> AppStartupConfig -> IO ()
runLoadedStartupConfig
  outputHandle
  configFileStatuses
  AppStartupConfig
    { startupEnvironmentConfig = environmentConfig,
      startupAppConfig = appConfig
    } = do
    announceConfigFileStatuses outputHandle configFileStatuses
    runWithConfig outputHandle appConfig environmentConfig

loadDefaultStartupConfigFileStatuses :: IO [(FilePath, Bool)]
loadDefaultStartupConfigFileStatuses =
  traverse
    (\filePath -> (filePath,) <$> doesFileExist filePath)
    [".env", ".env.local"]

announceConfigFileStatuses :: Handle -> [(FilePath, Bool)] -> IO ()
announceConfigFileStatuses outputHandle configFileStatuses = do
  mapM_ (TextIO.hPutStrLn outputHandle . renderConfigFileStatus) configFileStatuses
  hFlush outputHandle
  where
    renderConfigFileStatus (filePath, fileExists) =
      if fileExists
        then "Loaded config file: ./" <> Text.pack filePath
        else "Config file missing: ./" <> Text.pack filePath

announceParsedListenerConfigs :: Handle -> AppConfig -> IO ()
announceParsedListenerConfigs outputHandle appConfig = do
  mapM_ (TextIO.hPutStrLn outputHandle . renderParsedListenerConfig) (listenerConfigs appConfig)
  hFlush outputHandle
  where
    renderParsedListenerConfig listenerConfig =
      "Parsed listener config: "
        <> listenerUrlPrefix (listenerScheme listenerConfig)
        <> listenerHost listenerConfig
        <> ":"
        <> Text.pack (show (listenerPort listenerConfig))

listenerUrlPrefix :: ListenerScheme -> Text.Text
listenerUrlPrefix listenerScheme =
  case listenerScheme of
    Http -> "http://"
    Https -> "https://"

runtimeRequestObservabilityReporter :: AppMode -> AppConfig -> Observability.RequestObservability -> IO ()
runtimeRequestObservabilityReporter mode config =
  runtimeObservabilityReporter
    mode
    config
    "request observability"
    (HarchWeb.exportRequestObservabilityToOtlp otlpHttpManager "web-api")

runtimeConnectionObservabilityReporter :: AppMode -> AppConfig -> Observability.ConnectionObservability -> IO ()
runtimeConnectionObservabilityReporter mode config =
  runtimeObservabilityReporter
    mode
    config
    "connection observability"
    (HarchWeb.exportConnectionObservabilityToOtlp otlpHttpManager "web-api")

-- | The unstructured "TRACE " stderr dump is a local-debugging convenience,
-- not a private structured log: it carries client.address, user_agent, and
-- other per-request PII with no level or config gate. Kept for Development
-- and Test (where CI/local debugging value outweighs the exposure), but
-- suppressed in Production, where it would otherwise print PII for every
-- real request forever with no way to turn it off.
runtimeObservabilityReporter ::
  (Show observability) =>
  AppMode ->
  AppConfig ->
  Text.Text ->
  (HarchWeb.OtlpExporter -> observability -> IO ()) ->
  observability ->
  IO ()
runtimeObservabilityReporter mode config observabilityKind exportObservability observabilityValue = do
  -- 'unless's own no-op branch (not a local @pure ()@) is deliberate: a
  -- bare @()@ literal here is a lazy value nothing downstream forces, the
  -- same "genuinely never scrutinized" HPC gap this codebase has hit
  -- before (see the AC decision record in docs/design-guidance.md).
  -- Delegating the no-op to 'Control.Monad.unless' keeps that triviality
  -- inside @base@, outside this project's own coverage boundary, rather
  -- than adding a forced tick for a value with nothing to assert about.
  unless (mode == Production) (TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show observabilityValue)))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter ->
    enqueueOtlpExport observabilityKind (exportObservability exporter observabilityValue)

-- | Decision record (AU, updated BZ 2026-08-21): the request-handling thread
-- must never block on network I/O to the OTLP collector, so
-- 'runtimeObservabilityReporter' hands each export off to this bounded queue
-- instead of awaiting 'exportObservability' itself. A background worker —
-- started once, lazily, via 'unsafePerformIO' \/ 'NOINLINE' — drains the
-- queue and performs the actual blocking POST off the request path. A full
-- queue drops the export and counts it rather than blocking the caller: a
-- slow or hung collector degrades trace completeness, never response
-- latency. This is deliberately an application-layer fix rather than a
-- framework one (per the framework-capability-gap protocol in
-- @docs/design-guidance.md@): 'web-api' is this tree's only caller of
-- @HarchWeb.exportRequestObservabilityToOtlp@\/@exportConnectionObservabilityToOtlp@
-- today, so there is no shared boundary yet to extend. If a second
-- application adopts OTLP export, promote this queue into
-- @HarchWeb.Observability@ instead of duplicating it there. As of BZ,
-- 'otlpHttpManager' below follows this exact same reasoning: it used to be
-- @HarchWeb.Observability.Otlp@'s own global, but a framework module owning
-- ambient mutable state means two applications (or two parallel test
-- suites) in one process unavoidably share it with no way to substitute
-- their own — so ownership moved here, the one real caller, the same place
-- this queue already lives, rather than becoming a second framework-owned
-- global. See @docs/design-guidance.md@'s \"Follow-up decision — BZ\" for
-- the full record.
enqueueOtlpExport :: Text.Text -> IO () -> IO ()
enqueueOtlpExport observabilityKind exportAction = do
  enqueued <- atomically $ do
    full <- isFullTBQueue otlpExportQueue
    unless full (writeTBQueue otlpExportQueue (observabilityKind, exportAction))
    pure (not full)
  unless enqueued $ do
    droppedTotal <- atomicModifyIORef' otlpExportDroppedCount (\count -> (count + 1, count + 1))
    runtimeApplicationLogReporter (otlpExportQueueFullMessage observabilityKind droppedTotal)

otlpExportQueueCapacity :: Natural
otlpExportQueueCapacity = 256

otlpExportQueue :: TBQueue (Text.Text, IO ())
{-# NOINLINE otlpExportQueue #-}
otlpExportQueue =
  unsafePerformIO $ do
    queue <- newTBQueueIO otlpExportQueueCapacity
    _ <- forkIO (otlpExportWorker queue)
    pure queue

otlpExportDroppedCount :: IORef Int
{-# NOINLINE otlpExportDroppedCount #-}
otlpExportDroppedCount =
  unsafePerformIO (newIORef 0)

otlpHttpManager :: HttpClient.Manager
{-# NOINLINE otlpHttpManager #-}
otlpHttpManager =
  unsafePerformIO HarchWeb.newOtlpHttpManager

otlpExportWorker :: TBQueue (Text.Text, IO ()) -> IO ()
otlpExportWorker queue = forever $ do
  (observabilityKind, exportAction) <- atomically (readTBQueue queue)
  exportResult <- try exportAction :: IO (Either SomeException ())
  either
    (runtimeApplicationLogReporter . exportFailureMessage observabilityKind)
    (const (hFlush stderr))
    exportResult

otlpExportQueueFullMessage :: Text.Text -> Int -> Text.Text
otlpExportQueueFullMessage observabilityKind droppedTotal =
  "Dropped "
    <> observabilityKind
    <> " OTLP export because the export queue is full ("
    <> Text.pack (show droppedTotal)
    <> " dropped total)"

exportFailureMessage :: Text.Text -> SomeException -> Text.Text
exportFailureMessage observabilityKind exportError =
  "Failed to export "
    <> observabilityKind
    <> " to OTLP: "
    <> Text.pack (displayException exportError)

runtimeApplicationLogReporter :: Text.Text -> IO ()
runtimeApplicationLogReporter =
  TextIO.hPutStrLn stderr . ("ERROR " <>)

ignoreRequestObservability :: Observability.RequestObservability -> IO ()
ignoreRequestObservability requestObservability =
  let ignored = mempty :: ()
   in Observability.forceRequestObservability requestObservability `seq` ignored `seq` evaluate ignored

ignoreConnectionObservability :: Observability.ConnectionObservability -> IO ()
ignoreConnectionObservability connectionObservability =
  let ignored = mempty :: ()
   in Observability.forceConnectionObservability connectionObservability `seq` ignored `seq` evaluate ignored

ignoreApplicationLog :: Text.Text -> IO ()
ignoreApplicationLog logEntry =
  let ignored = mempty :: ()
   in Text.length logEntry `seq` ignored `seq` evaluate ignored

unavailableAccountWorkflow :: AccountWorkflow
unavailableAccountWorkflow =
  AccountWorkflow
    { accountWorkflowStore = unavailableAccountStore,
      accountWorkflowEmailDelivery = Email.EmailDelivery (\_ -> ioError (userError "email delivery is not configured")),
      accountWorkflowPasswordHasher = Password.hashPassword,
      accountWorkflowClock = pure 0,
      accountWorkflowMfaStore = unavailableMfaStore,
      accountWorkflowCredentialStore = unavailableAccountCredentialStore,
      accountWorkflowLoginAttemptStore = unavailableLoginAttemptStore,
      accountWorkflowSessionStore = unavailableAccountSessionStore,
      accountWorkflowMfaEnrollmentSessionStore = unavailableMfaEnrollmentSessionStore,
      accountWorkflowProfileStore = unavailableAccountProfileStore,
      accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
      accountWorkflowTotpClock = pure 0,
      accountWorkflowVerificationUrl = \_ _ -> "https://invalid.example.test/verify"
    }

unavailableAccountStore :: AccountStore
unavailableAccountStore =
  AccountStore
    { createPendingAccount = const (unavailableResult accountPersistenceUnavailable),
      replaceEmailVerification = const (unavailableResult accountPersistenceUnavailable),
      findEmailVerification = const (unavailableResult accountPersistenceUnavailable),
      consumeEmailVerification = \_ _ -> unavailableResult accountPersistenceUnavailable
    }

unavailableMfaStore :: MfaStore
unavailableMfaStore =
  MfaStore
    { saveUnconfirmedTotpEnrollment = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      loadTotpEnrollment = const (unavailableResult mfaPersistenceUnavailable),
      confirmTotpEnrollment = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      loadUnusedRecoveryCodeHashes = const (unavailableResult mfaPersistenceUnavailable),
      consumeRecoveryCodeHash = \_ _ _ -> unavailableResult mfaPersistenceUnavailable,
      markTotpCodeUsed = \_ _ -> unavailableResult mfaPersistenceUnavailable
    }

unavailableAccountCredentialStore :: AccountCredentialStore
unavailableAccountCredentialStore =
  AccountCredentialStore
    { findAccountCredentialByEmail = const (unavailableResult accountCredentialsUnavailable),
      findAccountCredentialByUsername = const (unavailableResult accountCredentialsUnavailable)
    }

unavailableLoginAttemptStore :: LoginAttemptStore
unavailableLoginAttemptStore =
  LoginAttemptStore
    { recordLoginAttempt = \_ _ -> unavailableResult loginAttemptsUnavailable,
      loadRecentLoginAttempts = \_ _ -> unavailableResult loginAttemptsUnavailable
    }

unavailableAccountSessionStore :: AccountSessionStore
unavailableAccountSessionStore =
  AccountSessionStore
    { saveAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      loadAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      invalidateAccountSession = const (const (unavailableResult AccountSessionStoreUnavailable))
    }

unavailableMfaEnrollmentSessionStore :: MfaEnrollmentSessionStore
unavailableMfaEnrollmentSessionStore =
  MfaEnrollmentSessionStore
    { saveMfaEnrollmentSession = const (unavailableResult MfaEnrollmentSessionStoreUnavailable),
      loadMfaEnrollmentSession = const (unavailableResult MfaEnrollmentSessionStoreUnavailable),
      invalidateMfaEnrollmentSession = const (const (unavailableResult MfaEnrollmentSessionStoreUnavailable))
    }

unavailableAccountProfileStore :: AccountProfileStore
unavailableAccountProfileStore =
  AccountProfileStore
    { findAccountProfile = const (unavailableResult accountProfilesUnavailable)
    }

accountPersistenceUnavailable :: AccountStoreError
accountPersistenceUnavailable =
  AccountStoreUnavailable "account persistence is not configured"

mfaPersistenceUnavailable :: MfaStoreError
mfaPersistenceUnavailable =
  MfaStoreUnavailable "MFA persistence is not configured"

accountCredentialsUnavailable :: AccountCredentialStoreError
accountCredentialsUnavailable =
  AccountCredentialStoreUnavailable "account credentials are not configured"

loginAttemptsUnavailable :: LoginAttemptStoreError
loginAttemptsUnavailable =
  LoginAttemptStoreUnavailable "login-attempt persistence is not configured"

accountProfilesUnavailable :: AccountStoreError
accountProfilesUnavailable =
  AccountStoreUnavailable "account profiles are not configured"

unavailableResult :: error -> IO (Either error value)
unavailableResult = pure . Left
