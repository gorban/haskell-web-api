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
    unavailableAccountWorkflow,
  )
where

import Control.Exception (SomeException, displayException, evaluate, try)
import Control.Monad (forM_)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb qualified
import HarchWeb.Account qualified as HarchAccount
import HarchWeb.Email qualified as Email
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.Site qualified as Site
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush, stderr)
import WebApi.Account (AccountProfileStore (..), AccountStore (..), AccountStoreError (..))
import WebApi.AccountPages (handleAccountAction)
import WebApi.App.Shell (buildAppPageShellConfig)
import WebApi.AppEffect (AccountWorkflow (..))
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    AppStartupConfig (..),
    AppStartupConfigLoadError,
    DatabaseConfig,
    ListenerConfig (..),
    ListenerScheme (..),
    SmtpDeliveryConfig (..),
    defaultAppEnvironmentConfig,
    loadAppStartupConfig,
  )
import WebApi.Database (DatabaseEffect, defaultDatabaseEffect)
import WebApi.Login (AccountCredentialStore (..), AccountCredentialStoreError (..))
import WebApi.Mfa (MfaStore (..), MfaStoreError (..))
import WebApi.Postgres (buildRuntimePostgresAccountCredentialStore, buildRuntimePostgresAccountProfileStore, buildRuntimePostgresAccountSessionStore, buildRuntimePostgresAccountStore, buildRuntimePostgresDatabaseEffect, buildRuntimePostgresMfaStore)
import WebApi.Response (selectResponseWithDatabaseAndAccountWorkflow)
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
    defaultRequestContext,
    renderRoutePath,
    requestContextFromWaiRequest,
    routeCodec,
  )
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..))

buildAppWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabase config databaseEffect =
  buildAppWithDatabaseAndAccountWorkflow config databaseEffect unavailableAccountWorkflow

buildAppWithDatabaseAndAccountWorkflow :: AppConfig -> DatabaseEffect -> AccountWorkflow -> HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabaseAndAccountWorkflow config databaseEffect accountWorkflow =
  buildAppWithDatabaseAndReporters config databaseEffect accountWorkflow ignoreRequestObservability ignoreConnectionObservability ignoreApplicationLog

buildAppWithDatabaseAndReporters ::
  AppConfig ->
  DatabaseEffect ->
  AccountWorkflow ->
  (Observability.RequestObservability -> IO ()) ->
  (Observability.ConnectionObservability -> IO ()) ->
  (Text.Text -> IO ()) ->
  HarchWeb.Application AppRoute AppRequestContext
buildAppWithDatabaseAndReporters config databaseEffect !accountWorkflow requestObservabilityReporter connectionObservabilityReporter applicationLogReporter =
  config `seq`
    Site.buildSiteApplication
      ( (Site.simpleSite "web-api" defaultRequestContext routeCodec (buildAppPageShellConfig config) (buildAppSiteRoutes config databaseEffect accountWorkflow))
          { Site.siteRequestContextFromRequest =
              requestContextFromWaiRequest (HarchWeb.trustForwardedHeaders (requestPolicy config)),
            Site.siteStaticAssets = staticAssets config,
            Site.siteNavigationRuntimePathPrefix = requestPathPrefix,
            Site.siteRequestPolicy = requestPolicy config,
            Site.siteReportRequestObservability = requestObservabilityReporter,
            Site.siteReportConnectionObservability = connectionObservabilityReporter,
            Site.siteReportApplicationLog = applicationLogReporter,
            Site.siteHandleClientAction = handleAccountAction accountWorkflow
          }
      )

buildApp :: AppConfig -> HarchWeb.Application AppRoute AppRequestContext
buildApp config =
  buildAppWithDatabase config defaultDatabaseEffect

buildAppSiteRoutes :: AppConfig -> DatabaseEffect -> AccountWorkflow -> [Site.SiteRoute AppRoute AppRequestContext]
buildAppSiteRoutes config databaseEffect accountWorkflow =
  let renderSelectedResponse = selectResponseWithDatabaseAndAccountWorkflow config databaseEffect accountWorkflow
   in [ Site.SiteRoute
          { Site.siteRouteValue = HomeRoute,
            Site.siteRouteNavigationLabel = Just "Home",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = SecondRoute,
            Site.siteRouteNavigationLabel = Just "Second",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = SpacesRoute,
            Site.siteRouteNavigationLabel = Just "Spaces",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = RegistrationRoute,
            Site.siteRouteNavigationLabel = Just "Create account",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = EmailVerificationRoute,
            Site.siteRouteNavigationLabel = Nothing,
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = MfaEnrollmentRoute,
            Site.siteRouteNavigationLabel = Nothing,
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = LoginRoute,
            Site.siteRouteNavigationLabel = Just "Sign in",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = LogoutRoute,
            Site.siteRouteNavigationLabel = Nothing,
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = ProfileRoute,
            Site.siteRouteNavigationLabel = Just "Profile",
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = StatusApiRoute,
            Site.siteRouteNavigationLabel = Nothing,
            Site.siteRouteResponse = renderSelectedResponse
          },
        Site.SiteRoute
          { Site.siteRouteValue = NotFoundRoute,
            Site.siteRouteNavigationLabel = Nothing,
            Site.siteRouteResponse = renderSelectedResponse
          }
      ]

buildRuntimeApp :: AppConfig -> AppEnvironmentConfig -> HarchWeb.Application AppRoute AppRequestContext
buildRuntimeApp config environmentConfig =
  let databaseConfiguration = databaseConfig environmentConfig
      databaseEffect = buildRuntimePostgresDatabaseEffect databaseConfiguration
      accountWorkflow = buildRuntimeAccountWorkflow environmentConfig
   in databaseEffect `seq`
        accountWorkflow `seq`
          buildAppWithDatabaseAndReporters
            config
            databaseEffect
            accountWorkflow
            (runtimeRequestObservabilityReporter config)
            (runtimeConnectionObservabilityReporter config)
            runtimeApplicationLogReporter

buildRuntimeAppWithDatabaseBuilder ::
  AppConfig ->
  (DatabaseConfig -> DatabaseEffect) ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AppRequestContext
buildRuntimeAppWithDatabaseBuilder config buildDatabaseEffect environmentConfig =
  let databaseEffect = buildDatabaseEffect (databaseConfig environmentConfig)
   in databaseEffect `seq`
        buildAppWithDatabaseAndReporters
          config
          databaseEffect
          unavailableAccountWorkflow
          (runtimeRequestObservabilityReporter config)
          (runtimeConnectionObservabilityReporter config)
          runtimeApplicationLogReporter

buildRuntimeAccountWorkflow :: AppEnvironmentConfig -> AccountWorkflow
buildRuntimeAccountWorkflow !environmentConfig =
  let databaseConfiguration = databaseConfig environmentConfig
   in databaseConfiguration `seq`
        AccountWorkflow
          { accountWorkflowStore = buildRuntimePostgresAccountStore databaseConfiguration,
            accountWorkflowEmailDelivery = runtimeEmailDelivery (smtpDeliveryConfig environmentConfig),
            accountWorkflowPasswordHasher = Password.hashPassword,
            accountWorkflowClock = getMonotonicTimeNSec,
            accountWorkflowMfaStore = buildRuntimePostgresMfaStore databaseConfiguration,
            accountWorkflowCredentialStore = buildRuntimePostgresAccountCredentialStore databaseConfiguration,
            accountWorkflowSessionStore = buildRuntimePostgresAccountSessionStore databaseConfiguration,
            accountWorkflowProfileStore = buildRuntimePostgresAccountProfileStore databaseConfiguration,
            accountWorkflowTotpEncryptionKey = totpEncryptionKey environmentConfig,
            accountWorkflowTotpClock = floor <$> getPOSIXTime,
            accountWorkflowVerificationUrl = runtimeVerificationUrl (publicBaseUrl environmentConfig)
          }

runtimeEmailDelivery :: SmtpDeliveryConfig -> Email.EmailDelivery
runtimeEmailDelivery smtpConfig =
  case Email.mkEmailAddress (smtpDeliverySender smtpConfig) of
    Just sender ->
      case Email.mkAuthenticatedSmtpConfig
        (smtpDeliveryHost smtpConfig)
        (fromIntegral (smtpDeliveryPort smtpConfig))
        (smtpDeliveryHeloName smtpConfig)
        sender
        (smtpDeliveryUsername smtpConfig)
        (smtpDeliveryPassword smtpConfig) of
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

runWithConfig :: Handle -> AppConfig -> AppEnvironmentConfig -> IO ()
runWithConfig outputHandle appConfig !environmentConfig = do
  announceParsedListenerConfigs outputHandle appConfig
  HarchWeb.runServer outputHandle appConfig (buildRuntimeApp appConfig environmentConfig)

run :: Handle -> IO ()
run outputHandle = do
  configFileStatuses <- loadDefaultStartupConfigFileStatuses
  loadAppStartupConfig
    >>= either throwStartupLoadError (runLoadedStartupConfig outputHandle configFileStatuses)

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

runtimeRequestObservabilityReporter :: AppConfig -> Observability.RequestObservability -> IO ()
runtimeRequestObservabilityReporter config =
  runtimeObservabilityReporter
    config
    "request observability"
    (HarchWeb.exportRequestObservabilityToOtlp "web-api")

runtimeConnectionObservabilityReporter :: AppConfig -> Observability.ConnectionObservability -> IO ()
runtimeConnectionObservabilityReporter config =
  runtimeObservabilityReporter
    config
    "connection observability"
    (HarchWeb.exportConnectionObservabilityToOtlp "web-api")

runtimeObservabilityReporter ::
  (Show observability) =>
  AppConfig ->
  Text.Text ->
  (HarchWeb.OtlpExporter -> observability -> IO ()) ->
  observability ->
  IO ()
runtimeObservabilityReporter config observabilityKind exportObservability observabilityValue = do
  TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show observabilityValue))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter -> do
    exportResult <-
      try (exportObservability exporter observabilityValue) ::
        IO (Either SomeException ())
    either
      (runtimeApplicationLogReporter . exportFailureMessage observabilityKind)
      (const (hFlush stderr))
      exportResult

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
      accountWorkflowSessionStore = unavailableAccountSessionStore,
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
      consumeRecoveryCodeHash = \_ _ _ -> unavailableResult mfaPersistenceUnavailable
    }

unavailableAccountCredentialStore :: AccountCredentialStore
unavailableAccountCredentialStore =
  AccountCredentialStore
    { findAccountCredentialByEmail = const (unavailableResult accountCredentialsUnavailable),
      findAccountCredentialByUsername = const (unavailableResult accountCredentialsUnavailable)
    }

unavailableAccountSessionStore :: AccountSessionStore
unavailableAccountSessionStore =
  AccountSessionStore
    { saveAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      loadAccountSession = const (unavailableResult AccountSessionStoreUnavailable),
      invalidateAccountSession = const (unavailableResult AccountSessionStoreUnavailable)
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

accountProfilesUnavailable :: AccountStoreError
accountProfilesUnavailable =
  AccountStoreUnavailable "account profiles are not configured"

unavailableResult :: error -> IO (Either error value)
unavailableResult = pure . Left
