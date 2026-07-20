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
import HarchWeb.Site qualified as Site
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush, stderr)
import WebApi.Account (AccountStore (..), AccountStoreError (..))
import WebApi.AccountPages (AccountWorkflow (..), handleAccountAction)
import WebApi.App.Shell (buildAppPageShellConfig)
import WebApi.Config
  ( AppConfig (..),
    AppEnvironmentConfig (..),
    AppStartupConfig (..),
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
import WebApi.Postgres (buildRuntimePostgresAccountCredentialStore, buildRuntimePostgresAccountSessionStore, buildRuntimePostgresAccountStore, buildRuntimePostgresDatabaseEffect, buildRuntimePostgresMfaStore)
import WebApi.Response (selectResponseWithDatabase)
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
      ( (Site.simpleSite "web-api" defaultRequestContext routeCodec (buildAppPageShellConfig config) (buildAppSiteRoutes config databaseEffect))
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

buildAppSiteRoutes :: AppConfig -> DatabaseEffect -> [Site.SiteRoute AppRoute AppRequestContext]
buildAppSiteRoutes config databaseEffect =
  let renderSelectedResponse = selectResponseWithDatabase config databaseEffect
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
            accountWorkflowClock = getMonotonicTimeNSec,
            accountWorkflowMfaStore = buildRuntimePostgresMfaStore databaseConfiguration,
            accountWorkflowCredentialStore = buildRuntimePostgresAccountCredentialStore databaseConfiguration,
            accountWorkflowSessionStore = buildRuntimePostgresAccountSessionStore databaseConfiguration,
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
  startupConfigResult <- loadAppStartupConfig
  either
    (\loadError -> ioError (userError ("Failed to load app startup config: " <> show loadError)))
    ( \AppStartupConfig {startupEnvironmentConfig = environmentConfig, startupAppConfig = appConfig} ->
        announceConfigFileStatuses outputHandle configFileStatuses
          >> runWithConfig outputHandle appConfig environmentConfig
    )
    startupConfigResult

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
runtimeRequestObservabilityReporter config requestObservability = do
  TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show requestObservability))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter -> do
    exportResult <-
      try
        (HarchWeb.exportRequestObservabilityToOtlp "web-api" exporter requestObservability) ::
        IO (Either SomeException ())
    case exportResult of
      Left exportError ->
        runtimeApplicationLogReporter
          ("Failed to export request observability to OTLP: " <> Text.pack (displayException exportError))
      Right () ->
        hFlush stderr

runtimeConnectionObservabilityReporter :: AppConfig -> Observability.ConnectionObservability -> IO ()
runtimeConnectionObservabilityReporter config connectionObservability = do
  TextIO.hPutStrLn stderr ("TRACE " <> Text.pack (show connectionObservability))
  forM_ (maybe [] pure (HarchWeb.tracingExporter (observability config))) $ \exporter -> do
    exportResult <-
      try
        (HarchWeb.exportConnectionObservabilityToOtlp "web-api" exporter connectionObservability) ::
        IO (Either SomeException ())
    case exportResult of
      Left exportError ->
        runtimeApplicationLogReporter
          ("Failed to export connection observability to OTLP: " <> Text.pack (displayException exportError))
      Right () ->
        hFlush stderr

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
    { accountWorkflowStore =
        AccountStore
          { createPendingAccount = \_ -> pure (Left (AccountStoreUnavailable "account persistence is not configured")),
            findEmailVerification = \_ -> pure (Left (AccountStoreUnavailable "account persistence is not configured")),
            consumeEmailVerification = \_ _ -> pure (Left (AccountStoreUnavailable "account persistence is not configured"))
          },
      accountWorkflowEmailDelivery = Email.EmailDelivery (\_ -> ioError (userError "email delivery is not configured")),
      accountWorkflowClock = pure 0,
      accountWorkflowMfaStore =
        MfaStore
          { saveUnconfirmedTotpEnrollment = \_ _ _ -> pure (Left (MfaStoreUnavailable "MFA persistence is not configured")),
            loadTotpEnrollment = \_ -> pure (Left (MfaStoreUnavailable "MFA persistence is not configured")),
            confirmTotpEnrollment = \_ _ _ -> pure (Left (MfaStoreUnavailable "MFA persistence is not configured")),
            loadUnusedRecoveryCodeHashes = \_ -> pure (Left (MfaStoreUnavailable "MFA persistence is not configured")),
            consumeRecoveryCodeHash = \_ _ _ -> pure (Left (MfaStoreUnavailable "MFA persistence is not configured"))
          },
      accountWorkflowCredentialStore = AccountCredentialStore (\_ -> pure (Left (AccountCredentialStoreUnavailable "account credentials are not configured"))),
      accountWorkflowSessionStore =
        AccountSessionStore
          { saveAccountSession = \_ -> pure (Left AccountSessionStoreUnavailable),
            loadAccountSession = \_ -> pure (Left AccountSessionStoreUnavailable),
            invalidateAccountSession = \_ -> pure (Left AccountSessionStoreUnavailable)
          },
      accountWorkflowTotpEncryptionKey = totpEncryptionKey defaultAppEnvironmentConfig,
      accountWorkflowTotpClock = pure 0,
      accountWorkflowVerificationUrl = \_ _ -> "https://invalid.example.test/verify"
    }
