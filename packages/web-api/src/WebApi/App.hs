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
import Data.ByteString qualified as ByteString
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
import System.Directory (doesFileExist)
import System.IO (Handle, hFlush)
import WebApi.Account (AccountProfileStore (..), AccountStore (..), AccountStoreError (..))
import WebApi.AccountPages (AccountAction, accountActions, handleAccountAction)
import WebApi.Api.Endpoints (secondApiRouteDefinition, statusApiRouteDefinition)
import WebApi.App.Observability
  ( ignoreApplicationLog,
    ignoreConnectionObservability,
    ignoreRequestObservability,
    runtimeApplicationLogReporter,
    runtimeConnectionObservabilityReporter,
    runtimeRequestObservabilityReporter,
  )
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
    databasePoolCapacity,
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
import WebApi.Postgres.Pool (PostgresPool, newPostgresPool)
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

-- | 'pool' is a bound variable referenced twice in this @let@ (once for
-- 'pageRepository', once below for 'accountWorkflow'); GHC's HPC
-- instrumentation credits only the first reference, the same "repeated
-- bound-variable arguments passed directly to a function call" pattern
-- documented against AW's coverage investigation. The @$!@ below forces the
-- second, uncredited reference directly rather than restructuring around it;
-- no ignore pragma is needed since 'pool' is an opaque function parameter,
-- not a value HLint could ever consider already in WHNF.
buildRuntimeApp ::
  PostgresPool ->
  AppConfig ->
  AppEnvironmentConfig ->
  HarchWeb.Application AppRoute AccountAction AppRequestContext
buildRuntimeApp pool config environmentConfig =
  let pageRepository = buildRuntimePostgresPageRepository pool
      accountWorkflow = (buildRuntimeAccountWorkflow $! pool) environmentConfig
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

buildRuntimeAccountWorkflow :: PostgresPool -> AppEnvironmentConfig -> AccountWorkflow
buildRuntimeAccountWorkflow pool !environmentConfig =
  AccountWorkflow
    { accountWorkflowStore = buildRuntimePostgresAccountStore pool,
      accountWorkflowEmailDelivery = runtimeEmailDelivery (smtpDeliveryConfig environmentConfig),
      accountWorkflowPasswordHasher = Password.hashPassword,
      accountWorkflowClock = getMonotonicTimeNSec,
      accountWorkflowMfaStore = buildRuntimePostgresMfaStore pool,
      accountWorkflowCredentialStore = buildRuntimePostgresAccountCredentialStore pool,
      accountWorkflowLoginAttemptStore = buildRuntimePostgresLoginAttemptStore pool,
      accountWorkflowSessionStore = buildRuntimePostgresAccountSessionStore pool,
      accountWorkflowMfaEnrollmentSessionStore = buildRuntimePostgresMfaEnrollmentSessionStore pool,
      accountWorkflowProfileStore = buildRuntimePostgresAccountProfileStore pool,
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
  let runtimeDatabaseConfig = databaseConfig environmentConfig
  pool <- newPostgresPool (databasePoolCapacity runtimeDatabaseConfig) runtimeDatabaseConfig
  announceParsedListenerConfigs outputHandle appConfig
  HarchWeb.runServer outputHandle appConfig (buildRuntimeApp pool appConfig environmentConfig)

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
