{-# LANGUAGE OverloadedStrings #-}

-- | Private certbot-backed ACME runtime lifecycle.
--
-- The public facade re-exports the supported plan and preparation helpers, but
-- this module owns temporary state, certbot process execution, and the TLS
-- server lifecycle that consumes the acquired certificate. Decision (DM,
-- 2026-08-25): its temporary state directory (which contains the ACME account
-- key) is removed on every failed preparation. A successful return transfers
-- cleanup ownership to the running server; failure diagnostics never expose or
-- preserve that private directory or its logs.
-- FQ8 passes the existing runtime's shared transport dependency record to the
-- acquired TLS listener, so ACME cannot accidentally start with a different
-- request-limit policy or WAI application than its sibling listeners.
module HarchWeb.Acme.Certbot.Runtime
  ( RunningAcmeRuntimeServer,
    RuntimeAcmeBindPlan (..),
    RuntimeAcmeServerEnvironment (..),
    certbotCertificateName,
    prepareCertbotManualTlsBindPlan,
    runtimeAcmeBindPlans,
    runtimeCertbotArguments,
    startAcmeRuntimeServersWithRequestTransportLimits,
    stopAcmeRuntimeServers,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, bracket_, onException, try)
import Data.Foldable (for_, traverse_)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Acme.Certbot.Options
  ( certbotHasFlag,
    certbotHasOption,
    certbotNeedsDerivedWebrootAuthenticator,
    certbotOptionValues,
    certbotShouldUseWebroot,
    firstCertbotDomain,
  )
import HarchWeb.Acme.Challenge
  ( CertbotWebrootStore,
    registerCertbotAcmeChallengeWebroot,
    unregisterCertbotAcmeChallengeWebroot,
  )
import HarchWeb.Observability qualified as Observability
import HarchWeb.Security (RequestHeadLimits, RequestTransportLimits)
import HarchWeb.Server.Config
  ( AcmeBindPlan (..),
    AcmeConfig (..),
    CertbotConfig (..),
    ListenerEndpoint (..),
    ManualTlsBindPlan (..),
    ServerStartupPlan (..),
    TlsCredentialSourceKind (..),
    TlsPolicy,
    TlsStartupMode (..),
    sharedCertificatePaths,
  )
import HarchWeb.Server.Transport
  ( RunningRuntimeServer,
    RuntimeTransportDependencies (..),
    ensureRuntimeFileExists,
    startManualTlsRuntimeServer,
    stopRuntimeServer,
  )
import Network.Wai qualified as Wai
import System.Directory (copyFile, createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

data RunningAcmeRuntimeServer = RunningAcmeRuntimeServer
  { runningAcmeRuntimeServer :: Maybe RunningRuntimeServer,
    runningAcmeCleanupDirectory :: FilePath
  }

data RuntimeAcmeBindPlan = RuntimeAcmeBindPlan
  { runtimeAcmeEndpoint :: ListenerEndpoint,
    runtimeAcmeTlsEndpoint :: Maybe ListenerEndpoint,
    runtimeAcmeListenerConfig :: AcmeConfig,
    runtimeAcmeTlsPolicy :: TlsPolicy
  }

-- | Dependencies shared by every ACME listener started for one application
-- runtime.  The webroot store, request limits, application, and reporters
-- all describe that runtime rather than an individual bind plan; keeping
-- them together makes the multi-listener and single-listener startup paths
-- use the same named configuration instead of seven positional arguments.
data RuntimeAcmeServerEnvironment = RuntimeAcmeServerEnvironment
  { runtimeAcmeWebrootStore :: CertbotWebrootStore,
    runtimeAcmeRequestHeadLimits :: RequestHeadLimits,
    runtimeAcmeRequestTransportLimits :: RequestTransportLimits,
    runtimeAcmeApplication :: Wai.Application,
    runtimeAcmeConnectionReporter :: Observability.ConnectionObservability -> IO (),
    runtimeAcmeApplicationLogger :: Text -> IO ()
  }

-- | The temporary certbot working directories, all derived from one state
-- directory. Grouping them stops a positional call site from transposing,
-- for example, 'certbotConfigDirectory' (holding the ACME account private
-- key) with 'certbotWebrootDirectory' (served publicly over HTTP).
data CertbotDirectories = CertbotDirectories
  { certbotConfigDirectory :: FilePath,
    certbotWorkDirectory :: FilePath,
    certbotLogsDirectory :: FilePath,
    certbotWebrootDirectory :: FilePath
  }

runtimeAcmeBindPlans :: ServerStartupPlan -> [RuntimeAcmeBindPlan]
runtimeAcmeBindPlans startupPlan =
  [ RuntimeAcmeBindPlan
      { runtimeAcmeEndpoint = acmeEndpoint acmePlan,
        runtimeAcmeTlsEndpoint = acmeTlsEndpoint acmePlan,
        runtimeAcmeListenerConfig = acmeListenerConfig acmePlan,
        runtimeAcmeTlsPolicy = acmeTlsPolicy acmePlan
      }
  | acmePlan <- acmeBindPlans startupPlan
  ]

-- | Start the given ACME-managed TLS listeners for one application runtime.
startAcmeRuntimeServersWithRequestTransportLimits :: RuntimeAcmeServerEnvironment -> [RuntimeAcmeBindPlan] -> IO [RunningAcmeRuntimeServer]
startAcmeRuntimeServersWithRequestTransportLimits environment acmePlans =
  connectionReporter `seq` applicationLogger `seq` go [] acmePlans
  where
    connectionReporter = runtimeAcmeConnectionReporter environment
    applicationLogger = runtimeAcmeApplicationLogger environment

    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        acmePlan : remaining ->
          ( do
              runningServer <- startAcmeRuntimeServer environment acmePlan
              go (runningServer : runningServers) remaining
                `onException` stopAcmeRuntimeServers (runningServer : runningServers)
          )
            `onException` stopAcmeRuntimeServers runningServers

startAcmeRuntimeServer :: RuntimeAcmeServerEnvironment -> RuntimeAcmeBindPlan -> IO RunningAcmeRuntimeServer
startAcmeRuntimeServer environment runtimeAcmePlan = do
  let certbotConfig = acmeCertbotConfig (runtimeAcmeListenerConfig runtimeAcmePlan)
  (maybeManualTlsPlan, cleanupDirectory) <-
    prepareCertbotManualTlsBindPlanWithLogger webrootStore applicationLogger runtimeAcmePlan certbotConfig
  maybeRunningServer <-
    connectionReporter `seq`
      traverse
        (\manualTlsPlan -> startManualTlsRuntimeServer transportDependencies manualTlsPlan connectionReporter)
        maybeManualTlsPlan
        `onException` removePathForcibly cleanupDirectory
  pure
    RunningAcmeRuntimeServer
      { runningAcmeRuntimeServer = maybeRunningServer,
        runningAcmeCleanupDirectory = cleanupDirectory
      }
  where
    webrootStore = runtimeAcmeWebrootStore environment
    requestHeadLimits = runtimeAcmeRequestHeadLimits environment
    transportLimits = runtimeAcmeRequestTransportLimits environment
    waiApplication = runtimeAcmeApplication environment
    transportDependencies =
      RuntimeTransportDependencies
        { runtimeTransportRequestHeadLimits = requestHeadLimits,
          runtimeTransportRequestLimits = transportLimits,
          runtimeTransportApplication = waiApplication
        }
    connectionReporter = runtimeAcmeConnectionReporter environment
    applicationLogger = runtimeAcmeApplicationLogger environment

runtimeAcmeManualTlsBindPlan :: RuntimeAcmeBindPlan -> FilePath -> FilePath -> Maybe ManualTlsBindPlan
runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath =
  fmap
    ( \tlsListenerEndpoint ->
        ManualTlsBindPlan
          { tlsEndpoint = tlsListenerEndpoint,
            tlsCertificateFile = resolvedCertificatePath,
            tlsPrivateKeyFile = resolvedPrivateKeyPath,
            tlsCredentialSourceKind = ManualTlsCredentials,
            tlsStartupMode = RequireCertificateFiles,
            tlsBindPolicy = runtimeAcmeTlsPolicy runtimeAcmePlan
          }
    )
    (runtimeAcmeTlsEndpoint runtimeAcmePlan)

prepareCertbotManualTlsBindPlan :: CertbotWebrootStore -> RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlan webrootStore =
  prepareCertbotManualTlsBindPlanWithOptionalLogger webrootStore Nothing

-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on 'unregisterCertbotAcmeChallengeWebroot'\'s last argument is a
-- last resort, tried only after the rule's own preferred fix did not apply
-- here. That fix (deduplicating a literal shared across two source
-- positions into one named binding) does not apply: 'webrootDirectory' is
-- already exactly that — one named, correctly-factored local binding — used
-- once each in 'bracket_'\'s register and unregister actions, which is the
-- correct shape for this code, not duplication to remove. Confirmed
-- directly, not assumed: removing the @$!@ and re-running the full coverage
-- gate reproduces a genuine, reproducible gap on this exact expression
-- (`webrootDirectory` at the unregister call site specifically — the
-- function's own body is separately, directly tested and fully covered on
-- its own). GHC shares the two references to this one `let`-bound thunk, and
-- only the first (the register call) earns its own HPC tick when forced;
-- the second reference — evaluating an already-WHNF thunk — does not.
{-# ANN prepareCertbotManualTlsBindPlanWithLogger ("HLint: ignore Redundant $!" :: String) #-}
prepareCertbotManualTlsBindPlanWithLogger :: CertbotWebrootStore -> (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlanWithLogger webrootStore applicationLogger runtimeAcmePlan certbotConfig = do
  prepareCertbotManualTlsBindPlanWithOptionalLogger webrootStore (Just applicationLogger) runtimeAcmePlan certbotConfig

-- | The public preparation helper has no application logger.  Keeping that
-- absence explicit avoids manufacturing a no-op callback solely to satisfy a
-- common implementation; the runtime variant supplies a real logger.
prepareCertbotManualTlsBindPlanWithOptionalLogger :: CertbotWebrootStore -> Maybe (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlanWithOptionalLogger webrootStore maybeApplicationLogger runtimeAcmePlan certbotConfig = do
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
  tempDirectory <- getCanonicalTemporaryDirectory
  stateDirectory <- createTempDirectory tempDirectory "harch-web-certbot"
  prepareWithStateDirectory stateDirectory endpointText `onException` removePathForcibly stateDirectory
  where
    prepareWithStateDirectory stateDirectory endpointText = do
      let configDirectory = stateDirectory </> "config"
          workDirectory = stateDirectory </> "work"
          logsDirectory = stateDirectory </> "logs"
          webrootDirectory = stateDirectory </> "webroot"
          directories =
            CertbotDirectories
              { certbotConfigDirectory = configDirectory,
                certbotWorkDirectory = workDirectory,
                certbotLogsDirectory = logsDirectory,
                certbotWebrootDirectory = webrootDirectory
              }
      mapM_
        (createDirectoryIfMissing True)
        [configDirectory, workDirectory, logsDirectory, webrootDirectory </> ".well-known" </> "acme-challenge"]
      certificateName <-
        either
          (ioError . userError)
          pure
          (certbotCertificateName runtimeAcmePlan)
      bracket_
        ( recordCertbotLog maybeApplicationLogger ("ACME certbot webroot registered for listener " <> endpointText)
            >> registerCertbotAcmeChallengeWebroot webrootStore webrootDirectory
        )
        ( (unregisterCertbotAcmeChallengeWebroot webrootStore $! webrootDirectory)
            >> recordCertbotLog maybeApplicationLogger ("ACME certbot webroot unregistered for listener " <> endpointText)
        )
        (runCertbotAcmeChallengeWithLogger maybeApplicationLogger runtimeAcmePlan certbotConfig directories)
      let certificateDirectory = configDirectory </> "live" </> Text.unpack certificateName
          certificatePath = certificateDirectory </> "fullchain.pem"
          privateKeyPath = certificateDirectory </> "privkey.pem"
      ensureRuntimeFileExists "Certbot ACME certificate file does not exist: " certificatePath
      ensureRuntimeFileExists "Certbot ACME private key file does not exist: " privateKeyPath
      (resolvedCertificatePath, resolvedPrivateKeyPath) <-
        case acmeCertificateDirectory (runtimeAcmeListenerConfig runtimeAcmePlan) of
          Nothing ->
            pure (certificatePath, privateKeyPath)
          Just sharedDirectory -> do
            publishedPaths <- publishCertificateFiles sharedDirectory certificatePath privateKeyPath
            recordCertbotLog maybeApplicationLogger ("Published ACME certificate files to shared directory " <> Text.pack sharedDirectory)
            pure publishedPaths
      pure
        ( runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath,
          stateDirectory
        )

runCertbotAcmeChallengeWithLogger :: Maybe (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> CertbotDirectories -> IO ()
runCertbotAcmeChallengeWithLogger maybeApplicationLogger runtimeAcmePlan certbotConfig directories = do
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
  recordCertbotLog maybeApplicationLogger ("Launching certbot for ACME listener on " <> endpointText)
  let commandArguments =
        certbotRuntimeArguments runtimeAcmePlan certbotConfig directories
  processResult <-
    try (readCreateProcessWithExitCode (proc (certbotExecutable certbotConfig) commandArguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError -> do
      recordCertbotLog maybeApplicationLogger ("Failed to launch certbot for ACME listener on " <> endpointText <> ": " <> Text.pack (show launchError))
      ioError . userError $
        "Failed to launch certbot for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, _, _) ->
      recordCertbotLog maybeApplicationLogger ("Certbot completed for ACME listener on " <> endpointText)
    Right (exitCode, stdoutText, stderrText) -> do
      recordCertbotLog maybeApplicationLogger ("Certbot failed for ACME listener on " <> endpointText <> " with exit code " <> Text.pack (show exitCode))
      ioError . userError $
        "Certbot failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText

recordCertbotLog :: Maybe (Text -> IO ()) -> Text -> IO ()
recordCertbotLog maybeApplicationLogger entry =
  traverse_ ($ entry) maybeApplicationLogger

certbotRuntimeArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> CertbotDirectories -> [String]
certbotRuntimeArguments runtimeAcmePlan certbotConfig directories =
  map Text.unpack (certbotCommandArguments certbotConfig)
    <> map Text.unpack (certbotArguments certbotConfig)
    <> certbotNonInteractiveArguments certbotConfig
    <> certbotAgreeTosArguments certbotConfig
    <> certbotAuthenticatorArguments certbotConfig
    <> certbotWebrootPathArguments certbotConfig (certbotWebrootDirectory directories)
    <> ["--config-dir", certbotConfigDirectory directories, "--work-dir", certbotWorkDirectory directories, "--logs-dir", certbotLogsDirectory directories]
    <> certbotHttp01PortArguments runtimeAcmePlan
    <> certbotDirectoryUrlArguments runtimeAcmePlan
    <> certbotContactEmailArguments runtimeAcmePlan certbotConfig
    <> certbotDomainArguments runtimeAcmePlan certbotConfig

certbotCommandArguments :: CertbotConfig -> [Text]
certbotCommandArguments certbotConfig =
  [ "certonly"
  | "certonly" `notElem` certbotArguments certbotConfig
  ]

certbotNonInteractiveArguments :: CertbotConfig -> [String]
certbotNonInteractiveArguments certbotConfig =
  [ "--non-interactive"
  | not (any (`certbotHasFlag` certbotArguments certbotConfig) ["--non-interactive", "-n"])
  ]

certbotAgreeTosArguments :: CertbotConfig -> [String]
certbotAgreeTosArguments certbotConfig =
  ["--agree-tos" | not (certbotHasFlag "--agree-tos" (certbotArguments certbotConfig))]

certbotAuthenticatorArguments :: CertbotConfig -> [String]
certbotAuthenticatorArguments certbotConfig =
  ["--webroot" | certbotNeedsDerivedWebrootAuthenticator (certbotArguments certbotConfig)]

certbotWebrootPathArguments :: CertbotConfig -> FilePath -> [String]
certbotWebrootPathArguments certbotConfig webrootDirectory =
  if certbotShouldUseWebroot (certbotArguments certbotConfig)
    && not (certbotHasOption "-w" (certbotArguments certbotConfig) || certbotHasOption "--webroot-path" (certbotArguments certbotConfig))
    then ["--webroot-path", webrootDirectory]
    else []

certbotHttp01PortArguments :: RuntimeAcmeBindPlan -> [String]
certbotHttp01PortArguments runtimeAcmePlan =
  if certbotHasOption "--http-01-port" (runtimeCertbotArguments runtimeAcmePlan)
    || certbotShouldUseWebroot (runtimeCertbotArguments runtimeAcmePlan)
    then []
    else ["--http-01-port", show (acmeHttp01Port (runtimeAcmeListenerConfig runtimeAcmePlan))]

certbotDirectoryUrlArguments :: RuntimeAcmeBindPlan -> [String]
certbotDirectoryUrlArguments runtimeAcmePlan =
  if certbotHasOption "--server" (runtimeCertbotArguments runtimeAcmePlan)
    then []
    else ["--server", Text.unpack (acmeDirectoryUrl (runtimeAcmeListenerConfig runtimeAcmePlan))]

certbotContactEmailArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> [String]
certbotContactEmailArguments runtimeAcmePlan certbotConfig =
  if certbotHasOption "--email" (certbotArguments certbotConfig)
    || certbotHasOption "-m" (certbotArguments certbotConfig)
    then []
    else case acmeContactEmails (runtimeAcmeListenerConfig runtimeAcmePlan) of
      firstContact : _ -> ["--email", Text.unpack firstContact]
      [] -> []

certbotDomainArguments :: RuntimeAcmeBindPlan -> CertbotConfig -> [String]
certbotDomainArguments runtimeAcmePlan certbotConfig =
  if any (`certbotHasOption` configuredArguments) ["-d", "--domain", "--domains"]
    then []
    else case acmeDomains (runtimeAcmeListenerConfig runtimeAcmePlan) of
      [] -> []
      domains -> ["--domains", Text.unpack (Text.intercalate "," domains)]
  where
    configuredArguments = certbotArguments certbotConfig

runtimeCertbotArguments :: RuntimeAcmeBindPlan -> [Text]
runtimeCertbotArguments runtimeAcmePlan =
  let certbotConfig = acmeCertbotConfig (runtimeAcmeListenerConfig runtimeAcmePlan)
   in certbotArguments certbotConfig

certbotCertificateName :: RuntimeAcmeBindPlan -> Either String Text
certbotCertificateName runtimeAcmePlan =
  maybe
    ( maybe
        ( Left $
            "Unsupported runtime listener startup plan: ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> " requires ACME domains or certbot arguments to declare --cert-name or a domain via -d/--domain/--domains."
        )
        Right
        ( firstCertbotDomain (runtimeCertbotArguments runtimeAcmePlan)
            <|> listToMaybe (acmeDomains (runtimeAcmeListenerConfig runtimeAcmePlan))
        )
    )
    Right
    (listToMaybe (certbotOptionValues "--cert-name" (runtimeCertbotArguments runtimeAcmePlan)))

stopAcmeRuntimeServers :: [RunningAcmeRuntimeServer] -> IO ()
stopAcmeRuntimeServers =
  mapM_ stopAcmeRuntimeServer

stopAcmeRuntimeServer :: RunningAcmeRuntimeServer -> IO ()
stopAcmeRuntimeServer runningServer = do
  for_ (runningAcmeRuntimeServer runningServer) stopRuntimeServer
  removePathForcibly (runningAcmeCleanupDirectory runningServer)

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)

publishCertificateFiles :: FilePath -> FilePath -> FilePath -> IO (FilePath, FilePath)
publishCertificateFiles certificateDirectory sourceCertificatePath sourcePrivateKeyPath = do
  createDirectoryIfMissing True certificateDirectory
  let (certificatePath, privateKeyPath) = sharedCertificatePaths certificateDirectory
  copyFile sourceCertificatePath certificatePath
  copyFile sourcePrivateKeyPath privateKeyPath
  pure (certificatePath, privateKeyPath)
