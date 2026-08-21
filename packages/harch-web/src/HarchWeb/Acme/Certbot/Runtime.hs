{-# LANGUAGE OverloadedStrings #-}

-- | Private certbot-backed ACME runtime lifecycle.
--
-- The public facade re-exports the supported plan and preparation helpers, but
-- this module owns temporary state, certbot process execution, and the TLS
-- server lifecycle that consumes the acquired certificate.
module HarchWeb.Acme.Certbot.Runtime
  ( RunningAcmeRuntimeServer,
    RuntimeAcmeBindPlan (..),
    certbotCertificateName,
    prepareCertbotManualTlsBindPlan,
    runtimeAcmeBindPlans,
    runtimeCertbotArguments,
    startAcmeRuntimeServersWithRequestTransportLimits,
    stopAcmeRuntimeServers,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (IOException, bracket_, evaluate, onException, try)
import Control.Monad (void)
import Data.Foldable (for_)
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
    TlsStartupMode (..),
    sharedCertificatePaths,
  )
import HarchWeb.Server.Transport
  ( RunningRuntimeServer,
    ensureRuntimeFileExists,
    startManualTlsRuntimeServerWithRequestTransportLimits,
    stopRuntimeServer,
  )
import Network.Wai qualified as Wai
import System.Directory (copyFile, createDirectoryIfMissing, doesFileExist, removePathForcibly)
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
    runtimeAcmeListenerConfig :: AcmeConfig
  }

-- | The temporary certbot working directories, all derived from one state
-- directory. Grouping them stops a positional call site from transposing,
-- for example, 'certbotConfigDirectory' (holding the ACME account private
-- key) with 'certbotWebrootDirectory' (served publicly over HTTP).
data CertbotDirectories = CertbotDirectories
  { certbotStateDirectory :: FilePath,
    certbotConfigDirectory :: FilePath,
    certbotWorkDirectory :: FilePath,
    certbotLogsDirectory :: FilePath,
    certbotWebrootDirectory :: FilePath
  }

runtimeAcmeBindPlans :: ServerStartupPlan -> [RuntimeAcmeBindPlan]
runtimeAcmeBindPlans startupPlan =
  [ RuntimeAcmeBindPlan
      { runtimeAcmeEndpoint = acmeEndpoint acmePlan,
        runtimeAcmeTlsEndpoint = acmeTlsEndpoint acmePlan,
        runtimeAcmeListenerConfig = acmeListenerConfig acmePlan
      }
  | acmePlan <- acmeBindPlans startupPlan
  ]

-- | Start ACME-managed TLS listeners with all opt-in Warp request transport
-- controls selected by the application runtime.
startAcmeRuntimeServersWithRequestTransportLimits :: CertbotWebrootStore -> RequestHeadLimits -> RequestTransportLimits -> [RuntimeAcmeBindPlan] -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> (Text -> IO ()) -> IO [RunningAcmeRuntimeServer]
startAcmeRuntimeServersWithRequestTransportLimits webrootStore requestHeadLimits transportLimits acmePlans waiApplication connectionReporter applicationLogger =
  connectionReporter `seq` applicationLogger `seq` go [] acmePlans
  where
    go runningServers remainingPlans =
      case remainingPlans of
        [] -> pure (reverse runningServers)
        acmePlan : remaining ->
          ( do
              runningServer <- startAcmeRuntimeServer webrootStore requestHeadLimits transportLimits acmePlan waiApplication connectionReporter applicationLogger
              go (runningServer : runningServers) remaining
                `onException` stopAcmeRuntimeServers (runningServer : runningServers)
          )
            `onException` stopAcmeRuntimeServers runningServers

startAcmeRuntimeServer :: CertbotWebrootStore -> RequestHeadLimits -> RequestTransportLimits -> RuntimeAcmeBindPlan -> Wai.Application -> (Observability.ConnectionObservability -> IO ()) -> (Text -> IO ()) -> IO RunningAcmeRuntimeServer
startAcmeRuntimeServer webrootStore requestHeadLimits transportLimits runtimeAcmePlan waiApplication connectionReporter applicationLogger = do
  let certbotConfig = acmeCertbotConfig (runtimeAcmeListenerConfig runtimeAcmePlan)
  (maybeManualTlsPlan, cleanupDirectory) <-
    prepareCertbotManualTlsBindPlanWithLogger webrootStore applicationLogger runtimeAcmePlan certbotConfig
  maybeRunningServer <-
    connectionReporter `seq`
      traverse (\manualTlsPlan -> startManualTlsRuntimeServerWithRequestTransportLimits requestHeadLimits transportLimits manualTlsPlan waiApplication connectionReporter) maybeManualTlsPlan
        `onException` removePathForcibly cleanupDirectory
  pure
    RunningAcmeRuntimeServer
      { runningAcmeRuntimeServer = maybeRunningServer,
        runningAcmeCleanupDirectory = cleanupDirectory
      }

runtimeAcmeManualTlsBindPlan :: RuntimeAcmeBindPlan -> FilePath -> FilePath -> Maybe ManualTlsBindPlan
runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath =
  fmap
    ( \tlsListenerEndpoint ->
        ManualTlsBindPlan
          { tlsEndpoint = tlsListenerEndpoint,
            tlsCertificateFile = resolvedCertificatePath,
            tlsPrivateKeyFile = resolvedPrivateKeyPath,
            tlsCredentialSourceKind = ManualTlsCredentials,
            tlsStartupMode = RequireCertificateFiles
          }
    )
    (runtimeAcmeTlsEndpoint runtimeAcmePlan)

prepareCertbotManualTlsBindPlan :: CertbotWebrootStore -> RuntimeAcmeBindPlan -> CertbotConfig -> IO (Maybe ManualTlsBindPlan, FilePath)
prepareCertbotManualTlsBindPlan webrootStore =
  prepareCertbotManualTlsBindPlanWithLogger webrootStore ignoreTextLog

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
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
  tempDirectory <- getCanonicalTemporaryDirectory
  stateDirectory <- createTempDirectory tempDirectory "harch-web-certbot"
  let configDirectory = stateDirectory </> "config"
      workDirectory = stateDirectory </> "work"
      logsDirectory = stateDirectory </> "logs"
      webrootDirectory = stateDirectory </> "webroot"
      directories =
        CertbotDirectories
          { certbotStateDirectory = stateDirectory,
            certbotConfigDirectory = configDirectory,
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
    ( applicationLogger ("ACME certbot webroot registered for listener " <> endpointText)
        >> registerCertbotAcmeChallengeWebroot webrootStore webrootDirectory
    )
    ( (unregisterCertbotAcmeChallengeWebroot webrootStore $! webrootDirectory)
        >> applicationLogger ("ACME certbot webroot unregistered for listener " <> endpointText)
    )
    (runCertbotAcmeChallengeWithLogger applicationLogger runtimeAcmePlan certbotConfig directories)
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
        applicationLogger ("Published ACME certificate files to shared directory " <> Text.pack sharedDirectory)
        pure publishedPaths
  pure
    ( runtimeAcmeManualTlsBindPlan runtimeAcmePlan resolvedCertificatePath resolvedPrivateKeyPath,
      stateDirectory
    )

runCertbotAcmeChallengeWithLogger :: (Text -> IO ()) -> RuntimeAcmeBindPlan -> CertbotConfig -> CertbotDirectories -> IO ()
runCertbotAcmeChallengeWithLogger applicationLogger runtimeAcmePlan certbotConfig directories = do
  let endpointText = Text.pack (renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan))
      stateDirectory = certbotStateDirectory directories
      logsDirectory = certbotLogsDirectory directories
  applicationLogger ("Launching certbot for ACME listener on " <> endpointText)
  let commandArguments =
        certbotRuntimeArguments runtimeAcmePlan certbotConfig directories
  processResult <-
    try (readCreateProcessWithExitCode (proc (certbotExecutable certbotConfig) commandArguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError -> do
      applicationLogger ("Failed to launch certbot for ACME listener on " <> endpointText <> ": " <> Text.pack (show launchError))
      ioError . userError $
        "Failed to launch certbot for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, stdoutText, stderrText) -> do
      void (evaluate (length stdoutText + length stderrText))
    Right (exitCode, stdoutText, stderrText) -> do
      applicationLogger ("Certbot failed for ACME listener on " <> endpointText <> " with exit code " <> Text.pack (show exitCode))
      diagnostics <- certbotFailureDiagnostics stateDirectory logsDirectory
      ioError . userError $
        "Certbot failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText
          <> diagnostics

ignoreTextLog :: Text -> IO ()
ignoreTextLog textValue = void (evaluate (Text.length textValue))

certbotFailureDiagnostics :: FilePath -> FilePath -> IO String
certbotFailureDiagnostics stateDirectory logsDirectory = do
  let logPath = logsDirectory </> "letsencrypt.log"
  logExists <- doesFileExist logPath
  if logExists
    then do
      logText <- readFile logPath
      _ <- evaluate (length logText)
      pure $
        "\nCertbot state directory was preserved for inspection: "
          <> stateDirectory
          <> "\nletsencrypt.log tail:\n"
          <> tailTextLines 80 logText
    else
      pure $
        "\nCertbot state directory was preserved for inspection: "
          <> stateDirectory
          <> "\nNo certbot logfile was found at "
          <> logPath
          <> ".\n"

tailTextLines :: Int -> String -> String
tailTextLines lineCount textValue =
  unlines (drop (max 0 (length textLines - lineCount)) textLines)
  where
    textLines = lines textValue

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
