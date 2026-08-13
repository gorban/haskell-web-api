{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Private runtime listener orchestration behind the public 'runServer' facade.
module HarchWeb.Server.Runtime
  ( runServer,
    runServerWithWaiMiddleware,
  )
where

import Control.Concurrent (newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Bifunctor (first)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Acme
import HarchWeb.Acme.Certbot.Runtime (runtimeAcmeBindPlans, startAcmeRuntimeServersWithRequestTransportLimits, stopAcmeRuntimeServers)
import HarchWeb.Acme.Challenge (acmeChallengeRoutePath)
import HarchWeb.Observability (planObservabilityStartup)
import HarchWeb.Security (RequestPolicyConfig, requestConcurrencyLimit, requestHeadLimits, requestTransportLimits)
import HarchWeb.Server.Application (Application (..))
import HarchWeb.Server.Config
import HarchWeb.Server.RequestExecution (concurrencyLimitedMiddleware, reportEarlyRequestObservability, toWaiApplication)
import HarchWeb.Server.Transport
  ( startHttpRuntimeServersWithRequestTransportLimits,
    startManualTlsRuntimeServersWithRequestTransportLimits,
    stopRuntimeServers,
  )
import Network.Wai qualified as Wai
import System.IO (Handle, hFlush, hPutStrLn)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Text.Read (readMaybe)

runServer ::
  (Eq route, HasServerConfig config) =>
  Handle ->
  config ->
  Application route action context ->
  IO ()
runServer = runServerWithWaiMiddleware id

-- | Like 'runServer', but composes a caller-supplied 'Wai.Middleware' in
-- front of the rendered application before it reaches any runtime listener,
-- e.g. an application-authored middleware handling paths outside the typed
-- 'Application' entirely. ACME HTTP-01 challenge responses bypass the
-- middleware; every other request passes through it first.
runServerWithWaiMiddleware ::
  (Eq route, HasServerConfig config) =>
  Wai.Middleware ->
  Handle ->
  config ->
  Application route action context ->
  IO ()
runServerWithWaiMiddleware waiMiddleware outputHandle config webApplication =
  either
    (ioError . userError)
    (runServerWithStartupPlan waiMiddleware outputHandle config webApplication)
    (validatedServerStartupPlan config)

validatedServerStartupPlan :: (HasServerConfig config) => config -> Either String ServerStartupPlan
validatedServerStartupPlan config = do
  startupPlan <- first (("Invalid listener startup plan: " <>) . show) (planServerStartup config)
  maybe (Right startupPlan) Left (runtimeStartupValidationError startupPlan)

runServerWithStartupPlan ::
  (Eq route, HasServerConfig config) =>
  Wai.Middleware ->
  Handle ->
  config ->
  Application route action context ->
  ServerStartupPlan ->
  IO ()
runServerWithStartupPlan waiMiddleware outputHandle config webApplication startupPlan = do
  let observabilityPlan = planObservabilityStartup (observability (toServerConfig config))
  challengeStore <- AcmeChallengeStore <$> newMVar []
  let runtimeRequestPolicy = requestPolicy (toServerConfig config)
  concurrencyGatedMiddleware <- concurrencyLimitedMiddleware (requestConcurrencyLimit runtimeRequestPolicy) waiMiddleware
  let runtimeApplication = toRuntimeWaiApplication concurrencyGatedMiddleware challengeStore webApplication
      connectionReporter = reportConnectionObservability webApplication
      runtimeRequestHeadLimits = requestHeadLimits runtimeRequestPolicy
      runtimeRequestTransportLimits = requestTransportLimits runtimeRequestPolicy
  connectionReporter `seq`
    observabilityPlan `seq`
      bracket
        (startHttpRuntimeServersWithRequestTransportLimits runtimeRequestHeadLimits runtimeRequestTransportLimits (httpEndpoints (httpBindPlan startupPlan)) runtimeApplication)
        stopRuntimeServers
        ( \httpServers ->
            bracket
              (startAcmeRuntimeServersWithRequestTransportLimits runtimeRequestHeadLimits runtimeRequestTransportLimits (runtimeAcmeBindPlans startupPlan) runtimeApplication connectionReporter (reportApplicationLog webApplication))
              stopAcmeRuntimeServers
              ( \acmeServers ->
                  bracket
                    (startManualTlsRuntimeServersWithRequestTransportLimits runtimeRequestHeadLimits runtimeRequestTransportLimits (manualTlsBindPlans startupPlan) runtimeApplication connectionReporter)
                    stopRuntimeServers
                    ( \manualTlsServers ->
                        httpServers `seq`
                          acmeServers `seq`
                            manualTlsServers `seq`
                              announceRuntimeStartup outputHandle startupPlan
                                >> waitForShutdownSignal
                    )
              )
        )

toRuntimeWaiApplication ::
  (Eq route) =>
  Wai.Middleware ->
  AcmeChallengeStore ->
  Application route action context ->
  Wai.Application
toRuntimeWaiApplication waiMiddleware challengeStore webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
  maybeChallengeResponse <- acmeChallengeResponseForRequest requestPolicyConfig challengeStore request
  maybe
    (waiMiddleware (toWaiApplication webApplication) request respond)
    (respondAcmeChallenge webApplication request requestPolicyConfig requestStartedAt respond)
    maybeChallengeResponse

respondAcmeChallenge ::
  (Eq route) =>
  Application route action context ->
  Wai.Request ->
  RequestPolicyConfig ->
  Word64 ->
  (Wai.Response -> IO Wai.ResponseReceived) ->
  Wai.Response ->
  IO Wai.ResponseReceived
respondAcmeChallenge webApplication request requestPolicyConfig requestStartedAt respond challengeResponse = do
  challengeResponseReportedAt <- challengeResponse `seq` getMonotonicTimeNSec
  reportEarlyRequestObservability
    webApplication
    request
    requestStartedAt
    challengeResponseReportedAt
    (acmeChallengeRoutePath requestPolicyConfig request)
    challengeResponse
  respond challengeResponse

announceRuntimeStartup :: Handle -> ServerStartupPlan -> IO ()
announceRuntimeStartup outputHandle startupPlan = do
  mapM_ (hPutStrLn outputHandle . uncurry listenerStartupMessage) (runtimeStartupListeners startupPlan)
  hFlush outputHandle

runtimeStartupListeners :: ServerStartupPlan -> [(ListenerScheme, ListenerEndpoint)]
runtimeStartupListeners startupPlan =
  map (Http,) (httpEndpoints (httpBindPlan startupPlan))
    <> map ((Https,) . tlsEndpoint) (manualTlsBindPlans startupPlan)
    <> mapMaybe (fmap (Https,) . acmeTlsEndpoint) (acmeBindPlans startupPlan)

listenerStartupMessage :: ListenerScheme -> ListenerEndpoint -> String
listenerStartupMessage listenerScheme endpoint =
  listenerSchemePrefix listenerScheme
    <> Text.unpack (endpointHost endpoint)
    <> ":"
    <> show (endpointPort endpoint)

listenerSchemePrefix :: ListenerScheme -> String
listenerSchemePrefix listenerScheme =
  case listenerScheme of
    Http -> "HTTP Server listening at http://"
    Https -> "HTTPS Server listening at https://"

waitForShutdownSignal :: IO ()
waitForShutdownSignal = do
  shutdownSignal <- newEmptyMVar
  let noSignalMask = Nothing
      installShutdownHandler signal handler = noSignalMask `seq` installHandler signal handler $! noSignalMask
      requestShutdown = void (tryPutMVar shutdownSignal ())
  previousInterruptHandler <- installShutdownHandler sigINT (Catch requestShutdown)
  previousTerminationHandler <- installShutdownHandler sigTERM (Catch requestShutdown)
  takeMVar shutdownSignal
    `finally` do
      _ <- installShutdownHandler sigINT previousInterruptHandler
      installShutdownHandler sigTERM previousTerminationHandler

runtimeStartupValidationError :: ServerStartupPlan -> Maybe String
runtimeStartupValidationError startupPlan =
  case ( null (acmeBindPlans startupPlan),
         null (httpEndpoints (httpBindPlan startupPlan)),
         null (manualTlsBindPlans startupPlan)
       ) of
    (True, True, True) ->
      Just "Unsupported runtime listener startup plan: no runtime listeners are configured."
    (False, _, _) ->
      firstAcmeRuntimeStartupError (httpEndpoints (httpBindPlan startupPlan)) (acmeBindPlans startupPlan)
    (True, _, _) ->
      Nothing

firstAcmeRuntimeStartupError :: [ListenerEndpoint] -> [AcmeBindPlan] -> Maybe String
firstAcmeRuntimeStartupError httpListenerEndpoints acmePlans =
  listToMaybe (mapMaybe (validateAcmeRuntimeBindPlan httpListenerEndpoints) acmePlans)

validateAcmeRuntimeBindPlan :: [ListenerEndpoint] -> AcmeBindPlan -> Maybe String
validateAcmeRuntimeBindPlan httpListenerEndpoints acmePlan =
  either Just (validateAcmeChallengePort httpListenerEndpoints acmePlan) (acmeHttp01ChallengePort acmePlan)

validateAcmeChallengePort :: [ListenerEndpoint] -> AcmeBindPlan -> Int -> Maybe String
validateAcmeChallengePort httpListenerEndpoints acmePlan challengePort =
  maybe
    (validateHttpOnlyAcmeChallengePort acmePlan challengePort)
    (const (validateTlsAcmeChallengePort httpListenerEndpoints acmePlan challengePort))
    (acmeTlsEndpoint acmePlan)

validateHttpOnlyAcmeChallengePort :: AcmeBindPlan -> Int -> Maybe String
validateHttpOnlyAcmeChallengePort acmePlan challengePort =
  if endpointPort (acmeEndpoint acmePlan) == challengePort
    then validateAcmeRuntimeConfiguration acmePlan
    else
      Just $
        "Unsupported runtime listener startup plan: ACME listener on "
          <> renderListenerEndpoint (acmeEndpoint acmePlan)
          <> " requires the configured http-01 port to match its HTTP listener port "
          <> show (endpointPort (acmeEndpoint acmePlan))
          <> "."

validateTlsAcmeChallengePort :: [ListenerEndpoint] -> AcmeBindPlan -> Int -> Maybe String
validateTlsAcmeChallengePort httpListenerEndpoints acmePlan challengePort =
  if hasMatchingAcmeHttp01ChallengeEndpoint challengePort httpListenerEndpoints acmePlan
    then validateAcmeRuntimeConfiguration acmePlan
    else
      Just $
        "Unsupported runtime listener startup plan: ACME listener on "
          <> renderListenerEndpoint (acmeEndpoint acmePlan)
          <> " requires an HTTP listener on port "
          <> show challengePort
          <> " for http-01 challenges."

validateAcmeRuntimeConfiguration :: AcmeBindPlan -> Maybe String
validateAcmeRuntimeConfiguration acmePlan =
  if isNothing (acmeTlsEndpoint acmePlan)
    && isNothing (acmeCertificateDirectory (acmeListenerConfig acmePlan))
    then
      Just $
        "Unsupported runtime listener startup plan: ACME listener on "
          <> renderListenerEndpoint (acmeEndpoint acmePlan)
          <> " requires an ACME certificate directory so HTTPS listeners can consume published certificates."
    else Nothing

hasMatchingAcmeHttp01ChallengeEndpoint :: Int -> [ListenerEndpoint] -> AcmeBindPlan -> Bool
hasMatchingAcmeHttp01ChallengeEndpoint challengePort httpListenerEndpoints acmePlan =
  any (isAcmeHttp01ChallengeEndpointFor challengePort (acmeEndpoint acmePlan)) httpListenerEndpoints

acmeHttp01ChallengePort :: AcmeBindPlan -> Either String Int
acmeHttp01ChallengePort acmePlan =
  let certbotConfig = acmeCertbotConfig (acmeListenerConfig acmePlan)
   in case certbotOptionValue "--http-01-port" (certbotArguments certbotConfig) of
        Nothing ->
          Right (acmeHttp01Port (acmeListenerConfig acmePlan))
        Just portText ->
          maybe
            ( Left $
                "Unsupported runtime listener startup plan: ACME listener on "
                  <> renderListenerEndpoint (acmeEndpoint acmePlan)
                  <> " has an invalid certbot http-01 port: "
                  <> Text.unpack portText
            )
            Right
            (readMaybe (Text.unpack portText))

certbotOptionValue :: Text -> [Text] -> Maybe Text
certbotOptionValue optionName arguments =
  listToMaybe (certbotOptionValues optionName arguments)

isAcmeHttp01ChallengeEndpointFor :: Int -> ListenerEndpoint -> ListenerEndpoint -> Bool
isAcmeHttp01ChallengeEndpointFor challengePort acmeListenerEndpoint httpListenerEndpoint =
  endpointPort httpListenerEndpoint == challengePort
    && ( endpointHost httpListenerEndpoint == "0.0.0.0"
           || endpointHost httpListenerEndpoint == endpointHost acmeListenerEndpoint
       )

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
