{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | Private runtime listener orchestration behind the public 'runServer' facade.
module HarchWeb.Server.Runtime
  ( runServer,
  )
where

import Control.Concurrent (newEmptyMVar, newMVar, takeMVar, tryPutMVar)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.List (find)
import Data.Maybe (isNothing, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb.Acme
import HarchWeb.Acme.Certbot.Runtime (runtimeAcmeBindPlans, startAcmeRuntimeServers, stopAcmeRuntimeServers)
import HarchWeb.Acme.Challenge (acmeChallengeRoutePath)
import HarchWeb.Observability (planObservabilityStartup)
import HarchWeb.Server
import HarchWeb.Server.Transport
  ( startHttpRuntimeServers,
    startManualTlsRuntimeServers,
    stopRuntimeServers,
  )
import Network.Wai qualified as Wai
import System.IO (Handle, hFlush, hPutStrLn)
import System.Posix.Signals (Handler (Catch), installHandler, sigINT, sigTERM)
import Text.Read (readMaybe)

runServer :: (Eq route, HasServerConfig config) => Handle -> config -> Application route context -> IO ()
runServer outputHandle config webApplication =
  case planServerStartup config of
    Left startupError -> ioError (userError ("Invalid listener startup plan: " <> show startupError))
    Right startupPlan -> do
      let observabilityPlan = planObservabilityStartup (observability (toServerConfig config))
      challengeStore <- AcmeChallengeStore <$> newMVar []
      let runtimeApplication = toRuntimeWaiApplication challengeStore webApplication
          connectionReporter = reportConnectionObservability webApplication
      case runtimeStartupValidationError startupPlan of
        Just runtimeError ->
          ioError (userError runtimeError)
        Nothing ->
          connectionReporter `seq`
            observabilityPlan `seq`
              bracket
                (startHttpRuntimeServers (httpEndpoints (httpBindPlan startupPlan)) runtimeApplication)
                stopRuntimeServers
                ( \httpServers ->
                    bracket
                      (startAcmeRuntimeServers (runtimeAcmeBindPlans startupPlan) runtimeApplication connectionReporter (reportApplicationLog webApplication))
                      stopAcmeRuntimeServers
                      ( \acmeServers ->
                          bracket
                            (startManualTlsRuntimeServers (manualTlsBindPlans startupPlan) runtimeApplication connectionReporter)
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

toRuntimeWaiApplication :: (Eq route) => AcmeChallengeStore -> Application route context -> Wai.Application
toRuntimeWaiApplication challengeStore webApplication request respond = do
  requestStartedAt <- getMonotonicTimeNSec
  let requestPolicyConfig = applicationRequestPolicy webApplication
  maybeChallengeResponse <- acmeChallengeResponseForRequest requestPolicyConfig challengeStore request
  case maybeChallengeResponse of
    Just challengeResponse -> do
      challengeResponseReportedAt <- challengeResponse `seq` getMonotonicTimeNSec
      reportEarlyRequestObservability
        webApplication
        request
        requestStartedAt
        challengeResponseReportedAt
        (acmeChallengeRoutePath requestPolicyConfig request)
        challengeResponse
      respond challengeResponse
    Nothing -> toWaiApplication webApplication request respond

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
  case acmeHttp01ChallengePort acmePlan of
    Left runtimeError ->
      Just runtimeError
    Right challengePort ->
      case acmeTlsEndpoint acmePlan of
        Nothing ->
          if endpointPort (acmeEndpoint acmePlan) == challengePort
            then validateAcmeRuntimeConfiguration acmePlan
            else
              Just $
                "Unsupported runtime listener startup plan: ACME listener on "
                  <> renderListenerEndpoint (acmeEndpoint acmePlan)
                  <> " requires the configured http-01 port to match its HTTP listener port "
                  <> show (endpointPort (acmeEndpoint acmePlan))
                  <> "."
        Just _ ->
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
  case find (isAcmeHttp01ChallengeEndpointFor challengePort (acmeEndpoint acmePlan)) httpListenerEndpoints of
    Just _ -> True
    Nothing -> False

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
