{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private ACME endpoint workflows and order polling.
module HarchWeb.Acme.Protocol.Workflow
  ( createAcmeAccount,
    createAcmeOrder,
    fetchAcmeCertificate,
    fetchAcmeDirectory,
    finalizeAcmeOrder,
    mailtoAcmeContact,
    pollAcmeOrder,
    pollAcmeOrderWithRetries,
    prepareAcmeAuthorization,
    triggerAcmeChallenge,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Acme.Certbot.Runtime (RuntimeAcmeBindPlan (..))
import HarchWeb.Acme.Challenge (ActiveAcmeChallenge (..))
import HarchWeb.Acme.Crypto (base64urlText)
import HarchWeb.Acme.Json (jsonArrayBytes, jsonBoolBytes, jsonObjectBytes, jsonStringBytes)
import HarchWeb.Acme.Protocol.Client
  ( buildAcmeKeyAuthorization,
    decodeAcmeJsonResponse,
    performAcmeJwsRequest,
    performAcmeRequest,
    renderAcmeResponseBody,
    responseHeaderText,
  )
import HarchWeb.Acme.Protocol.Decode
  ( parseAcmeAuthorizationResponse,
    parseAcmeDirectoryResponse,
    parseAcmeOrderResponse,
  )
import HarchWeb.Acme.Protocol.Types
  ( AcmeAccountSession (..),
    AcmeAuthorizationResponse (..),
    AcmeChallengeResponse (..),
    AcmeDirectoryContext (..),
    AcmeDirectoryResponse (..),
    AcmeJwk,
    AcmeJwsRequestBody (..),
    AcmeJwsResponseExpectation (..),
    AcmeOrderIdentifier (..),
    AcmeOrderResponse (..),
    AcmeRequestAuth (..),
    PreparedAcmeChallenge (..),
  )
import HarchWeb.Server.Config (AcmeConfig (..), ListenerEndpoint (..))
import Network.HTTP.Client qualified as HttpClient

fetchAcmeDirectory :: RuntimeAcmeBindPlan -> HttpClient.Manager -> IO AcmeDirectoryResponse
fetchAcmeDirectory !runtimeAcmePlan manager = do
  request <- HttpClient.parseRequest (Text.unpack (acmeDirectoryUrl (runtimeAcmeListenerConfig runtimeAcmePlan)))
  response <- performAcmeRequest runtimeAcmePlan manager "directory fetch" request [200]
  decodeAcmeJsonResponse runtimeAcmePlan "directory fetch" parseAcmeDirectoryResponse response

createAcmeAccount :: AcmeDirectoryContext -> AcmeJwk -> [Text] -> IO Text
createAcmeAccount context accountJwk contacts = do
  let runtimeAcmePlan = acmeContextBindPlan context
      directory = acmeContextDirectory context
  response <-
    performAcmeJwsRequest
      context
      "account creation"
      AcmeJwsRequestBody
        { acmeJwsRequestAuth = AcmeRequestJwk accountJwk,
          acmeJwsRequestUrl = acmeNewAccountUrl directory,
          acmeJwsRequestPayload =
            jsonObjectBytes
              [ ("termsOfServiceAgreed", jsonBoolBytes True),
                ("contact", jsonArrayBytes (map jsonStringBytes contacts))
              ]
        }
      AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200, 201]}
  maybe
    ( ioError . userError $
        "ACME account creation for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " did not return an account location header."
    )
    pure
    (responseHeaderText "Location" response)

createAcmeOrder :: AcmeAccountSession -> [Text] -> IO (Text, AcmeOrderResponse)
createAcmeOrder session domains = do
  let context = acmeSessionContext session
      runtimeAcmePlan = acmeContextBindPlan context
      directory = acmeContextDirectory context
  response <-
    performAcmeJwsRequest
      context
      "new order"
      AcmeJwsRequestBody
        { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
          acmeJwsRequestUrl = acmeNewOrderUrl directory,
          acmeJwsRequestPayload =
            jsonObjectBytes
              [ ( "identifiers",
                  jsonArrayBytes
                    [ jsonObjectBytes
                        [ ("type", jsonStringBytes "dns"),
                          ("value", jsonStringBytes domain)
                        ]
                    | domain <- domains
                    ]
                )
              ]
        }
      AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200, 201]}
  orderUrl <-
    maybe
      ( ioError . userError $
          "ACME new-order response for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> " did not return an order location header."
      )
      pure
      (responseHeaderText "Location" response)
  createdOrder <- decodeAcmeJsonResponse runtimeAcmePlan "new order" parseAcmeOrderResponse response
  pure (orderUrl, createdOrder)

prepareAcmeAuthorization :: AcmeAccountSession -> AcmeJwk -> Text -> IO PreparedAcmeChallenge
prepareAcmeAuthorization session accountJwk authorizationUrl = do
  let context = acmeSessionContext session
      runtimeAcmePlan = acmeContextBindPlan context
  response <-
    performAcmeJwsRequest
      context
      "authorization fetch"
      AcmeJwsRequestBody
        { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
          acmeJwsRequestUrl = authorizationUrl,
          acmeJwsRequestPayload = LazyByteString.empty
        }
      AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200]}
  authorization <- decodeAcmeJsonResponse runtimeAcmePlan "authorization fetch" parseAcmeAuthorizationResponse response
  challenge <-
    maybe
      ( ioError . userError $
          "ACME authorization for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> " did not provide an http-01 challenge."
      )
      pure
      (find ((== "http-01") . acmeChallengeKind) (acmeAuthorizationChallenges authorization))
  keyAuthorization <- buildAcmeKeyAuthorization runtimeAcmePlan accountJwk (acmeChallengeTokenValue challenge)
  pure
    PreparedAcmeChallenge
      { preparedAcmeChallengeRegistration =
          ActiveAcmeChallenge
            { activeAcmeChallengeDomain = acmeIdentifierValue (acmeAuthorizationIdentifier authorization),
              activeAcmeChallengeToken = acmeChallengeTokenValue challenge,
              activeAcmeChallengeResponse = keyAuthorization
            },
        preparedAcmeChallengeUrl = acmeChallengeUrl challenge
      }

triggerAcmeChallenge :: AcmeAccountSession -> Text -> IO ()
triggerAcmeChallenge session challengeUrl =
  void
    ( performAcmeJwsRequest
        (acmeSessionContext session)
        "challenge acknowledgement"
        AcmeJwsRequestBody
          { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
            acmeJwsRequestUrl = challengeUrl,
            acmeJwsRequestPayload = jsonObjectBytes []
          }
        AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200]}
    )

pollAcmeOrder :: AcmeAccountSession -> Text -> [Text] -> IO AcmeOrderResponse
pollAcmeOrder =
  pollAcmeOrderWithRetries 60 1000000

pollAcmeOrderWithRetries ::
  Int ->
  Int ->
  AcmeAccountSession ->
  Text ->
  [Text] ->
  IO AcmeOrderResponse
pollAcmeOrderWithRetries !maxAttempts !retryDelayMicros session orderUrl wantedStatuses =
  go maxAttempts
  where
    context = acmeSessionContext session
    runtimeAcmePlan = acmeContextBindPlan context
    go !remainingAttempts = do
      response <-
        performAcmeJwsRequest
          context
          "order fetch"
          AcmeJwsRequestBody
            { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
              acmeJwsRequestUrl = orderUrl,
              acmeJwsRequestPayload = LazyByteString.empty
            }
          AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200]}
      order <- decodeAcmeJsonResponse runtimeAcmePlan "order fetch" parseAcmeOrderResponse response
      if acmeOrderStatus order `elem` wantedStatuses
        then pure order
        else case acmeOrderStatus order of
          "pending"
            | remainingAttempts > 0 -> threadDelay retryDelayMicros >> go (remainingAttempts - 1)
          "processing"
            | remainingAttempts > 0 -> threadDelay retryDelayMicros >> go (remainingAttempts - 1)
          "invalid" ->
            ioError . userError $
              "ACME order for listener on "
                <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                <> " became invalid. Ensure the configured domains resolve publicly to this host and that TCP port 80 is reachable from the public internet for http-01 validation.\nbody:\n"
                <> renderAcmeResponseBody response
          statusText ->
            if remainingAttempts > 0
              then threadDelay retryDelayMicros >> go (remainingAttempts - 1)
              else
                ioError . userError $
                  "ACME order for listener on "
                    <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
                    <> " did not reach the expected status. Last status: "
                    <> Text.unpack statusText

finalizeAcmeOrder :: AcmeAccountSession -> Text -> ByteString.ByteString -> IO ()
finalizeAcmeOrder session finalizeUrl csrDerBytes =
  void
    ( performAcmeJwsRequest
        (acmeSessionContext session)
        "order finalization"
        AcmeJwsRequestBody
          { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
            acmeJwsRequestUrl = finalizeUrl,
            acmeJwsRequestPayload = jsonObjectBytes [("csr", jsonStringBytes (base64urlText csrDerBytes))]
          }
        AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Nothing, acmeJwsExpectedStatusCodes = [200]}
    )

fetchAcmeCertificate :: AcmeAccountSession -> Text -> IO LazyByteString.ByteString
fetchAcmeCertificate session certificateUrl = do
  response <-
    performAcmeJwsRequest
      (acmeSessionContext session)
      "certificate fetch"
      AcmeJwsRequestBody
        { acmeJwsRequestAuth = AcmeRequestKid (acmeSessionAccountKid session),
          acmeJwsRequestUrl = certificateUrl,
          acmeJwsRequestPayload = LazyByteString.empty
        }
      AcmeJwsResponseExpectation {acmeJwsAcceptHeader = Just "application/pem-certificate-chain", acmeJwsExpectedStatusCodes = [200]}
  pure (HttpClient.responseBody response)

mailtoAcmeContact :: Text -> Text
mailtoAcmeContact contactAddress =
  if "mailto:" `Text.isPrefixOf` contactAddress
    then contactAddress
    else "mailto:" <> contactAddress

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
