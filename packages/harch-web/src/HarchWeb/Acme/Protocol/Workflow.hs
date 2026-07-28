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
  ( AcmeAuthorizationResponse (..),
    AcmeChallengeResponse (..),
    AcmeDirectoryResponse (..),
    AcmeJwk,
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

createAcmeAccount :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> AcmeJwk -> [Text] -> IO Text
createAcmeAccount !runtimeAcmePlan manager directory accountKeyPath accountJwk contacts = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "account creation"
      (AcmeRequestJwk accountJwk)
      (acmeNewAccountUrl directory)
      ( jsonObjectBytes
          [ ("termsOfServiceAgreed", jsonBoolBytes True),
            ("contact", jsonArrayBytes (map jsonStringBytes contacts))
          ]
      )
      Nothing
      [200, 201]
  maybe
    ( ioError . userError $
        "ACME account creation for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " did not return an account location header."
    )
    pure
    (responseHeaderText "Location" response)

createAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> [Text] -> IO (Text, AcmeOrderResponse)
createAcmeOrder !runtimeAcmePlan manager directory accountKeyPath accountKid domains = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "new order"
      (AcmeRequestKid accountKid)
      (acmeNewOrderUrl directory)
      ( jsonObjectBytes
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
      )
      Nothing
      [200, 201]
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

prepareAcmeAuthorization :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> AcmeJwk -> Text -> IO PreparedAcmeChallenge
prepareAcmeAuthorization !runtimeAcmePlan manager directory accountKeyPath accountKid accountJwk authorizationUrl = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "authorization fetch"
      (AcmeRequestKid accountKid)
      authorizationUrl
      LazyByteString.empty
      Nothing
      [200]
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

triggerAcmeChallenge :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> IO ()
triggerAcmeChallenge !runtimeAcmePlan manager directory accountKeyPath accountKid challengeUrl =
  void
    ( performAcmeJwsRequest
        runtimeAcmePlan
        manager
        directory
        accountKeyPath
        "challenge acknowledgement"
        (AcmeRequestKid accountKid)
        challengeUrl
        (jsonObjectBytes [])
        Nothing
        [200]
    )

pollAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> [Text] -> IO AcmeOrderResponse
pollAcmeOrder !runtimeAcmePlan =
  pollAcmeOrderWithRetries 60 1000000 runtimeAcmePlan

pollAcmeOrderWithRetries ::
  Int ->
  Int ->
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  AcmeDirectoryResponse ->
  FilePath ->
  Text ->
  Text ->
  [Text] ->
  IO AcmeOrderResponse
pollAcmeOrderWithRetries !maxAttempts !retryDelayMicros !runtimeAcmePlan manager directory accountKeyPath accountKid orderUrl wantedStatuses =
  go maxAttempts
  where
    go !remainingAttempts = do
      response <-
        performAcmeJwsRequest
          runtimeAcmePlan
          manager
          directory
          accountKeyPath
          "order fetch"
          (AcmeRequestKid accountKid)
          orderUrl
          LazyByteString.empty
          Nothing
          [200]
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

finalizeAcmeOrder :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> ByteString.ByteString -> IO ()
finalizeAcmeOrder !runtimeAcmePlan manager directory accountKeyPath accountKid finalizeUrl csrDerBytes =
  void
    ( performAcmeJwsRequest
        runtimeAcmePlan
        manager
        directory
        accountKeyPath
        "order finalization"
        (AcmeRequestKid accountKid)
        finalizeUrl
        (jsonObjectBytes [("csr", jsonStringBytes (base64urlText csrDerBytes))])
        Nothing
        [200]
    )

fetchAcmeCertificate :: RuntimeAcmeBindPlan -> HttpClient.Manager -> AcmeDirectoryResponse -> FilePath -> Text -> Text -> IO LazyByteString.ByteString
fetchAcmeCertificate !runtimeAcmePlan manager directory accountKeyPath accountKid certificateUrl = do
  response <-
    performAcmeJwsRequest
      runtimeAcmePlan
      manager
      directory
      accountKeyPath
      "certificate fetch"
      (AcmeRequestKid accountKid)
      certificateUrl
      LazyByteString.empty
      (Just "application/pem-certificate-chain")
      [200]
  pure (HttpClient.responseBody response)

mailtoAcmeContact :: Text -> Text
mailtoAcmeContact contactAddress =
  if "mailto:" `Text.isPrefixOf` contactAddress
    then contactAddress
    else "mailto:" <> contactAddress

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
