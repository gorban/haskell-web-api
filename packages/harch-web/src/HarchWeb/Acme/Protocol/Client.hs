{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private ACME JWS request and HTTP response operations.
module HarchWeb.Acme.Protocol.Client
  ( buildAcmeJwsBody,
    buildAcmeKeyAuthorization,
    decodeAcmeJsonResponse,
    fetchAcmeNonce,
    performAcmeJwsRequest,
    performAcmeRequest,
    renderAcmeResponseBody,
    responseHeaderText,
  )
where

import Control.Exception (SomeException, try)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Acme.Certbot.Runtime (RuntimeAcmeBindPlan (..))
import HarchWeb.Acme.Crypto (acmeJwkThumbprintBytes, base64urlText)
import HarchWeb.Acme.Json (JsonValue, jsonObjectBytes, jsonStringBytes, parseJsonValue)
import HarchWeb.Acme.OpenSsl (openSslSha256, signOpenSslRs256)
import HarchWeb.Acme.Protocol.Types (AcmeDirectoryResponse (..), AcmeJwk (..), AcmeRequestAuth (..))
import HarchWeb.Server.Config (ListenerEndpoint (..))
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as Http

performAcmeJwsRequest ::
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  AcmeDirectoryResponse ->
  FilePath ->
  String ->
  AcmeRequestAuth ->
  Text ->
  LazyByteString.ByteString ->
  Maybe ByteString.ByteString ->
  [Int] ->
  IO (HttpClient.Response LazyByteString.ByteString)
performAcmeJwsRequest !runtimeAcmePlan manager directory accountKeyPath !actionLabel requestAuth endpointUrl payload maybeAcceptHeader expectedStatusCodes = do
  nonce <- fetchAcmeNonce runtimeAcmePlan manager (acmeNewNonceUrl directory)
  requestBody <- buildAcmeJwsBody runtimeAcmePlan accountKeyPath requestAuth nonce endpointUrl payload
  baseRequest <- HttpClient.parseRequest (Text.unpack endpointUrl)
  let request =
        baseRequest
          { HttpClient.method = "POST",
            HttpClient.requestBody = HttpClient.RequestBodyLBS requestBody,
            HttpClient.requestHeaders =
              [("Content-Type", "application/jose+json")]
                <> maybe [] (\acceptHeader -> [("Accept", acceptHeader)]) maybeAcceptHeader
          }
  performAcmeRequest runtimeAcmePlan manager actionLabel request expectedStatusCodes

fetchAcmeNonce :: RuntimeAcmeBindPlan -> HttpClient.Manager -> Text -> IO Text
fetchAcmeNonce !runtimeAcmePlan manager nonceUrl = do
  request <- HttpClient.parseRequest (Text.unpack nonceUrl)
  response <-
    performAcmeRequest
      runtimeAcmePlan
      manager
      "nonce fetch"
      (request {HttpClient.method = "HEAD"})
      [200, 204]
  maybe
    ( ioError . userError $
        "ACME nonce response for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " did not include a replay-nonce header."
    )
    pure
    (responseHeaderText "Replay-Nonce" response)

buildAcmeJwsBody :: RuntimeAcmeBindPlan -> FilePath -> AcmeRequestAuth -> Text -> Text -> LazyByteString.ByteString -> IO LazyByteString.ByteString
buildAcmeJwsBody !runtimeAcmePlan accountKeyPath requestAuth nonce endpointUrl payload = do
  let protectedBytes =
        LazyByteString.toStrict $
          jsonObjectBytes
            ( [ ("alg", jsonStringBytes "RS256"),
                ("nonce", jsonStringBytes nonce),
                ("url", jsonStringBytes endpointUrl)
              ]
                <> case requestAuth of
                  AcmeRequestJwk jwk ->
                    [ ( "jwk",
                        jsonObjectBytes
                          [ ("e", jsonStringBytes (acmeJwkExponent jwk)),
                            ("kty", jsonStringBytes "RSA"),
                            ("n", jsonStringBytes (acmeJwkModulus jwk))
                          ]
                      )
                    ]
                  AcmeRequestKid accountKid ->
                    [("kid", jsonStringBytes accountKid)]
            )
      protectedText = base64urlText protectedBytes
      payloadText = base64urlText (LazyByteString.toStrict payload)
      signingInput =
        LazyByteString.fromStrict
          (TextEncoding.encodeUtf8 protectedText <> "." <> TextEncoding.encodeUtf8 payloadText)
  signatureBytes <- signOpenSslRs256 runtimeAcmePlan accountKeyPath signingInput
  pure $
    jsonObjectBytes
      [ ("protected", jsonStringBytes protectedText),
        ("payload", jsonStringBytes payloadText),
        ("signature", jsonStringBytes (base64urlText signatureBytes))
      ]

performAcmeRequest ::
  RuntimeAcmeBindPlan ->
  HttpClient.Manager ->
  String ->
  HttpClient.Request ->
  [Int] ->
  IO (HttpClient.Response LazyByteString.ByteString)
performAcmeRequest !runtimeAcmePlan manager !actionLabel request expectedStatusCodes = do
  responseResult <- try (HttpClient.httpLbs request manager) :: IO (Either SomeException (HttpClient.Response LazyByteString.ByteString))
  response <-
    either
      ( \requestError ->
          ioError . userError $
            "Failed "
              <> actionLabel
              <> " for ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> ": "
              <> show requestError
      )
      pure
      responseResult
  let statusCode = Http.statusCode (HttpClient.responseStatus response)
  if statusCode `elem` expectedStatusCodes
    then pure response
    else
      ioError . userError $
        "ACME "
          <> actionLabel
          <> " for listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " failed with status "
          <> show statusCode
          <> ".\nbody:\n"
          <> renderAcmeResponseBody response

decodeAcmeJsonResponse ::
  RuntimeAcmeBindPlan ->
  String ->
  (JsonValue -> Either String a) ->
  HttpClient.Response LazyByteString.ByteString ->
  IO a
decodeAcmeJsonResponse !runtimeAcmePlan !actionLabel decodeJson response =
  either
    ( \decodeError ->
        ioError . userError $
          "Failed to decode ACME "
            <> actionLabel
            <> " response for listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
            <> ": "
            <> decodeError
            <> ".\nbody:\n"
            <> renderAcmeResponseBody response
    )
    pure
    (parseJsonValue (HttpClient.responseBody response) >>= decodeJson)

responseHeaderText :: Http.HeaderName -> HttpClient.Response body -> Maybe Text
responseHeaderText headerName response =
  fmap
    (Text.strip . TextEncoding.decodeUtf8)
    (lookup headerName (HttpClient.responseHeaders response))

renderAcmeResponseBody :: HttpClient.Response LazyByteString.ByteString -> String
renderAcmeResponseBody =
  Text.unpack . TextEncoding.decodeUtf8 . LazyByteString.toStrict . HttpClient.responseBody

buildAcmeKeyAuthorization :: RuntimeAcmeBindPlan -> AcmeJwk -> Text -> IO Text
buildAcmeKeyAuthorization !runtimeAcmePlan accountJwk challengeToken = do
  thumbprintDigest <- openSslSha256 runtimeAcmePlan (LazyByteString.fromStrict (acmeJwkThumbprintBytes accountJwk))
  pure (challengeToken <> "." <> base64urlText thumbprintDigest)

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
