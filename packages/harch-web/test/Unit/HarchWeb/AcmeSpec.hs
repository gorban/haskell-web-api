{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.HarchWeb.AcmeSpec (spec) where

import Control.Concurrent (forkIO, killThread, newMVar, readMVar, threadDelay)
import Control.Exception (IOException, finally, try)
import Control.Monad (void)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteStringChar8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import HarchWeb
import qualified Network.HTTP.Client as HttpClient
import qualified Network.HTTP.Types as Http
import qualified Network.Socket as Socket
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec
import Text.ParserCombinators.ReadP (readP_to_S)

sampleEndpoint :: ListenerEndpoint
sampleEndpoint =
  ListenerEndpoint
    { endpointHost = "127.0.0.1",
      endpointPort = 5443
    }

certbotBackend :: AcmeChallengeBackend
certbotBackend =
  CertbotHttp01
    CertbotConfig
      { certbotExecutable = "certbot",
        certbotArguments =
          [ "certonly",
            "--webroot",
            "--http-01-port",
            "8080",
            "--domain=cli.example.com",
            "--cert-name=cli-cert"
          ]
      }

inProcessConfig :: AcmeConfig
inProcessConfig =
  AcmeConfig
    { acmeDirectoryUrl = "https://acme.example/directory",
      acmeContactEmails = ["ops@example.com"],
      acmeDomains = ["example.com", "www.example.com"],
      acmeHttp01Port = 80,
      acmeCertificateDirectory = Nothing,
      acmeChallengeBackend = InProcessHttp01
    }

certbotConfigValue :: AcmeConfig
certbotConfigValue =
  AcmeConfig
    { acmeDirectoryUrl = "https://acme.example/directory",
      acmeContactEmails = ["ops@example.com"],
      acmeDomains = ["example.com", "www.example.com"],
      acmeHttp01Port = 8080,
      acmeCertificateDirectory = Nothing,
      acmeChallengeBackend = certbotBackend
    }

runtimeAcmePlanWith :: AcmeConfig -> RuntimeAcmeBindPlan
runtimeAcmePlanWith acmeConfig =
  RuntimeAcmeBindPlan
    { runtimeAcmeEndpoint = sampleEndpoint,
      runtimeAcmeTlsEndpoint = Just sampleEndpoint,
      runtimeAcmeListenerConfig = acmeConfig
    }

spec :: Spec
spec = do
  describe "ACME helper model coverage" $ do
    it "covers derived Eq and Show instances for internal ACME helper types" $ do
      let challenge =
            ActiveAcmeChallenge
              { activeAcmeChallengeDomain = "example.com",
                activeAcmeChallengeToken = "token",
                activeAcmeChallengeResponse = "token.thumbprint"
              }
          directory =
            AcmeDirectoryResponse
              { acmeNewNonceUrl = "https://acme.example/new-nonce",
                acmeNewAccountUrl = "https://acme.example/new-account",
                acmeNewOrderUrl = "https://acme.example/new-order"
              }
          identifier =
            AcmeOrderIdentifier
              { acmeIdentifierKind = "dns",
                acmeIdentifierValue = "example.com"
              }
          challengeResponse =
            AcmeChallengeResponse
              { acmeChallengeKind = "http-01",
                acmeChallengeUrl = "https://acme.example/challenge/1",
                acmeChallengeTokenValue = "token"
              }
          authorization =
            AcmeAuthorizationResponse
              { acmeAuthorizationIdentifier = identifier,
                acmeAuthorizationChallenges = [challengeResponse]
              }
          orderResponse =
            AcmeOrderResponse
              { acmeOrderStatus = "ready",
                acmeOrderAuthorizations = Just ["https://acme.example/authz/1"],
                acmeOrderFinalizeUrl = Just "https://acme.example/finalize/1",
                acmeOrderCertificateUrl = Just "https://acme.example/cert/1"
              }
          jwk = AcmeJwk {acmeJwkExponent = "AQAB", acmeJwkModulus = "modulus"}
          preparedChallenge =
            PreparedAcmeChallenge
              { preparedAcmeChallengeRegistration = challenge,
                preparedAcmeChallengeUrl = "https://acme.example/challenge/1"
              }
          jsonValue =
            JsonObject
              [ ("bool", JsonBool True),
                ("null", JsonNull),
                ("array", JsonArray [JsonString "value"])
              ]
      challenge `shouldBe` challenge
      show challenge `shouldContain` "activeAcmeChallengeDomain = \"example.com\""
      directory `shouldBe` directory
      show directory `shouldContain` "acmeNewNonceUrl = \"https://acme.example/new-nonce\""
      identifier `shouldBe` identifier
      show identifier `shouldContain` "acmeIdentifierKind = \"dns\""
      challengeResponse `shouldBe` challengeResponse
      show challengeResponse `shouldContain` "acmeChallengeKind = \"http-01\""
      authorization `shouldBe` authorization
      show authorization `shouldContain` "acmeAuthorizationIdentifier = AcmeOrderIdentifier"
      orderResponse `shouldBe` orderResponse
      show orderResponse `shouldContain` "acmeOrderStatus = \"ready\""
      jwk `shouldBe` jwk
      show jwk `shouldContain` "acmeJwkExponent = \"AQAB\""
      AcmeRequestJwk jwk `shouldBe` AcmeRequestJwk jwk
      show (AcmeRequestKid "kid-1") `shouldBe` "AcmeRequestKid \"kid-1\""
      preparedChallenge `shouldBe` preparedChallenge
      show preparedChallenge `shouldContain` "preparedAcmeChallengeUrl = \"https://acme.example/challenge/1\""
      jsonValue `shouldBe` jsonValue
      show jsonValue `shouldContain` "JsonBool True"

    it "covers certbot argument helpers and certificate-name selection branches" $ do
      runtimeCertbotArguments (runtimeAcmePlanWith certbotConfigValue)
        `shouldBe` certbotArguments
          CertbotConfig
            { certbotExecutable = "certbot",
              certbotArguments =
                [ "certonly",
                  "--webroot",
                  "--http-01-port",
                  "8080",
                  "--domain=cli.example.com",
                  "--cert-name=cli-cert"
                ]
            }
      runtimeCertbotArguments (runtimeAcmePlanWith inProcessConfig) `shouldBe` []
      certbotOptionValues "--cert-name" ["certonly", "--cert-name", "named-cert"]
        `shouldBe` ["named-cert"]
      certbotOptionValues "--domain" ["--domain=cli.example.com", "--domain", "other.example.com"]
        `shouldBe` ["other.example.com", "cli.example.com"]
      certbotHasOption "--http-01-port" (runtimeCertbotArguments (runtimeAcmePlanWith certbotConfigValue))
        `shouldBe` True
      certbotHasOption "--missing" ["certonly"] `shouldBe` False
      splitCertbotDomainValue " example.com , www.example.com ,, "
        `shouldBe` ["example.com", "www.example.com"]
      firstCertbotDomain ["-d", "one.example.com", "--domains=two.example.com,three.example.com"]
        `shouldBe` Just "one.example.com"
      certbotCertificateName (runtimeAcmePlanWith certbotConfigValue) `shouldBe` Right "cli-cert"
      certbotCertificateName
        ( runtimeAcmePlanWith
            certbotConfigValue
              { acmeChallengeBackend =
                  CertbotHttp01
                    CertbotConfig
                      { certbotExecutable = "certbot",
                        certbotArguments = ["certonly", "--domains=domain.example.com"]
                      },
                acmeDomains = []
              }
        )
        `shouldBe` Right "domain.example.com"
      certbotCertificateName
        ( runtimeAcmePlanWith
            certbotConfigValue
              { acmeChallengeBackend =
                  CertbotHttp01
                    CertbotConfig
                      { certbotExecutable = "certbot",
                        certbotArguments = ["certonly"]
                      }
              }
        )
        `shouldBe` Right "example.com"
      certbotCertificateName
        ( runtimeAcmePlanWith
            inProcessConfig
              { acmeDomains = []
              }
        )
        `shouldSatisfy` isLeftWith "requires ACME domains or certbot arguments"

    it "prepares ACME runtime plans as manual TLS listeners after certificate acquisition" $ do
      withSystemTempDirectory "harch-web-certbot-shared" $ \sharedDirectory ->
        withFakeCertbotScript
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "mkdir -p \"$config_dir/live/test-cert\"",
            "printf '%s\\n' 'FAKE CERT' > \"$config_dir/live/test-cert/fullchain.pem\"",
            "printf '%s\\n' 'FAKE KEY' > \"$config_dir/live/test-cert/privkey.pem\""
          ]
          $ \scriptPath -> do
            let certbotConfig =
                  CertbotConfig
                    { certbotExecutable = scriptPath,
                      certbotArguments = ["certonly", "--cert-name", "test-cert"]
                    }
                certbotPlan =
                  runtimeAcmePlanWith
                    certbotConfigValue
                      { acmeCertificateDirectory = Just sharedDirectory,
                        acmeChallengeBackend = CertbotHttp01 certbotConfig
                      }
            (certbotManualPlan, certbotCleanupDirectory) <-
              prepareCertbotManualTlsBindPlan certbotPlan certbotConfig
            ( do
                certbotManualPlan `shouldSatisfy` (/= Nothing)
                case certbotManualPlan of
                  Nothing ->
                    expectationFailure "Expected certbot-backed ACME plan to produce a manual TLS bind plan"
                  Just resolvedCertbotManualPlan -> do
                    tlsEndpoint resolvedCertbotManualPlan `shouldBe` sampleEndpoint
                    tlsCredentialSourceKind resolvedCertbotManualPlan `shouldBe` ManualTlsCredentials
                    tlsStartupMode resolvedCertbotManualPlan `shouldBe` RequireCertificateFiles
                    tlsCertificateFile resolvedCertbotManualPlan `shouldBe` sharedDirectory </> "fullchain.pem"
                    tlsPrivateKeyFile resolvedCertbotManualPlan `shouldBe` sharedDirectory </> "privkey.pem"
                    readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` "FAKE CERT\n"
                    readFile (sharedDirectory </> "privkey.pem") `shouldReturn` "FAKE KEY\n"
              )
              `finally` removePathForcibly certbotCleanupDirectory
      withHttpAcmeServer $ \server ->
        withSystemTempDirectory "harch-web-in-process-shared" $ \sharedDirectory ->
          withSystemTempDirectory "harch-web-in-process-account" $ \tempDirectory -> do
            challengeStore <- AcmeChallengeStore <$> newMVar []
            let accountKeyPath = tempDirectory </> "account-key.pem"
                inProcessPlan =
                  runtimeAcmePlanWith
                    inProcessConfig
                      { acmeDirectoryUrl = serverBaseUrl server <> "/directory-immediately-valid",
                        acmeCertificateDirectory = Just sharedDirectory
                      }
            withFakeOpenSslExecutable accountKeyPath $ \_ -> do
              writeFile accountKeyPath "fake-account-key"
              (inProcessManualPlan, inProcessCleanupDirectory) <-
                prepareInProcessManualTlsBindPlan inProcessPlan challengeStore
              ( do
                  inProcessManualPlan `shouldSatisfy` (/= Nothing)
                  case inProcessManualPlan of
                    Nothing ->
                      expectationFailure "Expected in-process ACME plan to produce a manual TLS bind plan"
                    Just resolvedInProcessManualPlan -> do
                      tlsEndpoint resolvedInProcessManualPlan `shouldBe` sampleEndpoint
                      tlsCredentialSourceKind resolvedInProcessManualPlan `shouldBe` ManualTlsCredentials
                      tlsStartupMode resolvedInProcessManualPlan `shouldBe` RequireCertificateFiles
                      tlsCertificateFile resolvedInProcessManualPlan `shouldBe` sharedDirectory </> "fullchain.pem"
                      tlsPrivateKeyFile resolvedInProcessManualPlan `shouldBe` sharedDirectory </> "privkey.pem"
                      readFile (sharedDirectory </> "fullchain.pem") `shouldReturn` "PEM CERT"
                      readFile (sharedDirectory </> "privkey.pem") `shouldReturn` "fake-account-key"
                )
                `finally` removePathForcibly inProcessCleanupDirectory

    it "covers challenge matching and store update helpers" $ do
      challengeStore <- AcmeChallengeStore <$> newMVar []
      let challenge =
            ActiveAcmeChallenge
              { activeAcmeChallengeDomain = "example.com",
                activeAcmeChallengeToken = "token-1",
                activeAcmeChallengeResponse = "response-1"
              }
          matchingRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1",
                Wai.requestHeaders = [("Host", "example.com:80")]
              }
          mismatchedHostRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1",
                Wai.requestHeaders = [("Host", "other.example.com")]
              }
          missingTokenRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/missing",
                Wai.requestHeaders = [("Host", "example.com")]
              }
          hostlessRequest =
            Wai.defaultRequest
              { Wai.rawPathInfo = "/.well-known/acme-challenge/token-1"
              }
      acmeHttp01ChallengeToken matchingRequest `shouldBe` Just "token-1"
      acmeHttp01ChallengeToken missingTokenRequest `shouldBe` Nothing
      requestHostWithoutPort matchingRequest `shouldBe` Just "example.com"
      matchesRuntimeAcmeChallenge matchingRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge mismatchedHostRequest challenge `shouldBe` False
      matchesRuntimeAcmeChallenge hostlessRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge missingTokenRequest challenge `shouldBe` False
      registerAcmeChallenges challengeStore [challenge]
      registeredChallenges <- unwrapChallengeStore challengeStore
      registeredChallenges `shouldBe` [challenge]
      challengeResponse <- acmeChallengeResponseForRequest challengeStore matchingRequest
      case challengeResponse of
        Just _ -> pure ()
        Nothing -> expectationFailure "expected a registered ACME challenge response"
      unregisterAcmeChallenges challengeStore [challenge]
      unwrapChallengeStore challengeStore `shouldReturn` []

  describe "ACME JSON parsing helpers" $ do
    it "parses JSON values including escapes, booleans, nulls, arrays, and empty collections" $ do
      parseJsonValue "{}" `shouldBe` Right (JsonObject [])
      parseJsonValue "[]" `shouldBe` Right (JsonArray [])
      parseJsonValue " [ \"a\" , \"b\" ] "
        `shouldBe` Right (JsonArray [JsonString "a", JsonString "b"])
      parseJsonValue "{\"field\":\"value\"}"
        `shouldBe` Right (JsonObject [("field", JsonString "value")])
      parseJsonValue "true" `shouldBe` Right (JsonBool True)
      parseJsonValue "false" `shouldBe` Right (JsonBool False)
      parseJsonValue "null" `shouldBe` Right JsonNull
      parseJsonValue "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u263a\""
        `shouldBe` Right (JsonString "\"\\/\b\f\n\r\t☺")
      parseJsonValue "\"\\uZZZZ\"" `shouldBe` Left "invalid JSON"
      parseJsonValue "not-json" `shouldBe` Left "invalid JSON"

    it "covers JSON field accessors, encoders, and ACME response decoders" $ do
      let objectFields =
            [ ("name", JsonString "value"),
              ("items", JsonArray [JsonString "a", JsonString "b"]),
              ("missingOrNull", JsonNull),
              ("flag", JsonBool False)
            ]
          objectValue = JsonObject objectFields
      jsonObjectFields "label" objectValue `shouldBe` Right objectFields
      jsonObjectFields "label" (JsonArray []) `shouldBe` Left "label was not a JSON object"
      jsonArrayItems "items" (JsonArray [JsonString "a"]) `shouldBe` Right [JsonString "a"]
      jsonArrayItems "items" (JsonString "a") `shouldBe` Left "items was not a JSON array"
      jsonRequiredField "name" objectFields `shouldBe` Right (JsonString "value")
      jsonRequiredField "missing" objectFields `shouldBe` Left "missing required field missing"
      jsonRequiredTextField "name" objectFields `shouldBe` Right "value"
      jsonRequiredTextField "flag" objectFields `shouldBe` Left "field flag was not a JSON string"
      jsonOptionalTextField "missing" objectFields `shouldBe` Right Nothing
      jsonOptionalTextField "missingOrNull" objectFields `shouldBe` Right Nothing
      jsonOptionalTextField "name" objectFields `shouldBe` Right (Just "value")
      jsonOptionalTextField "flag" objectFields `shouldBe` Left "field flag was not a JSON string"
      jsonOptionalTextArrayField "missing" objectFields `shouldBe` Right Nothing
      jsonOptionalTextArrayField "missingOrNull" objectFields `shouldBe` Right Nothing
      jsonOptionalTextArrayField "items" objectFields `shouldBe` Right (Just ["a", "b"])
      jsonOptionalTextArrayField "flag" objectFields `shouldBe` Left "flag was not a JSON array"
      jsonOptionalTextArrayField
        "items"
        [("items", JsonArray [JsonBool False])]
        `shouldBe` Left "field items was not a JSON string"
      jsonTextField "name" (JsonString "value") `shouldBe` Right "value"
      jsonTextField "flag" (JsonBool False) `shouldBe` Left "field flag was not a JSON string"
      jsonStringBytes "\"\\\b\f\n\r\tplain"
        `shouldBe` "\"\\\"\\\\\\b\\f\\n\\r\\tplain\""
      escapeJsonCharacter '"' `shouldBe` "\\\""
      escapeJsonCharacter '\\' `shouldBe` "\\\\"
      escapeJsonCharacter '\b' `shouldBe` "\\b"
      escapeJsonCharacter '\f' `shouldBe` "\\f"
      escapeJsonCharacter '\n' `shouldBe` "\\n"
      escapeJsonCharacter '\r' `shouldBe` "\\r"
      escapeJsonCharacter '\t' `shouldBe` "\\t"
      escapeJsonCharacter 'x' `shouldBe` "x"
      jsonBoolBytes True `shouldBe` "true"
      jsonBoolBytes False `shouldBe` "false"
      jsonArrayBytes [] `shouldBe` "[]"
      jsonArrayBytes ["1", "2"] `shouldBe` "[1,2]"
      jsonObjectBytes [("a", "1"), ("b", "2")] `shouldBe` "{\"a\":1,\"b\":2}"
      parseAcmeDirectoryResponse (JsonString "bad") `shouldBe` Left "AcmeDirectoryResponse was not a JSON object"
      parseAcmeDirectoryResponse objectValue `shouldBe` Left "missing required field newNonce"
      parseAcmeDirectoryResponse
        ( JsonObject
            [ ("newNonce", JsonString "nonce"),
              ("newAccount", JsonString "account"),
              ("newOrder", JsonString "order")
            ]
        )
        `shouldBe` Right
          AcmeDirectoryResponse
            { acmeNewNonceUrl = "nonce",
              acmeNewAccountUrl = "account",
              acmeNewOrderUrl = "order"
            }
      parseAcmeOrderIdentifier (JsonString "bad") `shouldBe` Left "AcmeOrderIdentifier was not a JSON object"
      parseAcmeOrderIdentifier
        (JsonObject [("type", JsonString "dns"), ("value", JsonString "example.com")])
        `shouldBe` Right
          AcmeOrderIdentifier
            { acmeIdentifierKind = "dns",
              acmeIdentifierValue = "example.com"
            }
      parseAcmeChallengeResponse
        (JsonString "bad")
        `shouldBe` Left "AcmeChallengeResponse was not a JSON object"
      parseAcmeChallengeResponse
        ( JsonObject
            [ ("type", JsonString "http-01"),
              ("url", JsonString "challenge-url"),
              ("token", JsonString "token")
            ]
        )
        `shouldBe` Right
          AcmeChallengeResponse
            { acmeChallengeKind = "http-01",
              acmeChallengeUrl = "challenge-url",
              acmeChallengeTokenValue = "token"
            }
      parseAcmeAuthorizationResponse
        (JsonString "bad")
        `shouldBe` Left "AcmeAuthorizationResponse was not a JSON object"
      parseAcmeAuthorizationResponse
        ( JsonObject
            [ ("identifier", JsonObject [("type", JsonString "dns"), ("value", JsonString "example.com")]),
              ("challenges", JsonString "bad")
            ]
        )
        `shouldBe` Left "challenges was not a JSON array"
      parseAcmeAuthorizationResponse
        ( JsonObject
            [ ("identifier", JsonObject [("type", JsonString "dns"), ("value", JsonString "example.com")]),
              ("challenges", JsonArray [JsonObject [("type", JsonString "http-01"), ("url", JsonString "challenge-url"), ("token", JsonString "token")]])
            ]
        )
        `shouldBe` Right
          AcmeAuthorizationResponse
            { acmeAuthorizationIdentifier =
                AcmeOrderIdentifier
                  { acmeIdentifierKind = "dns",
                    acmeIdentifierValue = "example.com"
                  },
              acmeAuthorizationChallenges =
                [ AcmeChallengeResponse
                    { acmeChallengeKind = "http-01",
                      acmeChallengeUrl = "challenge-url",
                      acmeChallengeTokenValue = "token"
                    }
                ]
            }
      parseAcmeOrderResponse
        (JsonString "bad")
        `shouldBe` Left "AcmeOrderResponse was not a JSON object"
      parseAcmeOrderResponse
        ( JsonObject
            [ ("status", JsonString "valid"),
              ("authorizations", JsonNull),
              ("finalize", JsonString "finalize-url"),
              ("certificate", JsonString "certificate-url")
            ]
        )
        `shouldBe` Right
          AcmeOrderResponse
            { acmeOrderStatus = "valid",
              acmeOrderAuthorizations = Nothing,
              acmeOrderFinalizeUrl = Just "finalize-url",
              acmeOrderCertificateUrl = Just "certificate-url"
            }
      parseAcmeOrderResponse
        ( JsonObject
            [ ("status", JsonString "ready"),
              ("authorizations", JsonArray [JsonString "authz-1", JsonString "authz-2"])
            ]
        )
        `shouldBe` Right
          AcmeOrderResponse
            { acmeOrderStatus = "ready",
              acmeOrderAuthorizations = Just ["authz-1", "authz-2"],
              acmeOrderFinalizeUrl = Nothing,
              acmeOrderCertificateUrl = Nothing
            }

    it "parses certificate-request config, contacts, thumbprints, and hex helpers" $ do
      acmeCertificateRequestConfig ["example.com", "www.example.com"]
        `shouldSatisfy` \rendered ->
          all
            (`isInfixOf` rendered)
            [ "CN = example.com",
              "subjectAltName = DNS:example.com,DNS:www.example.com"
            ]
      acmeCertificateRequestConfig []
        `shouldSatisfy` isInfixOf "CN = localhost"
      mailtoAcmeContact "ops@example.com" `shouldBe` "mailto:ops@example.com"
      mailtoAcmeContact "mailto:ops@example.com" `shouldBe` "mailto:ops@example.com"
      base64urlText "??" `shouldBe` "Pz8"
      acmeJwkThumbprintBytes (AcmeJwk "AQAB" "modulus")
        `shouldBe` "{\"e\":\"AQAB\",\"kty\":\"RSA\",\"n\":\"modulus\"}"
      hexTextToByteString "A1 b2\nC3" `shouldBe` Right "\xa1\xb2\xc3"
      hexTextToByteString "ABC" `shouldBe` Left "hex string had an odd length"
      hexTextToByteString "GG" `shouldBe` Left "invalid hex digit pair: GG"
      readP_to_S jsonStringCharacterParser "" `shouldBe` []
      readP_to_S unicodeJsonCharacterParser "ZZZZ" `shouldBe` []

  describe "ACME OpenSSL and HTTP helpers" $ do
    it "covers OpenSSL wrapper success, launch, and failure paths" $ do
      withFakeOpenSslScript
        [ "#!/bin/sh",
          "set -eu",
          "printf 'stdout-text'",
          "printf 'stderr-text' >&2",
          "exit 0"
        ]
        $ \scriptPath -> do
          withPrependedPathDirectory (takeDirectory scriptPath) $
            runOpenSslTextCommand (runtimeAcmePlanWith inProcessConfig) ["dgst"]
              `shouldReturn` "stdout-text"
      withEmptyExecutablePath $
        expectIOExceptionContainsAll
          (runOpenSslTextCommand (runtimeAcmePlanWith inProcessConfig) ["dgst"])
          ["Failed to launch openssl for ACME listener on 127.0.0.1:5443", "No such file or directory"]
      withFakeOpenSslScript
        [ "#!/bin/sh",
          "printf 'bad-stdout'",
          "printf 'bad-stderr' >&2",
          "exit 7"
        ]
        $ \scriptPath -> do
          withPrependedPathDirectory (takeDirectory scriptPath) $
            expectIOExceptionContainsAll
              (runOpenSslTextCommand (runtimeAcmePlanWith inProcessConfig) ["dgst"])
              ["OpenSSL failed for ACME listener on 127.0.0.1:5443", "exit code ExitFailure 7", "bad-stdout", "bad-stderr"]

    it "covers account-key, CSR, JWK, signing, and sha helpers" $
      withSystemTempDirectory "harch-web-acme-helper" $ \tempDirectory -> do
        let accountKeyPath = tempDirectory </> "account-key.pem"
            fixturePrivateKeyPath = tempDirectory </> "fixture-key.pem"
            privateKeyPath = tempDirectory </> "private-key.pem"
            csrConfigPath = tempDirectory </> "csr.cnf"
            csrPemPath = tempDirectory </> "request.csr"
            csrDerPath = tempDirectory </> "request.der"
        writeFile fixturePrivateKeyPath "fixture-private-key"
        withFakeOpenSslExecutable fixturePrivateKeyPath $ \_scriptPath -> do
          generateAcmeAccountKey (runtimeAcmePlanWith inProcessConfig) accountKeyPath
          ByteString.readFile accountKeyPath `shouldReturn` "FAKE ACCOUNT KEY\n"
          generateAcmeCertificateRequest (runtimeAcmePlanWith inProcessConfig) ["example.com", "www.example.com"] privateKeyPath csrConfigPath csrPemPath csrDerPath
          doesFileExist privateKeyPath `shouldReturn` True
          ByteString.readFile csrDerPath `shouldReturn` "FAKE DER"
          loadAcmeJwk (runtimeAcmePlanWith inProcessConfig) accountKeyPath
            `shouldReturn` AcmeJwk {acmeJwkExponent = "AQAB", acmeJwkModulus = "obLD1OX2BxgpOktcbX6PkA"}
          signOpenSslRs256 (runtimeAcmePlanWith inProcessConfig) accountKeyPath "signing-input"
            `shouldReturn` "fake-bytes"
          openSslSha256 (runtimeAcmePlanWith inProcessConfig) "hash-input"
            `shouldReturn` "fake-bytes"
          buildAcmeKeyAuthorization (runtimeAcmePlanWith inProcessConfig) (AcmeJwk "AQAB" "obLD1OX2BxgpOktcbX6PkA") "token"
            `shouldReturn` "token.ZmFrZS1ieXRlcw"
          body <- buildAcmeJwsBody (runtimeAcmePlanWith inProcessConfig) accountKeyPath (AcmeRequestJwk (AcmeJwk "AQAB" "obLD1OX2BxgpOktcbX6PkA")) "nonce-1" "https://acme.example/order" "{}"
          parseJsonValue body `shouldSatisfy` isRightValue
          kidBody <- buildAcmeJwsBody (runtimeAcmePlanWith inProcessConfig) accountKeyPath (AcmeRequestKid "kid-1") "nonce-2" "https://acme.example/account" ""
          parseJsonValue kidBody `shouldSatisfy` isRightValue

    it "covers JWK error handling for missing or invalid moduli" $
      withSystemTempDirectory "harch-web-acme-jwk-errors" $ \tempDirectory -> do
        let accountKeyPath = tempDirectory </> "account-key.pem"
        writeFile accountKeyPath "fake"
        withFakeOpenSslScript
          [ "#!/bin/sh",
            "printf 'no-prefix'"
          ]
          $ \scriptPath -> do
            withPrependedPathDirectory (takeDirectory scriptPath) $
              expectIOExceptionContainsAll
                (loadAcmeJwk (runtimeAcmePlanWith inProcessConfig) accountKeyPath)
                ["OpenSSL did not return an RSA modulus for ACME listener on 127.0.0.1:5443"]
        withFakeOpenSslScript
          [ "#!/bin/sh",
            "printf 'Modulus=GG'"
          ]
          $ \scriptPath -> do
            withPrependedPathDirectory (takeDirectory scriptPath) $
              expectIOExceptionContainsAll
                (loadAcmeJwk (runtimeAcmePlanWith inProcessConfig) accountKeyPath)
                ["OpenSSL returned an invalid RSA modulus for ACME listener on 127.0.0.1:5443", "invalid hex digit pair: GG"]

    it "covers HTTP ACME helpers across success and error responses" $
      withHttpAcmeServer $ \server -> do
        manager <- HttpClient.newManager HttpClient.defaultManagerSettings
        let plan = runtimeAcmePlanWith inProcessConfig {acmeDirectoryUrl = serverDirectoryUrl server}
            directory =
              AcmeDirectoryResponse
                { acmeNewNonceUrl = serverBaseUrl server <> "/new-nonce",
                  acmeNewAccountUrl = serverBaseUrl server <> "/new-account",
                  acmeNewOrderUrl = serverBaseUrl server <> "/new-order"
                }
        fetchAcmeDirectory plan manager `shouldReturn` directory
        fetchAcmeNonce plan manager (serverBaseUrl server <> "/new-nonce")
          `shouldReturn` "nonce-1"
        withSystemTempDirectory "harch-web-acme-http" $ \tempDirectory -> do
          let accountKeyPath = tempDirectory </> "account-key.pem"
          withFakeOpenSslExecutable accountKeyPath $ \_ -> do
            writeFile accountKeyPath "fake-account-key"
            response <-
              performAcmeJwsRequest
                plan
                manager
                directory
                accountKeyPath
                "account creation"
                (AcmeRequestKid "kid-1")
                (serverBaseUrl server <> "/capture")
                "{}"
                (Just "application/json")
                [200]
            responseHeaderText "X-Seen-Accept" response `shouldBe` Just "application/json"
            responseHeaderText "Missing" response `shouldBe` Nothing
            renderAcmeResponseBody response `shouldBe` "{\"status\":\"ok\"}"
            decodeAcmeJsonResponse plan "capture" parseAcmeDirectoryResponse response
              `shouldThrow` errorContaining "Failed to decode ACME capture response"
            createAcmeAccount plan manager directory accountKeyPath (AcmeJwk "AQAB" "modulus") ["mailto:ops@example.com"]
              `shouldReturn` (serverBaseUrl server <> "/account/1")
            createAcmeOrder plan manager directory accountKeyPath "kid-1" ["example.com"]
              `shouldReturn` ( serverBaseUrl server <> "/order/1",
                               AcmeOrderResponse
                                 { acmeOrderStatus = "ready",
                                   acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                                   acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                                   acmeOrderCertificateUrl = Nothing
                                 }
                             )
            preparedChallenge <-
              prepareAcmeAuthorization plan manager directory accountKeyPath "kid-1" (AcmeJwk "AQAB" "modulus") (serverBaseUrl server <> "/authz/1")
            preparedChallenge
              `shouldSatisfy` isPreparedChallengeFor (serverBaseUrl server <> "/challenge/1")
            triggerAcmeChallenge plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/challenge/1")
            finalizeAcmeOrder plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/finalize/1") "csr"
            fetchAcmeCertificate plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/cert/1")
              `shouldReturn` "PEM CERT"
            pollAcmeOrder
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-ready")
              ["ready"]
              `shouldReturn` AcmeOrderResponse
                { acmeOrderStatus = "ready",
                  acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                  acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                  acmeOrderCertificateUrl = Nothing
                }
            pollAcmeOrder
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-valid")
              ["valid"]
              `shouldReturn` AcmeOrderResponse
                { acmeOrderStatus = "valid",
                  acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                  acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                  acmeOrderCertificateUrl = Just (serverBaseUrl server <> "/cert/1")
                }
            pollAcmeOrderWithRetries
              1
              0
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-pending-ready")
              ["ready"]
              `shouldReturn` AcmeOrderResponse
                { acmeOrderStatus = "ready",
                  acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                  acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                  acmeOrderCertificateUrl = Nothing
                }
            pollAcmeOrderWithRetries
              1
              0
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-processing-valid")
              ["valid"]
              `shouldReturn` AcmeOrderResponse
                { acmeOrderStatus = "valid",
                  acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                  acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                  acmeOrderCertificateUrl = Just (serverBaseUrl server <> "/cert/1")
                }
            pollAcmeOrderWithRetries
              1
              0
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-waiting-ready")
              ["ready"]
              `shouldReturn` AcmeOrderResponse
                { acmeOrderStatus = "ready",
                  acmeOrderAuthorizations = Just [serverBaseUrl server <> "/authz/1"],
                  acmeOrderFinalizeUrl = Just (serverBaseUrl server <> "/finalize/1"),
                  acmeOrderCertificateUrl = Nothing
                }

    it "covers HTTP ACME error paths" $
      withHttpAcmeServer $ \server -> do
        manager <- HttpClient.newManager HttpClient.defaultManagerSettings
        let plan = runtimeAcmePlanWith inProcessConfig {acmeDirectoryUrl = serverDirectoryUrl server}
            directory =
              AcmeDirectoryResponse
                { acmeNewNonceUrl = serverBaseUrl server <> "/new-nonce",
                  acmeNewAccountUrl = serverBaseUrl server <> "/new-account",
                  acmeNewOrderUrl = serverBaseUrl server <> "/new-order"
                }
        withSystemTempDirectory "harch-web-acme-http-errors" $ \tempDirectory -> do
          let accountKeyPath = tempDirectory </> "account-key.pem"
          withFakeOpenSslExecutable accountKeyPath $ \_ -> do
            writeFile accountKeyPath "fake-account-key"
            fetchAcmeNonce plan manager (serverBaseUrl server <> "/new-nonce-missing")
              `shouldThrow` errorContaining "did not include a replay-nonce header"
            fetchAcmeDirectory
              plan
                { runtimeAcmeListenerConfig =
                    inProcessConfig
                      { acmeDirectoryUrl = serverBaseUrl server <> "/status-500"
                      }
                }
              manager
              `shouldThrow` errorContaining "directory fetch"
            fetchAcmeDirectory
              plan
                { runtimeAcmeListenerConfig =
                    inProcessConfig
                      { acmeDirectoryUrl = serverBaseUrl server <> "/directory-bad-json"
                      }
                }
              manager
              `shouldThrow` errorContaining "Failed to decode ACME directory fetch response"
            request <- HttpClient.parseRequest (Text.unpack (serverBaseUrl server <> "/status-500"))
            expectIOExceptionContainsAll
              (performAcmeRequest plan manager "failing request" request [200])
              ["ACME failing request for listener on 127.0.0.1:5443 failed with status 500", ".\nbody:\nboom"]
            missingRequest <- HttpClient.parseRequest "http://127.0.0.1:1/missing"
            expectIOExceptionContainsAll
              (performAcmeRequest plan manager "network request" missingRequest [200])
              ["Failed network request for ACME listener on 127.0.0.1:5443", "HttpExceptionRequest"]
            captureRequest <-
              (\httpRequest -> httpRequest {HttpClient.method = "POST"})
                <$> HttpClient.parseRequest (Text.unpack (serverBaseUrl server <> "/capture"))
            captureResponse <- performAcmeRequest plan manager "capture" captureRequest [200]
            expectIOExceptionContainsAll
              (decodeAcmeJsonResponse plan "invalid decode" parseAcmeDirectoryResponse captureResponse)
              ["Failed to decode ACME invalid decode response for listener on 127.0.0.1:5443", ".\nbody:\n{\"status\":\"ok\"}"]
            createAcmeAccount plan manager directory accountKeyPath (AcmeJwk "AQAB" "modulus") ["mailto:ops@example.com"]
              `shouldReturn` (serverBaseUrl server <> "/account/1")
            let missingAccountDirectory =
                  directory
                    { acmeNewAccountUrl = serverBaseUrl server <> "/new-account-missing-location"
                    }
                missingOrderDirectory =
                  directory
                    { acmeNewOrderUrl = serverBaseUrl server <> "/new-order-missing-location"
                    }
            createAcmeAccount plan manager missingAccountDirectory accountKeyPath (AcmeJwk "AQAB" "modulus") ["mailto:ops@example.com"]
              `shouldThrow` errorContaining "did not return an account location header"
            createAcmeOrder plan manager missingOrderDirectory accountKeyPath "kid-1" ["example.com"]
              `shouldThrow` errorContaining "did not return an order location header"
            createAcmeAccount
              plan
              manager
              directory {acmeNewAccountUrl = serverBaseUrl server <> "/status-500"}
              accountKeyPath
              (AcmeJwk "AQAB" "modulus")
              ["mailto:ops@example.com"]
              `shouldThrow` errorContaining "account creation"
            createAcmeOrder
              plan
              manager
              directory {acmeNewOrderUrl = serverBaseUrl server <> "/status-500"}
              accountKeyPath
              "kid-1"
              ["example.com"]
              `shouldThrow` errorContaining "ACME new order for listener"
            createAcmeOrder
              plan
              manager
              directory {acmeNewOrderUrl = serverBaseUrl server <> "/new-order-bad-json"}
              accountKeyPath
              "kid-1"
              ["example.com"]
              `shouldThrow` errorContaining "Failed to decode ACME new order response"
            prepareAcmeAuthorization plan manager directory accountKeyPath "kid-1" (AcmeJwk "AQAB" "modulus") (serverBaseUrl server <> "/authz-no-http01")
              `shouldThrow` errorContaining "did not provide an http-01 challenge"
            prepareAcmeAuthorization plan manager directory accountKeyPath "kid-1" (AcmeJwk "AQAB" "modulus") (serverBaseUrl server <> "/status-500")
              `shouldThrow` errorContaining "authorization fetch"
            prepareAcmeAuthorization plan manager directory accountKeyPath "kid-1" (AcmeJwk "AQAB" "modulus") (serverBaseUrl server <> "/authz-bad-json")
              `shouldThrow` errorContaining "Failed to decode ACME authorization fetch response"
            triggerAcmeChallenge plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/status-500")
              `shouldThrow` errorContaining "challenge acknowledgement"
            finalizeAcmeOrder plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/status-500") "csr"
              `shouldThrow` errorContaining "order finalization"
            pollAcmeOrder
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-invalid")
              ["ready"]
              `shouldThrow` errorContaining "became invalid"
            pollAcmeOrderWithRetries
              0
              0
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-pending-ready")
              ["ready"]
              `shouldThrow` errorContaining "Last status: pending"
            pollAcmeOrderWithRetries
              0
              0
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-processing-valid")
              ["ready"]
              `shouldThrow` errorContaining "Last status: processing"
            fetchAcmeCertificate plan manager directory accountKeyPath "kid-1" (serverBaseUrl server <> "/status-500")
              `shouldThrow` errorContaining "certificate fetch"

    it "covers in-process ACME challenge helper branches" $
      withHttpAcmeServer $ \server -> do
        let basePlan directoryUrl =
              runtimeAcmePlanWith inProcessConfig {acmeDirectoryUrl = directoryUrl}
        withSystemTempDirectory "harch-web-acme-in-process" $ \tempDirectory -> do
          challengeStore <- AcmeChallengeStore <$> newMVar []
          let accountKeyPath = tempDirectory </> "account-key.pem"
              privateKeyPath = tempDirectory </> "server-key.pem"
              certificatePath = tempDirectory </> "server-cert.pem"
          withFakeOpenSslExecutable accountKeyPath $ \_ -> do
            writeFile accountKeyPath "fake-account-key"
            runInProcessAcmeChallenge
              (runtimeAcmePlanWith inProcessConfig {acmeDomains = [], acmeDirectoryUrl = serverDirectoryUrl server})
              challengeStore
              tempDirectory
              certificatePath
              privateKeyPath
              `shouldThrow` errorContaining "requires ACME domains"
            runInProcessAcmeChallenge
              (basePlan (serverBaseUrl server <> "/directory-no-authorizations"))
              challengeStore
              tempDirectory
              certificatePath
              privateKeyPath
              `shouldThrow` errorContaining "did not include authorization URLs"
            runInProcessAcmeChallenge
              (basePlan (serverBaseUrl server <> "/directory-ready-no-finalize"))
              challengeStore
              tempDirectory
              certificatePath
              privateKeyPath
              `shouldThrow` errorContaining "did not include a finalize URL"
            runInProcessAcmeChallenge
              (basePlan (serverBaseUrl server <> "/directory-valid-no-certificate"))
              challengeStore
              tempDirectory
              certificatePath
              privateKeyPath
              `shouldThrow` errorContaining "did not include a certificate URL"
            runInProcessAcmeChallenge
              (basePlan (serverBaseUrl server <> "/directory-immediately-valid"))
              challengeStore
              tempDirectory
              certificatePath
              privateKeyPath
            ByteString.readFile certificatePath `shouldReturn` "PEM CERT"
        withSystemTempDirectory "harch-web-acme-in-process-missing-key" $ \tempDirectory -> do
          challengeStore <- AcmeChallengeStore <$> newMVar []
          let privateKeyPath = tempDirectory </> "server-key.pem"
              certificatePath = tempDirectory </> "server-cert.pem"
          withFakeOpenSslScript
            [ "#!/bin/sh",
              "set -eu",
              "command=\"$1\"",
              "shift",
              "case \"$command\" in",
              "  genrsa)",
              "    output=''",
              "    while [ \"$#\" -gt 0 ]; do",
              "      case \"$1\" in",
              "        -out) output=\"$2\"; shift 2 ;;",
              "        *) shift ;;",
              "      esac",
              "    done",
              "    printf '%s\\n' 'FAKE ACCOUNT KEY' > \"$output\"",
              "    ;;",
              "  rsa)",
              "    printf 'Modulus=A1B2C3D4E5F60718293A4B5C6D7E8F90\\n'",
              "    ;;",
              "  req)",
              "    input=''",
              "    output=''",
              "    while [ \"$#\" -gt 0 ]; do",
              "      case \"$1\" in",
              "        -in) input=\"$2\"; shift 2 ;;",
              "        -out) output=\"$2\"; shift 2 ;;",
              "        *) shift ;;",
              "      esac",
              "    done",
              "    if [ -n \"$input\" ]; then",
              "      printf 'FAKE DER' > \"$output\"",
              "    else",
              "      printf '%s\\n' 'FAKE CSR PEM' > \"$output\"",
              "    fi",
              "    ;;",
              "  dgst)",
              "    output=''",
              "    while [ \"$#\" -gt 0 ]; do",
              "      case \"$1\" in",
              "        -out) output=\"$2\"; shift 2 ;;",
              "        *) shift ;;",
              "      esac",
              "    done",
              "    printf 'fake-bytes' > \"$output\"",
              "    ;;",
              "esac"
            ]
            $ \scriptPath -> do
              withPrependedPathDirectory (takeDirectory scriptPath) $
                runInProcessAcmeChallenge
                  (basePlan (serverBaseUrl server <> "/directory-immediately-valid"))
                  challengeStore
                  tempDirectory
                  certificatePath
                  privateKeyPath
                  `shouldThrow` errorContaining "In-process ACME private key file does not exist"

instance Eq ActiveAcmeChallenge where
  left == right =
    activeAcmeChallengeDomain left == activeAcmeChallengeDomain right
      && activeAcmeChallengeToken left == activeAcmeChallengeToken right
      && activeAcmeChallengeResponse left == activeAcmeChallengeResponse right

instance Show ActiveAcmeChallenge where
  show challenge =
    "ActiveAcmeChallenge {activeAcmeChallengeDomain = "
      <> show (activeAcmeChallengeDomain challenge)
      <> ", activeAcmeChallengeToken = "
      <> show (activeAcmeChallengeToken challenge)
      <> ", activeAcmeChallengeResponse = "
      <> show (activeAcmeChallengeResponse challenge)
      <> "}"

instance Eq AcmeDirectoryResponse where
  left == right =
    acmeNewNonceUrl left == acmeNewNonceUrl right
      && acmeNewAccountUrl left == acmeNewAccountUrl right
      && acmeNewOrderUrl left == acmeNewOrderUrl right

instance Show AcmeDirectoryResponse where
  show directory =
    "AcmeDirectoryResponse {acmeNewNonceUrl = "
      <> show (acmeNewNonceUrl directory)
      <> ", acmeNewAccountUrl = "
      <> show (acmeNewAccountUrl directory)
      <> ", acmeNewOrderUrl = "
      <> show (acmeNewOrderUrl directory)
      <> "}"

instance Eq AcmeOrderIdentifier where
  left == right =
    acmeIdentifierKind left == acmeIdentifierKind right
      && acmeIdentifierValue left == acmeIdentifierValue right

instance Show AcmeOrderIdentifier where
  show identifier =
    "AcmeOrderIdentifier {acmeIdentifierKind = "
      <> show (acmeIdentifierKind identifier)
      <> ", acmeIdentifierValue = "
      <> show (acmeIdentifierValue identifier)
      <> "}"

instance Eq AcmeChallengeResponse where
  left == right =
    acmeChallengeKind left == acmeChallengeKind right
      && acmeChallengeUrl left == acmeChallengeUrl right
      && acmeChallengeTokenValue left == acmeChallengeTokenValue right

instance Show AcmeChallengeResponse where
  show challenge =
    "AcmeChallengeResponse {acmeChallengeKind = "
      <> show (acmeChallengeKind challenge)
      <> ", acmeChallengeUrl = "
      <> show (acmeChallengeUrl challenge)
      <> ", acmeChallengeTokenValue = "
      <> show (acmeChallengeTokenValue challenge)
      <> "}"

instance Eq AcmeAuthorizationResponse where
  left == right =
    acmeAuthorizationIdentifier left == acmeAuthorizationIdentifier right
      && acmeAuthorizationChallenges left == acmeAuthorizationChallenges right

instance Show AcmeAuthorizationResponse where
  show authorization =
    "AcmeAuthorizationResponse {acmeAuthorizationIdentifier = "
      <> show (acmeAuthorizationIdentifier authorization)
      <> ", acmeAuthorizationChallenges = "
      <> show (acmeAuthorizationChallenges authorization)
      <> "}"

instance Eq AcmeOrderResponse where
  left == right =
    acmeOrderStatus left == acmeOrderStatus right
      && acmeOrderAuthorizations left == acmeOrderAuthorizations right
      && acmeOrderFinalizeUrl left == acmeOrderFinalizeUrl right
      && acmeOrderCertificateUrl left == acmeOrderCertificateUrl right

instance Show AcmeOrderResponse where
  show orderResponse =
    "AcmeOrderResponse {acmeOrderStatus = "
      <> show (acmeOrderStatus orderResponse)
      <> ", acmeOrderAuthorizations = "
      <> show (acmeOrderAuthorizations orderResponse)
      <> ", acmeOrderFinalizeUrl = "
      <> show (acmeOrderFinalizeUrl orderResponse)
      <> ", acmeOrderCertificateUrl = "
      <> show (acmeOrderCertificateUrl orderResponse)
      <> "}"

instance Eq AcmeJwk where
  left == right =
    acmeJwkExponent left == acmeJwkExponent right
      && acmeJwkModulus left == acmeJwkModulus right

instance Show AcmeJwk where
  show jwk =
    "AcmeJwk {acmeJwkExponent = "
      <> show (acmeJwkExponent jwk)
      <> ", acmeJwkModulus = "
      <> show (acmeJwkModulus jwk)
      <> "}"

instance Eq AcmeRequestAuth where
  AcmeRequestJwk left == AcmeRequestJwk right = left == right
  AcmeRequestKid left == AcmeRequestKid right = left == right
  _ == _ = False

instance Show AcmeRequestAuth where
  show (AcmeRequestJwk jwk) = "AcmeRequestJwk " <> show jwk
  show (AcmeRequestKid accountKid) = "AcmeRequestKid " <> show accountKid

instance Eq PreparedAcmeChallenge where
  left == right =
    preparedAcmeChallengeRegistration left == preparedAcmeChallengeRegistration right
      && preparedAcmeChallengeUrl left == preparedAcmeChallengeUrl right

instance Show PreparedAcmeChallenge where
  show preparedChallenge =
    "PreparedAcmeChallenge {preparedAcmeChallengeRegistration = "
      <> show (preparedAcmeChallengeRegistration preparedChallenge)
      <> ", preparedAcmeChallengeUrl = "
      <> show (preparedAcmeChallengeUrl preparedChallenge)
      <> "}"

instance Eq JsonValue where
  JsonObject left == JsonObject right = left == right
  JsonArray left == JsonArray right = left == right
  JsonString left == JsonString right = left == right
  JsonBool left == JsonBool right = left == right
  JsonNull == JsonNull = True
  _ == _ = False

instance Show JsonValue where
  show (JsonObject fields) = "JsonObject " <> show fields
  show (JsonArray items) = "JsonArray " <> show items
  show (JsonString textValue) = "JsonString " <> show textValue
  show (JsonBool boolValue) = "JsonBool " <> show boolValue
  show JsonNull = "JsonNull"

isLeftWith :: String -> Either String a -> Bool
isLeftWith expectedMessage result =
  case result of
    Left message -> expectedMessage `isInfixOf` message
    Right _ -> False

isRightValue :: Either a b -> Bool
isRightValue result =
  case result of
    Right _ -> True
    Left _ -> False

isPreparedChallengeFor :: Text -> PreparedAcmeChallenge -> Bool
isPreparedChallengeFor expectedUrl preparedChallenge =
  preparedAcmeChallengeUrl preparedChallenge == expectedUrl
    && activeAcmeChallengeResponse (preparedAcmeChallengeRegistration preparedChallenge)
      == "token.ZmFrZS1ieXRlcw"

unwrapChallengeStore :: AcmeChallengeStore -> IO [ActiveAcmeChallenge]
unwrapChallengeStore (AcmeChallengeStore challengeStore) =
  readMVar challengeStore

withPrependedPathDirectory :: FilePath -> IO a -> IO a
withPrependedPathDirectory pathDirectory action = do
  originalPath <- lookupEnv "PATH"
  let updatedPath =
        maybe
          pathDirectory
          (\currentPath -> pathDirectory <> ":" <> currentPath)
          originalPath
  setEnv "PATH" updatedPath
  action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

withEmptyExecutablePath :: IO a -> IO a
withEmptyExecutablePath action =
  withSystemTempDirectory "missing-executable-path" $ \tempDirectory -> do
    originalPath <- lookupEnv "PATH"
    setEnv "PATH" tempDirectory
    action `finally` maybe (unsetEnv "PATH") (setEnv "PATH") originalPath

withFakeOpenSslScript :: [String] -> (FilePath -> IO a) -> IO a
withFakeOpenSslScript scriptLines action =
  withSystemTempDirectory "fake-openssl-script" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "openssl"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath

withFakeCertbotScript :: [String] -> (FilePath -> IO a) -> IO a
withFakeCertbotScript scriptLines action =
  withSystemTempDirectory "fake-certbot-script" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath

withFakeOpenSslExecutable :: FilePath -> (FilePath -> IO a) -> IO a
withFakeOpenSslExecutable privateKeyPath action =
  withFakeOpenSslScript
    [ "#!/bin/sh",
      "set -eu",
      "command=\"$1\"",
      "shift",
      "case \"$command\" in",
      "  genrsa)",
      "    output=''",
      "    while [ \"$#\" -gt 0 ]; do",
      "      case \"$1\" in",
      "        -out) output=\"$2\"; shift 2 ;;",
      "        *) shift ;;",
      "      esac",
      "    done",
      "    printf '%s\\n' 'FAKE ACCOUNT KEY' > \"$output\"",
      "    ;;",
      "  rsa)",
      "    printf 'Modulus=A1B2C3D4E5F60718293A4B5C6D7E8F90\\n'",
      "    ;;",
      "  req)",
      "    input=''",
      "    keyout=''",
      "    output=''",
      "    while [ \"$#\" -gt 0 ]; do",
      "      case \"$1\" in",
      "        -in) input=\"$2\"; shift 2 ;;",
      "        -keyout) keyout=\"$2\"; shift 2 ;;",
      "        -out) output=\"$2\"; shift 2 ;;",
      "        *) shift ;;",
      "      esac",
      "    done",
      "    if [ -n \"$keyout\" ]; then",
      "      cp " <> show privateKeyPath <> " \"$keyout\"",
      "      printf '%s\\n' 'FAKE CSR PEM' > \"$output\"",
      "    elif [ -n \"$input\" ]; then",
      "      printf 'FAKE DER' > \"$output\"",
      "    fi",
      "    ;;",
      "  dgst)",
      "    output=''",
      "    while [ \"$#\" -gt 0 ]; do",
      "      case \"$1\" in",
      "        -out) output=\"$2\"; shift 2 ;;",
      "        *) shift ;;",
      "      esac",
      "    done",
      "    printf 'fake-bytes' > \"$output\"",
      "    ;;",
      "  *)",
      "    echo unknown fake openssl command: \"$command\" >&2",
      "    exit 64",
      "    ;;",
      "esac"
    ]
    $ \scriptPath -> withPrependedPathDirectory (takeDirectory scriptPath) (action scriptPath)

data TestAcmeServer = TestAcmeServer
  { serverBaseUrl :: Text,
    serverDirectoryUrl :: Text
  }

withHttpAcmeServer :: (TestAcmeServer -> IO a) -> IO a
withHttpAcmeServer action =
  withUnusedLoopbackPort $ \port -> do
    let baseUrl = Text.pack ("http://127.0.0.1:" <> show port)
        nonceHeaders = [("Replay-Nonce", "nonce-1"), ("Content-Type", "application/json")]
    pendingOrderCount <- newIORef (0 :: Int)
    processingOrderCount <- newIORef (0 :: Int)
    waitingOrderCount <- newIORef (0 :: Int)
    let directoryResponse newOrderPath =
          "{\"newNonce\":\"" <> baseUrl <> "/new-nonce\",\"newAccount\":\"" <> baseUrl <> "/new-account\",\"newOrder\":\"" <> baseUrl <> newOrderPath <> "\"}"
        acmeApplication request respond = do
          _ <- Wai.strictRequestBody request
          case (Wai.requestMethod request, Wai.rawPathInfo request) of
            ("GET", "/directory") ->
              respond (jsonResponse Http.ok200 nonceHeaders (directoryResponse "/new-order"))
            ("GET", "/directory-bad-json") ->
              respond (jsonResponse Http.ok200 nonceHeaders "{}")
            ("GET", "/directory-no-authorizations") ->
              respond (jsonResponse Http.ok200 nonceHeaders (directoryResponse "/new-order-no-authorizations"))
            ("GET", "/directory-immediately-valid") ->
              respond (jsonResponse Http.ok200 nonceHeaders (directoryResponse "/new-order-immediately-valid"))
            ("GET", "/directory-ready-no-finalize") ->
              respond (jsonResponse Http.ok200 nonceHeaders (directoryResponse "/new-order-ready-no-finalize"))
            ("GET", "/directory-valid-no-certificate") ->
              respond (jsonResponse Http.ok200 nonceHeaders (directoryResponse "/new-order-valid-no-certificate"))
            ("HEAD", "/new-nonce") ->
              respond (Wai.responseLBS Http.noContent204 [("Replay-Nonce", "nonce-1")] "")
            ("HEAD", "/new-nonce-missing") ->
              respond (Wai.responseLBS Http.noContent204 [] "")
            ("POST", "/capture") ->
              respond
                ( Wai.responseLBS
                    Http.ok200
                    [ ("Replay-Nonce", "nonce-2"),
                      ("Content-Type", "application/json"),
                      ("X-Seen-Accept", fromMaybe "" (lookup "Accept" (Wai.requestHeaders request)))
                    ]
                    "{\"status\":\"ok\"}"
                )
            ("POST", "/new-account") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/account/1"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    "{}"
                )
            ("POST", "/new-account-missing-location") ->
              respond (jsonResponse Http.created201 nonceHeaders "{}")
            ("POST", "/new-order") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order/1"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 ("{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}")))
                )
            ("POST", "/new-order-bad-json") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order/1"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    "{}"
                )
            ("POST", "/new-order-missing-location") ->
              respond (jsonResponse Http.created201 nonceHeaders "{\"status\":\"ready\"}")
            ("POST", "/new-order-no-authorizations") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order/1"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    "{\"status\":\"ready\",\"finalize\":\"http://unused.invalid/finalize\"}"
                )
            ("POST", "/new-order-immediately-valid") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order-immediately-valid"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 ("{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}")))
                )
            ("POST", "/new-order-ready-no-finalize") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order-ready-no-finalize"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 ("{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"]}")))
                )
            ("POST", "/new-order-valid-no-certificate") ->
              respond
                ( Wai.responseLBS
                    Http.created201
                    [("Location", ByteStringChar8.pack (Text.unpack (baseUrl <> "/order-valid-no-certificate"))), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
                    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 ("{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}")))
                )
            ("POST", "/authz/1") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"identifier\":{\"type\":\"dns\",\"value\":\"example.com\"},\"challenges\":[{\"type\":\"http-01\",\"url\":\"" <> baseUrl <> "/challenge/1\",\"token\":\"token\"}]}"))
            ("POST", "/authz-no-http01") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"identifier\":{\"type\":\"dns\",\"value\":\"example.com\"},\"challenges\":[{\"type\":\"dns-01\",\"url\":\"" <> baseUrl <> "/challenge/2\",\"token\":\"token\"}]}"))
            ("POST", "/authz-bad-json") ->
              respond (jsonResponse Http.ok200 nonceHeaders "{}")
            ("POST", "/challenge/1") ->
              respond (jsonResponse Http.ok200 nonceHeaders "{}")
            ("POST", "/order-ready") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"))
            ("POST", "/order-valid") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}"))
            ("POST", "/order-pending-ready") -> do
              priorPolls <- atomicModifyIORef' pendingOrderCount (\count -> (count + 1, count))
              respond
                ( jsonResponse
                    Http.ok200
                    nonceHeaders
                    ( if priorPolls == 0
                        then "{\"status\":\"pending\"}"
                        else "{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"
                    )
                )
            ("POST", "/order-processing-valid") -> do
              priorPolls <- atomicModifyIORef' processingOrderCount (\count -> (count + 1, count))
              respond
                ( jsonResponse
                    Http.ok200
                    nonceHeaders
                    ( if priorPolls == 0
                        then "{\"status\":\"processing\"}"
                        else "{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}"
                    )
                )
            ("POST", "/order-waiting-ready") -> do
              priorPolls <- atomicModifyIORef' waitingOrderCount (\count -> (count + 1, count))
              respond
                ( jsonResponse
                    Http.ok200
                    nonceHeaders
                    ( if priorPolls == 0
                        then "{\"status\":\"waiting\"}"
                        else "{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"
                    )
                )
            ("POST", "/order-immediately-valid") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}"))
            ("POST", "/order-ready-no-finalize") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"]}"))
            ("POST", "/order-valid-no-certificate") ->
              respond (jsonResponse Http.ok200 nonceHeaders ("{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"))
            ("POST", "/order-invalid") ->
              respond (jsonResponse Http.ok200 nonceHeaders "{\"status\":\"invalid\"}")
            ("POST", "/finalize/1") ->
              respond (jsonResponse Http.ok200 nonceHeaders "{}")
            ("POST", "/cert/1") ->
              respond (Wai.responseLBS Http.ok200 [("Replay-Nonce", "nonce-2"), ("Content-Type", "application/pem-certificate-chain")] "PEM CERT")
            (_, "/status-500") ->
              respond (Wai.responseLBS Http.internalServerError500 [("Content-Type", "text/plain")] "boom")
            _ ->
              respond (Wai.responseLBS Http.notFound404 [] "not found")
    serverThreadId <- forkIO (Warp.run port acmeApplication)
    threadDelay 50000
    action TestAcmeServer {serverBaseUrl = baseUrl, serverDirectoryUrl = baseUrl <> "/directory"}
      `finally` killThread serverThreadId

jsonResponse :: Http.Status -> [Http.Header] -> Text -> Wai.Response
jsonResponse status headers body =
  Wai.responseLBS status headers (LazyByteString.fromStrict (TextEncoding.encodeUtf8 body))

withUnusedLoopbackPort :: (Int -> IO a) -> IO a
withUnusedLoopbackPort action = do
  reservedSocket <- Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol
  Socket.bind reservedSocket (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- Socket.getSocketName reservedSocket
  case socketAddress of
    Socket.SockAddrInet port _ -> do
      Socket.close reservedSocket
      action (fromIntegral port)
    _ -> do
      Socket.close reservedSocket
      error "expected IPv4 loopback reservation socket"

errorContaining :: String -> Selector IOException
errorContaining expectedMessage thrownError =
  expectedMessage `isInfixOf` show thrownError

expectIOExceptionContainsAll :: IO a -> [String] -> Expectation
expectIOExceptionContainsAll action expectedSubstrings = do
  actionResult <- try (void action) :: IO (Either IOException ())
  case actionResult of
    Left thrownError ->
      mapM_ (show thrownError `shouldContain`) expectedSubstrings
    Right _ ->
      expectationFailure "expected an IOException"
