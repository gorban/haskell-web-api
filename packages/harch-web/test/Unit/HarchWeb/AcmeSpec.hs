{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.HarchWeb.AcmeSpec (spec) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO, killThread, newMVar, readMVar, threadDelay)
import Control.Exception (IOException, evaluate, finally, try)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
import HarchWeb.Acme qualified as Acme
import Network.HTTP.Client qualified as HttpClient
import Network.HTTP.Types qualified as Http
import Network.Socket qualified as Socket
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist, removePathForcibly)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)
import Text.ParserCombinators.ReadP (readP_to_S)

sampleEndpoint :: ListenerEndpoint
sampleEndpoint =
  ListenerEndpoint
    { endpointHost = "127.0.0.1",
      endpointPort = 5443
    }

defaultRequestPolicy :: RequestPolicyConfig
defaultRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing,
      trustForwardedHeaders = False,
      requestHeadLimits = unboundedRequestHeadLimits,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }

certbotBackend :: CertbotConfig
certbotBackend =
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
      acmeCertbotConfig = certbotBackend
    }

certbotConfigValue :: AcmeConfig
certbotConfigValue =
  AcmeConfig
    { acmeDirectoryUrl = "https://acme.example/directory",
      acmeContactEmails = ["ops@example.com"],
      acmeDomains = ["example.com", "www.example.com"],
      acmeHttp01Port = 8080,
      acmeCertificateDirectory = Nothing,
      acmeCertbotConfig = certbotBackend
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
  describe "the public HarchWeb.Acme boundary" $ do
    it "exports supported helpers directly" $
      expectAll
        ( (Acme.validAcmeHttp01ChallengeToken "boundary-token" `shouldBe` Just "boundary-token")
            :| [Acme.hexTextToByteString "7a" `shouldBe` Right "z"]
        )

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
      expectAll
        ( (challenge `shouldBe` challenge)
            :| [ show challenge `shouldContain` "activeAcmeChallengeDomain = \"example.com\"",
                 directory `shouldBe` directory,
                 show directory `shouldContain` "acmeNewNonceUrl = \"https://acme.example/new-nonce\"",
                 identifier `shouldBe` identifier,
                 show identifier `shouldContain` "acmeIdentifierKind = \"dns\"",
                 challengeResponse `shouldBe` challengeResponse,
                 show challengeResponse `shouldContain` "acmeChallengeKind = \"http-01\"",
                 authorization `shouldBe` authorization,
                 show authorization `shouldContain` "acmeAuthorizationIdentifier = AcmeOrderIdentifier",
                 orderResponse `shouldBe` orderResponse,
                 show orderResponse `shouldContain` "acmeOrderStatus = \"ready\"",
                 jwk `shouldBe` jwk,
                 show jwk `shouldContain` "acmeJwkExponent = \"AQAB\"",
                 AcmeRequestJwk jwk `shouldBe` AcmeRequestJwk jwk,
                 show (AcmeRequestKid "kid-1") `shouldBe` "AcmeRequestKid \"kid-1\"",
                 preparedChallenge `shouldBe` preparedChallenge,
                 show preparedChallenge `shouldContain` "preparedAcmeChallengeUrl = \"https://acme.example/challenge/1\"",
                 jsonValue `shouldBe` jsonValue,
                 show jsonValue `shouldContain` "JsonBool True"
               ]
        )

    it "covers certbot argument helpers and certificate-name selection branches" $ do
      let configWithDomainArgument =
            certbotConfigValue
              { acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = ["certonly", "--domains=domain.example.com"]
                    },
                acmeDomains = []
              }
          configWithDefaultDomain =
            certbotConfigValue
              { acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = ["certonly"]
                    }
              }
          configWithoutCertificateName =
            inProcessConfig
              { acmeDomains = [],
                acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "certbot",
                      certbotArguments = []
                    }
              }
      expectAll
        ( ( runtimeCertbotArguments (runtimeAcmePlanWith certbotConfigValue)
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
          )
            :| [ runtimeCertbotArguments (runtimeAcmePlanWith inProcessConfig) `shouldBe` certbotArguments (case certbotBackend of config -> config),
                 certbotOptionValues "--cert-name" ["certonly", "--cert-name", "named-cert"] `shouldBe` ["named-cert"],
                 certbotOptionValues "--domain" ["--domain=cli.example.com", "--domain", "other.example.com"] `shouldBe` ["other.example.com", "cli.example.com"],
                 certbotHasOption "--http-01-port" (runtimeCertbotArguments (runtimeAcmePlanWith certbotConfigValue)) `shouldBe` True,
                 certbotHasOption "--missing" ["certonly"] `shouldBe` False,
                 splitCertbotDomainValue " example.com , www.example.com ,, " `shouldBe` ["example.com", "www.example.com"],
                 firstCertbotDomain ["-d", "one.example.com", "--domains=two.example.com,three.example.com"] `shouldBe` Just "one.example.com",
                 (certbotBackend == certbotBackend) `shouldBe` True,
                 (certbotBackend == CertbotConfig "other-certbot" []) `shouldBe` False,
                 certbotBackend
                   `shouldBe` CertbotConfig
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
               ]
        )
      evaluate
        ( certbotConfigValue
            == certbotConfigValue
              { acmeCertbotConfig =
                  CertbotConfig
                    { certbotExecutable = "other-certbot",
                      certbotArguments = []
                    }
              }
        )
        `shouldReturn` False
      expectAll
        ( (certbotCertificateName (runtimeAcmePlanWith certbotConfigValue) `shouldBe` Right "cli-cert")
            :| [ certbotCertificateName (runtimeAcmePlanWith configWithDomainArgument) `shouldBe` Right "domain.example.com",
                 certbotCertificateName (runtimeAcmePlanWith configWithDefaultDomain) `shouldBe` Right "example.com",
                 certbotCertificateName (runtimeAcmePlanWith configWithoutCertificateName) `shouldSatisfy` isLeftWith "requires ACME domains or certbot arguments"
               ]
        )

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
                        acmeCertbotConfig = certbotConfig
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
      acmeHttp01ChallengeToken defaultRequestPolicy matchingRequest `shouldBe` Just "token-1"
      acmeHttp01ChallengeToken defaultRequestPolicy missingTokenRequest `shouldBe` Nothing
      requestHostWithoutPort matchingRequest `shouldBe` Just "example.com"
      matchesRuntimeAcmeChallenge defaultRequestPolicy matchingRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge defaultRequestPolicy mismatchedHostRequest challenge `shouldBe` False
      matchesRuntimeAcmeChallenge defaultRequestPolicy hostlessRequest challenge `shouldBe` True
      matchesRuntimeAcmeChallenge defaultRequestPolicy missingTokenRequest challenge `shouldBe` False
      registerAcmeChallenges challengeStore [challenge]
      registeredChallenges <- unwrapChallengeStore challengeStore
      registeredChallenges `shouldBe` [challenge]
      challengeResponse <- acmeChallengeResponseForRequest defaultRequestPolicy challengeStore matchingRequest
      case challengeResponse of
        Just response -> do
          Wai.responseStatus response `shouldBe` Http.ok200
          Wai.responseHeaders response `shouldBe` [("Content-Type", "text/plain; charset=utf-8")]
          readResponseBody response `shouldReturn` "response-1"
        Nothing -> expectationFailure "expected a registered ACME challenge response"
      unregisterAcmeChallenges challengeStore [challenge]
      unwrapChallengeStore challengeStore `shouldReturn` []

  describe "ACME JSON parsing helpers" $ do
    it "parses JSON values including escapes, booleans, nulls, arrays, and empty collections" $ do
      expectAll
        ( (parseJsonValue "{}" `shouldBe` Right (JsonObject []))
            :| [ parseJsonValue "[]" `shouldBe` Right (JsonArray []),
                 parseJsonValue " [ \"a\" , \"b\" ] " `shouldBe` Right (JsonArray [JsonString "a", JsonString "b"]),
                 parseJsonValue "{\"field\":\"value\"}" `shouldBe` Right (JsonObject [("field", JsonString "value")]),
                 parseJsonValue "true" `shouldBe` Right (JsonBool True),
                 parseJsonValue "false" `shouldBe` Right (JsonBool False),
                 parseJsonValue "null" `shouldBe` Right JsonNull
               ]
        )
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
        `shouldBe` "\"\\\"\\\\\\u0008\\u000c\\n\\r\\tplain\""
      escapeJsonCharacter '"' `shouldBe` "\\\""
      escapeJsonCharacter '\\' `shouldBe` "\\\\"
      escapeJsonCharacter '\b' `shouldBe` "\\u0008"
      escapeJsonCharacter '\f' `shouldBe` "\\u000c"
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
            activeAcmeChallengeDomain (preparedAcmeChallengeRegistration preparedChallenge) `shouldBe` "example.com"
            activeAcmeChallengeToken (preparedAcmeChallengeRegistration preparedChallenge) `shouldBe` "token"
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
              `shouldThrow` errorContaining "TCP port 80 is reachable from the public internet for http-01 validation"
            pollAcmeOrder
              plan
              manager
              directory
              accountKeyPath
              "kid-1"
              (serverBaseUrl server <> "/order-invalid")
              ["ready"]
              `shouldThrow` errorContaining "Timeout during connect"
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

instance Eq ActiveAcmeChallenge where
  left == right =
    activeAcmeChallengeDomain left == activeAcmeChallengeDomain right
      && activeAcmeChallengeToken left == activeAcmeChallengeToken right
      && activeAcmeChallengeResponse left == activeAcmeChallengeResponse right

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (mconcat chunks)))

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

data TestAcmeServerState = TestAcmeServerState
  { testAcmeBaseUrl :: Text,
    testAcmeNonceHeaders :: [Http.Header],
    testAcmePendingOrderCount :: IORef Int,
    testAcmeProcessingOrderCount :: IORef Int,
    testAcmeWaitingOrderCount :: IORef Int
  }

type TestAcmeHandler = Wai.Request -> IO Wai.Response

withHttpAcmeServer :: (TestAcmeServer -> IO a) -> IO a
withHttpAcmeServer action =
  withUnusedLoopbackPort $ \port -> do
    let baseUrl = Text.pack ("http://127.0.0.1:" <> show port)
        nonceHeaders = [("Replay-Nonce", "nonce-1"), ("Content-Type", "application/json")]
    pendingOrderCount <- newIORef (0 :: Int)
    processingOrderCount <- newIORef (0 :: Int)
    waitingOrderCount <- newIORef (0 :: Int)
    let serverState = TestAcmeServerState baseUrl nonceHeaders pendingOrderCount processingOrderCount waitingOrderCount
    serverThreadId <- forkIO (Warp.run port (acmeApplication serverState))
    threadDelay 50000
    action TestAcmeServer {serverBaseUrl = baseUrl, serverDirectoryUrl = baseUrl <> "/directory"}
      `finally` killThread serverThreadId

acmeApplication :: TestAcmeServerState -> Wai.Application
acmeApplication serverState request respond = do
  _ <- Wai.strictRequestBody request
  let routeKey = (Wai.requestMethod request, Wai.rawPathInfo request)
      handler = fromMaybe notFoundHandler (lookup routeKey (acmeRoutes serverState) <|> wildcardRoute routeKey)
  handler request >>= respond
  where
    wildcardRoute (_, "/status-500") = Just (constantHandler (Wai.responseLBS Http.internalServerError500 [("Content-Type", "text/plain")] "boom"))
    wildcardRoute _ = Nothing

acmeRoutes :: TestAcmeServerState -> [((ByteString.ByteString, ByteString.ByteString), TestAcmeHandler)]
acmeRoutes serverState =
  [ (("GET", "/directory"), jsonHandler Http.ok200 headers (directoryResponse serverState "/new-order")),
    (("GET", "/directory-bad-json"), jsonHandler Http.ok200 headers "{}"),
    (("GET", "/directory-no-authorizations"), jsonHandler Http.ok200 headers (directoryResponse serverState "/new-order-no-authorizations")),
    (("GET", "/directory-immediately-valid"), jsonHandler Http.ok200 headers (directoryResponse serverState "/new-order-immediately-valid")),
    (("GET", "/directory-ready-no-finalize"), jsonHandler Http.ok200 headers (directoryResponse serverState "/new-order-ready-no-finalize")),
    (("GET", "/directory-valid-no-certificate"), jsonHandler Http.ok200 headers (directoryResponse serverState "/new-order-valid-no-certificate")),
    (("HEAD", "/new-nonce"), constantHandler (Wai.responseLBS Http.noContent204 [("Replay-Nonce", "nonce-1")] "")),
    (("HEAD", "/new-nonce-missing"), constantHandler (Wai.responseLBS Http.noContent204 [] "")),
    (("POST", "/capture"), captureHandler),
    (("POST", "/new-account"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/account/1") "{}")),
    (("POST", "/new-account-missing-location"), jsonHandler Http.created201 headers "{}"),
    (("POST", "/new-order"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order/1") (readyOrderBody baseUrl))),
    (("POST", "/new-order-bad-json"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order/1") "{}")),
    (("POST", "/new-order-missing-location"), jsonHandler Http.created201 headers "{\"status\":\"ready\"}"),
    (("POST", "/new-order-no-authorizations"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order/1") "{\"status\":\"ready\",\"finalize\":\"http://unused.invalid/finalize\"}")),
    (("POST", "/new-order-immediately-valid"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order-immediately-valid") (validOrderBody baseUrl))),
    (("POST", "/new-order-ready-no-finalize"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order-ready-no-finalize") (readyWithoutFinalizeBody baseUrl))),
    (("POST", "/new-order-valid-no-certificate"), constantHandler (locatedResponse Http.created201 (baseUrl <> "/order-valid-no-certificate") (validWithoutCertificateBody baseUrl))),
    (("POST", "/authz/1"), jsonHandler Http.ok200 headers (httpAuthorizationBody baseUrl)),
    (("POST", "/authz-no-http01"), jsonHandler Http.ok200 headers (dnsAuthorizationBody baseUrl)),
    (("POST", "/authz-bad-json"), jsonHandler Http.ok200 headers "{}"),
    (("POST", "/challenge/1"), jsonHandler Http.ok200 headers "{}"),
    (("POST", "/order-ready"), jsonHandler Http.ok200 headers (readyOrderBody baseUrl)),
    (("POST", "/order-valid"), jsonHandler Http.ok200 headers (validOrderBody baseUrl)),
    (("POST", "/order-pending-ready"), pollingHandler (testAcmePendingOrderCount serverState) "{\"status\":\"pending\"}" (readyOrderBody baseUrl) headers),
    (("POST", "/order-processing-valid"), pollingHandler (testAcmeProcessingOrderCount serverState) "{\"status\":\"processing\"}" (validOrderBody baseUrl) headers),
    (("POST", "/order-waiting-ready"), pollingHandler (testAcmeWaitingOrderCount serverState) "{\"status\":\"waiting\"}" (readyOrderBody baseUrl) headers),
    (("POST", "/order-immediately-valid"), jsonHandler Http.ok200 headers (validOrderBody baseUrl)),
    (("POST", "/order-ready-no-finalize"), jsonHandler Http.ok200 headers (readyWithoutFinalizeBody baseUrl)),
    (("POST", "/order-valid-no-certificate"), jsonHandler Http.ok200 headers (validWithoutCertificateBody baseUrl)),
    (("POST", "/order-invalid"), jsonHandler Http.ok200 headers "{\"status\":\"invalid\",\"error\":{\"type\":\"urn:ietf:params:acme:error:connection\",\"detail\":\"Fetching http://bruckdev.com/.well-known/acme-challenge/token: Timeout during connect\"}}"),
    (("POST", "/finalize/1"), jsonHandler Http.ok200 headers "{}"),
    (("POST", "/cert/1"), constantHandler (Wai.responseLBS Http.ok200 [("Replay-Nonce", "nonce-2"), ("Content-Type", "application/pem-certificate-chain")] "PEM CERT"))
  ]
  where
    baseUrl = testAcmeBaseUrl serverState
    headers = testAcmeNonceHeaders serverState
    captureHandler request =
      pure
        ( Wai.responseLBS
            Http.ok200
            [("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json"), ("X-Seen-Accept", fromMaybe "" (lookup "Accept" (Wai.requestHeaders request)))]
            "{\"status\":\"ok\"}"
        )

directoryResponse :: TestAcmeServerState -> Text -> Text
directoryResponse serverState newOrderPath =
  "{\"newNonce\":\"" <> baseUrl <> "/new-nonce\",\"newAccount\":\"" <> baseUrl <> "/new-account\",\"newOrder\":\"" <> baseUrl <> newOrderPath <> "\"}"
  where
    baseUrl = testAcmeBaseUrl serverState

locatedResponse :: Http.Status -> Text -> Text -> Wai.Response
locatedResponse status location body =
  Wai.responseLBS
    status
    [("Location", TextEncoding.encodeUtf8 location), ("Replay-Nonce", "nonce-2"), ("Content-Type", "application/json")]
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 body))

constantHandler :: Wai.Response -> TestAcmeHandler
constantHandler response _ = pure response

jsonHandler :: Http.Status -> [Http.Header] -> Text -> TestAcmeHandler
jsonHandler status headers body = constantHandler (jsonResponse status headers body)

pollingHandler :: IORef Int -> Text -> Text -> [Http.Header] -> TestAcmeHandler
pollingHandler pollCount firstBody laterBody headers _ = do
  priorPolls <- atomicModifyIORef' pollCount (\count -> (count + 1, count))
  pure (jsonResponse Http.ok200 headers (if priorPolls == 0 then firstBody else laterBody))

notFoundHandler :: TestAcmeHandler
notFoundHandler = constantHandler (Wai.responseLBS Http.notFound404 [] "not found")

readyOrderBody :: Text -> Text
readyOrderBody baseUrl = "{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"

validOrderBody :: Text -> Text
validOrderBody baseUrl = "{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\",\"certificate\":\"" <> baseUrl <> "/cert/1\"}"

readyWithoutFinalizeBody :: Text -> Text
readyWithoutFinalizeBody baseUrl = "{\"status\":\"ready\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"]}"

validWithoutCertificateBody :: Text -> Text
validWithoutCertificateBody baseUrl = "{\"status\":\"valid\",\"authorizations\":[\"" <> baseUrl <> "/authz/1\"],\"finalize\":\"" <> baseUrl <> "/finalize/1\"}"

httpAuthorizationBody :: Text -> Text
httpAuthorizationBody baseUrl = "{\"identifier\":{\"type\":\"dns\",\"value\":\"example.com\"},\"challenges\":[{\"type\":\"http-01\",\"url\":\"" <> baseUrl <> "/challenge/1\",\"token\":\"token\"}]}"

dnsAuthorizationBody :: Text -> Text
dnsAuthorizationBody baseUrl = "{\"identifier\":{\"type\":\"dns\",\"value\":\"example.com\"},\"challenges\":[{\"type\":\"dns-01\",\"url\":\"" <> baseUrl <> "/challenge/2\",\"token\":\"token\"}]}"

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
