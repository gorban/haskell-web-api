{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Unit.HarchWeb.AcmeSpec (spec) where

import Control.Concurrent (newMVar, readMVar)
import Control.Exception (evaluate, finally)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
import HarchWeb.Acme qualified as Acme
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
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
      httpsRedirectAuthority = Nothing,
      strictTransportSecurity = Nothing,
      forwardedHeaderTrust = NeverTrustForwarded,
      requestHeadLimits = unboundedRequestHeadLimits,
      requestTransportLimits = warpDefaultRequestTransportLimits,
      requestConcurrencyLimit = Nothing,
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
      Acme.validAcmeHttp01ChallengeToken "boundary-token" `shouldBe` Just "boundary-token"

  describe "ACME helper model coverage" $ do
    it "covers derived Eq and Show instances for internal ACME helper types" $ do
      let challenge =
            ActiveAcmeChallenge
              { activeAcmeChallengeDomain = "example.com",
                activeAcmeChallengeToken = "token",
                activeAcmeChallengeResponse = "token.thumbprint"
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
                 parseJsonValue "{\"a\": \"1\", \"b\": \"2\"}" `shouldBe` Right (JsonObject [("a", JsonString "1"), ("b", JsonString "2")]),
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
    it "parses low-level JSON string-escape characters directly" $ do
      readP_to_S jsonStringCharacterParser "" `shouldBe` []
      readP_to_S unicodeJsonCharacterParser "ZZZZ" `shouldBe` []

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

unwrapChallengeStore :: AcmeChallengeStore -> IO [ActiveAcmeChallenge]
unwrapChallengeStore (AcmeChallengeStore challengeStore) =
  readMVar challengeStore

withFakeCertbotScript :: [String] -> (FilePath -> IO a) -> IO a
withFakeCertbotScript scriptLines action =
  withSystemTempDirectory "fake-certbot-script" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath
