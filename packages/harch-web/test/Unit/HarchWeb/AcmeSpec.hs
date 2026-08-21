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
import HarchWeb.Acme.Json (jsonArrayBytes, jsonObjectBytes, jsonStringBytes)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

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
      expectAll
        ( (challenge `shouldBe` challenge)
            :| [show challenge `shouldContain` "activeAcmeChallengeDomain = \"example.com\""]
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
            webrootStore <- newCertbotWebrootStore
            (certbotManualPlan, certbotCleanupDirectory) <-
              prepareCertbotManualTlsBindPlan webrootStore certbotPlan certbotConfig
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
      webrootStore <- newCertbotWebrootStore
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
      challengeResponse <- acmeChallengeResponseForRequest defaultRequestPolicy challengeStore webrootStore matchingRequest
      case challengeResponse of
        Just response -> do
          Wai.responseStatus response `shouldBe` Http.ok200
          Wai.responseHeaders response `shouldBe` [("Content-Type", "text/plain; charset=utf-8")]
          readResponseBody response `shouldReturn` "response-1"
        Nothing -> expectationFailure "expected a registered ACME challenge response"
      unregisterAcmeChallenges challengeStore [challenge]
      unwrapChallengeStore challengeStore `shouldReturn` []
      registerCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-a"
      registerCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-b"
      unwrapWebrootStore webrootStore `shouldReturn` ["/tmp/webroot-b", "/tmp/webroot-a"]
      unregisterCertbotAcmeChallengeWebroot webrootStore "/tmp/webroot-a"
      unwrapWebrootStore webrootStore `shouldReturn` ["/tmp/webroot-b"]

  describe "ACME JSON encoding helpers" $ do
    it "encodes strings, arrays, and objects as JSON bytes" $ do
      expectAll
        ( ( jsonStringBytes "\"\\\b\f\n\r\tplain"
              `shouldBe` "\"\\\"\\\\\\u0008\\u000c\\n\\r\\tplain\""
          )
            :| [ jsonArrayBytes [] `shouldBe` "[]",
                 jsonArrayBytes ["1", "2"] `shouldBe` "[1,2]",
                 jsonObjectBytes [] `shouldBe` "{}",
                 jsonObjectBytes [("a", "1"), ("b", "2")] `shouldBe` "{\"a\":1,\"b\":2}"
               ]
        )

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

isLeftWith :: String -> Either String a -> Bool
isLeftWith expectedMessage result =
  case result of
    Left message -> expectedMessage `isInfixOf` message
    Right _ -> False

unwrapChallengeStore :: AcmeChallengeStore -> IO [ActiveAcmeChallenge]
unwrapChallengeStore (AcmeChallengeStore challengeStore) =
  readMVar challengeStore

unwrapWebrootStore :: CertbotWebrootStore -> IO [FilePath]
unwrapWebrootStore (CertbotWebrootStore webrootStore) =
  readMVar webrootStore

withFakeCertbotScript :: [String] -> (FilePath -> IO a) -> IO a
withFakeCertbotScript scriptLines action =
  withSystemTempDirectory "fake-certbot-script" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath
