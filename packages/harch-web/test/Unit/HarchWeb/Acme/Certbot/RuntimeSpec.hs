{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate, finally, try)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb
import System.Directory (doesDirectoryExist, removePathForcibly)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

sampleEndpoint :: ListenerEndpoint
sampleEndpoint =
  ListenerEndpoint
    { endpointHost = "127.0.0.1",
      endpointPort = 5443
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
      runtimeAcmeListenerConfig = acmeConfig,
      runtimeAcmeTlsPolicy = defaultTlsPolicy
    }

isLeftWith :: String -> Either String a -> Bool
isLeftWith expectedMessage result =
  case result of
    Left message -> expectedMessage `isInfixOf` message
    Right _ -> False

withFakeCertbotScript :: [String] -> (FilePath -> IO a) -> IO a
withFakeCertbotScript scriptLines action =
  withSystemTempDirectory "fake-certbot-script" $ \tempDirectory -> do
    let scriptPath = tempDirectory </> "certbot"
    writeFile scriptPath (unlines scriptLines)
    callProcess "chmod" ["+x", scriptPath]
    action scriptPath

spec =
  describe "certbot-backed ACME runtime helpers" $ do
    it "derives certbot arguments and resolves certificate names from an ACME configuration" $ do
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
                 certbotHasOption "--http-01-port" (runtimeCertbotArguments (runtimeAcmePlanWith certbotConfigValue)) `shouldBe` True,
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

    it "removes the temporary ACME account-key directory when certbot preparation fails" $
      withSystemTempDirectory "harch-web-certbot-capture" $ \captureDirectory -> do
        let capturedConfigDirectory = captureDirectory </> "config-directory"
            certbotConfig =
              CertbotConfig
                { certbotExecutable = "placeholder",
                  certbotArguments = ["certonly", "--cert-name", "test-cert", "--capture-config-directory", Text.pack capturedConfigDirectory]
                }
        withFakeCertbotScript
          [ "#!/bin/sh",
            "set -eu",
            "config_dir=''",
            "capture_path=''",
            "while [ \"$#\" -gt 0 ]; do",
            "  case \"$1\" in",
            "    --config-dir) config_dir=\"$2\"; shift 2 ;;",
            "    --capture-config-directory) capture_path=\"$2\"; shift 2 ;;",
            "    *) shift ;;",
            "  esac",
            "done",
            "printf '%s' \"$config_dir\" > \"$capture_path\"",
            "exit 42"
          ]
          $ \scriptPath -> do
            webrootStore <- newCertbotWebrootStore
            let failingConfig = certbotConfig {certbotExecutable = scriptPath}
                failingPlan = runtimeAcmePlanWith (certbotConfigValue {acmeCertbotConfig = failingConfig})
            result <-
              try (prepareCertbotManualTlsBindPlan webrootStore failingPlan failingConfig) :: IO (Either IOError (Maybe ManualTlsBindPlan, FilePath))
            case result of
              Left _ -> pure ()
              Right _ -> expectationFailure "expected the failing certbot process to abort preparation"
            stateDirectory <- takeDirectory <$> readFile capturedConfigDirectory
            doesDirectoryExist stateDirectory `shouldReturn` False
