{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Private ACME account-key, certificate-request, and JWK material operations.
module HarchWeb.Acme.KeyMaterial
  ( AcmeCertificateRequestPaths (..),
    acmeCertificateRequestConfig,
    generateAcmeAccountKey,
    generateAcmeCertificateRequest,
    loadAcmeJwk,
  )
where

import Data.List (intercalate)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Acme.Certbot.Runtime (RuntimeAcmeBindPlan (..))
import HarchWeb.Acme.Crypto (base64urlText, hexTextToByteString)
import HarchWeb.Acme.OpenSsl (runOpenSslCommand, runOpenSslTextCommand)
import HarchWeb.Acme.Protocol.Types (AcmeJwk (..))
import HarchWeb.Server.Config (ListenerEndpoint (..))

generateAcmeAccountKey :: RuntimeAcmeBindPlan -> FilePath -> IO ()
generateAcmeAccountKey !runtimeAcmePlan accountKeyPath =
  runOpenSslCommand runtimeAcmePlan ["genrsa", "-out", accountKeyPath, "4096"]

-- | The four CSR-generation file paths. Grouping them stops a positional call
-- site from transposing, for example, 'acmeCsrConfigPath' with
-- 'acmeCsrPemPath'.
data AcmeCertificateRequestPaths = AcmeCertificateRequestPaths
  { acmeCsrPrivateKeyPath :: FilePath,
    acmeCsrConfigPath :: FilePath,
    acmeCsrPemPath :: FilePath,
    acmeCsrDerPath :: FilePath
  }

generateAcmeCertificateRequest :: RuntimeAcmeBindPlan -> [Text] -> AcmeCertificateRequestPaths -> IO ()
generateAcmeCertificateRequest !runtimeAcmePlan domains paths = do
  writeFile (acmeCsrConfigPath paths) (acmeCertificateRequestConfig domains)
  runOpenSslCommand
    runtimeAcmePlan
    [ "req",
      "-new",
      "-newkey",
      "rsa:2048",
      "-nodes",
      "-keyout",
      acmeCsrPrivateKeyPath paths,
      "-out",
      acmeCsrPemPath paths,
      "-config",
      acmeCsrConfigPath paths
    ]
  runOpenSslCommand
    runtimeAcmePlan
    ["req", "-in", acmeCsrPemPath paths, "-outform", "DER", "-out", acmeCsrDerPath paths]

acmeCertificateRequestConfig :: [Text] -> String
acmeCertificateRequestConfig domains =
  unlines
    [ "[req]",
      "distinguished_name = req_distinguished_name",
      "prompt = no",
      "req_extensions = req_ext",
      "",
      "[req_distinguished_name]",
      "CN = " <> Text.unpack firstDomain,
      "",
      "[req_ext]",
      "subjectAltName = " <> intercalate "," (map (("DNS:" <>) . Text.unpack) domains)
    ]
  where
    firstDomain =
      case domains of
        domain : _ -> domain
        [] -> "localhost"

loadAcmeJwk :: RuntimeAcmeBindPlan -> FilePath -> IO AcmeJwk
loadAcmeJwk !runtimeAcmePlan accountKeyPath = do
  modulusOutput <- runOpenSslTextCommand runtimeAcmePlan ["rsa", "-in", accountKeyPath, "-modulus", "-noout"]
  modulusText <-
    maybe
      ( ioError . userError $
          "OpenSSL did not return an RSA modulus for ACME listener on "
            <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
      )
      pure
      (Text.stripPrefix "Modulus=" (Text.strip (Text.pack modulusOutput)))
  modulusBytes <-
    either
      ( \decodeError ->
          ioError . userError $
            "OpenSSL returned an invalid RSA modulus for ACME listener on "
              <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
              <> ": "
              <> decodeError
      )
      pure
      (hexTextToByteString modulusText)
  pure
    AcmeJwk
      { acmeJwkExponent = "AQAB",
        acmeJwkModulus = base64urlText modulusBytes
      }

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
