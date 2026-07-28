{-# LANGUAGE BangPatterns #-}

-- | Private OpenSSL process adapter for ACME key and signature operations.
module HarchWeb.Acme.OpenSsl
  ( openSslSha256,
    runOpenSslCommand,
    runOpenSslTextCommand,
    signOpenSslRs256,
  )
where

import Control.Exception (IOException, bracket, evaluate, try)
import Control.Monad (void)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text qualified as Text
import HarchWeb.Acme.Certbot.Runtime (RuntimeAcmeBindPlan (..))
import HarchWeb.Server.Config (ListenerEndpoint (..))
import System.Directory (removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import System.Process (proc, readCreateProcessWithExitCode)

runOpenSslTextCommand :: RuntimeAcmeBindPlan -> [String] -> IO String
runOpenSslTextCommand !runtimeAcmePlan arguments = do
  processResult <-
    try (readCreateProcessWithExitCode (proc "openssl" arguments) "") ::
      IO (Either IOException (ExitCode, String, String))
  case processResult of
    Left launchError ->
      ioError . userError $
        "Failed to launch openssl for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> ": "
          <> show launchError
    Right (ExitSuccess, stdoutText, stderrText) -> do
      void (evaluate (length stderrText))
      pure stdoutText
    Right (exitCode, stdoutText, stderrText) ->
      ioError . userError $
        "OpenSSL failed for ACME listener on "
          <> renderListenerEndpoint (runtimeAcmeEndpoint runtimeAcmePlan)
          <> " with exit code "
          <> show exitCode
          <> ".\nstdout:\n"
          <> stdoutText
          <> "\nstderr:\n"
          <> stderrText

runOpenSslCommand :: RuntimeAcmeBindPlan -> [String] -> IO ()
runOpenSslCommand !runtimeAcmePlan arguments =
  void (runOpenSslTextCommand runtimeAcmePlan arguments)

signOpenSslRs256 :: RuntimeAcmeBindPlan -> FilePath -> LazyByteString.ByteString -> IO ByteString.ByteString
signOpenSslRs256 !runtimeAcmePlan accountKeyPath signingInput = do
  temporaryDirectory <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory temporaryDirectory "harch-web-acme-sign")
    removePathForcibly
    $ \signatureDirectory -> do
      let inputPath = signatureDirectory </> "signing-input.bin"
          outputPath = signatureDirectory </> "signature.bin"
      LazyByteString.writeFile inputPath signingInput
      runOpenSslCommand runtimeAcmePlan ["dgst", "-sha256", "-binary", "-sign", accountKeyPath, "-out", outputPath, inputPath]
      ByteString.readFile outputPath

openSslSha256 :: RuntimeAcmeBindPlan -> LazyByteString.ByteString -> IO ByteString.ByteString
openSslSha256 !runtimeAcmePlan inputBytes = do
  temporaryDirectory <- getCanonicalTemporaryDirectory
  bracket
    (createTempDirectory temporaryDirectory "harch-web-acme-sha256")
    removePathForcibly
    $ \hashDirectory -> do
      let inputPath = hashDirectory </> "hash-input.bin"
          outputPath = hashDirectory </> "hash-output.bin"
      LazyByteString.writeFile inputPath inputBytes
      runOpenSslCommand runtimeAcmePlan ["dgst", "-sha256", "-binary", "-out", outputPath, inputPath]
      ByteString.readFile outputPath

renderListenerEndpoint :: ListenerEndpoint -> String
renderListenerEndpoint endpoint =
  Text.unpack (endpointHost endpoint) <> ":" <> show (endpointPort endpoint)
