{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (threadDelay)
import Control.Exception (evaluate, try)
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (loadReloadingTlsCredentials, loadTlsCredentialSnapshotOrThrowWithLoader, reloadTlsCredentialsIfChanged, tlsCertificateFilePath, tlsCertificateFilePathValue, tlsPrivateKeyFilePath, tlsPrivateKeyFilePathValue)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory (removePathForcibly)
import System.Environment ()
import System.Exit ()
import System.FilePath ((</>))
import System.IO ()
import System.IO.Error ()
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (manualTlsCertificatePem, manualTlsPrivateKeyPem, withManualTlsFiles)

spec = do
  describe "TlsCertificateFilePath and TlsPrivateKeyFilePath" $
    it "keeps certificate and private-key paths distinct at TLS loader call sites" $
      expectAll
        ( (tlsCertificateFilePathValue (tlsCertificateFilePath "a.pem") `shouldBe` "a.pem")
            :| [ tlsPrivateKeyFilePathValue (tlsPrivateKeyFilePath "a.key") `shouldBe` "a.key"
               ]
        )

  describe "reloadTlsCredentialsIfChanged" $ do
    it "fails explicitly when initial TLS files exist but do not load as credentials" $
      withSystemTempDirectory "harch-web-reloading-tls" $ \tempDirectory -> do
        let certificatePath = tempDirectory </> "fullchain.pem"
            privateKeyPath = tempDirectory </> "privkey.pem"
        writeFile certificatePath "not a certificate"
        writeFile privateKeyPath "not a private key"
        startupResult <- try (loadReloadingTlsCredentials (tlsCertificateFilePath certificatePath) (tlsPrivateKeyFilePath privateKeyPath))
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException `shouldSatisfy` ("Failed to load manual TLS credentials from " `isInfixOf`)
            renderedException `shouldSatisfy` (certificatePath `isInfixOf`)
            renderedException `shouldSatisfy` (privateKeyPath `isInfixOf`)
          Right _ ->
            expectationFailure "Expected invalid TLS credentials to fail during initial load"

    it "reloads rewritten TLS files and keeps the last valid credentials across missing or invalid updates" $
      withSystemTempDirectory "harch-web-reloading-tls" $ \tempDirectory -> do
        let certificatePath = tempDirectory </> "fullchain.pem"
            privateKeyPath = tempDirectory </> "privkey.pem"
        writeFile certificatePath manualTlsCertificatePem
        writeFile privateKeyPath manualTlsPrivateKeyPem
        reloadingTlsCredentials <- loadReloadingTlsCredentials (tlsCertificateFilePath certificatePath) (tlsPrivateKeyFilePath privateKeyPath)
        initialCredentials <- show <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
        threadDelay 100000
        writeFile certificatePath manualTlsCertificatePem
        writeFile privateKeyPath manualTlsPrivateKeyPem
        show
          <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
            `shouldReturn` initialCredentials
        threadDelay 100000
        writeFile certificatePath "not a certificate"
        writeFile privateKeyPath "not a private key"
        show
          <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
            `shouldReturn` initialCredentials
        removePathForcibly certificatePath
        removePathForcibly privateKeyPath
        show
          <$> reloadTlsCredentialsIfChanged reloadingTlsCredentials
            `shouldReturn` initialCredentials

  describe "loadTlsCredentialSnapshotOrThrowWithLoader" $ do
    it "fails explicitly when the TLS credential files disappear during startup loading" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                "Manual TLS"
                (tlsCertificateFilePath certificatePath)
                (tlsPrivateKeyFilePath privateKeyPath)
                (pure Nothing)
            )
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException
              `shouldBe` ("user error (Failed to load manual TLS credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": credential files disappeared while loading)")
          Right _ ->
            expectationFailure "Expected disappearing TLS credential files to fail during startup loading"

    it "fails explicitly when the TLS loader returns a startup credential error" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                "Manual TLS"
                (tlsCertificateFilePath certificatePath)
                (tlsPrivateKeyFilePath privateKeyPath)
                (pure (Just (Left "synthetic TLS credential error")))
            )
        case startupResult of
          Left exception -> do
            let renderedException = show (exception :: IOError)
            evaluate (length renderedException) `shouldReturn` length renderedException
            renderedException
              `shouldBe` ("user error (Failed to load manual TLS credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": synthetic TLS credential error)")
          Right _ ->
            expectationFailure "Expected startup TLS credential errors to surface explicitly"

    it "handles an empty TLS label when surfacing startup credential errors" $
      withManualTlsFiles $ \certificatePath privateKeyPath -> do
        startupResult <-
          try
            ( loadTlsCredentialSnapshotOrThrowWithLoader
                ""
                (tlsCertificateFilePath certificatePath)
                (tlsPrivateKeyFilePath privateKeyPath)
                (pure (Just (Left "synthetic TLS credential error")))
            )
        case startupResult of
          Left exception ->
            show (exception :: IOError)
              `shouldBe` ("user error (Failed to load  credentials from " <> certificatePath <> " and " <> privateKeyPath <> ": synthetic TLS credential error)")
          Right _ ->
            expectationFailure "Expected empty-label startup TLS credential errors to surface explicitly"
