{-# LANGUAGE BangPatterns #-}

-- | Private TLS credential loading and reload mechanics.
--
-- This module owns filesystem polling and credential snapshots. Each TLS
-- handshake stats the configured files, but parses them only after a change;
-- a reload lock serializes that exceptional parse-and-store path. Socket and
-- Warp lifecycle ownership stays in 'HarchWeb.Server.Transport'.
module HarchWeb.Server.Transport.Tls
  ( ReloadingTlsCredentials,
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    ensureRuntimeFileExists,
    loadReloadingTlsCredentials,
    loadReloadingTlsCredentialsWithLabel,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    awaitReloadingTlsCredentials,
    reloadTlsCredentialsIfChanged,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
  )
where

import Control.Concurrent (MVar, modifyMVar, newMVar, readMVar, threadDelay)
import Control.Monad (unless)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import GHC.Clock (getMonotonicTimeNSec)
import Network.TLS qualified as TLS
import System.Directory (doesFileExist, getModificationTime)

data TlsCredentialSnapshot = TlsCredentialSnapshot
  { tlsCredentialModifiedTimes :: (UTCTime, UTCTime),
    tlsCredentialValues :: TLS.Credentials
  }

data ReloadingTlsCredentials = ReloadingTlsCredentials
  { tlsCredentialCertificatePath :: FilePath,
    tlsCredentialPrivateKeyPath :: FilePath,
    tlsCredentialSnapshotReference :: MVar TlsCredentialSnapshot
  }

-- | A TLS certificate file path. Opaque so it cannot be transposed with
-- 'TlsPrivateKeyFilePath' at a loader call site.
newtype TlsCertificateFilePath = TlsCertificateFilePath FilePath

tlsCertificateFilePath :: FilePath -> TlsCertificateFilePath
tlsCertificateFilePath = TlsCertificateFilePath

tlsCertificateFilePathValue :: TlsCertificateFilePath -> FilePath
tlsCertificateFilePathValue (TlsCertificateFilePath path) = path

-- | A TLS private key file path. Opaque so it cannot be transposed with
-- 'TlsCertificateFilePath' at a loader call site.
newtype TlsPrivateKeyFilePath = TlsPrivateKeyFilePath FilePath

tlsPrivateKeyFilePath :: FilePath -> TlsPrivateKeyFilePath
tlsPrivateKeyFilePath = TlsPrivateKeyFilePath

tlsPrivateKeyFilePathValue :: TlsPrivateKeyFilePath -> FilePath
tlsPrivateKeyFilePathValue (TlsPrivateKeyFilePath path) = path

ensureRuntimeFileExists :: String -> FilePath -> IO ()
ensureRuntimeFileExists errorPrefix filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists (ioError (userError (errorPrefix <> filePath)))

loadReloadingTlsCredentials :: TlsCertificateFilePath -> TlsPrivateKeyFilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentials (TlsCertificateFilePath certificatePath) (TlsPrivateKeyFilePath privateKeyPath) = do
  snapshot <- loadTlsCredentialSnapshotOrThrow certificatePath privateKeyPath
  snapshotReference <- newMVar snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

loadReloadingTlsCredentialsWithLabel :: String -> TlsCertificateFilePath -> TlsPrivateKeyFilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentialsWithLabel tlsLabel (TlsCertificateFilePath certificatePath) (TlsPrivateKeyFilePath privateKeyPath) = do
  snapshot <- loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath
  snapshotReference <- newMVar snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

awaitReloadingTlsCredentials :: Maybe Int -> TlsCertificateFilePath -> TlsPrivateKeyFilePath -> IO ReloadingTlsCredentials
awaitReloadingTlsCredentials waitTimeoutSeconds (TlsCertificateFilePath certificatePath) (TlsPrivateKeyFilePath privateKeyPath) = do
  startedAt <- getMonotonicTimeNSec
  go startedAt
  where
    timeoutWindow =
      fmap
        (\seconds -> (seconds, fromIntegral seconds * 1000000000))
        waitTimeoutSeconds

    go !startedAt = do
      snapshotResult <- loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath
      case snapshotResult of
        Just (Right snapshot) -> do
          snapshotReference <- newMVar snapshot
          pure
            ReloadingTlsCredentials
              { tlsCredentialCertificatePath = certificatePath,
                tlsCredentialPrivateKeyPath = privateKeyPath,
                tlsCredentialSnapshotReference = snapshotReference
              }
        _ -> do
          currentTime <- getMonotonicTimeNSec
          case timeoutWindow of
            Just (waitSeconds, timeoutNs)
              | currentTime - startedAt >= timeoutNs ->
                  let timeoutSuffix = " after " <> show waitSeconds <> " seconds"
                   in ioError . userError $
                        case snapshotResult of
                          Just (Left loadError) ->
                            "Timed out waiting for shared TLS credentials at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
                              <> ": "
                              <> loadError
                          _ ->
                            "Timed out waiting for shared TLS certificate files at "
                              <> certificatePath
                              <> " and "
                              <> privateKeyPath
                              <> timeoutSuffix
            _ -> threadDelay 100000 >> go startedAt

reloadTlsCredentialsIfChanged :: ReloadingTlsCredentials -> IO TLS.Credentials
reloadTlsCredentialsIfChanged reloadingTlsCredentials = do
  cachedSnapshot <- readMVar (tlsCredentialSnapshotReference reloadingTlsCredentials)
  latestTimes <- currentModificationTimes
  case latestTimes of
    Just times
      | times /= tlsCredentialModifiedTimes cachedSnapshot ->
          modifyMVar (tlsCredentialSnapshotReference reloadingTlsCredentials) reloadChangedCredentials
    _ ->
      pure (tlsCredentialValues cachedSnapshot)
  where
    currentModificationTimes =
      currentTlsCredentialModificationTimes
        (tlsCredentialCertificatePath reloadingTlsCredentials)
        (tlsCredentialPrivateKeyPath reloadingTlsCredentials)

    reloadChangedCredentials cachedSnapshot = do
      latestTimes <- currentModificationTimes
      case latestTimes of
        Just times
          | times /= tlsCredentialModifiedTimes cachedSnapshot -> do
              latestSnapshotResult <-
                loadTlsCredentialSnapshotForModificationTimes
                  (tlsCredentialCertificatePath reloadingTlsCredentials)
                  (tlsCredentialPrivateKeyPath reloadingTlsCredentials)
                  times
              case latestSnapshotResult of
                Right latestSnapshot -> do
                  pure (latestSnapshot, tlsCredentialValues latestSnapshot)
                Left _ ->
                  pure (cachedSnapshot, tlsCredentialValues cachedSnapshot)
        _ ->
          pure (cachedSnapshot, tlsCredentialValues cachedSnapshot)

loadTlsCredentialSnapshotOrThrow :: FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrow =
  loadTlsCredentialSnapshotOrThrowWithLabel "Manual TLS"

loadTlsCredentialSnapshotOrThrowWithLabel :: String -> FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath =
  loadTlsCredentialSnapshotOrThrowWithLoader
    tlsLabel
    (TlsCertificateFilePath certificatePath)
    (TlsPrivateKeyFilePath privateKeyPath)
    (loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath)

loadTlsCredentialSnapshotOrThrowWithLoader :: String -> TlsCertificateFilePath -> TlsPrivateKeyFilePath -> IO (Maybe (Either String TlsCredentialSnapshot)) -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLoader tlsLabel (TlsCertificateFilePath certificatePath) (TlsPrivateKeyFilePath privateKeyPath) loadSnapshot = do
  ensureRuntimeFileExists (tlsLabel <> " certificate file does not exist: ") certificatePath
  ensureRuntimeFileExists (tlsLabel <> " private key file does not exist: ") privateKeyPath
  snapshotResult <- loadSnapshot
  case fromMaybe (Left "credential files disappeared while loading") snapshotResult of
    Right snapshot ->
      pure snapshot
    Left loadError ->
      ioError . userError $
        "Failed to load "
          <> lowerFirst tlsLabel
          <> " credentials from "
          <> certificatePath
          <> " and "
          <> privateKeyPath
          <> ": "
          <> loadError
  where
    lowerFirst [] = []
    lowerFirst (firstCharacter : remainingCharacters) =
      toLower firstCharacter : remainingCharacters

loadTlsCredentialSnapshotIfPresent :: FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot))
loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath = do
  modificationTimes <- currentTlsCredentialModificationTimes certificatePath privateKeyPath
  traverse
    ( loadTlsCredentialSnapshotForModificationTimes
        certificatePath
        privateKeyPath
    )
    modificationTimes

currentTlsCredentialModificationTimes :: FilePath -> FilePath -> IO (Maybe (UTCTime, UTCTime))
currentTlsCredentialModificationTimes certificatePath privateKeyPath = do
  certificateExists <- doesFileExist certificatePath
  privateKeyExists <- doesFileExist privateKeyPath
  if certificateExists && privateKeyExists
    then
      Just <$> ((,) <$> getModificationTime certificatePath <*> getModificationTime privateKeyPath)
    else pure Nothing

loadTlsCredentialSnapshotForModificationTimes :: FilePath -> FilePath -> (UTCTime, UTCTime) -> IO (Either String TlsCredentialSnapshot)
loadTlsCredentialSnapshotForModificationTimes certificatePath privateKeyPath modificationTimes = do
  credentialResult <- TLS.credentialLoadX509 certificatePath privateKeyPath
  pure
    ( fmap
        ( \credential ->
            TlsCredentialSnapshot
              { tlsCredentialModifiedTimes = modificationTimes,
                tlsCredentialValues = TLS.Credentials [credential]
              }
        )
        credentialResult
    )
