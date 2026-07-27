{-# LANGUAGE BangPatterns #-}

-- | Private TLS credential loading and reload mechanics.
--
-- This module owns filesystem polling and credential snapshots. Socket and
-- Warp lifecycle ownership stays in 'HarchWeb.Server.Transport'.
module HarchWeb.Server.Transport.Tls
  ( ReloadingTlsCredentials,
    ensureRuntimeFileExists,
    loadReloadingTlsCredentials,
    loadReloadingTlsCredentialsWithLabel,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    awaitReloadingTlsCredentials,
    reloadTlsCredentialsIfChanged,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
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
    tlsCredentialSnapshotReference :: IORef TlsCredentialSnapshot
  }

ensureRuntimeFileExists :: String -> FilePath -> IO ()
ensureRuntimeFileExists errorPrefix filePath = do
  fileExists <- doesFileExist filePath
  unless fileExists (ioError (userError (errorPrefix <> filePath)))

loadReloadingTlsCredentials :: FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentials certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrow certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

loadReloadingTlsCredentialsWithLabel :: String -> FilePath -> FilePath -> IO ReloadingTlsCredentials
loadReloadingTlsCredentialsWithLabel tlsLabel certificatePath privateKeyPath = do
  snapshot <- loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath
  snapshotReference <- newIORef snapshot
  pure
    ReloadingTlsCredentials
      { tlsCredentialCertificatePath = certificatePath,
        tlsCredentialPrivateKeyPath = privateKeyPath,
        tlsCredentialSnapshotReference = snapshotReference
      }

awaitReloadingTlsCredentials :: Maybe Int -> FilePath -> FilePath -> IO ReloadingTlsCredentials
awaitReloadingTlsCredentials waitTimeoutSeconds certificatePath privateKeyPath = do
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
          snapshotReference <- newIORef snapshot
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
  cachedSnapshot <-
    atomicModifyIORef'
      (tlsCredentialSnapshotReference reloadingTlsCredentials)
      (\snapshot -> (snapshot, snapshot))
  latestSnapshotResult <-
    loadTlsCredentialSnapshotIfPresent
      (tlsCredentialCertificatePath reloadingTlsCredentials)
      (tlsCredentialPrivateKeyPath reloadingTlsCredentials)
  case latestSnapshotResult of
    Just (Right latestSnapshot)
      | tlsCredentialModifiedTimes latestSnapshot /= tlsCredentialModifiedTimes cachedSnapshot ->
          latestSnapshot `seq`
            atomicModifyIORef'
              (tlsCredentialSnapshotReference reloadingTlsCredentials)
              (const (latestSnapshot, tlsCredentialValues latestSnapshot))
    _ ->
      pure (tlsCredentialValues cachedSnapshot)

loadTlsCredentialSnapshotOrThrow :: FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrow =
  loadTlsCredentialSnapshotOrThrowWithLabel "Manual TLS"

loadTlsCredentialSnapshotOrThrowWithLabel :: String -> FilePath -> FilePath -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLabel tlsLabel certificatePath privateKeyPath =
  loadTlsCredentialSnapshotOrThrowWithLoader
    tlsLabel
    certificatePath
    privateKeyPath
    (loadTlsCredentialSnapshotIfPresent certificatePath privateKeyPath)

loadTlsCredentialSnapshotOrThrowWithLoader :: String -> FilePath -> FilePath -> IO (Maybe (Either String TlsCredentialSnapshot)) -> IO TlsCredentialSnapshot
loadTlsCredentialSnapshotOrThrowWithLoader tlsLabel certificatePath privateKeyPath loadSnapshot = do
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
  certificateExists <- doesFileExist certificatePath
  privateKeyExists <- doesFileExist privateKeyPath
  if certificateExists && privateKeyExists
    then do
      certificateModifiedAt <- getModificationTime certificatePath
      privateKeyModifiedAt <- getModificationTime privateKeyPath
      credentialResult <- TLS.credentialLoadX509 certificatePath privateKeyPath
      pure
        ( Just
            ( fmap
                ( \credential ->
                    TlsCredentialSnapshot
                      { tlsCredentialModifiedTimes = (certificateModifiedAt, privateKeyModifiedAt),
                        tlsCredentialValues = TLS.Credentials [credential]
                      }
                )
                credentialResult
            )
        )
    else
      pure Nothing
