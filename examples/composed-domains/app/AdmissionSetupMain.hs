{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Composed
  ( AdmissionSetupCommand (..),
    AdmissionSetupError (..),
    ComposedDeploymentConfig,
    closeComposedDatabaseRuntime,
    composedDeploymentAdmissionEncryptionKey,
    composedDeploymentDatabase,
    encryptAdmissionTotpSecret,
    newComposedDatabaseRuntime,
    parseAdmissionSetupCommand,
    parseComposedDeploymentConfig,
    provisionPostgresAdmissionCredentialWithRunner,
    renderAdmissionSetupError,
    runComposedDatabaseChanges,
    runComposedDatabaseQuery,
  )
import Control.Exception (bracket)
import Core.Config (loadConfigOverridesFile)
import Data.Bifunctor (first)
import Data.Text qualified as Text
import Data.Text.IO qualified as TextIO
import System.Environment (getArgs, getEnvironment)
import System.IO (Handle, hFlush, hGetEcho, hIsTerminalDevice, hPutStr, hPutStrLn, hSetEcho, stdin, stdout)

main :: IO ()
main = getArgs >>= runAdmissionSetupArgs stdin stdout

runAdmissionSetupArgs :: Handle -> Handle -> [String] -> IO ()
runAdmissionSetupArgs inputHandle outputHandle arguments = do
  command <- either throwAdmissionSetupError pure (parseAdmissionSetupCommand arguments)
  config <- loadAdmissionSetupConfig >>= either throwAdmissionSetupError pure
  case command of
    MigrateAdmissionDatabase -> do
      migrationResult <- runComposedDatabaseChanges (composedDeploymentDatabase config)
      either (const (throwAdmissionSetupError AdmissionSetupMigrationFailed)) (const (hPutStrLn outputHandle "Applied composed admission database changes.")) migrationResult
    ProvisionAdmissionCredential principalId loginName -> do
      rawSecret <- readTerminalTotpSecret inputHandle outputHandle >>= either throwAdmissionSetupError pure
      encryptedSecret <- encryptAdmissionTotpSecret (composedDeploymentAdmissionEncryptionKey config) rawSecret >>= either throwAdmissionSetupError pure
      provisioned <-
        bracket
          (newComposedDatabaseRuntime (composedDeploymentDatabase config))
          closeComposedDatabaseRuntime
          (\runtime -> provisionPostgresAdmissionCredentialWithRunner runComposedDatabaseQuery runtime principalId loginName encryptedSecret)
      case provisioned of
        Right True -> hPutStrLn outputHandle "Provisioned an admission credential."
        Right False -> throwAdmissionSetupError AdmissionSetupCredentialProvisionFailed
        Left _ -> throwAdmissionSetupError AdmissionSetupCredentialProvisionFailed

loadAdmissionSetupConfig :: IO (Either AdmissionSetupError ComposedDeploymentConfig)
loadAdmissionSetupConfig = do
  committedDefaults <- loadConfigOverridesFile ".env"
  localOverrides <- loadConfigOverridesFile ".env.local"
  environmentOverrides <- fmap (map (first Text.pack . fmap Text.pack)) getEnvironment
  pure $ do
    committed <- first (const AdmissionSetupConfigUnavailable) committedDefaults
    local <- first (const AdmissionSetupConfigUnavailable) localOverrides
    first (const AdmissionSetupConfigUnavailable) (parseComposedDeploymentConfig committed local environmentOverrides)

readTerminalTotpSecret :: Handle -> Handle -> IO (Either AdmissionSetupError Text.Text)
readTerminalTotpSecret inputHandle outputHandle = do
  isTerminal <- hIsTerminalDevice inputHandle
  if not isTerminal
    then pure (Left AdmissionSetupSecretInputMustBeTerminal)
    else do
      echoEnabled <- hGetEcho inputHandle
      bracket
        (hSetEcho inputHandle False)
        (const (hSetEcho inputHandle echoEnabled))
        ( \_ -> do
            hPutStr outputHandle "TOTP secret: "
            hFlush outputHandle
            secret <- TextIO.hGetLine inputHandle
            hPutStrLn outputHandle ""
            pure (Right secret)
        )

throwAdmissionSetupError :: AdmissionSetupError -> IO value
throwAdmissionSetupError = ioError . userError . renderAdmissionSetupError
