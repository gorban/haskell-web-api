{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.App (buildApplication, twoPageServerConfig)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb qualified
import HarchWeb.Time (currentUnixTimeNanoseconds)
import System.IO (stdout)

main :: IO ()
main = do
  signingKey <- HarchWeb.generateCsrfSigningKey
  case HarchWeb.mkCsrfKeyId "two-pages-development-v1" of
    Nothing -> ioError (userError "invalid two-pages development CSRF key identifier")
    Just keyId ->
      case HarchWeb.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []) of
        Nothing -> ioError (userError "invalid two-pages development CSRF key ring")
        Just keyring ->
          HarchWeb.runServer
            stdout
            twoPageServerConfig
            ( buildApplication
                ( HarchWeb.signedCsrfProtection
                    keyring
                    HarchWeb.defaultSignedCsrfPolicy
                    currentUnixTimeNanoseconds
                    (const (pure HarchWeb.AnonymousCsrfBinding))
                )
            )
