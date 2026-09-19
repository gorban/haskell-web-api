{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.App (buildApplication, twoPageServerConfig)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb qualified
import HarchWeb.Csrf.Signed qualified as Signed
import HarchWeb.Time (currentUnixTimeNanoseconds)
import System.IO (stdout)

main :: IO ()
main = do
  signingKey <- Signed.generateCsrfSigningKey
  case Signed.mkCsrfKeyId "two-pages-development-v1" of
    Nothing -> ioError (userError "invalid two-pages development CSRF key identifier")
    Just keyId ->
      case Signed.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []) of
        Nothing -> ioError (userError "invalid two-pages development CSRF key ring")
        Just keyring ->
          HarchWeb.runServer
            stdout
            twoPageServerConfig
            ( buildApplication
                ( Signed.signedCsrfProtection
                    Signed.SignedCsrfDependencies
                      { Signed.signedCsrfDependenciesKeyring = keyring,
                        Signed.signedCsrfDependenciesPolicy = Signed.defaultSignedCsrfPolicy,
                        Signed.signedCsrfDependenciesCurrentTime = currentUnixTimeNanoseconds,
                        Signed.signedCsrfDependenciesResolveBinding = const (pure HarchWeb.AnonymousCsrfBinding)
                      }
                )
            )
