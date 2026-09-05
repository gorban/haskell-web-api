{-# LANGUAGE OverloadedStrings #-}

-- | Ephemeral JWK material for tests that deliberately start the real server.
--
-- The fixture writes a freshly generated keypair into a temporary directory,
-- rather than putting a reusable private key in the repository.  It returns
-- both the typed environment used by in-process startup tests and the exact
-- public configuration lines required by a spawned executable.
module TestSupport.AccountJwt
  ( withTestAccountJwtFixture,
  )
where

import Control.Lens ((&), (?~))
import Crypto.JOSE.JWA.JWK qualified as JwaJwk
import Crypto.JOSE.JWK qualified as JoseJwk
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import WebApi.AccountJwt (mkAccountJwtConfiguration)
import WebApi.Config (AppEnvironmentConfig (..), defaultAppEnvironmentConfig)

withTestAccountJwtFixture :: (AppEnvironmentConfig -> [String] -> IO value) -> IO value
withTestAccountJwtFixture action =
  withSystemTempDirectory "web-api-account-jwt-runtime" $ \directory -> do
    signingKey <- JoseJwk.genJWK (JwaJwk.RSAGenParam 1024)
    let namedSigningKey = signingKey & JoseJwk.jwkKid ?~ "test-account-key-v1"
        signingFile = directory </> "private.jwk"
        verificationFile = directory </> "verification.jwks"
        configuration =
          case mkAccountJwtConfiguration "http://127.0.0.1:5001" "web-api-account" "test-account-key-v1" signingFile verificationFile "__Host-harch-session" 28800 of
            Right value -> value
            Left _ -> error "expected a valid test account-JWT configuration"
        configLines =
          [ "ACCOUNT_JWT_ISSUER=http://127.0.0.1:5001",
            "ACCOUNT_JWT_AUDIENCE=web-api-account",
            "ACCOUNT_JWT_ACTIVE_KEY_ID=test-account-key-v1",
            "ACCOUNT_JWT_SIGNING_JWK_FILE=" <> signingFile,
            "ACCOUNT_JWT_VERIFICATION_JWK_SET_FILE=" <> verificationFile,
            "ACCOUNT_JWT_COOKIE_NAME=__Host-harch-session",
            "ACCOUNT_JWT_COOKIE_MAX_AGE_SECONDS=28800"
          ]
    ByteString.writeFile signingFile (LazyByteString.toStrict (Aeson.encode namedSigningKey))
    ByteString.writeFile verificationFile (LazyByteString.toStrict (Aeson.encode (JoseJwk.JWKSet [namedSigningKey])))
    action (defaultAppEnvironmentConfig {accountJwtConfiguration = configuration}) configLines
