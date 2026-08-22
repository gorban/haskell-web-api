{-# LANGUAGE OverloadedStrings #-}

module Unit.WebApi.TestSupport
  ( shouldReturnEqual,
    databaseConfig,
    accountId,
    testSessionId,
    sessionIdValue,
    csrfTokenValue,
    opaqueSession,
  )
where

import Control.Monad (unless)
import Data.Text (Text)
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import Test.Hspec (Expectation, expectationFailure)
import WebApi.Config (DatabaseConfig (..))

-- | Fail-fast equality check with a fixed message, for postgres-runner tests
-- whose actual/expected values have no useful 'Show' rendering to include.
shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = 10
    }

accountId :: AccountId
accountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

testSessionId :: SessionId
testSessionId =
  case mkSessionId sessionIdValue of
    Just value -> value
    Nothing -> error "expected a valid session id"

csrfTokenValue :: Text
csrfTokenValue = "abcdefghijklmnopqrstuvwxyz0123456789-_"

sessionIdValue :: Text
sessionIdValue = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

opaqueSession :: OpaqueSession AccountId
opaqueSession =
  case mkCsrfToken csrfTokenValue of
    Just csrfToken ->
      OpaqueSession
        { sessionId = testSessionId,
          sessionPrincipal = accountId,
          sessionCsrfToken = csrfToken,
          sessionIssuedAtNanoseconds = 100,
          sessionExpiresAtNanoseconds = 200
        }
    Nothing -> error "expected a valid csrf token"
