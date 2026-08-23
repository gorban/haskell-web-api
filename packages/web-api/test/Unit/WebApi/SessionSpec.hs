{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Word (Word64)
import HarchWeb.Session (OpaqueSession (..))
import Unit.WebApi.TestSupport (accountId, shouldReturnEqual)
import WebApi.Session (AccountSessionStore (..), AccountSessionStoreError (..), issueAccountSession)

spec = do
  describe "account-session issuance" $ do
    it "generates and persists an opaque session after authentication succeeds" $ do
      savedSessionReference <- newIORef Nothing
      let store =
            AccountSessionStore
              { saveAccountSession = \session -> writeIORef savedSessionReference (Just session) >> pure (Right True),
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \_ _ -> pure (Right False)
              }
      issuedSessionResult <- issueAccountSession store accountId 100
      case issuedSessionResult of
        Left _ -> expectationFailure "expected session issuance to succeed"
        Right issuedSession -> do
          savedSession <- readIORef savedSessionReference
          expectAll
            ( (sessionPrincipal issuedSession `shouldBe` accountId)
                :| [ sessionIssuedAtNanoseconds issuedSession `shouldBe` 100,
                     sessionExpiresAtNanoseconds issuedSession `shouldBe` 28800000000100,
                     savedSession `shouldBe` Just issuedSession
                   ]
            )

    it "preserves storage failures and refuses collisions or overflowing expirations" $ do
      let store result =
            AccountSessionStore
              { saveAccountSession = \_ -> pure result,
                loadAccountSession = \_ -> pure (Right Nothing),
                invalidateAccountSession = \_ _ -> pure (Right False)
              }
      issueAccountSession (store (Left AccountSessionStoreUnavailable)) accountId 100 `shouldReturnEqual` Left AccountSessionStoreUnavailable
      issueAccountSession (store (Right False)) accountId 100 `shouldReturnEqual` Left AccountSessionStoreCorruptData
      issueAccountSession (store (Right True)) accountId (maxBound :: Word64) `shouldReturnEqual` Left AccountSessionStoreCorruptData
