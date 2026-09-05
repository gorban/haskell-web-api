{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Session (OpaqueSession (..), sessionCookieName, sessionCookieNameText)
import HarchWeb.Time (UnixTimeNanoseconds)
import Unit.WebApi.TestSupport (accountId, shouldReturnEqual)
import WebApi.Session (MfaEnrollmentSessionStore (..), MfaEnrollmentSessionStoreError (..), issueMfaEnrollmentSession, mfaEnrollmentSessionCookiePolicy)

spec = do
  describe "mfaEnrollmentSessionCookiePolicy" $
    it "uses a distinct, short-lived cookie separate from the ordinary login session" $ do
      sessionCookieNameText (sessionCookieName mfaEnrollmentSessionCookiePolicy) `shouldBe` "__Host-harch-mfa-enrollment"

  describe "MFA-enrollment-session issuance" $ do
    it "generates and persists an opaque session after email or password verification succeeds" $ do
      savedSessionReference <- newIORef Nothing
      let store =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \session -> writeIORef savedSessionReference (Just session) >> pure (Right True),
                loadMfaEnrollmentSession = \_ -> pure (Right Nothing),
                invalidateMfaEnrollmentSession = \_ _ -> pure (Right False)
              }
      issuedSessionResult <- issueMfaEnrollmentSession store accountId 100
      case issuedSessionResult of
        Left _ -> expectationFailure "expected MFA-enrollment session issuance to succeed"
        Right issuedSession -> do
          savedSession <- readIORef savedSessionReference
          expectAll
            ( (sessionPrincipal issuedSession `shouldBe` accountId)
                :| [ sessionIssuedAtNanoseconds issuedSession `shouldBe` 100,
                     sessionExpiresAtNanoseconds issuedSession `shouldBe` 600000000100,
                     savedSession `shouldBe` Just issuedSession
                   ]
            )

    it "preserves storage failures and refuses collisions or overflowing expirations" $ do
      let store result =
            MfaEnrollmentSessionStore
              { saveMfaEnrollmentSession = \_ -> pure result,
                loadMfaEnrollmentSession = \_ -> pure (Right Nothing),
                invalidateMfaEnrollmentSession = \_ _ -> pure (Right False)
              }
      issueMfaEnrollmentSession (store (Left MfaEnrollmentSessionStoreUnavailable)) accountId 100 `shouldReturnEqual` Left MfaEnrollmentSessionStoreUnavailable
      issueMfaEnrollmentSession (store (Right False)) accountId 100 `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
      issueMfaEnrollmentSession (store (Right True)) accountId (maxBound :: UnixTimeNanoseconds) `shouldReturnEqual` Left MfaEnrollmentSessionStoreCorruptData
