{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.SessionSpec (spec) where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Session
import Test.Hspec

validSessionToken :: Text
validSessionToken = "0123456789abcdef0123456789abcdef"

otherSessionToken :: Text
otherSessionToken = "abcdef0123456789abcdef0123456789"

sampleSessionId :: SessionId
sampleSessionId = required "session id" (mkSessionId validSessionToken)

sampleCsrfToken :: CsrfToken
sampleCsrfToken = required "csrf token" (mkCsrfToken otherSessionToken)

sampleSession :: OpaqueSession Text
sampleSession =
  OpaqueSession
    { sessionId = sampleSessionId,
      sessionPrincipal = "account-123",
      sessionCsrfToken = sampleCsrfToken,
      sessionIssuedAtNanoseconds = 100,
      sessionExpiresAtNanoseconds = 200
    }

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

spec :: Spec
spec = do
  describe "opaque token generation" $ do
    it "uses 256-bit URL-safe values for session identifiers and CSRF tokens" $ do
      generatedSessionId <- generateSessionId
      generatedCsrfToken <- generateCsrfToken
      sessionIdText generatedSessionId `shouldSatisfy` (\token -> Text.length token == 43)
      csrfTokenText generatedCsrfToken `shouldSatisfy` (\token -> Text.length token == 43)
      mkSessionId (sessionIdText generatedSessionId) `shouldBe` Just generatedSessionId
      mkCsrfToken (csrfTokenText generatedCsrfToken) `shouldBe` Just generatedCsrfToken

  describe "opaque tokens" $ do
    it "accepts long URL-safe values and rejects short or unsafe cookie values" $ do
      sessionIdText sampleSessionId `shouldBe` validSessionToken
      csrfTokenText sampleCsrfToken `shouldBe` otherSessionToken
      sampleSessionId /= required "other session id" (mkSessionId otherSessionToken) `shouldBe` True
      sampleCsrfToken /= required "other csrf token" (mkCsrfToken validSessionToken) `shouldBe` True
      show sampleSessionId `shouldBe` "SessionId \"0123456789abcdef0123456789abcdef\""
      show [sampleSessionId] `shouldBe` "[SessionId \"0123456789abcdef0123456789abcdef\"]"
      show sampleCsrfToken `shouldBe` "CsrfToken \"abcdef0123456789abcdef0123456789\""
      show [sampleCsrfToken] `shouldBe` "[CsrfToken \"abcdef0123456789abcdef0123456789\"]"
      mkSessionId "short" `shouldBe` Nothing
      mkCsrfToken "0123456789abcdef0123456789abcde=" `shouldBe` Nothing
      mkCsrfToken "0123456789abcdef0123456789abcde/" `shouldBe` Nothing
      mkSessionId "0123456789abcdef0123456789abcde_" `shouldSatisfy` (/= Nothing)
      mkSessionId "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" `shouldSatisfy` (/= Nothing)

  describe "SessionCookiePolicy" $ do
    it "renders host-only secure session cookies without putting CSRF state in the browser" $ do
      let cookieName = required "cookie name" (mkSessionCookieName "__Host-account")
          policy = SessionCookiePolicy {sessionCookieName = cookieName, sessionCookieMaxAgeSeconds = 900}
      sessionCookieNameText cookieName `shouldBe` "__Host-account"
      sessionCookieName defaultSessionCookiePolicy `shouldBe` required "default cookie name" (mkSessionCookieName "__Host-harch-session")
      sessionCookieMaxAgeSeconds defaultSessionCookiePolicy `shouldBe` 28800
      policy /= defaultSessionCookiePolicy `shouldBe` True
      cookieName /= required "default cookie name" (mkSessionCookieName "__Host-harch-session") `shouldBe` True
      show cookieName `shouldBe` "SessionCookieName \"__Host-account\""
      show [cookieName] `shouldBe` "[SessionCookieName \"__Host-account\"]"
      show policy `shouldBe` "SessionCookiePolicy {sessionCookieName = SessionCookieName \"__Host-account\", sessionCookieMaxAgeSeconds = 900}"
      show [policy] `shouldBe` "[SessionCookiePolicy {sessionCookieName = SessionCookieName \"__Host-account\", sessionCookieMaxAgeSeconds = 900}]"
      renderSessionCookie policy sampleSessionId
        `shouldBe` "__Host-account=0123456789abcdef0123456789abcdef; Path=/; Max-Age=900; HttpOnly; Secure; SameSite=Strict"
      mkSessionCookieName "" `shouldBe` Nothing
      mkSessionCookieName "account session" `shouldBe` Nothing
      mkSessionCookieName "account;session" `shouldBe` Nothing

  describe "validateSession" $ do
    it "distinguishes missing, expired, and active server-side session state" $ do
      validateSession 100 Nothing `shouldBe` (MissingSession :: SessionValidation Text)
      validateSession 200 (Just sampleSession) `shouldBe` ExpiredSession
      validateSession 199 (Just sampleSession) `shouldBe` ActiveSession sampleSession
      MissingSession /= (ExpiredSession :: SessionValidation Text) `shouldBe` True
      sessionId sampleSession `shouldBe` sampleSessionId
      sessionPrincipal sampleSession `shouldBe` "account-123"
      sessionCsrfToken sampleSession `shouldBe` sampleCsrfToken
      sessionIssuedAtNanoseconds sampleSession `shouldBe` 100
      sessionExpiresAtNanoseconds sampleSession `shouldBe` 200
      sampleSession == sampleSession `shouldBe` True
      sampleSession /= sampleSession {sessionExpiresAtNanoseconds = 201} `shouldBe` True
      show sampleSession `shouldContain` "sessionPrincipal = \"account-123\""
      show [sampleSession] `shouldContain` "OpaqueSession"
      show (ActiveSession sampleSession) `shouldContain` "ActiveSession"
      show [MissingSession, ExpiredSession :: SessionValidation Text] `shouldBe` "[MissingSession,ExpiredSession]"

    it "keeps lookup and invalidation persistence application-owned" $ do
      storedSession <- newIORef (Just sampleSession)
      let sessionLookup =
            SessionLookup
              { lookupOpaqueSession = const (readIORef storedSession),
                invalidateOpaqueSession = \_ -> writeIORef storedSession Nothing
              }
      lookupOpaqueSession sessionLookup sampleSessionId `shouldReturn` Just sampleSession
      invalidateOpaqueSession sessionLookup sampleSessionId
      lookupOpaqueSession sessionLookup sampleSessionId `shouldReturn` Nothing

  describe "validateCsrfToken" $ do
    it "accepts only the matching synchronizer token" $ do
      validateCsrfToken sampleCsrfToken sampleCsrfToken `shouldBe` True
      validateCsrfToken sampleCsrfToken (required "different csrf token" (mkCsrfToken validSessionToken)) `shouldBe` False

  describe "SafeReturnPath" $ do
    it "keeps navigation same-origin and rejects redirect, control-character, and backslash escapes" $ do
      let returnPath = required "safe return path" (mkSafeReturnPath "/account/settings?tab=security")
      renderSafeReturnPath returnPath `shouldBe` "/account/settings?tab=security"
      returnPath /= required "other safe return path" (mkSafeReturnPath "/") `shouldBe` True
      returnPath == returnPath `shouldBe` True
      show returnPath `shouldBe` "SafeReturnPath \"/account/settings?tab=security\""
      show [returnPath] `shouldBe` "[SafeReturnPath \"/account/settings?tab=security\"]"
      mkSafeReturnPath "https://attacker.test" `shouldBe` Nothing
      mkSafeReturnPath "//attacker.test" `shouldBe` Nothing
      mkSafeReturnPath "/\\attacker.test" `shouldBe` Nothing
      mkSafeReturnPath "/account\r\nLocation: https://attacker.test" `shouldBe` Nothing
      mkSafeReturnPath "/account\0" `shouldBe` Nothing
