{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Bits (xor)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Either (fromRight)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Csrf (CsrfToken, csrfTokenText, generateCsrfToken, mkCsrfToken, validateCsrfToken)
import HarchWeb.Csrf qualified as Csrf
import HarchWeb.Document qualified as Document
import HarchWeb.Session
import HarchWeb.Time qualified as Time
import Network.HTTP.Types qualified as Http

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
      sessionIssuedAtNanoseconds = 100,
      sessionExpiresAtNanoseconds = 200
    }

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

signedTokenBytes :: ByteString.ByteString -> ByteString.ByteString -> ByteString.ByteString
signedTokenBytes magic keyBytes =
  ByteString.concat
    [ magic,
      ByteString.singleton (fromIntegral (ByteString.length keyBytes)),
      keyBytes,
      ByteString.replicate (8 + 8 + 32 + 32) 0,
      ByteString.replicate 32 0
    ]

tamperSignedTokenMac :: CsrfToken -> Text
tamperSignedTokenMac token =
  TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded tampered)
  where
    decoded = fromRight (error "issued CSRF token was not base64url") (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 (csrfTokenText token)))
    tampered =
      case ByteString.unsnoc decoded of
        Nothing -> error "issued CSRF token unexpectedly decoded to no bytes"
        Just (prefix, finalByte) -> ByteString.snoc prefix (finalByte `xor` 1)

spec = do
  describe "opaque token generation" $ do
    it "uses 256-bit URL-safe values for session identifiers and CSRF tokens" $ do
      generatedSessionId <- generateSessionId
      generatedCsrfToken <- generateCsrfToken
      expectAll
        ( (sessionIdText generatedSessionId `shouldSatisfy` (\token -> Text.length token == 43))
            :| [ csrfTokenText generatedCsrfToken `shouldSatisfy` (\token -> Text.length token == 43),
                 mkSessionId (sessionIdText generatedSessionId) `shouldBe` Just generatedSessionId,
                 mkCsrfToken (csrfTokenText generatedCsrfToken) `shouldBe` Just generatedCsrfToken
               ]
        )

  describe "opaque tokens" $ do
    it "accepts long URL-safe values and rejects short or unsafe cookie values" $ do
      expectAll
        ( (sessionIdText sampleSessionId `shouldBe` validSessionToken)
            :| [ csrfTokenText sampleCsrfToken `shouldBe` otherSessionToken,
                 sampleSessionId /= required "other session id" (mkSessionId otherSessionToken) `shouldBe` True,
                 sampleCsrfToken /= required "other csrf token" (mkCsrfToken validSessionToken) `shouldBe` True,
                 show sampleSessionId `shouldBe` "SessionId <redacted>",
                 show [sampleSessionId] `shouldBe` "[SessionId <redacted>]",
                 show sampleCsrfToken `shouldBe` "CsrfToken <redacted>",
                 show [sampleCsrfToken] `shouldBe` "[CsrfToken <redacted>]",
                 show sampleSessionId `shouldNotContain` Text.unpack validSessionToken,
                 show sampleCsrfToken `shouldNotContain` Text.unpack otherSessionToken,
                 mkSessionId "short" `shouldBe` Nothing,
                 mkSessionId (Text.replicate 31 "a" <> "/") `shouldBe` Nothing,
                 mkCsrfToken "0123456789abcdef0123456789abcde=" `shouldBe` Nothing,
                 mkCsrfToken "0123456789abcdef0123456789abcde/" `shouldBe` Nothing,
                 mkSessionId "0123456789abcdef0123456789abcde_" `shouldSatisfy` (/= Nothing),
                 mkSessionId "ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ" `shouldSatisfy` (/= Nothing)
               ]
        )

  describe "SessionCookiePolicy" $ do
    it "renders host-only secure session cookies without putting CSRF state in the browser" $ do
      let cookieName = required "cookie name" (mkSessionCookieName "__Host-account")
          policy = SessionCookiePolicy {sessionCookieName = cookieName, sessionCookieMaxAgeSeconds = 900}
      expectAll
        ( (sessionCookieNameText cookieName `shouldBe` "__Host-account")
            :| [ sessionCookieName defaultSessionCookiePolicy `shouldBe` required "default cookie name" (mkSessionCookieName "__Host-harch-session"),
                 sessionCookieMaxAgeSeconds defaultSessionCookiePolicy `shouldBe` 28800,
                 policy /= defaultSessionCookiePolicy `shouldBe` True,
                 cookieName /= required "default cookie name" (mkSessionCookieName "__Host-harch-session") `shouldBe` True,
                 show cookieName `shouldBe` "SessionCookieName \"__Host-account\"",
                 show [cookieName] `shouldBe` "[SessionCookieName \"__Host-account\"]",
                 show policy `shouldBe` "SessionCookiePolicy {sessionCookieName = SessionCookieName \"__Host-account\", sessionCookieMaxAgeSeconds = 900}",
                 show [policy] `shouldBe` "[SessionCookiePolicy {sessionCookieName = SessionCookieName \"__Host-account\", sessionCookieMaxAgeSeconds = 900}]",
                 renderSessionCookie policy sampleSessionId
                   `shouldBe` "__Host-account=0123456789abcdef0123456789abcdef; Path=/; Max-Age=900; HttpOnly; Secure; SameSite=Strict",
                 mkSessionCookieName "" `shouldBe` Nothing,
                 mkSessionCookieName "account session" `shouldBe` Nothing,
                 mkSessionCookieName "account;session" `shouldBe` Nothing
               ]
        )

    it "extracts one configured opaque session cookie without choosing a duplicate" $ do
      let cookieName = required "cookie name" (mkSessionCookieName "__Host-admission")
          cookieHeaders values = [(Http.hCookie, TextEncoding.encodeUtf8 values)]
      expectAll
        ( ( extractSessionCookieId cookieName (cookieHeaders "other=value; __Host-admission=0123456789abcdef0123456789abcdef")
              `shouldBe` SessionCookieFound sampleSessionId
          )
            :| [ extractSessionCookieId cookieName [] `shouldBe` SessionCookieMissing,
                 extractSessionCookieId cookieName (cookieHeaders "__Host-admission=short") `shouldBe` SessionCookieMalformed,
                 extractSessionCookieId cookieName (cookieHeaders "__Host-admission=0123456789abcdef0123456789abcdef; __Host-admission=abcdef0123456789abcdef0123456789") `shouldBe` SessionCookieAmbiguous,
                 extractSessionCookieId cookieName [(Http.hCookie, "__Host-admission=0123456789abcdef0123456789abcdef"), (Http.hCookie, "__Host-admission=abcdef0123456789abcdef0123456789")] `shouldBe` SessionCookieAmbiguous,
                 extractSessionCookieId cookieName (cookieHeaders "__Host-admission") `shouldBe` SessionCookieMalformed,
                 extractSessionCookieId cookieName [(Http.hCookie, "__Host-admission=" <> ByteString.singleton 255)] `shouldBe` SessionCookieMalformed,
                 SessionCookieMissing == SessionCookieMissing `shouldBe` True,
                 SessionCookieMalformed == SessionCookieMalformed `shouldBe` True,
                 SessionCookieAmbiguous == SessionCookieAmbiguous `shouldBe` True,
                 SessionCookieFound sampleSessionId == SessionCookieFound sampleSessionId `shouldBe` True,
                 all (\left -> all (\right -> (left == right) /= (left /= right)) [SessionCookieMissing, SessionCookieMalformed, SessionCookieAmbiguous, SessionCookieFound sampleSessionId]) [SessionCookieMissing, SessionCookieMalformed, SessionCookieAmbiguous, SessionCookieFound sampleSessionId] `shouldBe` True,
                 show SessionCookieMissing `shouldBe` "SessionCookieMissing",
                 show SessionCookieMalformed `shouldBe` "SessionCookieMalformed",
                 show SessionCookieAmbiguous `shouldBe` "SessionCookieAmbiguous",
                 show (extractSessionCookieId cookieName (cookieHeaders "__Host-admission=0123456789abcdef0123456789abcdef")) `shouldBe` "SessionCookieFound <redacted>",
                 showList [SessionCookieMissing, SessionCookieMalformed, SessionCookieAmbiguous, SessionCookieFound sampleSessionId] "" `shouldBe` "[SessionCookieMissing,SessionCookieMalformed,SessionCookieAmbiguous,SessionCookieFound <redacted>]"
               ]
        )

  describe "validateSession" $ do
    it "distinguishes missing, expired, and active server-side session state" $ do
      expectAll
        ( (validateSession 100 Nothing `shouldBe` (MissingSession :: SessionValidation Text))
            :| [ validateSession 200 (Just sampleSession) `shouldBe` ExpiredSession,
                 validateSession 199 (Just sampleSession) `shouldBe` ActiveSession sampleSession,
                 MissingSession /= (ExpiredSession :: SessionValidation Text) `shouldBe` True,
                 sessionId sampleSession `shouldBe` sampleSessionId,
                 sessionPrincipal sampleSession `shouldBe` "account-123",
                 sessionIssuedAtNanoseconds sampleSession `shouldBe` 100,
                 sessionExpiresAtNanoseconds sampleSession `shouldBe` 200,
                 sampleSession /= sampleSession {sessionExpiresAtNanoseconds = 201} `shouldBe` True,
                 show sampleSession `shouldContain` "sessionPrincipal = \"account-123\"",
                 show sampleSession `shouldNotContain` Text.unpack validSessionToken,
                 show sampleSession `shouldNotContain` Text.unpack otherSessionToken,
                 show [sampleSession] `shouldContain` "OpaqueSession",
                 show (ActiveSession sampleSession) `shouldContain` "ActiveSession",
                 show [MissingSession, ExpiredSession :: SessionValidation Text] `shouldBe` "[MissingSession,ExpiredSession]"
               ]
        )

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
      expectAll
        ( (validateCsrfToken sampleCsrfToken sampleCsrfToken `shouldBe` True)
            :| [validateCsrfToken sampleCsrfToken (required "different csrf token" (mkCsrfToken validSessionToken)) `shouldBe` False]
        )

    it "constructs only positive backend-approved cookie lifetimes" $ do
      expectAll
        ( (fmap Csrf.csrfCookieMaxAgeSeconds (Csrf.mkCsrfCookieMaxAgeSeconds 1) `shouldBe` Just 1)
            :| [Csrf.mkCsrfCookieMaxAgeSeconds 0 `shouldBe` Nothing]
        )

    it "renders only the hashed, domain-separated binding evidence for durable stores" $ do
      let firstBinding = Csrf.csrfBindingFromCanonicalBytes "account:session-a"
          secondBinding = Csrf.csrfBindingFromCanonicalBytes "admission:session-a"
          firstDigest = Csrf.csrfBindingDigest firstBinding
      expectAll
        ( (Csrf.csrfBindingDigestText firstDigest `shouldSatisfy` (not . Text.null))
            :| [ Csrf.csrfBindingDigest firstBinding `shouldNotBe` Csrf.csrfBindingDigest secondBinding,
                 show firstDigest `shouldBe` "CsrfBindingDigest <redacted>"
               ]
        )

  describe "signed CSRF protection" $ do
    it "validates bounded key configuration without rendering signing material" $ do
      let keyId = required "CSRF key ID" (Csrf.mkCsrfKeyId "current")
          otherKeyId = required "other CSRF key ID" (Csrf.mkCsrfKeyId "previous")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      expectAll
        ( (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []) `shouldSatisfy` isJust)
            :| [ Csrf.mkCsrfKeyId "" `shouldBe` Nothing,
                 Csrf.mkCsrfKeyId "key with space" `shouldBe` Nothing,
                 Csrf.mkCsrfKeyId (Text.replicate 33 "a") `shouldBe` Nothing,
                 Csrf.mkCsrfSigningKey "not-base64url" `shouldBe` Nothing,
                 Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA" `shouldBe` Nothing,
                 Csrf.mkSignedCsrfKeyring otherKeyId ((keyId, signingKey) :| []) `shouldSatisfy` isNothing,
                 Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| [(keyId, signingKey)]) `shouldSatisfy` isNothing,
                 show signingKey `shouldBe` "CsrfSigningKey <redacted>"
               ]
        )

    it "requires positive signed-token lifetime and clock-skew policy" $ do
      expectAll
        ( (Csrf.mkSignedCsrfPolicy 1 1 `shouldSatisfy` (/= Nothing))
            :| [ Csrf.mkSignedCsrfPolicy 0 1 `shouldBe` Nothing,
                 Csrf.mkSignedCsrfPolicy 1 0 `shouldBe` Nothing
               ]
        )

    it "binds a signed token to current application grants without exposing its value" $ do
      currentTime <- newIORef 1000000000
      let keyId = required "CSRF key ID" (Csrf.mkCsrfKeyId "current")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          keyring = required "CSRF key ring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))
          sessionBinding = Csrf.csrfBindingFromCanonicalBytes "account-session:session-1"
          activeExpiry = 4000000000
          protection =
            Csrf.signedCsrfProtection
              keyring
              Csrf.defaultSignedCsrfPolicy
              (readIORef currentTime)
              (\isActive -> pure (if isActive then Csrf.BoundCsrfBinding sessionBinding activeExpiry else Csrf.AnonymousCsrfBinding))
      issued <- Csrf.issueCsrfToken protection True
      case issued of
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected a signed CSRF token"
        Csrf.CsrfTokenIssued token cookieMaxAge -> do
          Csrf.csrfCookieMaxAgeSeconds cookieMaxAge `shouldBe` 3
          Csrf.verifyCsrfToken protection True token `shouldReturn` Csrf.CsrfVerified
          retainedPageSecurity <- Csrf.preparePageSecurity protection (Just token) True
          case retainedPageSecurity of
            Left Csrf.CsrfPageProtectionUnavailable -> expectationFailure "expected a valid token to be retained"
            Right pageSecurity -> do
              Csrf.pageCsrfValue (Csrf.pageSecurityCsrf pageSecurity) `shouldBe` token
              Csrf.pageCsrfCookieDisposition (Csrf.pageSecurityCsrf pageSecurity) `shouldBe` Csrf.RetainCsrfCookie
          Csrf.verifyCsrfToken protection False token `shouldReturn` Csrf.CsrfRejected
          replacementPageSecurity <- Csrf.preparePageSecurity protection (Just token) False
          case replacementPageSecurity of
            Left Csrf.CsrfPageProtectionUnavailable -> expectationFailure "expected a changed binding to receive a replacement token"
            Right pageSecurity -> do
              Csrf.pageCsrfValue (Csrf.pageSecurityCsrf pageSecurity) `shouldNotBe` token
              Csrf.pageCsrfCookieDisposition (Csrf.pageSecurityCsrf pageSecurity) `shouldBe` Csrf.SetCsrfCookie
          Csrf.verifyCsrfToken protection True (required "altered CSRF token" (mkCsrfToken (csrfTokenText token <> "A"))) `shouldReturn` Csrf.CsrfRejected
          writeIORef currentTime 4060000000
          Csrf.verifyCsrfToken protection True token `shouldReturn` Csrf.CsrfRejected
          show token `shouldBe` "CsrfToken <redacted>"

    it "preserves backend unavailability rather than issuing an anonymous substitute" $ do
      let keyId = required "CSRF key ID" (Csrf.mkCsrfKeyId "current")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          keyring = required "CSRF key ring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))
          unavailableProtection =
            Csrf.signedCsrfProtection
              keyring
              Csrf.defaultSignedCsrfPolicy
              (pure 1000000000)
              (const (pure Csrf.CsrfBindingUnavailable))
      Csrf.issueCsrfToken unavailableProtection () `shouldReturn` Csrf.CsrfProtectionUnavailable

    it "rejects malformed, unavailable, stale, and structurally invalid signed-token states" $ do
      currentTime <- newIORef 100
      bindingResolution <- newIORef (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 1000)
      let currentKeyId = required "current CSRF key ID" (Csrf.mkCsrfKeyId "current")
          otherKeyId = required "other CSRF key ID" (Csrf.mkCsrfKeyId "other")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          otherSigningKey = required "other CSRF signing key" (Csrf.mkCsrfSigningKey "MTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTE")
          keyring = required "CSRF key ring" (Csrf.mkSignedCsrfKeyring currentKeyId ((currentKeyId, signingKey) :| []))
          otherKeyring = required "other CSRF key ring" (Csrf.mkSignedCsrfKeyring otherKeyId ((otherKeyId, otherSigningKey) :| []))
          policy = required "CSRF policy" (Csrf.mkSignedCsrfPolicy 1000 1)
          protection = Csrf.signedCsrfProtection keyring policy (readIORef currentTime) (const (readIORef bindingResolution))
          unavailableIssuer = Csrf.CsrfProtection {Csrf.issueCsrfToken = const (pure Csrf.CsrfProtectionUnavailable), Csrf.verifyCsrfToken = \_ _ -> pure Csrf.CsrfRejected}
          tokenFromBytes bytes = required "valid transport token" (mkCsrfToken (TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded bytes)))
          malformedTokens =
            [ required "bad base64 token" (mkCsrfToken (Text.replicate 33 "A")),
              tokenFromBytes (ByteString.replicate 150 0),
              tokenFromBytes (ByteString.replicate 24 0),
              tokenFromBytes (signedTokenBytes "HCS1" (ByteString.pack [255])),
              tokenFromBytes (signedTokenBytes "BAD1" "current"),
              required "oversize text token" (mkCsrfToken (Text.replicate 257 "A"))
            ]
      Csrf.preparePageSecurity unavailableIssuer Nothing () >>= \case
        Left Csrf.CsrfPageProtectionUnavailable -> pure ()
        Right _ -> expectationFailure "expected unavailable page CSRF preparation"
      issued <- Csrf.issueCsrfToken protection ()
      case issued of
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected a signed CSRF token"
        Csrf.CsrfTokenIssued token _ -> do
          let alteredText = if Text.last (csrfTokenText token) == 'A' then Text.init (csrfTokenText token) <> "B" else Text.init (csrfTokenText token) <> "A"
              alteredToken = required "same-length altered token" (mkCsrfToken alteredText)
              tamperedMacToken = required "MAC-tampered CSRF token" (mkCsrfToken (tamperSignedTokenMac token))
              unknownKeyProtection = Csrf.signedCsrfProtection otherKeyring policy (readIORef currentTime) (const (readIORef bindingResolution))
          Csrf.verifyCsrfToken protection () alteredToken `shouldReturn` Csrf.CsrfRejected
          Csrf.verifyCsrfToken protection () tamperedMacToken `shouldReturn` Csrf.CsrfRejected
          Csrf.verifyCsrfToken unknownKeyProtection () token `shouldReturn` Csrf.CsrfRejected
          mapM_ (\malformedToken -> Csrf.verifyCsrfToken protection () malformedToken `shouldReturn` Csrf.CsrfRejected) malformedTokens
          writeIORef bindingResolution Csrf.CsrfBindingUnavailable
          Csrf.verifyCsrfToken protection () token `shouldReturn` Csrf.CsrfVerificationUnavailable
          writeIORef bindingResolution (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 100)
          Csrf.verifyCsrfToken protection () token `shouldReturn` Csrf.CsrfRejected

    it "rejects otherwise-authentic tokens at every binding and clock boundary" $ do
      currentTime <- newIORef 100
      bindingResolution <- newIORef (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 2000)
      let keyId = required "CSRF key ID" (Csrf.mkCsrfKeyId "current")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          keyring = required "CSRF key ring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))
          policy = required "CSRF policy" (Csrf.mkSignedCsrfPolicy 1000 1)
          protection = Csrf.signedCsrfProtection keyring policy (readIORef currentTime) (const (readIORef bindingResolution))
          anonymousProtection = Csrf.signedCsrfProtection keyring policy (readIORef currentTime) (const (pure Csrf.AnonymousCsrfBinding))
      issued <- Csrf.issueCsrfToken protection ()
      case issued of
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected a signed CSRF token"
        Csrf.CsrfTokenIssued token _ -> do
          writeIORef bindingResolution (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 900)
          Csrf.verifyCsrfToken protection () token `shouldReturn` Csrf.CsrfRejected
          writeIORef bindingResolution (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 2000)
          writeIORef currentTime 0
          Csrf.verifyCsrfToken protection () token `shouldReturn` Csrf.CsrfRejected
      writeIORef currentTime 100
      anonymousIssued <- Csrf.issueCsrfToken anonymousProtection ()
      case anonymousIssued of
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected an anonymous signed CSRF token"
        Csrf.CsrfTokenIssued token _ -> do
          writeIORef currentTime 1200
          Csrf.verifyCsrfToken anonymousProtection () token `shouldReturn` Csrf.CsrfRejected
          writeIORef currentTime (maxBound :: Time.UnixTimeNanoseconds)
          Csrf.verifyCsrfToken anonymousProtection () token `shouldReturn` Csrf.CsrfVerificationUnavailable
      writeIORef currentTime 100
      writeIORef bindingResolution (Csrf.BoundCsrfBinding (Csrf.csrfBindingFromCanonicalBytes "session:one") 100)
      Csrf.issueCsrfToken protection () `shouldReturn` Csrf.CsrfProtectionUnavailable

    it "handles clock arithmetic bounds without accepting an expired token" $ do
      let keyId = required "CSRF key ID" (Csrf.mkCsrfKeyId "current")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          keyring = required "CSRF key ring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))
          binding = Csrf.csrfBindingFromCanonicalBytes "session:one"
          maximumTime = maxBound :: Time.UnixTimeNanoseconds
          largeSkewPolicy = required "large skew policy" (Csrf.mkSignedCsrfPolicy 1 maxBound)
          overflowPolicy = required "overflow policy" (Csrf.mkSignedCsrfPolicy maxBound 1)
          boundedProtection = Csrf.signedCsrfProtection keyring largeSkewPolicy (pure 100) (const (pure (Csrf.BoundCsrfBinding binding maximumTime)))
          overflowProtection = Csrf.signedCsrfProtection keyring overflowPolicy (pure maximumTime) (const (pure Csrf.AnonymousCsrfBinding))
      issued <- Csrf.issueCsrfToken boundedProtection ()
      case issued of
        Csrf.CsrfProtectionUnavailable -> expectationFailure "expected an unexpired bound token"
        Csrf.CsrfTokenIssued token _ -> Csrf.verifyCsrfToken boundedProtection () token `shouldReturn` Csrf.CsrfVerified
      Csrf.issueCsrfToken overflowProtection () `shouldReturn` Csrf.CsrfProtectionUnavailable

    it "keeps every public CSRF state typed, bounded, and redacted" $ do
      let currentKeyId = required "current CSRF key ID" (Csrf.mkCsrfKeyId "current")
          previousKeyId = required "previous CSRF key ID" (Csrf.mkCsrfKeyId "previous")
          signingKey = required "CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          previousSigningKey = required "previous CSRF signing key" (Csrf.mkCsrfSigningKey "MTExMTExMTExMTExMTExMTExMTExMTExMTExMTExMTE")
          cookieLifetime = required "CSRF cookie lifetime" (Csrf.mkCsrfCookieMaxAgeSeconds 42)
          policy = required "CSRF policy" (Csrf.mkSignedCsrfPolicy 42 7)
          otherPolicy = required "other CSRF policy" (Csrf.mkSignedCsrfPolicy 43 7)
          keyring = required "CSRF keyring" (Csrf.mkSignedCsrfKeyring currentKeyId ((currentKeyId, signingKey) :| [(previousKeyId, previousSigningKey)]))
          otherKeyring = required "other CSRF keyring" (Csrf.mkSignedCsrfKeyring previousKeyId ((previousKeyId, previousSigningKey) :| [(currentKeyId, signingKey)]))
          differentActiveSigningKeyring = required "CSRF keyring with a different active signing key" (Csrf.mkSignedCsrfKeyring currentKeyId ((currentKeyId, previousSigningKey) :| [(previousKeyId, previousSigningKey)]))
          differentVerificationKeyring = required "CSRF keyring with a different verification set" (Csrf.mkSignedCsrfKeyring currentKeyId ((currentKeyId, signingKey) :| []))
          firstBinding = Csrf.csrfBindingFromCanonicalBytes "account:session-1"
          secondBinding = Csrf.csrfBindingFromCanonicalBytes "admission:session-1"
          firstPageCsrf = Csrf.mkPageCsrf sampleCsrfToken "account:session-1"
          retainedPageCsrf =
            firstPageCsrf
              { Csrf.pageCsrfCookieDisposition = Csrf.RetainCsrfCookie,
                Csrf.pageCsrfCookieMaxAge = cookieLifetime
              }
          unavailablePreparation = Csrf.preparePageSecurity Csrf.csrfProtectionUnavailable (Just sampleCsrfToken) ()
      nonce <- Document.generateRuntimeNonce
      let pageSecurity = Csrf.mkPageSecurity nonce retainedPageCsrf
      generatedSigningKey <- Csrf.generateCsrfSigningKey
      unavailablePreparation >>= \case
        Left Csrf.CsrfPageProtectionUnavailable -> pure ()
        Right _ -> expectationFailure "expected unavailable page CSRF preparation"
      Csrf.issueCsrfToken Csrf.csrfProtectionUnavailable () `shouldReturn` Csrf.CsrfProtectionUnavailable
      Csrf.verifyCsrfToken Csrf.csrfProtectionUnavailable () sampleCsrfToken `shouldReturn` Csrf.CsrfVerificationUnavailable
      expectAll
        ( (Csrf.csrfCookieMaxAgeSeconds cookieLifetime `shouldBe` 42)
            :| [ cookieLifetime < Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` True,
                 cookieLifetime <= Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` True,
                 Csrf.defaultCsrfCookieMaxAgeSeconds > cookieLifetime `shouldBe` True,
                 Csrf.defaultCsrfCookieMaxAgeSeconds >= cookieLifetime `shouldBe` True,
                 min cookieLifetime Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` cookieLifetime,
                 max cookieLifetime Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` Csrf.defaultCsrfCookieMaxAgeSeconds,
                 cookieLifetime /= Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` True,
                 show cookieLifetime `shouldBe` "CsrfCookieMaxAgeSeconds 42",
                 show [cookieLifetime, Csrf.defaultCsrfCookieMaxAgeSeconds] `shouldBe` "[CsrfCookieMaxAgeSeconds 42,CsrfCookieMaxAgeSeconds 3600]",
                 compare cookieLifetime Csrf.defaultCsrfCookieMaxAgeSeconds `shouldBe` LT,
                 currentKeyId < previousKeyId `shouldBe` True,
                 currentKeyId <= previousKeyId `shouldBe` True,
                 previousKeyId > currentKeyId `shouldBe` True,
                 previousKeyId >= currentKeyId `shouldBe` True,
                 min currentKeyId previousKeyId `shouldBe` currentKeyId,
                 max currentKeyId previousKeyId `shouldBe` previousKeyId,
                 currentKeyId == currentKeyId `shouldBe` True,
                 currentKeyId /= previousKeyId `shouldBe` True,
                 show [currentKeyId, previousKeyId] `shouldBe` "[CsrfKeyId \"current\",CsrfKeyId \"previous\"]",
                 compare currentKeyId previousKeyId `shouldBe` LT,
                 signingKey == signingKey `shouldBe` True,
                 signingKey /= previousSigningKey `shouldBe` True,
                 show signingKey `shouldBe` "CsrfSigningKey <redacted>",
                 show generatedSigningKey `shouldBe` "CsrfSigningKey <redacted>",
                 generatedSigningKey == generatedSigningKey `shouldBe` True,
                 show [signingKey, generatedSigningKey] `shouldBe` "[CsrfSigningKey <redacted>,CsrfSigningKey <redacted>]",
                 show keyring `shouldBe` "SignedCsrfKeyring {signedCsrfActiveKey = CsrfKeyId \"current\", signedCsrfVerificationKeys = <redacted>}",
                 show [keyring] `shouldBe` "[SignedCsrfKeyring {signedCsrfActiveKey = CsrfKeyId \"current\", signedCsrfVerificationKeys = <redacted>}]",
                 keyring /= otherKeyring `shouldBe` True,
                 keyring /= differentActiveSigningKeyring `shouldBe` True,
                 keyring /= differentVerificationKeyring `shouldBe` True,
                 policy == policy `shouldBe` True,
                 policy /= otherPolicy `shouldBe` True,
                 show policy `shouldBe` "SignedCsrfPolicy {signedCsrfAnonymousLifetimeNanoseconds = 42, signedCsrfClockSkewNanoseconds = 7}",
                 show [policy] `shouldBe` "[SignedCsrfPolicy {signedCsrfAnonymousLifetimeNanoseconds = 42, signedCsrfClockSkewNanoseconds = 7}]",
                 Csrf.csrfBindingDigest firstBinding == Csrf.csrfBindingDigest firstBinding `shouldBe` True,
                 Csrf.csrfBindingDigest firstBinding /= Csrf.csrfBindingDigest secondBinding `shouldBe` True,
                 show firstBinding `shouldBe` "CsrfBinding <redacted>",
                 show [Csrf.csrfBindingDigest firstBinding, Csrf.csrfBindingDigest secondBinding] `shouldBe` "[CsrfBindingDigest <redacted>,CsrfBindingDigest <redacted>]",
                 show [firstBinding, secondBinding] `shouldBe` "[CsrfBinding <redacted>,CsrfBinding <redacted>]",
                 showList [firstBinding, secondBinding] "" `shouldBe` "[CsrfBinding <redacted>,CsrfBinding <redacted>]",
                 Csrf.CsrfTokenIssued sampleCsrfToken cookieLifetime == Csrf.CsrfTokenIssued sampleCsrfToken cookieLifetime `shouldBe` True,
                 Csrf.CsrfTokenIssued sampleCsrfToken cookieLifetime /= Csrf.CsrfProtectionUnavailable `shouldBe` True,
                 show (Csrf.CsrfTokenIssued sampleCsrfToken cookieLifetime) `shouldBe` "CsrfTokenIssued CsrfToken <redacted> (CsrfCookieMaxAgeSeconds 42)",
                 show Csrf.CsrfProtectionUnavailable `shouldBe` "CsrfProtectionUnavailable",
                 show [Csrf.CsrfTokenIssued sampleCsrfToken cookieLifetime, Csrf.CsrfProtectionUnavailable] `shouldBe` "[CsrfTokenIssued CsrfToken <redacted> (CsrfCookieMaxAgeSeconds 42),CsrfProtectionUnavailable]",
                 show Csrf.CsrfPageProtectionUnavailable `shouldBe` "CsrfPageProtectionUnavailable",
                 show [Csrf.CsrfPageProtectionUnavailable] `shouldBe` "[CsrfPageProtectionUnavailable]",
                 show Csrf.CsrfVerified `shouldBe` "CsrfVerified",
                 show Csrf.CsrfRejected `shouldBe` "CsrfRejected",
                 show Csrf.CsrfVerificationUnavailable `shouldBe` "CsrfVerificationUnavailable",
                 show [Csrf.CsrfVerified, Csrf.CsrfRejected, Csrf.CsrfVerificationUnavailable] `shouldBe` "[CsrfVerified,CsrfRejected,CsrfVerificationUnavailable]",
                 all (\left -> all (\right -> (left == right) /= (left /= right)) [Csrf.CsrfVerified, Csrf.CsrfRejected, Csrf.CsrfVerificationUnavailable]) [Csrf.CsrfVerified, Csrf.CsrfRejected, Csrf.CsrfVerificationUnavailable] `shouldBe` True,
                 Csrf.SetCsrfCookie /= Csrf.RetainCsrfCookie `shouldBe` True,
                 show Csrf.SetCsrfCookie `shouldBe` "SetCsrfCookie",
                 show Csrf.RetainCsrfCookie `shouldBe` "RetainCsrfCookie",
                 show [Csrf.SetCsrfCookie, Csrf.RetainCsrfCookie] `shouldBe` "[SetCsrfCookie,RetainCsrfCookie]",
                 Csrf.pageCsrfValue firstPageCsrf `shouldBe` sampleCsrfToken,
                 Csrf.csrfBindingDigest (Csrf.pageCsrfBinding firstPageCsrf) `shouldBe` Csrf.csrfBindingDigest firstBinding,
                 Csrf.pageCsrfCookieDisposition retainedPageCsrf `shouldBe` Csrf.RetainCsrfCookie,
                 Csrf.pageCsrfCookieMaxAge retainedPageCsrf `shouldBe` cookieLifetime,
                 firstPageCsrf /= retainedPageCsrf `shouldBe` True,
                 show firstPageCsrf `shouldBe` "PageCsrf <redacted>",
                 show [firstPageCsrf, retainedPageCsrf] `shouldBe` "[PageCsrf <redacted>,PageCsrf <redacted>]",
                 Csrf.pageSecurityCsrf pageSecurity `shouldBe` retainedPageCsrf,
                 Document.runtimeNonceValue (Csrf.pageSecurityRuntimeNonce pageSecurity) `shouldBe` Document.runtimeNonceValue nonce,
                 show pageSecurity `shouldBe` "PageSecurity <redacted>",
                 show [pageSecurity] `shouldBe` "[PageSecurity <redacted>]",
                 showList [pageSecurity] "" `shouldBe` "[PageSecurity <redacted>]",
                 Csrf.csrfClearCookieHeader `shouldBe` ("Set-Cookie", "__Host-harch-csrf=; Path=/; Max-Age=0; Secure; HttpOnly; SameSite=Strict")
               ]
        )

  describe "SafeReturnPath" $ do
    it "keeps navigation same-origin and rejects redirect, control-character, and backslash escapes" $ do
      let returnPath = required "safe return path" (mkSafeReturnPath "/account/settings?tab=security")
      expectAll
        ( (renderSafeReturnPath returnPath `shouldBe` "/account/settings?tab=security")
            :| [ returnPath /= required "other safe return path" (mkSafeReturnPath "/") `shouldBe` True,
                 -- 'deriving' only writes '=='; GHC's HPC instrumentation
                 -- attributes the same-value '==' path to its own box,
                 -- separate from the different-value path above. Comparing
                 -- two independently-parsed-but-equal values (rather than a
                 -- bare self-comparison) exercises it without proving
                 -- nothing.
                 mkSafeReturnPath "/account/settings?tab=security" == Just returnPath `shouldBe` True,
                 show returnPath `shouldBe` "SafeReturnPath \"/account/settings?tab=security\"",
                 show [returnPath] `shouldBe` "[SafeReturnPath \"/account/settings?tab=security\"]",
                 mkSafeReturnPath "https://attacker.test" `shouldBe` Nothing,
                 mkSafeReturnPath "//attacker.test" `shouldBe` Nothing,
                 mkSafeReturnPath "/\\attacker.test" `shouldBe` Nothing,
                 mkSafeReturnPath "/account\r\nLocation: https://attacker.test" `shouldBe` Nothing,
                 mkSafeReturnPath "/account\0" `shouldBe` Nothing
               ]
        )
