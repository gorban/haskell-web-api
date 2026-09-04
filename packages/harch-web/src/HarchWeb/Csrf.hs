{-# LANGUAGE OverloadedStrings #-}

-- | Opaque page/action CSRF state.
--
-- AHI-4C moves page security construction before a page handler builds its
-- markup.  The framework owns the CSP nonce and the application-selected
-- protection backend owns token issuance and binding.  Neither raw value
-- belongs in a 'HarchWeb.Document.Page', request context, bootstrap hook, or
-- diagnostic rendering.
--
-- Decision (AHI-4C, 2026-09-03): extend the existing page/action response
-- lifecycle with one 'CsrfProtection' capability rather than add a CSRF
-- middleware or application-local action dispatcher.  The existing action
-- executor already owns body intake, exact-one form/cookie parsing, and the
-- point immediately before a handler.  It therefore performs mandatory
-- transport validation once, while this capability owns only issuance and
-- verification against application-selected current state.  The supplied
-- signed backend is storage-neutral; a synchronizer backend can use the same
-- capability without making Harch depend on PostgreSQL. 'CsrfToken' also
-- belongs here rather than in 'HarchWeb.Session': a native fallback receives
-- the opaque token from its page's 'PageSecurity' and uses the same
-- framework-owned host cookie/transport check as an enhanced action.
module HarchWeb.Csrf
  ( CsrfBinding,
    CsrfBindingDigest,
    CsrfBindingResolution (..),
    CsrfCookieDisposition (..),
    CsrfCookieMaxAgeSeconds,
    CsrfKeyId,
    CsrfIssuance (..),
    CsrfPagePreparationFailure (..),
    CsrfProtection (..),
    CsrfSigningKey,
    CsrfToken,
    CsrfVerification (..),
    SignedCsrfKeyring,
    SignedCsrfPolicy,
    PageCsrf,
    PageSecurity,
    csrfBindingFromCanonicalBytes,
    csrfBindingDigest,
    csrfBindingDigestText,
    csrfClearCookieHeader,
    csrfCookieMaxAgeSeconds,
    mkCsrfCookieMaxAgeSeconds,
    csrfTokenText,
    defaultCsrfCookieMaxAgeSeconds,
    defaultSignedCsrfPolicy,
    generateCsrfSigningKey,
    generateCsrfToken,
    mkCsrfKeyId,
    mkCsrfToken,
    mkSignedCsrfPolicy,
    mkCsrfSigningKey,
    mkSignedCsrfKeyring,
    mkPageCsrf,
    mkPageSecurity,
    pageCsrfBinding,
    pageCsrfCookieDisposition,
    pageCsrfCookieMaxAge,
    pageCsrfValue,
    pageSecurityCsrf,
    pageSecurityRuntimeNonce,
    samePageSecurity,
    preparePageSecurity,
    csrfProtectionUnavailable,
    signedCsrfProtection,
    validateCsrfToken,
  )
where

import Crypto.Hash (Digest, SHA256, hash)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random.Entropy (getEntropy)
import Data.Bits (shiftL, (.|.))
import Data.ByteArray (convert)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.ByteString.Builder qualified as ByteStringBuilder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (find, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Document (RuntimeNonce, generateRuntimeNonce, runtimeNonceValue)
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import Network.HTTP.Types qualified as Http

-- | The backend outcome while preparing an SSR page token. A dependency such
-- as a synchronizer-token store may be unavailable, but issuance never turns
-- an expected request into a textual exception or an implicit anonymous token.
data CsrfIssuance
  = CsrfTokenIssued CsrfToken CsrfCookieMaxAgeSeconds
  | CsrfProtectionUnavailable
  deriving (Eq, Show)

-- | A non-negative browser cookie lifetime derived by the selected token
-- authority. It is distinct from a token's internal expiry: the renderer
-- receives only this bounded transport value and cannot invent a longer
-- browser lifetime than the authority approved.
newtype CsrfCookieMaxAgeSeconds = CsrfCookieMaxAgeSeconds Word64
  deriving (Eq, Ord, Show)

-- | One hour is the ordinary deterministic test lifetime. Signed authorities
-- calculate a shorter value when the current application grant expires first.
defaultCsrfCookieMaxAgeSeconds :: CsrfCookieMaxAgeSeconds
defaultCsrfCookieMaxAgeSeconds = CsrfCookieMaxAgeSeconds 3600

csrfCookieMaxAgeSeconds :: CsrfCookieMaxAgeSeconds -> Word64
csrfCookieMaxAgeSeconds (CsrfCookieMaxAgeSeconds seconds) = seconds

-- | Construct a positive, backend-approved browser-cookie lifetime.  A
-- backend may not advertise a zero or negative-equivalent lifetime merely to
-- bypass the page-security transport contract.
mkCsrfCookieMaxAgeSeconds :: Word64 -> Maybe CsrfCookieMaxAgeSeconds
mkCsrfCookieMaxAgeSeconds seconds
  | seconds == 0 = Nothing
  | otherwise = Just (CsrfCookieMaxAgeSeconds seconds)

-- | Preparing an SSR page has exactly one expected failure: the selected
-- authority cannot safely issue or verify its token. A rejected existing token
-- is ordinary replacement flow; an issuance result is never an error value.
data CsrfPagePreparationFailure
  = CsrfPageProtectionUnavailable
  deriving (Show)

-- | A redacted action token. It is CSRF state rather than session state: an
-- application may select signed or durable synchronization without changing
-- the server-side session representation.
newtype CsrfToken = CsrfToken Text
  deriving (Eq)

instance Show CsrfToken where
  show _ = "CsrfToken <redacted>"

generateCsrfToken :: IO CsrfToken
generateCsrfToken = CsrfToken . TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy csrfSigningKeyBytes

mkCsrfToken :: Text -> Maybe CsrfToken
mkCsrfToken token
  | Text.length token < minCsrfTokenCharacters = Nothing
  | Text.all isCsrfTokenCharacter token = Just (CsrfToken token)
  | otherwise = Nothing

csrfTokenText :: CsrfToken -> Text
csrfTokenText (CsrfToken token) = token

-- | Constant-work comparison after the transport parser has established that
-- each side is one bounded token. Length is public, but mismatch position is
-- not used as an early exit.
validateCsrfToken :: CsrfToken -> CsrfToken -> Bool
validateCsrfToken expected supplied =
  constantWorkEquals
    (TextEncoding.encodeUtf8 (csrfTokenText expected))
    (TextEncoding.encodeUtf8 (csrfTokenText supplied))

-- | The backend outcome after the framework has already completed the exact
-- cookie/submission transport check. A rejected token is an ordinary 403
-- outcome; unavailability is a distinct 503 rail at the action boundary.
data CsrfVerification
  = CsrfVerified
  | CsrfRejected
  | CsrfVerificationUnavailable
  deriving (Eq, Show)

-- | An application-selected token authority. Harch retains mandatory
-- transport parsing and constant-work double-submit comparison; the backend
-- only issues tokens and verifies the current application binding. This makes
-- signed and durable synchronizer implementations interchangeable without a
-- second action dispatcher.
data CsrfProtection context = CsrfProtection
  { issueCsrfToken :: context -> IO CsrfIssuance,
    verifyCsrfToken :: context -> CsrfToken -> IO CsrfVerification
  }

-- | The current application authentication state as far as CSRF needs to
-- know it. Anonymous pages are deliberately a valid state; an unavailable
-- durable session or synchronizer store is not silently treated as anonymous.
data CsrfBindingResolution
  = AnonymousCsrfBinding
  | BoundCsrfBinding CsrfBinding UnixTimeNanoseconds
  | CsrfBindingUnavailable

-- | A public key identifier is non-secret but remains tightly bounded because
-- it is parsed from an untrusted cookie before selecting a verification key.
newtype CsrfKeyId = CsrfKeyId Text
  deriving (Eq, Ord, Show)

-- | Dedicated HMAC key material. It never renders in diagnostics.
newtype CsrfSigningKey = CsrfSigningKey ByteString.ByteString
  deriving (Eq)

instance Show CsrfSigningKey where
  show _ = "CsrfSigningKey <redacted>"

-- | An immutable rotation set. The active key issues tokens and every listed
-- key verifies a live overlap. Process reload, rather than per-request key
-- polling, is the default rotation boundary.
data SignedCsrfKeyring = SignedCsrfKeyring
  { signedCsrfActiveKey :: CsrfKeyId,
    signedCsrfActiveSigningKey :: CsrfSigningKey,
    signedCsrfVerificationKeys :: NonEmpty (CsrfKeyId, CsrfSigningKey)
  }

-- | Keyring equality is needed by application configuration tests and must
-- compare every rotation entry, while its 'Show' instance remains redacted.
instance Eq SignedCsrfKeyring where
  left == right =
    signedCsrfActiveKey left == signedCsrfActiveKey right
      && signedCsrfActiveSigningKey left == signedCsrfActiveSigningKey right
      && signedCsrfVerificationKeys left == signedCsrfVerificationKeys right

instance Show SignedCsrfKeyring where
  show keyring =
    "SignedCsrfKeyring {signedCsrfActiveKey = "
      <> show (signedCsrfActiveKey keyring)
      <> ", signedCsrfVerificationKeys = <redacted>}"

-- | Bounded policy for the supplied storage-neutral signed backend.
data SignedCsrfPolicy = SignedCsrfPolicy
  { signedCsrfAnonymousLifetimeNanoseconds :: Word64,
    signedCsrfClockSkewNanoseconds :: Word64
  }

instance Eq SignedCsrfPolicy where
  left == right =
    signedCsrfAnonymousLifetimeNanoseconds left == signedCsrfAnonymousLifetimeNanoseconds right
      && signedCsrfClockSkewNanoseconds left == signedCsrfClockSkewNanoseconds right

instance Show SignedCsrfPolicy where
  show policy =
    "SignedCsrfPolicy {signedCsrfAnonymousLifetimeNanoseconds = "
      <> show (signedCsrfAnonymousLifetimeNanoseconds policy)
      <> ", signedCsrfClockSkewNanoseconds = "
      <> show (signedCsrfClockSkewNanoseconds policy)
      <> "}"

-- | The supplied browser policy: anonymous pages have a one-hour token and
-- verification tolerates at most sixty seconds of clock skew.
defaultSignedCsrfPolicy :: SignedCsrfPolicy
defaultSignedCsrfPolicy =
  SignedCsrfPolicy
    { signedCsrfAnonymousLifetimeNanoseconds = 60 * 60 * 1000000000,
      signedCsrfClockSkewNanoseconds = 60 * 1000000000
    }

-- | Construct a positive token lifetime and bounded positive skew policy at
-- startup. Zero would make every anonymous page token immediately unusable or
-- silently change the advertised clock-tolerance contract.
mkSignedCsrfPolicy :: Word64 -> Word64 -> Maybe SignedCsrfPolicy
mkSignedCsrfPolicy anonymousLifetime clockSkew
  | anonymousLifetime == 0 = Nothing
  | clockSkew == 0 = Nothing
  | otherwise =
      Just
        SignedCsrfPolicy
          { signedCsrfAnonymousLifetimeNanoseconds = anonymousLifetime,
            signedCsrfClockSkewNanoseconds = clockSkew
          }

-- | Accept a compact key identifier only when it is safe to parse from a
-- cookie and has a bounded encoded representation.
mkCsrfKeyId :: Text -> Maybe CsrfKeyId
mkCsrfKeyId value
  | Text.null value = Nothing
  | Text.length value > maxCsrfKeyIdCharacters = Nothing
  | Text.all isCsrfTokenCharacter value = Just (CsrfKeyId value)
  | otherwise = Nothing

-- | Decode exactly 256 bits of dedicated HMAC material from deployment
-- configuration. JWT, session, and CSRF keys are distinct types and cannot be
-- passed here by accident without an explicit configuration conversion.
mkCsrfSigningKey :: Text -> Maybe CsrfSigningKey
mkCsrfSigningKey encodedKey = do
  key <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 encodedKey))
  if ByteString.length key == csrfSigningKeyBytes
    then Just (CsrfSigningKey key)
    else Nothing

-- | Generate dedicated signing material for a development-only or otherwise
-- explicitly ephemeral deployment. Production key rotation should instead
-- construct a ring from deployment-owned configuration with
-- 'mkCsrfSigningKey'.
generateCsrfSigningKey :: IO CsrfSigningKey
generateCsrfSigningKey = CsrfSigningKey <$> getEntropy csrfSigningKeyBytes

-- | A safe capability for protocol-only applications. It never turns a
-- missing CSRF authority into an accepting verifier: if an impossible page or
-- action reaches this application, the shared boundary reports 503.
csrfProtectionUnavailable :: CsrfProtection context
csrfProtectionUnavailable =
  CsrfProtection
    { issueCsrfToken = const (pure CsrfProtectionUnavailable),
      verifyCsrfToken = \_ _ -> pure CsrfVerificationUnavailable
    }

-- | Clear the one framework-owned host CSRF cookie after an authentication
-- grant changes. A following complete page GET prepares and sets the sole
-- replacement authority; action responses never mint a second token.
csrfClearCookieHeader :: Http.Header
csrfClearCookieHeader =
  ( "Set-Cookie",
    "__Host-harch-csrf=; Path=/; Max-Age=0; Secure; HttpOnly; SameSite=Strict"
  )

-- | Validate the immutable key ring at startup. Duplicate key IDs and an
-- active key outside the verification set are authored configuration errors,
-- not request outcomes.
mkSignedCsrfKeyring :: CsrfKeyId -> NonEmpty (CsrfKeyId, CsrfSigningKey) -> Maybe SignedCsrfKeyring
mkSignedCsrfKeyring activeKey verificationKeys
  | duplicateKeyIds verificationKeys = Nothing
  | otherwise = do
      activeSigningKey <- lookupVerificationKey activeKey verificationKeys
      pure
        SignedCsrfKeyring
          { signedCsrfActiveKey = activeKey,
            signedCsrfActiveSigningKey = activeSigningKey,
            signedCsrfVerificationKeys = verificationKeys
          }

-- | Backend-private binding evidence. Its constructor is deliberately not
-- exported: application-provided canonical grant bytes are immediately
-- digested, so neither a durable session ID nor a raw application subject is
-- retained in the token or made available to diagnostics.
newtype CsrfBindingDigest = CsrfBindingDigest ByteString.ByteString

instance Eq CsrfBindingDigest where
  CsrfBindingDigest left == CsrfBindingDigest right = left == right

instance Show CsrfBindingDigest where
  show _ = "CsrfBindingDigest <redacted>"

-- | A storage-safe base64url rendering of the already-hashed, domain-tagged
-- current grant set. It contains no raw principal or session value and is
-- provided so durable CSRF backends can persist and query the same binding
-- that Harch verifies, without reimplementing the canonicalization protocol.
csrfBindingDigestText :: CsrfBindingDigest -> Text
csrfBindingDigestText (CsrfBindingDigest digest) = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded digest)

newtype CsrfBinding = CsrfBinding CsrfBindingDigest

instance Show CsrfBinding where
  show _ = "CsrfBinding <redacted>"

-- | Binding comparison stays inside the CSRF interpreter. A binding is
-- evidence for a backend, not ordinary application data which callers may
-- compare or use to correlate requests.
sameCsrfBinding :: CsrfBinding -> CsrfBinding -> Bool
sameCsrfBinding (CsrfBinding left) (CsrfBinding right) = left == right

-- | The action token paired with the backend's opaque binding evidence.
-- | Whether rendering a complete page should replace the browser's CSRF
-- cookie or retain the verified value already shared by open tabs.
data CsrfCookieDisposition
  = SetCsrfCookie
  | RetainCsrfCookie
  deriving (Eq, Show)

data PageCsrf = PageCsrf
  { pageCsrfValue :: CsrfToken,
    pageCsrfBinding :: CsrfBinding,
    pageCsrfCookieDisposition :: CsrfCookieDisposition,
    pageCsrfCookieMaxAge :: CsrfCookieMaxAgeSeconds
  }

instance Show PageCsrf where
  show _ = "PageCsrf <redacted>"

instance Eq PageCsrf where
  left == right =
    pageCsrfValue left == pageCsrfValue right
      && sameCsrfBinding (pageCsrfBinding left) (pageCsrfBinding right)
      && pageCsrfCookieDisposition left == pageCsrfCookieDisposition right
      && pageCsrfCookieMaxAge left == pageCsrfCookieMaxAge right

-- | Security values constructed before a page handler runs.  Page code may
-- pass the opaque token to a framework action-control helper, but document
-- rendering alone consumes the nonce and cookie material.
data PageSecurity = PageSecurity
  { pageSecurityRuntimeNonce :: RuntimeNonce,
    pageSecurityCsrf :: PageCsrf
  }

instance Show PageSecurity where
  show _ = "PageSecurity <redacted>"

mkPageCsrf :: CsrfToken -> Text -> PageCsrf
mkPageCsrf csrfToken binding =
  PageCsrf
    { pageCsrfValue = csrfToken,
      pageCsrfBinding = csrfBindingFromCanonicalBytes (TextEncoding.encodeUtf8 binding),
      pageCsrfCookieDisposition = SetCsrfCookie,
      pageCsrfCookieMaxAge = defaultCsrfCookieMaxAgeSeconds
    }

mkPageSecurity :: RuntimeNonce -> PageCsrf -> PageSecurity
mkPageSecurity = PageSecurity

-- | Compare the framework-owned response capability at the two internal
-- rendering boundaries. This is deliberately a named operation rather than
-- a public 'Eq' instance: a runtime nonce is an affine capability, not
-- general application data to compare or reuse.
samePageSecurity :: PageSecurity -> PageSecurity -> Bool
samePageSecurity left right =
  runtimeNonceValue (pageSecurityRuntimeNonce left) == runtimeNonceValue (pageSecurityRuntimeNonce right)
    && pageSecurityCsrf left == pageSecurityCsrf right

-- | Build the complete security value before a page handler sees it. Token
-- issuance is allowed to fail explicitly; a page is never rendered with a
-- nonce but without a matching mandatory action token.
preparePageSecurity :: CsrfProtection context -> Maybe CsrfToken -> context -> IO (Either CsrfPagePreparationFailure PageSecurity)
preparePageSecurity protection maybeExistingToken context =
  case maybeExistingToken of
    Nothing -> issueReplacementToken
    Just existingToken -> do
      verification <- verifyCsrfToken protection context existingToken
      case verification of
        CsrfVerificationUnavailable -> pure (Left CsrfPageProtectionUnavailable)
        CsrfVerified -> preparedPageSecurity RetainCsrfCookie defaultCsrfCookieMaxAgeSeconds existingToken
        CsrfRejected -> issueReplacementToken
  where
    issueReplacementToken = do
      issuance <- issueCsrfToken protection context
      case issuance of
        CsrfTokenIssued csrfToken cookieMaxAge -> preparedPageSecurity SetCsrfCookie cookieMaxAge csrfToken
        CsrfProtectionUnavailable -> pure (Left CsrfPageProtectionUnavailable)

    preparedPageSecurity cookieDisposition cookieMaxAge csrfToken = do
      runtimeNonce <- generateRuntimeNonce
      pure
        ( Right
            ( PageSecurity
                runtimeNonce
                ( (mkPageCsrf csrfToken "backend")
                    { pageCsrfCookieDisposition = cookieDisposition,
                      pageCsrfCookieMaxAge = cookieMaxAge
                    }
                )
            )
        )

-- | Create the digest that a signed token binds to. Callers must supply a
-- canonical domain-separated encoding of their active grants; the bytes are
-- hashed immediately and never stored in a 'CsrfBinding' value as plaintext.
csrfBindingFromCanonicalBytes :: ByteString.ByteString -> CsrfBinding
csrfBindingFromCanonicalBytes = CsrfBinding . CsrfBindingDigest . sha256

csrfBindingDigest :: CsrfBinding -> CsrfBindingDigest
csrfBindingDigest (CsrfBinding digest) = digest

-- | Build the supplied signed double-submit backend. The application resolves
-- its current authentication grants on both issuance and verification; Harch
-- signs only their opaque digest and never imports a session store.
signedCsrfProtection ::
  SignedCsrfKeyring ->
  SignedCsrfPolicy ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  CsrfProtection context
signedCsrfProtection keyring policy currentTime resolveBinding =
  CsrfProtection
    { issueCsrfToken = issueSignedToken keyring policy currentTime resolveBinding,
      verifyCsrfToken = verifySignedToken keyring policy currentTime resolveBinding
    }

issueSignedToken ::
  SignedCsrfKeyring ->
  SignedCsrfPolicy ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  context ->
  IO CsrfIssuance
issueSignedToken keyring policy currentTime resolveBinding context = do
  bindingResolution <- resolveBinding context
  now <- currentTime
  case expiryForBinding policy now bindingResolution of
    Nothing -> pure CsrfProtectionUnavailable
    Just (binding, expiresAt) -> do
      nonce <- getEntropy csrfNonceBytes
      let activeKey = signedCsrfActiveKey keyring
          payload = renderSignedPayload activeKey now expiresAt nonce binding
          mac = hmacSha256 (signedCsrfActiveSigningKey keyring) payload
          tokenText = TextEncoding.decodeUtf8 (Base64Url.encodeUnpadded (payload <> mac))
      pure (CsrfTokenIssued (CsrfToken tokenText) (cookieMaxAgeUntil now expiresAt))

verifySignedToken ::
  SignedCsrfKeyring ->
  SignedCsrfPolicy ->
  IO UnixTimeNanoseconds ->
  (context -> IO CsrfBindingResolution) ->
  context ->
  CsrfToken ->
  IO CsrfVerification
verifySignedToken keyring policy currentTime resolveBinding context csrfToken =
  case parseSignedToken csrfToken of
    Nothing -> pure CsrfRejected
    Just parsedToken ->
      case lookupVerificationKey (parsedCsrfKeyId parsedToken) (signedCsrfVerificationKeys keyring) of
        Nothing -> pure CsrfRejected
        Just verificationKey -> do
          let expectedMac = hmacSha256 verificationKey (parsedCsrfPayload parsedToken)
          if not (constantWorkEquals expectedMac (parsedCsrfMac parsedToken))
            then pure CsrfRejected
            else do
              bindingResolution <- resolveBinding context
              now <- currentTime
              case bindingResolution of
                CsrfBindingUnavailable -> pure CsrfVerificationUnavailable
                BoundCsrfBinding _ expiresAt
                  | expiresAt <= now -> pure CsrfRejected
                _ ->
                  case expiryForBinding policy now bindingResolution of
                    Nothing -> pure CsrfVerificationUnavailable
                    Just (expectedBinding, bindingExpiry) ->
                      pure
                        ( verifyParsedToken
                            policy
                            now
                            bindingExpiry
                            expectedBinding
                            parsedToken
                        )

verifyParsedToken :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> CsrfBinding -> ParsedCsrfToken -> CsrfVerification
verifyParsedToken policy now bindingExpiry expectedBinding parsedToken
  | parsedCsrfExpiresAt parsedToken > bindingExpiry = CsrfRejected
  | isTooFarInFuture policy now (parsedCsrfIssuedAt parsedToken) = CsrfRejected
  | isExpired policy now (parsedCsrfExpiresAt parsedToken) = CsrfRejected
  | otherwise =
      if constantWorkEquals (csrfBindingBytes expectedBinding) (csrfBindingBytes (parsedCsrfBinding parsedToken))
        then CsrfVerified
        else CsrfRejected

expiryForBinding :: SignedCsrfPolicy -> UnixTimeNanoseconds -> CsrfBindingResolution -> Maybe (CsrfBinding, UnixTimeNanoseconds)
expiryForBinding policy now bindingResolution =
  case bindingResolution of
    AnonymousCsrfBinding -> do
      expiresAt <- addUnixTimeNanoseconds now (signedCsrfAnonymousLifetimeNanoseconds policy)
      pure (anonymousCsrfBinding, expiresAt)
    BoundCsrfBinding binding expiresAt
      | expiresAt > now -> Just (binding, expiresAt)
      | otherwise -> Nothing
    CsrfBindingUnavailable -> Nothing

data ParsedCsrfToken = ParsedCsrfToken
  { parsedCsrfKeyId :: CsrfKeyId,
    parsedCsrfIssuedAt :: UnixTimeNanoseconds,
    parsedCsrfExpiresAt :: UnixTimeNanoseconds,
    parsedCsrfBinding :: CsrfBinding,
    parsedCsrfPayload :: ByteString.ByteString,
    parsedCsrfMac :: ByteString.ByteString
  }

parseSignedToken :: CsrfToken -> Maybe ParsedCsrfToken
parseSignedToken token
  | Text.length (csrfTokenText token) > maxSignedCsrfTokenCharacters = Nothing
  | otherwise = do
      tokenBytes <- either (const Nothing) Just (Base64Url.decodeUnpadded (TextEncoding.encodeUtf8 (csrfTokenText token)))
      if ByteString.length tokenBytes > maxSignedCsrfTokenBytes
        then Nothing
        else do
          let fixedPrefixBytes = ByteString.length signedCsrfMagic + 1
              keyLength = fromIntegral (ByteString.index tokenBytes (ByteString.length signedCsrfMagic))
              payloadLength = fixedPrefixBytes + keyLength + 8 + 8 + csrfNonceBytes + csrfBindingBytesLength
              totalLength = payloadLength + csrfMacBytes
          if keyLength == 0 || ByteString.length tokenBytes /= totalLength
            then Nothing
            else do
              let (payload, mac) = ByteString.splitAt payloadLength tokenBytes
                  keyStart = fixedPrefixBytes
                  keyBytes = ByteString.take keyLength (ByteString.drop keyStart payload)
                  issuedAtOffset = keyStart + keyLength
                  expiresAtOffset = issuedAtOffset + 8
                  bindingOffset = expiresAtOffset + 8 + csrfNonceBytes
              keyIdText <- either (const Nothing) Just (TextEncoding.decodeUtf8' keyBytes)
              keyId <- mkCsrfKeyId keyIdText
              if ByteString.take (ByteString.length signedCsrfMagic) payload /= signedCsrfMagic
                then Nothing
                else
                  Just
                    ParsedCsrfToken
                      { parsedCsrfKeyId = keyId,
                        parsedCsrfIssuedAt = unixTimeNanoseconds (word64At payload issuedAtOffset),
                        parsedCsrfExpiresAt = unixTimeNanoseconds (word64At payload expiresAtOffset),
                        parsedCsrfBinding = CsrfBinding (CsrfBindingDigest (ByteString.take csrfBindingBytesLength (ByteString.drop bindingOffset payload))),
                        parsedCsrfPayload = payload,
                        parsedCsrfMac = mac
                      }

renderSignedPayload :: CsrfKeyId -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> ByteString.ByteString -> CsrfBinding -> ByteString.ByteString
renderSignedPayload keyId issuedAt expiresAt nonce binding =
  LazyByteString.toStrict
    ( ByteStringBuilder.toLazyByteString
        ( ByteStringBuilder.byteString signedCsrfMagic
            <> ByteStringBuilder.word8 (fromIntegral (ByteString.length keyIdBytes))
            <> ByteStringBuilder.byteString keyIdBytes
            <> ByteStringBuilder.word64BE (unixTimeNanosecondsValue issuedAt)
            <> ByteStringBuilder.word64BE (unixTimeNanosecondsValue expiresAt)
            <> ByteStringBuilder.byteString nonce
            <> ByteStringBuilder.byteString (csrfBindingBytes binding)
        )
    )
  where
    keyIdBytes = csrfKeyIdBytes keyId

lookupVerificationKey :: CsrfKeyId -> NonEmpty (CsrfKeyId, CsrfSigningKey) -> Maybe CsrfSigningKey
lookupVerificationKey keyId =
  fmap snd . find ((== keyId) . fst) . NonEmpty.toList

duplicateKeyIds :: NonEmpty (CsrfKeyId, CsrfSigningKey) -> Bool
duplicateKeyIds verificationKeys =
  let keyIds = sortOn id (map fst (NonEmpty.toList verificationKeys))
   in any (uncurry (==)) (zip keyIds (drop 1 keyIds))

csrfBindingBytes :: CsrfBinding -> ByteString.ByteString
csrfBindingBytes (CsrfBinding (CsrfBindingDigest value)) = value

anonymousCsrfBinding :: CsrfBinding
anonymousCsrfBinding = csrfBindingFromCanonicalBytes "harch-csrf-anonymous-v1"

csrfKeyIdBytes :: CsrfKeyId -> ByteString.ByteString
csrfKeyIdBytes (CsrfKeyId value) = TextEncoding.encodeUtf8 value

hmacSha256 :: CsrfSigningKey -> ByteString.ByteString -> ByteString.ByteString
hmacSha256 (CsrfSigningKey key) payload = convert (hmac key payload :: HMAC SHA256)

sha256 :: ByteString.ByteString -> ByteString.ByteString
sha256 = convert . (hash :: ByteString.ByteString -> Digest SHA256)

isTooFarInFuture :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> Bool
isTooFarInFuture policy now issuedAt =
  case addUnixTimeNanoseconds now (signedCsrfClockSkewNanoseconds policy) of
    Nothing -> False
    Just latestIssuedAt -> issuedAt > latestIssuedAt

isExpired :: SignedCsrfPolicy -> UnixTimeNanoseconds -> UnixTimeNanoseconds -> Bool
isExpired policy now expiresAt =
  case addUnixTimeNanoseconds expiresAt (signedCsrfClockSkewNanoseconds policy) of
    Nothing -> False
    Just latestAcceptedAt -> now > latestAcceptedAt

cookieMaxAgeUntil :: UnixTimeNanoseconds -> UnixTimeNanoseconds -> CsrfCookieMaxAgeSeconds
cookieMaxAgeUntil now expiresAt =
  CsrfCookieMaxAgeSeconds
    ((unixTimeNanosecondsValue expiresAt - unixTimeNanosecondsValue now) `div` nanosecondsPerSecond)

word64At :: ByteString.ByteString -> Int -> Word64
word64At bytes offset =
  foldl
    (\value index -> value `shiftL` 8 .|. fromIntegral (ByteString.index bytes (offset + index)))
    0
    [0 .. 7]

isCsrfTokenCharacter :: Char -> Bool
isCsrfTokenCharacter character =
  isAsciiLower character
    || isAsciiUpper character
    || isDigit character
    || character == '-'
    || character == '_'

signedCsrfMagic :: ByteString.ByteString
signedCsrfMagic = "HCS1"

maxCsrfKeyIdCharacters :: Int
maxCsrfKeyIdCharacters = 32

csrfSigningKeyBytes :: Int
csrfSigningKeyBytes = 32

csrfNonceBytes :: Int
csrfNonceBytes = 32

csrfBindingBytesLength :: Int
csrfBindingBytesLength = 32

csrfMacBytes :: Int
csrfMacBytes = 32

maxSignedCsrfTokenBytes :: Int
maxSignedCsrfTokenBytes = ByteString.length signedCsrfMagic + 1 + maxCsrfKeyIdCharacters + 8 + 8 + csrfNonceBytes + csrfBindingBytesLength + csrfMacBytes

maxSignedCsrfTokenCharacters :: Int
maxSignedCsrfTokenCharacters = 256

minCsrfTokenCharacters :: Int
minCsrfTokenCharacters = 32

nanosecondsPerSecond :: Word64
nanosecondsPerSecond = 1000000000
