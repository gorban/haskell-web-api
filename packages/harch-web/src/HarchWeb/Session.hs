{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Session
  ( CsrfToken,
    OpaqueSession (..),
    SafeReturnPath,
    SessionCookieName,
    SessionCookiePolicy (..),
    SessionId,
    SessionLookup (..),
    SessionValidation (..),
    csrfTokenText,
    defaultSessionCookiePolicy,
    generateCsrfToken,
    generateSessionId,
    mkCsrfToken,
    mkSafeReturnPath,
    mkSessionCookieName,
    mkSessionId,
    renderSafeReturnPath,
    renderSessionCookie,
    sessionCookieNameText,
    sessionIdText,
    validateCsrfToken,
    validateSession,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Security.ConstantTime (constantWorkEquals)

newtype SessionId = SessionId Text
  deriving (Eq)

-- | Redacted: a session id is a bearer credential, and 'Show' is reachable
-- from ordinary diagnostics (an uncaught 'error', a failing @shouldBe@, a
-- record-showing log line) that must never print it in the clear.
instance Show SessionId where
  show _ = "SessionId <redacted>"

newtype CsrfToken = CsrfToken Text
  deriving (Eq)

-- | Redacted for the same reason as 'SessionId': a valid CSRF token is a
-- bearer credential for the client action boundary.
instance Show CsrfToken where
  show _ = "CsrfToken <redacted>"

newtype SessionCookieName = SessionCookieName Text
  deriving (Eq, Show)

newtype SafeReturnPath = SafeReturnPath Text
  deriving (Eq, Show)

-- | Server-side session state. The cookie contains only 'sessionId'; the
-- principal and CSRF token never appear in it.
data OpaqueSession principal = OpaqueSession
  { sessionId :: SessionId,
    sessionPrincipal :: principal,
    sessionCsrfToken :: CsrfToken,
    sessionIssuedAtNanoseconds :: Word64,
    sessionExpiresAtNanoseconds :: Word64
  }
  deriving (Eq, Show)

-- | Application-owned persistence operations. Token generation is deliberately
-- injected: production callers must use a cryptographically secure generator.
data SessionLookup principal = SessionLookup
  { lookupOpaqueSession :: SessionId -> IO (Maybe (OpaqueSession principal)),
    invalidateOpaqueSession :: SessionId -> IO ()
  }

data SessionValidation principal
  = MissingSession
  | ExpiredSession
  | ActiveSession (OpaqueSession principal)
  deriving (Eq, Show)

data SessionCookiePolicy = SessionCookiePolicy
  { sessionCookieName :: SessionCookieName,
    sessionCookieMaxAgeSeconds :: Word64
  }
  deriving (Eq, Show)

defaultSessionCookiePolicy :: SessionCookiePolicy
defaultSessionCookiePolicy =
  SessionCookiePolicy
    { sessionCookieName = SessionCookieName "__Host-harch-session",
      sessionCookieMaxAgeSeconds = 28800
    }

generateSessionId :: IO SessionId
generateSessionId = SessionId <$> generateOpaqueToken

generateCsrfToken :: IO CsrfToken
generateCsrfToken = CsrfToken <$> generateOpaqueToken

mkSessionId :: Text -> Maybe SessionId
mkSessionId token = SessionId <$> opaqueTokenText token

mkCsrfToken :: Text -> Maybe CsrfToken
mkCsrfToken token = CsrfToken <$> opaqueTokenText token

mkSessionCookieName :: Text -> Maybe SessionCookieName
mkSessionCookieName name =
  case Text.null name || Text.any (not . isCookieTokenCharacter) name of
    True -> Nothing
    False -> Just (SessionCookieName name)

mkSafeReturnPath :: Text -> Maybe SafeReturnPath
mkSafeReturnPath path =
  case Text.isPrefixOf "//" path of
    True -> Nothing
    False ->
      case Text.isPrefixOf "/" path && not (Text.any isUnsafePathCharacter path) of
        True -> Just (SafeReturnPath path)
        False -> Nothing

sessionIdText :: SessionId -> Text
sessionIdText (SessionId token) = token

csrfTokenText :: CsrfToken -> Text
csrfTokenText (CsrfToken token) = token

sessionCookieNameText :: SessionCookieName -> Text
sessionCookieNameText (SessionCookieName name) = name

renderSafeReturnPath :: SafeReturnPath -> Text
renderSafeReturnPath (SafeReturnPath path) = path

renderSessionCookie :: SessionCookiePolicy -> SessionId -> Text
renderSessionCookie policy sessionToken =
  sessionCookieNameText (sessionCookieName policy)
    <> "="
    <> sessionIdText sessionToken
    <> "; Path=/; Max-Age="
    <> Text.pack (show (sessionCookieMaxAgeSeconds policy))
    <> "; HttpOnly; Secure; SameSite=Strict"

validateSession :: Word64 -> Maybe (OpaqueSession principal) -> SessionValidation principal
validateSession _ Nothing = MissingSession
validateSession now (Just session) =
  case now >= sessionExpiresAtNanoseconds session of
    True -> ExpiredSession
    False -> ActiveSession session

-- | Constant-work comparison for synchronizer tokens. Length is public; token
-- bytes are compared without an early mismatch exit.
validateCsrfToken :: CsrfToken -> CsrfToken -> Bool
validateCsrfToken expected supplied =
  constantWorkEquals
    (TextEncoding.encodeUtf8 (csrfTokenText expected))
    (TextEncoding.encodeUtf8 (csrfTokenText supplied))

opaqueTokenText :: Text -> Maybe Text
opaqueTokenText token =
  case Text.length token < 32 of
    True -> Nothing
    False ->
      case Text.all isOpaqueTokenCharacter token of
        True -> Just token
        False -> Nothing

generateOpaqueToken :: IO Text
generateOpaqueToken = TextEncoding.decodeUtf8 . Base64Url.encodeUnpadded <$> getEntropy 32

isOpaqueTokenCharacter :: Char -> Bool
isOpaqueTokenCharacter character =
  isAscii character
    && (character == '-' || character == '_' || isAsciiLower character || isAsciiUpper character || isDigit character)

isCookieTokenCharacter :: Char -> Bool
isCookieTokenCharacter character =
  isAscii character
    && (character == '-' || character == '_' || isAsciiLower character || isAsciiUpper character || isDigit character)

isUnsafePathCharacter :: Char -> Bool
isUnsafePathCharacter character = character == '\\' || character == '\r' || character == '\n' || character == '\0'
