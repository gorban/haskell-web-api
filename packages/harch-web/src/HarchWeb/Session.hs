{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Session
  ( OpaqueSession (..),
    SafeReturnPath,
    SessionCookieName,
    SessionCookieExtraction (..),
    SessionCookiePolicy (..),
    SessionId,
    SessionLookup (..),
    SessionValidation (..),
    defaultSessionCookiePolicy,
    extractSessionCookieId,
    generateSessionId,
    mkSafeReturnPath,
    mkSessionCookieName,
    mkSessionId,
    renderSafeReturnPath,
    renderSessionCookie,
    sessionCookieNameText,
    sessionIdText,
    validateSession,
  )
where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64.URL qualified as Base64Url
import Data.Char (isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http

newtype SessionId = SessionId Text
  deriving (Eq)

-- | Redacted: a session id is a bearer credential, and 'Show' is reachable
-- from ordinary diagnostics (an uncaught 'error', a failing @shouldBe@, a
-- record-showing log line) that must never print it in the clear.
instance Show SessionId where
  show _ = "SessionId <redacted>"

newtype SessionCookieName = SessionCookieName Text
  deriving (Eq, Show)

-- | The only outcomes of parsing an application-selected opaque-session
-- cookie.  A duplicate or malformed matching cookie is not resolved by
-- browser/header order; callers can fail its guarded route closed without
-- making a raw bearer value observable.
data SessionCookieExtraction
  = SessionCookieMissing
  | SessionCookieMalformed
  | SessionCookieAmbiguous
  | SessionCookieFound SessionId
  deriving (Eq)

instance Show SessionCookieExtraction where
  show extraction =
    case extraction of
      SessionCookieMissing -> "SessionCookieMissing"
      SessionCookieMalformed -> "SessionCookieMalformed"
      SessionCookieAmbiguous -> "SessionCookieAmbiguous"
      SessionCookieFound _ -> "SessionCookieFound <redacted>"

newtype SafeReturnPath = SafeReturnPath Text
  deriving (Eq, Show)

-- | Server-side session state. The cookie contains only 'sessionId'; the
-- principal and any CSRF material never appear in it. AHI-4C keeps CSRF
-- binding in 'HarchWeb.Csrf', so a durable session records only grant facts.
data OpaqueSession principal = OpaqueSession
  { sessionId :: SessionId,
    sessionPrincipal :: principal,
    sessionIssuedAtNanoseconds :: UnixTimeNanoseconds,
    sessionExpiresAtNanoseconds :: UnixTimeNanoseconds
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

mkSessionId :: Text -> Maybe SessionId
mkSessionId token = SessionId <$> opaqueTokenText token

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

-- | Extract exactly one syntactically valid session identifier from all
-- @Cookie@ headers.  Request-head limits bound the raw headers before this
-- application-independent parser runs; this function owns only name matching,
-- duplicate detection, and the opaque-session grammar.  It deliberately does
-- not select a first duplicate cookie.
extractSessionCookieId :: SessionCookieName -> Http.RequestHeaders -> SessionCookieExtraction
extractSessionCookieId (SessionCookieName cookieName) headers =
  case traverse matchingCookieValue cookieFragments of
    Left () -> SessionCookieMalformed
    Right matchingValues ->
      case concat matchingValues of
        [] -> SessionCookieMissing
        [value] ->
          case TextEncoding.decodeUtf8' value of
            Left _ -> SessionCookieMalformed
            Right textValue ->
              maybe SessionCookieMalformed SessionCookieFound (mkSessionId textValue)
        _ -> SessionCookieAmbiguous
  where
    encodedCookieName = TextEncoding.encodeUtf8 cookieName
    cookieFragments =
      concat
        [ ByteString.split 59 rawHeader
        | (headerName, rawHeader) <- headers,
          headerName == Http.hCookie
        ]
    matchingCookieValue rawCookie =
      let strippedCookie = ByteString.dropWhile (== 32) rawCookie
          (cookieKey, cookieValueWithSeparator) = ByteString.break (== 61) strippedCookie
       in if cookieKey /= encodedCookieName
            then Right []
            else case ByteString.uncons cookieValueWithSeparator of
              Nothing -> Left ()
              Just (_, cookieValue) -> Right [cookieValue]

validateSession :: UnixTimeNanoseconds -> Maybe (OpaqueSession principal) -> SessionValidation principal
validateSession _ Nothing = MissingSession
validateSession now (Just session) =
  case now >= sessionExpiresAtNanoseconds session of
    True -> ExpiredSession
    False -> ActiveSession session

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
