-- | The shared ASCII token grammar for cookie names.
--
-- Decision record (AHI-4C, 2026-09-05): session-cookie configuration,
-- authentication-cookie configuration, and the pre-routing request-budget
-- scanner are three views of one HTTP cookie-name contract. Keeping their
-- character predicates separately had already allowed the authentication
-- policy to issue names the other two boundaries could not recognize. This
-- leaf module owns the RFC token subset used by all three; it has no request,
-- session, or authentication dependency, so sharing it does not create a
-- second cookie parser or an import cycle.
module HarchWeb.Cookie
  ( isCookieTokenByte,
    isCookieTokenCharacter,
  )
where

import Data.Char (isAscii, ord)
import Data.Word (Word8)

isCookieTokenCharacter :: Char -> Bool
isCookieTokenCharacter character =
  isAscii character && isCookieTokenByte (fromIntegral (ord character))

isCookieTokenByte :: Word8 -> Bool
isCookieTokenByte byte =
  byte == 33
    || (byte >= 35 && byte <= 39)
    || byte == 42
    || byte == 43
    || byte == 45
    || byte == 46
    || (byte >= 48 && byte <= 57)
    || (byte >= 65 && byte <= 90)
    || (byte >= 94 && byte <= 122)
    || byte == 124
    || byte == 126
