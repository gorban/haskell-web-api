{-# LANGUAGE OverloadedStrings #-}

-- | Validated authentication-proof transport and browser cookie policy.
--
-- Decision record (PR-F6, 2026-09-05): this internal owner keeps the existing
-- cookie policy and bounded proof extraction together because both interpret
-- untrusted request transport. It supplies the one typed extractor consumed by
-- 'HarchWeb.Authentication.Pipeline'; it neither verifies a proof nor
-- establishes a principal. Keeping it below the pipeline avoids a cycle and
-- prevents an application from obtaining a second extraction path.
--
-- Decision record (AHI-4D slice 2, 2026-09-13): cookie-or-bearer JWT
-- extraction extends this owner rather than composing two generic extractors.
-- Cookie and Authorization are two encodings of one credential, so equal
-- bounded values are accepted with a source fact, conflicting values fail,
-- and malformed explicit authorization never falls back to a cookie. The
-- source value contains no credential bytes and permits later CSRF policy to
-- distinguish bearer-only from ambient or dual-source requests.
module HarchWeb.Authentication.Transport
  ( AuthenticationCookieName,
    AuthenticationCookiePolicy,
    AuthenticationProofExtractor (..),
    AuthenticationProofMaximumBytes,
    EncodedJwt,
    JwtProof,
    JwtProofSource (..),
    ProofExtractionFailure (..),
    authenticationCookieName,
    bearerJwtExtractor,
    clearAuthenticationCookie,
    combineProofExtractors,
    cookieOrBearerJwtExtractor,
    cookieJwtExtractor,
    encodedJwtFromBytes,
    encodedJwtBytes,
    jwtProofFromCookie,
    jwtProofEncodedJwt,
    jwtProofSource,
    mkAuthenticationCookieName,
    mkAuthenticationCookiePolicy,
    mkAuthenticationProofMaximumBytes,
    renderAuthenticationCookie,
    requiredAuthenticationCookiePolicyOrDie,
    requiredAuthenticationProofMaximumBytesOrDie,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Cookie (isCookieTokenCharacter)
import HarchWeb.EndpointSecurity (EndpointRequest (..))
import HarchWeb.Security.ConstantTime (constantWorkEquals)
import Network.Wai qualified as Wai

-- | A validated cookie name selected by an application configuration. The
-- extractor never uses a raw header-derived name as a lookup key.
newtype AuthenticationCookieName = AuthenticationCookieName ByteString
  deriving (Eq, Show)

mkAuthenticationCookieName :: Text -> Either Text AuthenticationCookieName
mkAuthenticationCookieName value
  | Text.null value = Left "authentication cookie name cannot be empty"
  | Text.length value > 128 = Left "authentication cookie name is too long"
  | Text.all isCookieTokenCharacter value = Right (AuthenticationCookieName (TextEncoding.encodeUtf8 value))
  | otherwise = Left "authentication cookie name has invalid characters"

-- | The only browser-session cookie policy supplied by Harch's JWT boundary.
-- A policy is deliberately host-only: the @__Host-@ prefix plus the fixed
-- @Path=/@, @Secure@, @HttpOnly@, and @SameSite=Strict@ attributes make a
-- deployment-specific domain or broad path impossible to author here.
--
-- Decision (AHI-4C, 2026-09-03): JWT proof extraction already belonged to
-- this module, but issuing one required every application to reconstruct the
-- security-sensitive cookie string.  Extend that existing capability with a
-- validated policy and opaque-token renderer rather than adding a web-api
-- helper. Applications still own the chosen name and lifetime; Harch never
-- loads keys or issues a JWT itself.
data AuthenticationCookiePolicy = AuthenticationCookiePolicy
  { authenticationCookieName :: AuthenticationCookieName,
    authenticationCookieMaxAgeSeconds :: Word64
  }
  deriving (Eq, Show)

-- | Construct a non-sliding host-only JWT cookie policy. A zero lifetime is
-- reserved for 'clearAuthenticationCookie', so successful authentication
-- cannot accidentally issue an immediately expired credential.
mkAuthenticationCookiePolicy :: Text -> Word64 -> Either Text AuthenticationCookiePolicy
mkAuthenticationCookiePolicy name maxAgeSeconds
  | maxAgeSeconds == 0 = Left "authentication cookie max age must be positive"
  | not ("__Host-" `Text.isPrefixOf` name) = Left "authentication cookie name must use the __Host- prefix"
  | otherwise =
      AuthenticationCookiePolicy <$> mkAuthenticationCookieName name <*> pure maxAgeSeconds

-- | Require a host-only cookie declaration authored by application code to
-- remain valid. Request-derived names or durations must use
-- 'mkAuthenticationCookiePolicy' and handle its rejection rail instead.
--
-- This follows the established @required...OrDie@ declaration boundary: an
-- error identifies an invalid program declaration, never a client outcome.
requiredAuthenticationCookiePolicyOrDie :: Text -> Word64 -> AuthenticationCookiePolicy
requiredAuthenticationCookiePolicyOrDie name maxAgeSeconds =
  fromRight
    (error "invalid authentication cookie declaration")
    (mkAuthenticationCookiePolicy name maxAgeSeconds)

-- | Render an issued compact JWT only when its opaque bytes are valid cookie
-- octets. 'EncodedJwt' intentionally also represents untrusted received
-- proofs for verifier tests, so rendering is partial rather than assuming its
-- bytes came from 'issueJwt'.
renderAuthenticationCookie :: AuthenticationCookiePolicy -> EncodedJwt -> Maybe Text
renderAuthenticationCookie policy encodedJwt = do
  token <- either (const Nothing) Just (TextEncoding.decodeUtf8' (encodedJwtBytes encodedJwt))
  if Text.null token || Text.any (not . validCookieValueCharacter) token
    then Nothing
    else
      pure
        ( authenticationCookieNameText (authenticationCookieName policy)
            <> "="
            <> token
            <> cookieAttributes (authenticationCookieMaxAgeSeconds policy)
        )

-- | Expire the configured host-only cookie after durable session revocation.
-- It deliberately accepts no token value, so logout cannot reflect an
-- untrusted credential back into a response header.
clearAuthenticationCookie :: AuthenticationCookiePolicy -> Text
clearAuthenticationCookie policy =
  authenticationCookieNameText (authenticationCookieName policy)
    <> "="
    <> cookieAttributes 0

authenticationCookieNameText :: AuthenticationCookieName -> Text
authenticationCookieNameText (AuthenticationCookieName name) = TextEncoding.decodeUtf8 name

cookieAttributes :: Word64 -> Text
cookieAttributes maxAgeSeconds =
  "; Path=/; Max-Age="
    <> Text.pack (show maxAgeSeconds)
    <> "; HttpOnly; Secure; SameSite=Strict"

validCookieValueCharacter :: Char -> Bool
validCookieValueCharacter character =
  character > ' '
    && character <= '~'
    && character /= '"'
    && character /= ','
    && character /= ';'
    && character /= '\\'

-- | A positive, application-selected byte budget for one compact proof.
-- Keeping the bound validated makes a missing or non-positive extraction
-- limit impossible to pass into the request path.
newtype AuthenticationProofMaximumBytes = AuthenticationProofMaximumBytes Int
  deriving (Eq, Show)

mkAuthenticationProofMaximumBytes :: Int -> Either Text AuthenticationProofMaximumBytes
mkAuthenticationProofMaximumBytes value
  | value <= 0 = Left "authentication proof maximum bytes must be positive"
  | otherwise = Right (AuthenticationProofMaximumBytes value)

-- | Require an application-authored positive proof limit. Runtime-selected
-- limits must use 'mkAuthenticationProofMaximumBytes' so configuration errors
-- remain on the ordinary validation rail.
requiredAuthenticationProofMaximumBytesOrDie :: Int -> AuthenticationProofMaximumBytes
requiredAuthenticationProofMaximumBytesOrDie value =
  fromRight
    (error "invalid authentication proof limit declaration")
    (mkAuthenticationProofMaximumBytes value)

-- | Opaque compact-JWT bytes. It intentionally has no 'Show' instance: a
-- proof must not reach assertion failures, logs, or telemetry by accident.
newtype EncodedJwt = EncodedJwt ByteString
  deriving (Eq)

encodedJwtBytes :: EncodedJwt -> ByteString
encodedJwtBytes (EncodedJwt value) = value

encodedJwtFromBytes :: ByteString -> EncodedJwt
encodedJwtFromBytes = EncodedJwt

-- | The transport(s) that supplied a verified-candidate JWT. The dual-source
-- variant is deliberately distinct: it records that an ambient credential
-- participated even when the explicit bearer value was byte-identical, so a
-- later CSRF policy cannot accidentally treat that request as bearer-only.
data JwtProofSource
  = JwtProofFromCookie
  | JwtProofFromBearer
  | JwtProofFromCookieAndBearer
  deriving (Eq, Ord, Show)

-- | An opaque JWT paired with the request transport that supplied it. Its
-- 'Show' instance redacts the credential; callers may branch on
-- 'jwtProofSource' or pass 'jwtProofEncodedJwt' only to a verifier.
data JwtProof = JwtProof JwtProofSource EncodedJwt
  deriving (Eq)

instance Show JwtProof where
  show (JwtProof source _) = "JwtProof " <> show source <> " <redacted>"

jwtProofSource :: JwtProof -> JwtProofSource
jwtProofSource (JwtProof source _) = source

jwtProofEncodedJwt :: JwtProof -> EncodedJwt
jwtProofEncodedJwt (JwtProof _ encodedJwt) = encodedJwt

-- | Pair a JWT already extracted from the configured browser cookie with its
-- non-forgeable-in-request-processing transport fact. This is for adapters
-- which have already used 'cookieJwtExtractor'; ordinary request code should
-- prefer 'cookieOrBearerJwtExtractor'.
jwtProofFromCookie :: EncodedJwt -> JwtProof
jwtProofFromCookie = JwtProof JwtProofFromCookie

data ProofExtractionFailure
  = ProofMalformed
  | ProofAmbiguous
  | ProofConflicting
  | ProofTooLarge
  deriving (Eq, Show)

newtype AuthenticationProofExtractor route context authorization proof = AuthenticationProofExtractor
  { extractAuthenticationProof :: EndpointRequest route context authorization -> Either ProofExtractionFailure (Maybe proof)
  }

combineProofExtractors :: NonEmpty (AuthenticationProofExtractor route context authorization proof) -> AuthenticationProofExtractor route context authorization proof
combineProofExtractors extractors =
  AuthenticationProofExtractor $ \endpointRequest -> do
    extractedProofs <- traverse (`extractAuthenticationProof` endpointRequest) (NonEmpty.toList extractors)
    case catMaybes extractedProofs of
      [] -> Right Nothing
      [proof] -> Right (Just proof)
      _ -> Left ProofAmbiguous

-- | Extract one bounded browser-session JWT from the configured cookie. A
-- duplicate cookie is ambiguous rather than precedence-selected.
cookieJwtExtractor :: AuthenticationCookieName -> AuthenticationProofMaximumBytes -> AuthenticationProofExtractor route context authorization EncodedJwt
cookieJwtExtractor (AuthenticationCookieName cookieName) maximumBytes =
  AuthenticationProofExtractor (extractCookieJwt cookieName maximumBytes)

-- | Extract a JWT from the configured cookie and/or one bearer header. This
-- is intentionally separate from 'combineProofExtractors': unrelated proof
-- types remain ambiguous whenever more than one is present, while the one
-- JWT credential may appear in both standard transports only when its bounded
-- opaque bytes match by the private constant-work primitive. A malformed or
-- unsupported explicit authorization header is rejected even if a valid
-- cookie is present; an attacker cannot make header processing fall back to
-- ambient browser state.
cookieOrBearerJwtExtractor :: AuthenticationCookieName -> AuthenticationProofMaximumBytes -> AuthenticationProofExtractor route context authorization JwtProof
cookieOrBearerJwtExtractor (AuthenticationCookieName cookieName) maximumBytes =
  AuthenticationProofExtractor $ \endpointRequest -> do
    cookieProof <- extractCookieJwt cookieName maximumBytes endpointRequest
    bearerProof <- extractBearerJwt maximumBytes endpointRequest
    case (cookieProof, bearerProof) of
      (Nothing, Nothing) -> Right Nothing
      (Just cookieJwt, Nothing) -> Right (Just (JwtProof JwtProofFromCookie cookieJwt))
      (Nothing, Just bearerJwt) -> Right (Just (JwtProof JwtProofFromBearer bearerJwt))
      (Just cookieJwt, Just bearerJwt)
        | sameEncodedJwt cookieJwt bearerJwt -> Right (Just (JwtProof JwtProofFromCookieAndBearer cookieJwt))
        | otherwise -> Left ProofConflicting

sameEncodedJwt :: EncodedJwt -> EncodedJwt -> Bool
sameEncodedJwt left right = constantWorkEquals (encodedJwtBytes left) (encodedJwtBytes right)

extractCookieJwt :: ByteString -> AuthenticationProofMaximumBytes -> EndpointRequest route context authorization -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractCookieJwt cookieName maximumBytes endpointRequest = do
  matchingValues <- concat . concat <$> traverse matchingCookieValues (cookieHeaders endpointRequest)
  extractBoundedJwt maximumBytes (: []) matchingValues
  where
    cookieHeaders request =
      [ rawValue
      | (headerName, rawValue) <- Wai.requestHeaders (endpointWaiRequest request),
        headerName == "Cookie"
      ]
    matchingCookieValues rawHeader = traverse matchingCookieValue (ByteString.split 59 rawHeader)
    matchingCookieValue rawCookie =
      let strippedCookie = ByteString.dropWhile (== 32) rawCookie
          (cookieKey, cookieValueWithSeparator) = ByteString.break (== 61) strippedCookie
       in if cookieKey /= cookieName
            then Right []
            else case ByteString.uncons cookieValueWithSeparator of
              Nothing -> Left ProofMalformed
              Just (_, cookieValue)
                | ByteString.null cookieValue -> Left ProofMalformed
                | otherwise -> Right [cookieValue]

-- | Extract one bounded bearer JWT. Any Authorization header that is present
-- but not exactly one well-formed @Bearer <token>@ value is a malformed proof;
-- API applications therefore never fall back to a browser cookie by accident.
bearerJwtExtractor :: AuthenticationProofMaximumBytes -> AuthenticationProofExtractor route context authorization EncodedJwt
bearerJwtExtractor maximumBytes =
  AuthenticationProofExtractor (extractBearerJwt maximumBytes)

extractBearerJwt :: AuthenticationProofMaximumBytes -> EndpointRequest route context authorization -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractBearerJwt maximumBytes endpointRequest =
  let authorizationValues =
        [ rawValue
        | (headerName, rawValue) <- Wai.requestHeaders (endpointWaiRequest endpointRequest),
          headerName == "Authorization"
        ]
   in case authorizationValues of
        [] -> Right Nothing
        [authorizationValue] ->
          case ByteString.stripPrefix "Bearer " authorizationValue of
            Just rawJwt
              | not (ByteString.null rawJwt) && not (ByteString.elem 32 rawJwt) -> extractJwtValue maximumBytes rawJwt
            _ -> Left ProofMalformed
        _ -> Left ProofAmbiguous

extractBoundedJwt :: AuthenticationProofMaximumBytes -> (ByteString -> [ByteString]) -> [ByteString] -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractBoundedJwt (AuthenticationProofMaximumBytes maximumBytes) selectValues headers =
  case concatMap selectValues headers of
    [] -> Right Nothing
    [rawJwt]
      | ByteString.length rawJwt > maximumBytes -> Left ProofTooLarge
      | otherwise -> Right (Just (EncodedJwt rawJwt))
    _ -> Left ProofAmbiguous

extractJwtValue :: AuthenticationProofMaximumBytes -> ByteString -> Either ProofExtractionFailure (Maybe EncodedJwt)
extractJwtValue maximumBytes rawJwt = extractBoundedJwt maximumBytes (: []) [rawJwt]
