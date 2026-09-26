{-# LANGUAGE OverloadedStrings #-}

-- | OAuth 2.0 protocol declarations that use Harch's ordinary typed API
-- request boundary.
--
-- Decision record (AHI-4D slice 3, 2026-09-14): decode the fixed
-- client-credentials form fields through the existing bounded API form codec
-- before adding client storage or token issuance.  This retains duplicate and
-- malformed field rejection at the one request-decoding boundary and gives a
-- later endpoint one typed value to combine with HTTP Basic authentication.
-- It does not parse a second request body, authenticate a client, or decide
-- whether requested scopes are allowed; those remain protocol and
-- application-owned steps of the later workflow.
module HarchWeb.Authentication.OAuth2
  ( OAuth2ClientCredentials,
    OAuth2ClientId,
    OAuth2ClientCredentialsMaximumBytes,
    OAuth2ClientCredentialsRequest,
    OAuth2ScopeRequest (..),
    mkOAuth2ClientCredentialsMaximumBytes,
    oauth2ClientCredentialsRequestCodec,
    oauth2ClientCredentialsId,
    oauth2ClientCredentialsSecret,
    oauth2ClientIdText,
    oauth2ClientSecretBasicCodec,
    oauth2ClientCredentialsGrant,
    oauth2ClientCredentialsScopes,
    oauth2RequestedScopeTexts,
    requiredOAuth2ClientCredentialsMaximumBytesOrDie,
  )
where

import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Base64 qualified as Base64
import Data.Char (digitToInt, isHexDigit)
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api.HeaderName (apiHeaderNameLiteral)
import HarchWeb.Api.Request
  ( ApiFieldValue,
    ApiHeaderName,
    RequestCodec,
    formField,
    headerField,
    optionalField,
    parseApiField,
    requiredField,
  )
import HarchWeb.Authentication.Flow
  ( ClientAuthenticationMethod (ClientSecretBasic),
    OAuth2Grant (ClientCredentialsGrant),
    OAuth2Scope,
    mkOAuth2Scope,
    oauth2ScopeText,
  )
import HarchWeb.Password (Password, mkPassword)

-- | One OAuth client ID decoded from HTTP Basic credentials.  It deliberately
-- has no 'Show' instance: client identities are application data and must not
-- become diagnostics merely because a malformed or unknown client is handled.
newtype OAuth2ClientId = OAuth2ClientId Text

-- | The Basic client ID and secret after strict Base64 and form decoding.  The
-- secret uses Harch's existing opaque 'Password' type so a later adapter can
-- hand it directly to the Argon2 verification boundary without converting it
-- back to text.
data OAuth2ClientCredentials = OAuth2ClientCredentials OAuth2ClientId Password

-- | A positive byte ceiling applied to the complete Basic value and to its
-- decoded credentials before either becomes application input.
newtype OAuth2ClientCredentialsMaximumBytes = OAuth2ClientCredentialsMaximumBytes Int

mkOAuth2ClientCredentialsMaximumBytes :: Int -> Either Text OAuth2ClientCredentialsMaximumBytes
mkOAuth2ClientCredentialsMaximumBytes value
  | value <= 0 = Left "OAuth client-credentials maximum bytes must be positive"
  | otherwise = Right (OAuth2ClientCredentialsMaximumBytes value)

requiredOAuth2ClientCredentialsMaximumBytesOrDie :: Int -> OAuth2ClientCredentialsMaximumBytes
requiredOAuth2ClientCredentialsMaximumBytesOrDie value =
  fromRight
    (error "invalid OAuth client-credentials maximum-byte declaration")
    (mkOAuth2ClientCredentialsMaximumBytes value)

-- | Reveal a decoded client ID only to the application storage adapter which
-- establishes the current client.  The value must not be rendered in public
-- OAuth errors or copied into observability data.
oauth2ClientIdText :: OAuth2ClientId -> Text
oauth2ClientIdText (OAuth2ClientId value) = value

-- | The opaque client ID selected by the request.  Only a durable client
-- adapter should render it into its private lookup representation.
oauth2ClientCredentialsId :: OAuth2ClientCredentials -> OAuth2ClientId
oauth2ClientCredentialsId (OAuth2ClientCredentials identifier _) = identifier

-- | Pass the opaque submitted secret to the existing password verifier.
oauth2ClientCredentialsSecret :: OAuth2ClientCredentials -> Password
oauth2ClientCredentialsSecret (OAuth2ClientCredentials _ secret) = secret

-- | Decode the one HTTP Basic client-authentication declaration accepted by
-- the client-credentials grant.  This is a field in the existing API request
-- codec, so missing and duplicate @Authorization@ fields retain the same
-- accumulated typed rejection as every other endpoint input.
--
-- Decision record (AHI-4D slice 3, 2026-09-16): extend the existing bounded
-- API request codec with a strict OAuth Basic value decoder rather than add a
-- second header parser at the token endpoint. The codec already owns
-- case-insensitive header selection and duplicate rejection. This decoder
-- owns only the Basic scheme, byte bounds, Base64, and RFC form-component
-- decoding; durable client lookup, Argon2 verification, and protocol error
-- mapping remain application work. Credentials stay opaque and have no
-- 'Show' instance, so this boundary cannot accidentally turn them into a
-- public diagnostic or log value.
oauth2ClientSecretBasicCodec :: OAuth2ClientCredentialsMaximumBytes -> RequestCodec OAuth2ClientCredentials
oauth2ClientSecretBasicCodec maximumBytes =
  requiredField (headerField authorizationHeader (parseApiField (decodeClientCredentials maximumBytes)))

authorizationHeader :: ApiHeaderName
authorizationHeader = apiHeaderNameLiteral "Authorization"

decodeClientCredentials :: OAuth2ClientCredentialsMaximumBytes -> Text -> Maybe OAuth2ClientCredentials
decodeClientCredentials (OAuth2ClientCredentialsMaximumBytes maximumBytes) headerValue = do
  let (scheme, encodedWithSeparator) = Text.breakOn " " headerValue
  encodedValue <- Text.stripPrefix " " encodedWithSeparator
  guard (not (Text.null encodedValue || Text.isInfixOf " " encodedValue || ByteString.length (TextEncoding.encodeUtf8 headerValue) > maximumBytes))
  guard (Text.toCaseFold scheme == "basic")
  decoded <- either (const Nothing) Just (Base64.decode (TextEncoding.encodeUtf8 encodedValue))
  -- The bounded complete header is always larger than its Base64-decoded
  -- credentials, so the first byte limit also bounds this value.
  splitCredentials decoded

splitCredentials :: ByteString -> Maybe OAuth2ClientCredentials
splitCredentials decoded = do
  let (encodedClientId, encodedSecretWithSeparator) = ByteString.break (== 58) decoded
  if ByteString.null encodedClientId || ByteString.null encodedSecretWithSeparator
    then Nothing
    else do
      clientId <- decodeFormComponent encodedClientId
      secret <- decodeFormComponent (ByteString.drop 1 encodedSecretWithSeparator)
      if Text.null clientId || Text.null secret
        then Nothing
        else Just (OAuth2ClientCredentials (OAuth2ClientId clientId) (mkPassword secret))

decodeFormComponent :: ByteString -> Maybe Text
decodeFormComponent bytes = do
  decoded <- ByteString.pack <$> go (ByteString.unpack bytes)
  either (const Nothing) Just (TextEncoding.decodeUtf8' decoded)
  where
    go [] = Just []
    go (43 : remaining) = (32 :) <$> go remaining
    go (37 : high : low : remaining)
      | isHexByte high && isHexByte low =
          (fromIntegral (digitToInt (toChar high) * 16 + digitToInt (toChar low)) :) <$> go remaining
    go (37 : _) = Nothing
    go (value : remaining) = (value :) <$> go remaining

    isHexByte = isHexDigit . toChar
    toChar = toEnum . fromIntegral

-- | A decoded OAuth client-credentials request.  HTTP Basic authentication
-- is intentionally absent: it belongs to the header adapter, while this type
-- owns only the one URL-encoded body declared by the token endpoint.
data OAuth2ClientCredentialsRequest = OAuth2ClientCredentialsRequest
  { oauth2ClientCredentialsGrant :: OAuth2Grant,
    oauth2ClientCredentialsScopes :: OAuth2ScopeRequest
  }

-- | Whether a token request leaves scope selection to the client default or
-- explicitly asks for one or more RFC 6749 scope tokens.  The explicit case
-- preserves request order; the client workflow later checks each requested
-- token against its current durable allowance.
data OAuth2ScopeRequest
  = UseClientDefaultScopes
  | RequestOAuth2Scopes (NonEmpty OAuth2Scope)

-- | The one client-credentials body codec.  It requires exactly one
-- @grant_type=client_credentials@ field and accepts zero or one @scope@
-- field.  Scope values keep RFC 6749's single-space grammar: leading,
-- trailing, repeated, or invalid characters are rejected rather than being
-- normalized into a different authorization request.
oauth2ClientCredentialsRequestCodec :: RequestCodec OAuth2ClientCredentialsRequest
oauth2ClientCredentialsRequestCodec =
  OAuth2ClientCredentialsRequest
    <$> requiredField (formField "grant_type" clientCredentialsGrant)
    <*> scopeRequest
  where
    scopeRequest = maybe UseClientDefaultScopes RequestOAuth2Scopes <$> optionalField (formField "scope" scopeValue)

clientCredentialsGrant :: ApiFieldValue OAuth2Grant
clientCredentialsGrant = parseApiField $ \value ->
  if value == "client_credentials"
    then Just (ClientCredentialsGrant ClientSecretBasic)
    else Nothing

scopeValue :: ApiFieldValue (NonEmpty OAuth2Scope)
scopeValue = parseApiField $ \value ->
  NonEmpty.nonEmpty (Text.split (== ' ') value)
    >>= traverse (either (const Nothing) Just . mkOAuth2Scope)

-- | Render the opaque requested scopes only for protocol encoding or
-- application authorization checks.  The returned list is empty exactly
-- when the request selected its configured default scopes.
oauth2RequestedScopeTexts :: OAuth2ScopeRequest -> [Text]
oauth2RequestedScopeTexts scopeRequest =
  case scopeRequest of
    UseClientDefaultScopes -> []
    RequestOAuth2Scopes scopes -> oauth2ScopeText <$> NonEmpty.toList scopes
