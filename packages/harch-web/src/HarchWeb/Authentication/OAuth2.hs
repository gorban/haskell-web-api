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
  ( OAuth2ClientCredentialsRequest,
    OAuth2ScopeRequest (..),
    oauth2ClientCredentialsRequestCodec,
    oauth2ClientCredentialsGrant,
    oauth2ClientCredentialsScopes,
    oauth2RequestedScopeTexts,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api.Request
  ( ApiFieldValue,
    RequestCodec,
    formField,
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
