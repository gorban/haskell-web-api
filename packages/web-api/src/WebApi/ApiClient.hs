-- | Application-owned durable OAuth API-client declarations.
--
-- Decision record (AHI-4D slice 4, 2026-09-16): keep client identity,
-- configured secret hashes, and scope policy in @web-api@ while Harch keeps
-- only the storage-neutral 'HarchWeb.ApiClientStore' capability. API clients
-- are not account principals: they have separate identifiers, secret rotation
-- records, and scope intersections. This prevents a bearer client from being
-- accidentally represented as an account merely because both proof types use
-- the same JWT issuer. PostgreSQL decoding and the OAuth endpoint remain
-- later adapters; this module is the pure application policy they share.
module WebApi.ApiClient
  ( ApiClient,
    ApiClientConfigurationError (..),
    ApiClientId,
    ApiClientIdError (..),
    ApiClientScopeError (..),
    EstablishedApiClient,
    apiClientAllowedScopes,
    apiClientDefaultScopes,
    apiClientId,
    apiClientIdText,
    apiClientSecretHashes,
    establishApiClient,
    establishedApiClientAllowedScopes,
    establishedApiClientId,
    intersectEstablishedApiClientScopes,
    mkApiClient,
    mkApiClientId,
    mkEstablishedApiClient,
    selectApiClientScopes,
  )
where

import Data.Char (isAscii)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Authentication (OAuth2Scope, oauth2ScopeText)
import HarchWeb.Password (PasswordHash)

-- | An application-issued API-client identifier. It has no 'Show' instance
-- because client identifiers remain private application data even though they
-- are not bearer credentials.
newtype ApiClientId = ApiClientId Text

data ApiClientIdError
  = ApiClientIdEmpty
  | ApiClientIdTooLong
  | ApiClientIdInvalidCharacter

-- | A currently enabled API client. Disabled or unknown clients are modeled
-- by an adapter returning 'Nothing'; they cannot reach a principal-establishing
-- workflow as a partly-valid record.
data ApiClient = ApiClient
  { apiClientId :: ApiClientId,
    apiClientSecretHashes :: NonEmpty PasswordHash,
    apiClientAllowedScopes :: [OAuth2Scope],
    apiClientDefaultScopes :: [OAuth2Scope]
  }

-- | The current durable principal established for a bearer token.  It omits
-- secret hashes because a bearer token remains valid through secret rotation;
-- request authorization instead intersects its granted scopes with this
-- client's current allowed scopes.
data EstablishedApiClient = EstablishedApiClient
  { establishedApiClientId :: ApiClientId,
    establishedApiClientAllowedScopes :: [OAuth2Scope]
  }

data ApiClientConfigurationError
  = ApiClientAllowedScopesEmpty
  | ApiClientAllowedScopesDuplicate
  | ApiClientDefaultScopesDuplicate
  | ApiClientDefaultScopeNotAllowed

data ApiClientScopeError
  = ApiClientRequestedScopeDuplicate
  | ApiClientRequestedScopeNotAllowed

mkApiClientId :: Text -> Either ApiClientIdError ApiClientId
mkApiClientId value
  | Text.null value = Left ApiClientIdEmpty
  | Text.length value > 128 = Left ApiClientIdTooLong
  | Text.all isClientIdCharacter value = Right (ApiClientId value)
  | otherwise = Left ApiClientIdInvalidCharacter
  where
    isClientIdCharacter character = isAscii character && character > ' ' && character <= '~'

apiClientIdText :: ApiClientId -> Text
apiClientIdText (ApiClientId value) = value

-- | Validate one operator-authored durable client. An omitted token-request
-- scope selects the configured defaults; an explicit request is validated by
-- 'selectApiClientScopes' against this same immutable allowance.
mkApiClient :: ApiClientId -> NonEmpty PasswordHash -> [OAuth2Scope] -> [OAuth2Scope] -> Either ApiClientConfigurationError ApiClient
mkApiClient clientId secretHashes allowedScopes defaultScopes
  | null allowedScopes = Left ApiClientAllowedScopesEmpty
  | hasDuplicateScopes allowedScopes = Left ApiClientAllowedScopesDuplicate
  | hasDuplicateScopes defaultScopes = Left ApiClientDefaultScopesDuplicate
  | not (all (`containsScope` allowedScopes) defaultScopes) = Left ApiClientDefaultScopeNotAllowed
  | otherwise = Right (ApiClient clientId secretHashes allowedScopes defaultScopes)

-- | Resolve a token request's effective scopes. An empty requested list means
-- the caller omitted @scope@ and uses the client's configured default; an
-- explicit request must contain no duplicates and be a subset of the current
-- durable allowance.
selectApiClientScopes :: ApiClient -> [OAuth2Scope] -> Either ApiClientScopeError [OAuth2Scope]
selectApiClientScopes client requestedScopes
  | null requestedScopes = Right (apiClientDefaultScopes client)
  | hasDuplicateScopes requestedScopes = Left ApiClientRequestedScopeDuplicate
  | not (all (`containsScope` apiClientAllowedScopes client) requestedScopes) = Left ApiClientRequestedScopeNotAllowed
  | otherwise = Right requestedScopes

-- | Discard client-authentication material after it has been used to issue a
-- token.  PostgreSQL establishment constructs the same bearer view directly
-- from current enabled-client rows, so an inactive secret can never reject an
-- otherwise valid bearer token.
establishApiClient :: ApiClient -> EstablishedApiClient
establishApiClient client =
  EstablishedApiClient
    { establishedApiClientId = apiClientId client,
      establishedApiClientAllowedScopes = apiClientAllowedScopes client
    }

-- | Validate the current bearer view reconstructed by a durable store.  This
-- deliberately has no secret-hash argument: established bearer tokens remain
-- valid through secret rotation, while a disabled client or removed scope is
-- decided from the current durable row on every request.
mkEstablishedApiClient :: ApiClientId -> [OAuth2Scope] -> Either ApiClientConfigurationError EstablishedApiClient
mkEstablishedApiClient clientId allowedScopes
  | null allowedScopes = Left ApiClientAllowedScopesEmpty
  | hasDuplicateScopes allowedScopes = Left ApiClientAllowedScopesDuplicate
  | otherwise = Right (EstablishedApiClient clientId allowedScopes)

-- | Restrict scopes carried by an issued bearer token to the client's current
-- durable allowance.  The result retains token order: removing a durable
-- grant takes effect immediately, while adding one cannot enlarge a token
-- already issued with a narrower scope set.
intersectEstablishedApiClientScopes :: EstablishedApiClient -> [OAuth2Scope] -> [OAuth2Scope]
intersectEstablishedApiClientScopes client = filter (`containsScope` establishedApiClientAllowedScopes client)

containsScope :: OAuth2Scope -> [OAuth2Scope] -> Bool
containsScope scope = any ((== oauth2ScopeText scope) . oauth2ScopeText)

hasDuplicateScopes :: [OAuth2Scope] -> Bool
hasDuplicateScopes scopes =
  let scopeTexts = oauth2ScopeText <$> scopes
   in length scopeTexts /= length (nub scopeTexts)
