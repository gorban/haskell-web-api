-- | Application-owned OAuth client declarations for the composed example.
--
-- Decision record (OpenAPI documentation and Swagger UI, 2026-09-27): client identity, the active Argon2id
-- hash, and scope policy stay in the composed application, while the
-- framework's storage-neutral 'HarchWeb.ApiClientStore' remains the one
-- lookup boundary. Composed API clients are a distinct principal kind from
-- account sessions even though both proofs use the same signing key set;
-- their access tokens carry only the API audience and their currently
-- granted scopes. The initial example keeps one active hash and rotates it
-- atomically, which gives unknown and known rejected credentials one
-- verification each; overlapping hashes require a separate work-parity
-- decision.
module App.Composed.ApiClient
  ( ComposedApiClient,
    ComposedApiClientConfigurationError (..),
    ComposedApiClientId,
    ComposedApiClientIdError (..),
    ComposedApiClientScopeError (..),
    EstablishedComposedApiClient,
    composedExampleApiClientId,
    composedExampleApiClientScopeTexts,
    composedExampleApiClientScopes,
    composedApiClientAllowedScopes,
    composedApiClientDefaultScopes,
    composedApiClientId,
    composedApiClientIdText,
    composedApiClientSecretHash,
    establishComposedApiClient,
    establishedComposedApiClientAllowedScopes,
    establishedComposedApiClientId,
    intersectEstablishedComposedApiClientScopes,
    mkComposedApiClient,
    mkComposedApiClientId,
    mkEstablishedComposedApiClient,
    selectComposedApiClientScopes,
  )
where

import Data.Char (isAscii)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Authentication (OAuth2Scope, OAuth2ScopeError, mkOAuth2Scope, oauth2ScopeText)
import HarchWeb.Password (PasswordHash)

-- | A bounded application-issued client identifier. It has no 'Show'
-- instance so malformed or unknown submitted values do not become ordinary
-- diagnostics.
newtype ComposedApiClientId = ComposedApiClientId Text

data ComposedApiClientIdError
  = ComposedApiClientIdEmpty
  | ComposedApiClientIdTooLong
  | ComposedApiClientIdInvalidCharacter
  deriving (Eq, Show)

-- | One enabled client and its current secret/scope policy. It has exactly
-- one active secret hash; replacing that hash is an atomic rotation so every
-- rejected credential performs one Argon2 verification. Disabled or unknown
-- clients are represented by the store as absence.
data ComposedApiClient = ComposedApiClient
  { composedApiClientId :: ComposedApiClientId,
    composedApiClientSecretHash :: PasswordHash,
    composedApiClientAllowedScopes :: [OAuth2Scope],
    composedApiClientDefaultScopes :: [OAuth2Scope]
  }

-- | The current durable principal established for an API bearer. It carries
-- authorization state but no client secret hashes.
data EstablishedComposedApiClient = EstablishedComposedApiClient
  { establishedComposedApiClientId :: ComposedApiClientId,
    establishedComposedApiClientAllowedScopes :: [OAuth2Scope]
  }

data ComposedApiClientConfigurationError
  = ComposedApiClientAllowedScopesEmpty
  | ComposedApiClientAllowedScopesDuplicate
  | ComposedApiClientDefaultScopesDuplicate
  | ComposedApiClientDefaultScopeNotAllowed
  deriving (Eq, Show)

data ComposedApiClientScopeError
  = ComposedApiClientRequestedScopeDuplicate
  | ComposedApiClientRequestedScopeNotAllowed
  deriving (Eq, Show)

mkComposedApiClientId :: Text -> Either ComposedApiClientIdError ComposedApiClientId
mkComposedApiClientId value
  | Text.null value = Left ComposedApiClientIdEmpty
  | Text.length value > 128 = Left ComposedApiClientIdTooLong
  | Text.all isClientIdCharacter value = Right (ComposedApiClientId value)
  | otherwise = Left ComposedApiClientIdInvalidCharacter
  where
    isClientIdCharacter character = isAscii character && character > ' ' && character <= '~'

-- | The one durable reference client and its two default scopes. The scope
-- text list is the single authored declaration: the PostgreSQL provisioner
-- writes those exact values, while OAuth startup validates them into typed
-- scopes for discovery and token policy. This keeps the trusted static seed
-- separate from unchecked client input without turning a bad authored value
-- into a partial function.
composedExampleApiClientId :: ComposedApiClientId
composedExampleApiClientId = ComposedApiClientId "composed-example"

composedExampleApiClientScopeTexts :: NonEmpty Text
composedExampleApiClientScopeTexts = "catalog:read" :| ["orders:write"]

composedExampleApiClientScopes :: Either OAuth2ScopeError (NonEmpty OAuth2Scope)
composedExampleApiClientScopes = traverse mkOAuth2Scope composedExampleApiClientScopeTexts

composedApiClientIdText :: ComposedApiClientId -> Text
composedApiClientIdText (ComposedApiClientId value) = value

-- | Validate one operator-authored client. An omitted token-request scope
-- selects the configured defaults; an explicit request is checked against
-- the same immutable allowance.
mkComposedApiClient :: ComposedApiClientId -> PasswordHash -> [OAuth2Scope] -> [OAuth2Scope] -> Either ComposedApiClientConfigurationError ComposedApiClient
mkComposedApiClient clientId secretHash allowedScopes defaultScopes
  | null allowedScopes = Left ComposedApiClientAllowedScopesEmpty
  | hasDuplicateScopes allowedScopes = Left ComposedApiClientAllowedScopesDuplicate
  | hasDuplicateScopes defaultScopes = Left ComposedApiClientDefaultScopesDuplicate
  | not (all (`containsScope` allowedScopes) defaultScopes) = Left ComposedApiClientDefaultScopeNotAllowed
  | otherwise =
      Right
        ComposedApiClient
          { composedApiClientId = clientId,
            composedApiClientSecretHash = secretHash,
            composedApiClientAllowedScopes = allowedScopes,
            composedApiClientDefaultScopes = defaultScopes
          }

selectComposedApiClientScopes :: ComposedApiClient -> [OAuth2Scope] -> Either ComposedApiClientScopeError [OAuth2Scope]
selectComposedApiClientScopes client requestedScopes
  | null requestedScopes = Right (composedApiClientDefaultScopes client)
  | hasDuplicateScopes requestedScopes = Left ComposedApiClientRequestedScopeDuplicate
  | not (all (`containsScope` composedApiClientAllowedScopes client) requestedScopes) = Left ComposedApiClientRequestedScopeNotAllowed
  | otherwise = Right requestedScopes

-- | Build the bearer-principal view from a just-verified client record.
establishComposedApiClient :: ComposedApiClient -> EstablishedComposedApiClient
establishComposedApiClient client =
  EstablishedComposedApiClient
    { establishedComposedApiClientId = composedApiClientId client,
      establishedComposedApiClientAllowedScopes = composedApiClientAllowedScopes client
    }

-- | Reconstruct the current bearer authorization view from durable client
-- rows. It has no secret-hash argument, so secret rotation does not revoke
-- already issued bearers while client disablement and scope removal remain
-- visible on the next request.
mkEstablishedComposedApiClient :: ComposedApiClientId -> [OAuth2Scope] -> Either ComposedApiClientConfigurationError EstablishedComposedApiClient
mkEstablishedComposedApiClient clientId allowedScopes
  | null allowedScopes = Left ComposedApiClientAllowedScopesEmpty
  | hasDuplicateScopes allowedScopes = Left ComposedApiClientAllowedScopesDuplicate
  | otherwise = Right (EstablishedComposedApiClient clientId allowedScopes)

-- | Apply current durable scope policy without enlarging the scopes already
-- present on an issued token.
intersectEstablishedComposedApiClientScopes :: EstablishedComposedApiClient -> [OAuth2Scope] -> [OAuth2Scope]
intersectEstablishedComposedApiClientScopes client = filter (`containsScope` establishedComposedApiClientAllowedScopes client)

containsScope :: OAuth2Scope -> [OAuth2Scope] -> Bool
containsScope scope = any ((== oauth2ScopeText scope) . oauth2ScopeText)

hasDuplicateScopes :: [OAuth2Scope] -> Bool
hasDuplicateScopes scopes =
  let scopeTexts = oauth2ScopeText <$> scopes
   in length scopeTexts /= length (nub scopeTexts)
