-- | Closed, extensible vocabulary for authentication flows.
--
-- Decision record (AHI-4D slice 3, 2026-09-13): model interactive flow,
-- OAuth grant, and client-authentication method as separate axes before the
-- OAuth protocol adapter exists.  A route or storage adapter can therefore
-- select client credentials without treating it as a browser session, and a
-- later authorization-code, device, or mTLS method can add one constructor
-- without changing the meaning of the existing values.  These declarations
-- carry policy only: endpoint parsing, secret verification, and principal
-- establishment remain with their existing owners until the corresponding
-- adapters are introduced.
module HarchWeb.Authentication.Flow
  ( InteractiveAuthenticationFlow (..),
    BrowserSessionFlowConfiguration (..),
    OAuth2FlowConfiguration (..),
    OAuth2Grant (..),
    ClientAuthenticationMethod (..),
    OAuth2Scope,
    OAuth2ScopeError (..),
    mkOAuth2Scope,
    oauth2ScopeText,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text

-- | The authentication interaction an application exposes.  The custom
-- branch remains application-defined without reinterpreting browser or OAuth
-- policy at this framework boundary.
data InteractiveAuthenticationFlow custom
  = BrowserSessionFlow BrowserSessionFlowConfiguration
  | OAuth2Flow OAuth2FlowConfiguration
  | CustomAuthenticationFlow custom
  deriving (Eq, Show)

-- | Browser-session flow has no protocol grant settings.  Session issuance
-- and authentication remain selected by the existing account guard.
data BrowserSessionFlowConfiguration = BrowserSessionFlowConfiguration
  deriving (Eq, Show)

-- | OAuth protocol settings shared by its endpoint adapters.  At least one
-- supported grant is required before an OAuth flow can be declared.
newtype OAuth2FlowConfiguration = OAuth2FlowConfiguration
  { oauth2FlowGrants :: NonEmpty OAuth2Grant
  }
  deriving (Eq, Show)

-- | A standard OAuth 2.0 grant and its client authentication requirement.
newtype OAuth2Grant = ClientCredentialsGrant ClientAuthenticationMethod
  deriving (Eq, Show)

-- | Client authentication accepted for a grant.  Further methods are added
-- as constructors rather than overloading the client-credentials grant.
data ClientAuthenticationMethod = ClientSecretBasic
  deriving (Eq, Show)

-- | A single OAuth 2.0 scope token.  OAuth's space-delimited wire syntax
-- belongs to the token endpoint; this value represents one already-separated
-- token and prevents metadata, grants, and authorization declarations from
-- accepting a space or control character as part of a scope.  Derives 'Eq'
-- because 'HarchWeb.Authentication.Pipeline.ScopeRequirement' derives
-- @(Eq, Show)@ over this type and therefore needs it; see haskell-web-api's
-- AHI-4D authorization-widening decision record in @docs\/design-guidance.md@.
newtype OAuth2Scope = OAuth2Scope
  { oauth2ScopeText :: Text
  }
  deriving (Eq, Show)

-- | The safe construction failures for an OAuth scope declaration.  These
-- are configuration or protocol-input outcomes, not a reason to retain a
-- client-provided scope value in a public response.
data OAuth2ScopeError
  = OAuth2ScopeEmpty
  | OAuth2ScopeInvalidCharacter
  deriving (Eq, Show)

-- | Validate one OAuth scope token using RFC 6749's @NQCHAR@ grammar:
-- printable ASCII excluding space, quotation mark, and backslash.  Keeping
-- this at the shared flow boundary makes later form parsing and metadata
-- publication agree on the same syntax without adding another protocol
-- dispatcher.
mkOAuth2Scope :: Text -> Either OAuth2ScopeError OAuth2Scope
mkOAuth2Scope value
  | Text.null value = Left OAuth2ScopeEmpty
  | Text.all validCharacter value = Right (OAuth2Scope value)
  | otherwise = Left OAuth2ScopeInvalidCharacter
  where
    validCharacter character =
      let codePoint = fromEnum character
       in codePoint == 0x21
            || (codePoint >= 0x23 && codePoint <= 0x5B)
            || (codePoint >= 0x5D && codePoint <= 0x7E)
