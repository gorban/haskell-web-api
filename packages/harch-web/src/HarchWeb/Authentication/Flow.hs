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
  )
where

import Data.List.NonEmpty (NonEmpty)

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
