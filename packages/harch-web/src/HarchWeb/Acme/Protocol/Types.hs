-- | Private ACME protocol model shared by the protocol, JWS, and OpenSSL code.
--
-- Decision (BM\/DI, 2026-08-19): extend this existing protocol-model module
-- with grouping records rather than adding a new module or hand-threading
-- more positional arguments. 'AcmeDirectoryContext' names the
-- @('RuntimeAcmeBindPlan', 'HttpClient.Manager', 'AcmeDirectoryResponse',
-- accountKeyPath)@ quadruple every directory-scoped ACME request already
-- passed together; 'AcmeAccountSession' adds the account key ID every
-- request after account creation also carries. 'AcmeJwsRequestBody' and
-- 'AcmeJwsResponseExpectation' do the same for a JWS request's "what to
-- send" and "what counts as success" halves. Together these took every
-- \>= 6-parameter function this module's callers exposed
-- ('HarchWeb.Acme.Protocol.Client.performAcmeJwsRequest' was 10; every
-- 'HarchWeb.Acme.Protocol.Workflow' step was 6\-\-9) down to 2\-\-4, without
-- changing any request's on-the-wire behavior. This is additive grouping of
-- already-adjacent parameters, not a new dispatch or protocol abstraction,
-- so it does not implicate the extend-vs-new-abstraction question in
-- @docs\/design-guidance.md@.
module HarchWeb.Acme.Protocol.Types
  ( AcmeAccountSession (..),
    AcmeAuthorizationResponse (..),
    AcmeChallengeResponse (..),
    AcmeDirectoryContext (..),
    AcmeDirectoryResponse (..),
    AcmeJwk (..),
    AcmeJwsRequestBody (..),
    AcmeJwsResponseExpectation (..),
    AcmeOrderIdentifier (..),
    AcmeOrderResponse (..),
    AcmeRequestAuth (..),
    PreparedAcmeChallenge (..),
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Text (Text)
import HarchWeb.Acme.Certbot.Runtime (RuntimeAcmeBindPlan)
import HarchWeb.Acme.Challenge (ActiveAcmeChallenge)
import Network.HTTP.Client qualified as HttpClient

data AcmeDirectoryResponse = AcmeDirectoryResponse
  { acmeNewNonceUrl :: Text,
    acmeNewAccountUrl :: Text,
    acmeNewOrderUrl :: Text
  }

data AcmeOrderIdentifier = AcmeOrderIdentifier
  { acmeIdentifierKind :: Text,
    acmeIdentifierValue :: Text
  }

data AcmeChallengeResponse = AcmeChallengeResponse
  { acmeChallengeKind :: Text,
    acmeChallengeUrl :: Text,
    acmeChallengeTokenValue :: Text
  }

data AcmeAuthorizationResponse = AcmeAuthorizationResponse
  { acmeAuthorizationIdentifier :: AcmeOrderIdentifier,
    acmeAuthorizationChallenges :: [AcmeChallengeResponse]
  }

data AcmeOrderResponse = AcmeOrderResponse
  { acmeOrderStatus :: Text,
    acmeOrderAuthorizations :: Maybe [Text],
    acmeOrderFinalizeUrl :: Maybe Text,
    acmeOrderCertificateUrl :: Maybe Text
  }

data AcmeJwk = AcmeJwk
  { acmeJwkExponent :: Text,
    acmeJwkModulus :: Text
  }

data AcmeRequestAuth
  = AcmeRequestJwk AcmeJwk
  | AcmeRequestKid Text

data PreparedAcmeChallenge = PreparedAcmeChallenge
  { preparedAcmeChallengeRegistration :: ActiveAcmeChallenge,
    preparedAcmeChallengeUrl :: Text
  }

-- | The stable, always-together context every ACME directory request needs.
-- Grouping these stops a positional call site from, for example, dropping in
-- the wrong 'FilePath' (there is only one adjacent to this many same-shaped
-- fields once other request-specific arguments join it).
data AcmeDirectoryContext = AcmeDirectoryContext
  { acmeContextBindPlan :: RuntimeAcmeBindPlan,
    acmeContextManager :: HttpClient.Manager,
    acmeContextDirectory :: AcmeDirectoryResponse,
    acmeContextAccountKeyPath :: FilePath
  }

-- | 'AcmeDirectoryContext' plus the account key ID ACME issues after account
-- creation. Every workflow step after 'HarchWeb.Acme.createAcmeAccount' needs
-- both together, so callers construct this once and reuse it.
data AcmeAccountSession = AcmeAccountSession
  { acmeSessionContext :: AcmeDirectoryContext,
    acmeSessionAccountKid :: Text
  }

-- | The three pieces that define one ACME JWS-signed request body: who is
-- asserted (a fresh JWK for account creation, or an existing account KID for
-- everything after), the target URL, and the JSON payload to sign.
data AcmeJwsRequestBody = AcmeJwsRequestBody
  { acmeJwsRequestAuth :: AcmeRequestAuth,
    acmeJwsRequestUrl :: Text,
    acmeJwsRequestPayload :: LazyByteString.ByteString
  }

-- | What a caller of 'HarchWeb.Acme.performAcmeJwsRequest' will accept back:
-- an optional non-JSON @Accept@ header (for example a PEM certificate chain)
-- and the HTTP status codes that count as success for this request.
data AcmeJwsResponseExpectation = AcmeJwsResponseExpectation
  { acmeJwsAcceptHeader :: Maybe ByteString.ByteString,
    acmeJwsExpectedStatusCodes :: [Int]
  }
