-- | Private ACME protocol model shared by the protocol, JWS, and OpenSSL code.
module HarchWeb.Acme.Protocol.Types
  ( AcmeAuthorizationResponse (..),
    AcmeChallengeResponse (..),
    AcmeDirectoryResponse (..),
    AcmeJwk (..),
    AcmeOrderIdentifier (..),
    AcmeOrderResponse (..),
    AcmeRequestAuth (..),
    PreparedAcmeChallenge (..),
  )
where

import Data.Text (Text)
import HarchWeb.Acme.Challenge (ActiveAcmeChallenge)

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
