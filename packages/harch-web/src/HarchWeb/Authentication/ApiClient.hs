-- | Storage-neutral durable API-client establishment.
--
-- Decision record (AHI-4D slice 3, 2026-09-13): keep durable API-client
-- lookup at the existing proof-to-principal boundary, but make the storage
-- capability application supplied.  JWT verification alone cannot establish
-- a revocable client principal: an adapter must load its current disabled,
-- secret, and scope state for every accepted bearer token.  Harch therefore
-- names the two operations a protocol workflow and a bearer establisher need,
-- without importing PostgreSQL or owning an acceptance cache.  A PostgreSQL
-- adapter can map its private failure details to the stable unavailable case;
-- the authentication pipeline continues to own public failure interpretation.
module HarchWeb.Authentication.ApiClient
  ( ApiClientStore (..),
    ApiClientStoreError (..),
  )
where

import HarchWeb.Authentication.Pipeline (AuthenticationDependency)

-- | Storage outcomes that can be safely interpreted at Harch's
-- authentication boundary.  Adapter-specific causes stay private to the
-- application while the request pipeline can distinguish unavailability from
-- an unknown or disabled client represented by 'Nothing'.
newtype ApiClientStoreError = ApiClientStoreUnavailable AuthenticationDependency
  deriving (Eq, Show)

-- | Application-supplied durable API-client storage.  Discovery returns the
-- issuance view, including active secret hashes.  Establishment returns the
-- current bearer-principal view, which must not require an active secret: a
-- secret is a client-authentication credential, not a property carried by an
-- issued bearer token.  Both operations return 'Nothing' for an absent,
-- disabled, or otherwise non-establishable client so the protocol adapter can
-- preserve anti-enumeration behavior.  Each bearer request uses
-- 'establishApiClient'; applications must not substitute a cross-request
-- acceptance cache without an explicit invalidation contract.
data ApiClientStore clientId issuanceClient establishedClient = ApiClientStore
  { findApiClient :: clientId -> IO (Either ApiClientStoreError (Maybe issuanceClient)),
    establishApiClient :: clientId -> IO (Either ApiClientStoreError (Maybe establishedClient))
  }
