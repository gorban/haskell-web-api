module WebApi.Session
  ( AccountSessionStore (..),
    AccountSessionStoreError (..),
  )
where

import HarchWeb.Account (AccountId)
import HarchWeb.Session (OpaqueSession, SessionId)

data AccountSessionStoreError
  = AccountSessionStoreUnavailable
  | AccountSessionStoreCorruptData
  deriving (Eq)

-- | Application-owned persistence for opaque account sessions. The framework
-- owns the opaque token and cookie semantics; this adapter owns the principal
-- mapping and revocation state.
data AccountSessionStore = AccountSessionStore
  { saveAccountSession :: OpaqueSession AccountId -> IO (Either AccountSessionStoreError Bool),
    loadAccountSession :: SessionId -> IO (Either AccountSessionStoreError (Maybe (OpaqueSession AccountId))),
    invalidateAccountSession :: SessionId -> IO (Either AccountSessionStoreError Bool)
  }
