-- | The application-owned, atomic persistence boundary for a successful
-- account login.  An opaque account session and its required audit activity
-- commit together; neither Harch Web nor the general session store chooses
-- the application's audit schema or transaction policy.
module WebApi.AccountSessionAudit
  ( AccountSessionAuditStore (..),
    AccountSessionAuditStoreError (..),
  )
where

import HarchWeb.Account (AccountId)
import HarchWeb.Session (OpaqueSession)
import WebApi.ActivityAudit (AccountActivity)

-- | A narrow durable-operation port.  The adapter must return success only
-- after both the account-session insert and the required audit append commit
-- in the same database transaction.
newtype AccountSessionAuditStore = AccountSessionAuditStore
  { saveAccountSessionWithAudit ::
      OpaqueSession AccountId ->
      AccountActivity ->
      IO (Either AccountSessionAuditStoreError Bool)
  }

data AccountSessionAuditStoreError
  = AccountSessionAuditStoreUnavailable
  | AccountSessionAuditCapacityExceeded
  | AccountSessionAuditStoreCorruptData
