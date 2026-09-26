-- | The application-owned atomic persistence boundary for a registration
-- email that has already been accepted by SMTP.
--
-- Decision record (AHI-5, 2026-09-10): the existing 'AccountStore' continues
-- to own generic pending-registration lifecycle semantics.  The reference
-- application replaces only its successful-delivery settlement operation at
-- the action boundary with this narrow port, so PostgreSQL can commit the
-- settlement and required closed audit event together.  This avoids putting
-- application audit policy in Harch or falsely treating a post-commit append
-- as atomic.
module WebApi.PendingRegistrationAudit
  ( PendingRegistrationAuditStore (..),
    PendingRegistrationAuditStoreError (..),
  )
where

import WebApi.Account (PendingRegistrationClaim)
import WebApi.ActivityAudit (AccountActivity)

-- | A successful result means the matching claimed registration delivery was
-- settled and its required activity was appended in one durable operation.
-- 'False' means that the claim was no longer current; neither effect commits.
newtype PendingRegistrationAuditStore = PendingRegistrationAuditStore
  { completePendingRegistrationDeliveryWithAudit ::
      PendingRegistrationClaim ->
      AccountActivity ->
      IO (Either PendingRegistrationAuditStoreError Bool)
  }

data PendingRegistrationAuditStoreError
  = PendingRegistrationAuditStoreUnavailable
  | PendingRegistrationAuditCapacityExceeded
  | PendingRegistrationAuditStoreCorruptData
