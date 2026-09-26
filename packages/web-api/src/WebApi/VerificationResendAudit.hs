-- | The application-owned atomic persistence boundary for a verification
-- resend email that SMTP has already accepted.
--
-- Decision record (AHI-5, 2026-09-10): 'AccountStore' remains the owner of
-- generic verification-resend claims, promotion, and rolling delivery
-- history.  The reference application replaces only its successful
-- settlement callback at the action boundary, so the existing promotion and
-- required closed audit event either commit together or leave the claim
-- retryable.  This keeps durable audit policy out of Harch and avoids a
-- post-commit audit gap.
module WebApi.VerificationResendAudit
  ( VerificationResendAuditStore (..),
    VerificationResendAuditStoreError (..),
  )
where

import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.Account (VerificationResendClaim, VerificationResendClaimSettlement)
import WebApi.ActivityAudit (AccountActivity)

-- | A successful result means the matching resend claim was promoted and its
-- required activity was appended in one durable operation. A lost claim
-- leaves neither effect committed and retains the generic workflow's normal
-- suppression behavior.
newtype VerificationResendAuditStore = VerificationResendAuditStore
  { completeVerificationResendWithAudit ::
      VerificationResendClaim ->
      UnixTimeNanoseconds ->
      AccountActivity ->
      IO (Either VerificationResendAuditStoreError VerificationResendClaimSettlement)
  }

data VerificationResendAuditStoreError
  = VerificationResendAuditStoreUnavailable
  | VerificationResendAuditCapacityExceeded
  | VerificationResendAuditStoreCorruptData
