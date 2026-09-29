module WebApi.Mfa
  ( MfaStore (..),
    MfaStoreError (..),
    MfaConfirmationAuditContext (..),
    StoredTotpEnrollment (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.RequestId (RequestId)
import HarchWeb.Time (UnixTimeNanoseconds)
import WebApi.ActivityAudit (ActivityAuditStoreError, AuditRouteObservation)

data MfaStoreError
  = MfaStoreUnavailable Text
  | MfaStoreCorruptData Text
  | MfaStoreAuditAppendFailed ActivityAuditStoreError
  deriving (Eq)

-- | Trusted correlation fields required by the atomic enrollment-confirmation
-- operation. The audit event and subject are selected by the store operation;
-- callers cannot replace them with arbitrary catalog values.
data MfaConfirmationAuditContext = MfaConfirmationAuditContext
  { mfaConfirmationRequestId :: RequestId,
    mfaConfirmationRoute :: Maybe AuditRouteObservation
  }

data StoredTotpEnrollment = StoredTotpEnrollment
  { storedTotpEncryptedSecret :: Text,
    storedTotpConfirmedAtNanoseconds :: Maybe UnixTimeNanoseconds,
    -- | The highest TOTP counter ('HarchWeb.Totp.validateTotpCodeCounter')
    -- already accepted for this account, or 'Nothing' if none has been.
    -- Login must reject a counter at or below this value: without it, an
    -- observed code stays valid for the rest of its skew window.
    storedTotpLastUsedCounter :: Maybe Word64
  }
  deriving (Eq)

data MfaStore = MfaStore
  { saveUnconfirmedTotpEnrollment :: AccountId -> Text -> UnixTimeNanoseconds -> IO (Either MfaStoreError Bool),
    loadTotpEnrollment :: AccountId -> IO (Either MfaStoreError (Maybe StoredTotpEnrollment)),
    -- | Extend the existing confirmation write so confirming TOTP, replacing
    -- recovery-code hashes, and appending the required @MfaEnrolled@ event
    -- share one database transaction. A post-commit hook could not roll back
    -- the authenticator state when audit persistence failed. Implementations
    -- must roll back all three effects on that failure. This audited store
    -- operation covers enrollment only; auditing successful TOTP login and
    -- recovery-code consumption remains follow-up work under AHI-5.
    confirmTotpEnrollment :: AccountId -> NonEmpty Text -> UnixTimeNanoseconds -> MfaConfirmationAuditContext -> IO (Either MfaStoreError Bool),
    loadUnusedRecoveryCodeHashes :: AccountId -> IO (Either MfaStoreError [Text]),
    consumeRecoveryCodeHash :: AccountId -> Text -> UnixTimeNanoseconds -> IO (Either MfaStoreError Bool),
    -- | Atomically records that this TOTP counter has now been used,
    -- succeeding only if the stored counter is still lower (or unset) —
    -- the same conditional-update shape as 'consumeRecoveryCodeHash', so a
    -- concurrent request for the same account cannot both accept the same
    -- or an older counter.
    markTotpCodeUsed :: AccountId -> Word64 -> IO (Either MfaStoreError Bool)
  }
