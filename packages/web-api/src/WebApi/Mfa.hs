module WebApi.Mfa
  ( MfaStore (..),
    MfaStoreError (..),
    StoredTotpEnrollment (..),
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Word (Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.Time (UnixTimeNanoseconds)

data MfaStoreError
  = MfaStoreUnavailable Text
  | MfaStoreCorruptData Text
  deriving (Eq)

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
    confirmTotpEnrollment :: AccountId -> NonEmpty Text -> UnixTimeNanoseconds -> IO (Either MfaStoreError Bool),
    loadUnusedRecoveryCodeHashes :: AccountId -> IO (Either MfaStoreError [Text]),
    consumeRecoveryCodeHash :: AccountId -> Text -> UnixTimeNanoseconds -> IO (Either MfaStoreError Bool),
    -- | Atomically records that this TOTP counter has now been used,
    -- succeeding only if the stored counter is still lower (or unset) —
    -- the same conditional-update shape as 'consumeRecoveryCodeHash', so a
    -- concurrent request for the same account cannot both accept the same
    -- or an older counter.
    markTotpCodeUsed :: AccountId -> Word64 -> IO (Either MfaStoreError Bool)
  }
