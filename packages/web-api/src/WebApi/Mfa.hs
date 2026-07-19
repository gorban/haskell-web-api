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

data MfaStoreError
  = MfaStoreUnavailable Text
  | MfaStoreCorruptData Text
  deriving (Eq)

data StoredTotpEnrollment = StoredTotpEnrollment
  { storedTotpEncryptedSecret :: Text,
    storedTotpConfirmedAtNanoseconds :: Maybe Word64
  }
  deriving (Eq)

data MfaStore = MfaStore
  { saveUnconfirmedTotpEnrollment :: AccountId -> Text -> Word64 -> IO (Either MfaStoreError Bool),
    loadTotpEnrollment :: AccountId -> IO (Either MfaStoreError (Maybe StoredTotpEnrollment)),
    confirmTotpEnrollment :: AccountId -> NonEmpty Text -> Word64 -> IO (Either MfaStoreError Bool)
  }
