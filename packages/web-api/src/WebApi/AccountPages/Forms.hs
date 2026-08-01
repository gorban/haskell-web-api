module WebApi.AccountPages.Forms
  ( LoginForm (..),
    MfaEnrollmentForm (..),
    PendingProfileForm (..),
    RegistrationForm (..),
    VerificationForm (..),
    emptyRegistrationForm,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Text,
    registrationFormEmail :: Text,
    registrationFormDisplayName :: Text,
    registrationFormMessage :: Maybe Text,
    registrationFormIsError :: Bool
  }
  deriving (Eq)

data VerificationForm = VerificationForm
  { verificationFormToken :: Text,
    verificationFormMessage :: Maybe Text,
    verificationFormIsError :: Bool
  }
  deriving (Eq)

data PendingProfileForm = PendingProfileForm
  { pendingProfileFormEmail :: Text,
    pendingProfileFormMessage :: Maybe Text,
    pendingProfileFormIsError :: Bool,
    pendingProfileFormResendLabel :: Text
  }
  deriving (Eq)

data MfaEnrollmentForm = MfaEnrollmentForm
  { mfaEnrollmentFormAccountId :: Text,
    mfaEnrollmentFormSecret :: Maybe Text,
    mfaEnrollmentFormRecoveryCodes :: [Text],
    mfaEnrollmentFormMessage :: Maybe Text,
    mfaEnrollmentFormIsError :: Bool
  }
  deriving (Eq)

data LoginForm = LoginForm
  { loginFormEmail :: Text,
    loginFormMessage :: Maybe Text,
    loginFormIsError :: Bool
  }
  deriving (Eq)

emptyRegistrationForm :: RegistrationForm
emptyRegistrationForm = RegistrationForm Text.empty Text.empty Text.empty Nothing False
