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
  deriving (Eq, Show)

data VerificationForm = VerificationForm
  { verificationFormToken :: Text,
    verificationFormMessage :: Maybe Text,
    verificationFormIsError :: Bool
  }
  deriving (Eq, Show)

data PendingProfileForm = PendingProfileForm
  { pendingProfileFormEmail :: Text,
    pendingProfileFormMessage :: Maybe Text,
    pendingProfileFormIsError :: Bool,
    pendingProfileFormResendLabel :: Text
  }
  deriving (Eq, Show)

data MfaEnrollmentForm = MfaEnrollmentForm
  { mfaEnrollmentFormSecret :: Maybe Text,
    mfaEnrollmentFormRecoveryCodes :: [Text],
    mfaEnrollmentFormMessage :: Maybe Text,
    mfaEnrollmentFormIsError :: Bool
  }
  deriving (Eq)

-- | MFA setup values are rendered to the page exactly once, but must never
-- enter diagnostics through a derived 'Show' instance.  Keep the public form
-- state inspectable while redacting the secret and one-time recovery codes.
instance Show MfaEnrollmentForm where
  showsPrec precedence MfaEnrollmentForm {mfaEnrollmentFormMessage, mfaEnrollmentFormIsError} =
    showParen
      (precedence > 10)
      ( showString "MfaEnrollmentForm {mfaEnrollmentFormSecret = <redacted>, mfaEnrollmentFormRecoveryCodes = <redacted>, mfaEnrollmentFormMessage = "
          . shows mfaEnrollmentFormMessage
          . showString ", mfaEnrollmentFormIsError = "
          . shows mfaEnrollmentFormIsError
          . showChar '}'
      )

data LoginForm = LoginForm
  { loginFormEmail :: Text,
    loginFormMessage :: Maybe Text,
    loginFormIsError :: Bool
  }
  deriving (Eq, Show)

emptyRegistrationForm :: RegistrationForm
emptyRegistrationForm = RegistrationForm Text.empty Text.empty Text.empty Nothing False
