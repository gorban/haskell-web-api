module WebApi.AccountPages.Forms
  ( LoginForm (..),
    FormFeedback (..),
    FormStatus (..),
    FormStatusKind (..),
    MfaEnrollmentForm (..),
    PendingProfileForm (..),
    RegistrationForm (..),
    RegistrationValidationError (..),
    VerificationForm (..),
    emptyRegistrationForm,
    initialPendingProfileForm,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text

data FormStatusKind = FormStatusSuccess | FormStatusFailure
  deriving (Eq, Show)

data FormStatus = FormStatus
  { formStatusMessage :: Text,
    formStatusKind :: FormStatusKind
  }
  deriving (Eq, Show)

-- | Validation rejection and lifecycle status are different states.  Keeping
-- them separate prevents a page-level message/boolean pair from standing in
-- for field relationships, while preserving success and infrastructure
-- feedback that does not belong to one field.
data FormFeedback error
  = FormReady
  | FormRejected (NonEmpty error)
  | FormStatusMessage FormStatus
  deriving (Eq, Show)

data RegistrationValidationError
  = RegistrationUsernameInvalid
  | RegistrationEmailInvalid
  | RegistrationPasswordTooShort
  | RegistrationUsernameUnavailable
  deriving (Eq, Show)

data RegistrationForm = RegistrationForm
  { registrationFormUsername :: Text,
    registrationFormEmail :: Text,
    registrationFormDisplayName :: Text,
    registrationFormFeedback :: FormFeedback RegistrationValidationError
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
emptyRegistrationForm = RegistrationForm Text.empty Text.empty Text.empty FormReady

-- | The server-rendered pending-profile page has no action outcome yet.  This
-- constructor keeps that valid initial state named and reusable rather than
-- making callers repeat an unrelated error flag.
initialPendingProfileForm :: Text -> Text -> PendingProfileForm
initialPendingProfileForm emailAddress =
  PendingProfileForm emailAddress Nothing False
