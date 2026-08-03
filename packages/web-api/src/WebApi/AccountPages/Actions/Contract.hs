module WebApi.AccountPages.Actions.Contract
  ( AccountAction (..),
    LoginSubmission (..),
    MfaEnrollmentSubmission (..),
    ProfileSubmission (..),
    RegistrationSubmission (..),
    VerificationSubmission (..),
  )
where

import Data.Text (Text)

data AccountAction
  = RegisterAccount RegistrationSubmission
  | VerifyEmail VerificationSubmission
  | EnrollMfa MfaEnrollmentSubmission
  | LoginAccount LoginSubmission
  | UpdateProfile ProfileSubmission
  | LogoutAccount

data RegistrationSubmission = RegistrationSubmission
  { registrationUsernameValue :: Text,
    registrationEmailValue :: Text,
    registrationDisplayNameValue :: Text,
    registrationPasswordValue :: Text
  }

newtype VerificationSubmission = VerificationSubmission {verificationTokenValue :: Text}

data MfaEnrollmentSubmission = MfaEnrollmentSubmission
  { mfaEnrollmentAccountValue :: Text,
    mfaEnrollmentIntentValue :: Text,
    mfaEnrollmentCodeValue :: Text
  }

data LoginSubmission = LoginSubmission
  { loginEmailValue :: Text,
    loginUsernameValue :: Text,
    loginPasswordValue :: Text,
    loginProofValue :: Text,
    loginCodeValue :: Text
  }

newtype ProfileSubmission = ProfileSubmission {profileIntentValue :: Text}
