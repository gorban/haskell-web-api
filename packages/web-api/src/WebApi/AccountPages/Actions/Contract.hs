{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions.Contract
  ( AccountAction (..),
    AccountActionTarget (..),
    accountActions,
    buildActionCodecOrDie,
    LoginSubmission (..),
    MfaEnrollmentSubmission (..),
    ProfileSubmission (..),
    RegistrationSubmission (..),
    VerificationSubmission (..),
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Action
  ( ActionCodec,
    ActionDecoder,
    ActionEndpoint,
    FieldValue,
    action,
    actionCodec,
    formField,
    parseField,
    postAt,
    singleOrDefault,
    textValue,
  )
import WebApi.AccountPages.Forms (LoginProofChoice (..))
import WebApi.Route (AppRequestContext, AppRoute (..), renderRoutePath)

data AccountActionTarget
  = RegisterAccountTarget
  | VerifyEmailTarget
  | EnrollMfaTarget
  | LoginAccountTarget
  | UpdateProfileTarget
  | LogoutAccountTarget
  deriving (Eq, Show)

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
  { mfaEnrollmentIntentValue :: Text,
    mfaEnrollmentCodeValue :: Text
  }

data LoginSubmission = LoginSubmission
  { loginIdentifierValue :: Text,
    loginPasswordValue :: Text,
    loginProofChoiceValue :: Maybe LoginProofChoice,
    loginTotpCodeValue :: Text,
    loginRecoveryCodeValue :: Text
  }

newtype ProfileSubmission = ProfileSubmission {profileIntentValue :: Text}

accountActions :: ActionCodec AccountActionTarget AppRequestContext AccountAction
accountActions = buildActionCodecOrDie accountActionEndpoints

-- | Build a codec from a statically-known-duplicate-free endpoint list, or
-- crash naming the offending declaration. @accountActionEndpoints@ is
-- reviewed to never trigger the error branch, so it is exercised directly
-- (with a deliberately duplicate list) by a dedicated unit test instead.
buildActionCodecOrDie :: [ActionEndpoint target context action] -> ActionCodec target context action
buildActionCodecOrDie endpoints =
  case actionCodec endpoints of
    Left codecError -> error (show codecError)
    Right codec -> codec

accountActionEndpoints :: [ActionEndpoint AccountActionTarget AppRequestContext AccountAction]
accountActionEndpoints =
  [ action
      RegisterAccountTarget
      (postAt "/register" (`accountActionPath` RegistrationRoute))
      (RegisterAccount <$> registrationSubmission),
    action
      VerifyEmailTarget
      (postAt "/verify-email" (`accountActionPath` EmailVerificationRoute))
      (VerifyEmail . VerificationSubmission <$> singleOrDefault "" (formField "token" textValue)),
    action
      EnrollMfaTarget
      (postAt "/mfa" (`accountActionPath` MfaEnrollmentRoute))
      (EnrollMfa <$> mfaEnrollmentSubmission),
    action
      LoginAccountTarget
      (postAt "/login" (`accountActionPath` LoginRoute))
      (LoginAccount <$> loginSubmission),
    action
      UpdateProfileTarget
      (postAt "/profile" (`accountActionPath` ProfileRoute))
      (UpdateProfile . ProfileSubmission <$> textFormField "intent" textValue),
    action LogoutAccountTarget (postAt "/logout" (`accountActionPath` LogoutRoute)) (pure LogoutAccount)
  ]

-- | The text-field convention is part of the action contract: missing fields
-- decode to empty text while duplicate and malformed fields still carry their
-- decoder errors. Keeping it in one codec builder prevents separate defaults
-- from becoming coverage-oriented evaluation sites.
textFormField :: Text -> FieldValue Text -> ActionDecoder Text
textFormField fieldName field = singleOrDefault Text.empty (formField fieldName field)

registrationSubmission :: ActionDecoder RegistrationSubmission
registrationSubmission =
  RegistrationSubmission
    <$> textFormField "username" textValue
    <*> textFormField "email" textValue
    <*> textFormField "displayName" textValue
    <*> textFormField "password" textValue

mfaEnrollmentSubmission :: ActionDecoder MfaEnrollmentSubmission
mfaEnrollmentSubmission =
  MfaEnrollmentSubmission
    <$> textFormField "intent" textValue
    <*> textFormField "code" textValue

loginSubmission :: ActionDecoder LoginSubmission
loginSubmission =
  LoginSubmission
    <$> textFormField "identifier" textValue
    <*> textFormField "password" textValue
    <*> singleOrDefault Nothing (formField "proof" (parseField (Just . loginProofChoice)))
    <*> textFormField "totpCode" textValue
    <*> textFormField "recoveryCode" textValue

loginProofChoice :: Text -> Maybe LoginProofChoice
loginProofChoice proofValue =
  case proofValue of
    "totp" -> Just LoginAuthenticatorProof
    "recovery" -> Just LoginRecoveryProof
    _ -> Nothing

accountActionPath :: AppRequestContext -> AppRoute -> Text
accountActionPath requestContext route =
  renderRoutePath HarchWeb.RouteRequest {HarchWeb.requestRoute = route, HarchWeb.requestContext = requestContext}
