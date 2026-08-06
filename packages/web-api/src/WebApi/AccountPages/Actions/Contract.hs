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
import HarchWeb qualified
import HarchWeb.Action
  ( ActionCodec,
    ActionDecoder,
    ActionEndpoint,
    action,
    actionCodec,
    formField,
    postAt,
    singleOrDefault,
    textValue,
  )
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
      (UpdateProfile . ProfileSubmission <$> singleOrDefault "" (formField "intent" textValue)),
    action LogoutAccountTarget (postAt "/logout" (`accountActionPath` LogoutRoute)) (pure LogoutAccount)
  ]

-- The `$!` applications below (on already-WHNF empty-`Text` defaults) exist
-- so HPC ticks each field's default on every invocation instead of treating
-- the shared literal as a once-ticked CAF reference; they have no runtime
-- effect.
{-# ANN registrationSubmission ("HLint: ignore Redundant $!" :: String) #-}
registrationSubmission :: ActionDecoder RegistrationSubmission
registrationSubmission =
  RegistrationSubmission
    <$> (singleOrDefault $! "") (formField "username" textValue)
    <*> (singleOrDefault $! "") (formField "email" textValue)
    <*> (singleOrDefault $! "") (formField "displayName" textValue)
    <*> (singleOrDefault $! "") (formField "password" textValue)

{-# ANN mfaEnrollmentSubmission ("HLint: ignore Redundant $!" :: String) #-}
mfaEnrollmentSubmission :: ActionDecoder MfaEnrollmentSubmission
mfaEnrollmentSubmission =
  MfaEnrollmentSubmission
    <$> (singleOrDefault $! "") (formField "account" textValue)
    <*> (singleOrDefault $! "") (formField "intent" textValue)
    <*> (singleOrDefault $! "") (formField "code" textValue)

{-# ANN loginSubmission ("HLint: ignore Redundant $!" :: String) #-}
loginSubmission :: ActionDecoder LoginSubmission
loginSubmission =
  LoginSubmission
    <$> (singleOrDefault $! "") (formField "email" textValue)
    <*> (singleOrDefault $! "") (formField "username" textValue)
    <*> (singleOrDefault $! "") (formField "password" textValue)
    <*> (singleOrDefault $! "") (formField "proof" textValue)
    <*> (singleOrDefault $! "") (formField "code" textValue)

accountActionPath :: AppRequestContext -> AppRoute -> Text
accountActionPath requestContext route =
  renderRoutePath HarchWeb.RouteRequest {HarchWeb.requestRoute = route, HarchWeb.requestContext = requestContext}
