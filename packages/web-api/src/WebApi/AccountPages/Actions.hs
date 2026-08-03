{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions
  ( AccountAction,
    decodeAccountAction,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import Data.Maybe (fromMaybe)
import HarchWeb qualified
import WebApi.AccountPages.Actions.Common
  ( accountRoutePathForContext,
    attachClientActionFailure,
  )
import WebApi.AccountPages.Actions.Contract
import WebApi.AccountPages.Actions.Workflows
  ( handleLoginSubmission,
    handleLogout,
    handleMfaEnrollmentSubmission,
    handleProfileSubmission,
    handleRegistrationSubmission,
    handleVerificationSubmission,
    mfaEnrollmentFailureDiagnostics,
  )
import WebApi.AppEffect
  ( AccountWorkflow,
    AppM,
    AppServices (..),
    runAppM,
  )
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
  )

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

type AccountActionWorkflow = AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse

handleAccountAction :: AccountWorkflow -> AccountActionRequest -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  Just <$> runSelectedAccountAction (accountActionCodec actionRequest)
  where
    runSelectedAccountAction action =
      either attachClientActionFailure id <$> runAppM (AppServices workflow) action

accountActionCodec :: AccountActionRequest -> AccountActionWorkflow
accountActionCodec actionRequest =
  case HarchWeb.clientAction actionRequest of
    RegisterAccount submission -> handleRegistrationSubmission actionRequest submission
    VerifyEmail submission -> handleVerificationSubmission actionRequest submission
    EnrollMfa submission -> handleMfaEnrollmentSubmission actionRequest submission
    LoginAccount submission -> handleLoginSubmission actionRequest submission
    UpdateProfile submission -> handleProfileSubmission actionRequest submission
    LogoutAccount -> handleLogout actionRequest

decodeAccountAction :: HarchWeb.ClientActionPayload AppRequestContext -> Maybe AccountAction
decodeAccountAction actionPayload
  | HarchWeb.clientActionMethod actionPayload /= "POST" = Nothing
  | otherwise =
      case HarchWeb.clientActionPath actionPayload of
        path
          | path == routePath RegistrationRoute ->
              Just
                ( RegisterAccount
                    RegistrationSubmission
                      { registrationUsernameValue = field "username",
                        registrationEmailValue = field "email",
                        registrationDisplayNameValue = field "displayName",
                        registrationPasswordValue = field "password"
                      }
                )
        path
          | path == routePath EmailVerificationRoute ->
              Just (VerifyEmail (VerificationSubmission (field "token")))
        path
          | path == routePath MfaEnrollmentRoute ->
              Just
                ( EnrollMfa
                    MfaEnrollmentSubmission
                      { mfaEnrollmentAccountValue = field "account",
                        mfaEnrollmentIntentValue = field "intent",
                        mfaEnrollmentCodeValue = field "code"
                      }
                )
        path
          | path == routePath LoginRoute ->
              Just
                ( LoginAccount
                    LoginSubmission
                      { loginEmailValue = field "email",
                        loginUsernameValue = field "username",
                        loginPasswordValue = field "password",
                        loginProofValue = field "proof",
                        loginCodeValue = field "code"
                      }
                )
        path | path == routePath ProfileRoute -> Just (UpdateProfile (ProfileSubmission (field "intent")))
        path | path == routePath LogoutRoute -> Just LogoutAccount
        _ -> Nothing
  where
    field name = fromMaybe "" (lookup name (HarchWeb.clientActionFields actionPayload))
    routePath = accountRoutePathForContext (HarchWeb.clientActionPayloadContext actionPayload)
