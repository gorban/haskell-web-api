{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions
  ( AccountAction,
    AccountActionDecodeError (..),
    decodeAccountActionResult,
    decodeAccountActionWithError,
    handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import Data.Text (Text)
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

newtype AccountActionDecodeError
  = DuplicateAccountActionField Text

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

decodeAccountActionResult :: HarchWeb.ClientActionPayload AppRequestContext -> HarchWeb.ClientActionDecodeResult AccountAction
decodeAccountActionResult actionPayload =
  case decodeAccountActionWithError actionPayload of
    Left _ -> HarchWeb.MalformedClientAction
    Right Nothing -> HarchWeb.UnrecognizedClientAction
    Right (Just action) -> HarchWeb.DecodedClientAction action

decodeAccountActionWithError :: HarchWeb.ClientActionPayload AppRequestContext -> Either AccountActionDecodeError (Maybe AccountAction)
decodeAccountActionWithError actionPayload
  | HarchWeb.clientActionMethod actionPayload /= "POST" = Right Nothing
  | otherwise =
      case HarchWeb.clientActionPath actionPayload of
        path
          | path == routePath RegistrationRoute ->
              Just . RegisterAccount
                <$> ( RegistrationSubmission
                        <$> field "username"
                        <*> field "email"
                        <*> field "displayName"
                        <*> field "password"
                    )
        path
          | path == routePath EmailVerificationRoute ->
              Just . VerifyEmail . VerificationSubmission <$> field "token"
        path
          | path == routePath MfaEnrollmentRoute ->
              Just . EnrollMfa
                <$> ( MfaEnrollmentSubmission
                        <$> field "account"
                        <*> field "intent"
                        <*> field "code"
                    )
        path
          | path == routePath LoginRoute ->
              Just . LoginAccount
                <$> ( LoginSubmission
                        <$> field "email"
                        <*> field "username"
                        <*> field "password"
                        <*> field "proof"
                        <*> field "code"
                    )
        path | path == routePath ProfileRoute -> Just . UpdateProfile . ProfileSubmission <$> field "intent"
        path | path == routePath LogoutRoute -> Right (Just LogoutAccount)
        _ -> Right Nothing
  where
    field name =
      case [fieldValue | (fieldName, fieldValue) <- HarchWeb.clientActionFields actionPayload, fieldName == name] of
        [] -> Right ""
        [fieldValue] -> Right fieldValue
        _ -> Left (DuplicateAccountActionField name)
    routePath = accountRoutePathForContext (HarchWeb.clientActionPayloadContext actionPayload)
