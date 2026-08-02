{-# LANGUAGE OverloadedStrings #-}

module WebApi.AccountPages.Actions
  ( handleAccountAction,
    mfaEnrollmentFailureDiagnostics,
  )
where

import HarchWeb qualified
import WebApi.AccountPages.Actions.Common
  ( accountRoutePath,
    attachClientActionFailure,
  )
import WebApi.AccountPages.Actions.Workflows
  ( handleLogin,
    handleLogout,
    handleMfaEnrollment,
    handleProfile,
    handleRegistration,
    handleVerification,
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

handleAccountAction :: AccountWorkflow -> HarchWeb.ClientActionRequest AppRequestContext -> IO (Maybe HarchWeb.ClientActionResponse)
handleAccountAction workflow actionRequest =
  traverse runSelectedAccountAction (accountActionCodec actionRequest)
  where
    runSelectedAccountAction action =
      either attachClientActionFailure id <$> runAppM (AppServices workflow) action

accountActionCodec :: HarchWeb.ClientActionRequest AppRequestContext -> Maybe (AppM HarchWeb.ClientActionResponse HarchWeb.ClientActionResponse)
accountActionCodec actionRequest =
  if HarchWeb.clientActionMethod actionRequest /= "POST"
    then Nothing
    else case HarchWeb.clientActionPath actionRequest of
      path | path == accountRoutePath actionRequest RegistrationRoute -> Just (handleRegistration actionRequest)
      path | path == accountRoutePath actionRequest EmailVerificationRoute -> Just (handleVerification actionRequest)
      path | path == accountRoutePath actionRequest MfaEnrollmentRoute -> Just (handleMfaEnrollment actionRequest)
      path | path == accountRoutePath actionRequest LoginRoute -> Just (handleLogin actionRequest)
      path | path == accountRoutePath actionRequest ProfileRoute -> Just (handleProfile actionRequest)
      path | path == accountRoutePath actionRequest LogoutRoute -> Just (handleLogout actionRequest)
      _ -> Nothing
