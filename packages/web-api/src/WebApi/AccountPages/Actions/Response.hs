-- | Region-patch response construction for account actions.
--
-- Decision (AHI-5-WA-MH, 2026-09-29): keep response metadata capture and
-- action-specific region rendering together. Workflows continue to build
-- the same typed client-action response through the existing Harch boundary;
-- this collaborator owns only the shared application rendering adaptation.
module WebApi.AccountPages.Actions.Response
  ( AccountActionResponseContext,
    accountActionResponseContext,
    registrationResponse,
    verificationResponse,
    mfaEnrollmentResponse,
    loginResponse,
    logoutResponse,
    profileResponse,
    noHeaders,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import Network.HTTP.Types qualified as Http
import WebApi.AccountPages.Actions.Contract (AccountActionTarget (UpdateProfileTarget))
import WebApi.AccountPages.Actions.Support (actionLocale)
import WebApi.AccountPages.Actions.Types (AccountActionRequest, AccountActionResponse)
import WebApi.AccountPages.Forms
  ( LoginForm,
    MfaEnrollmentForm,
    PendingProfileForm,
    RegistrationForm,
    VerificationForm,
  )
import WebApi.AccountPages.Rendering
  ( loginRegion,
    logoutRegion,
    mfaEnrollmentRegion,
    pendingProfileRegion,
    registrationRegion,
    replaceRegionPatch,
    verificationRegion,
  )
import WebApi.Route (AppLocale, AppRequestContext)

-- | Response metadata is captured once before rendering. The existing action
-- boundary still carries route context for cookie parsing, but each renderer
-- receives one cohesive value instead of manually assembling a response.
data AccountActionResponseContext = AccountActionResponseContext
  { accountActionResponseLocale :: AppLocale,
    accountActionResponseRequestContext :: AppRequestContext,
    accountActionResponseStatus :: Http.Status,
    accountActionResponseFocusId :: Maybe HarchWeb.ElementId,
    accountActionResponseHeaders :: Http.ResponseHeaders
  }

-- | Derive response metadata from the one action request before a workflow
-- chooses its form/body. This keeps locale and route context coupled to the
-- request that supplied them instead of passing transposable copies to every
-- renderer.
accountActionResponseContext :: AccountActionRequest -> Http.Status -> Maybe HarchWeb.ElementId -> Http.ResponseHeaders -> AccountActionResponseContext
accountActionResponseContext actionRequest status focusId headers =
  AccountActionResponseContext
    { accountActionResponseLocale = actionLocale actionRequest,
      accountActionResponseRequestContext = HarchWeb.clientActionContext actionRequest,
      accountActionResponseStatus = status,
      accountActionResponseFocusId = focusId,
      accountActionResponseHeaders = headers
    }

registrationResponse :: AccountActionResponseContext -> RegistrationForm -> AccountActionResponse
registrationResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (registrationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

verificationResponse :: AccountActionResponseContext -> VerificationForm -> AccountActionResponse
verificationResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (verificationRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

mfaEnrollmentResponse :: AccountActionResponseContext -> MfaEnrollmentForm -> AccountActionResponse
mfaEnrollmentResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (mfaEnrollmentRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

loginResponse :: AccountActionResponseContext -> LoginForm -> AccountActionResponse
loginResponse responseContext form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (loginRegion requestContext locale form),
      HarchWeb.clientActionFocusId = focusId,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    focusId = accountActionResponseFocusId responseContext
    headers = accountActionResponseHeaders responseContext

logoutResponse :: AccountActionResponseContext -> Maybe (Text, Bool) -> AccountActionResponse
logoutResponse responseContext message =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (logoutRegion requestContext locale message),
      HarchWeb.clientActionFocusId = accountActionResponseFocusId responseContext,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = headers,
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }
  where
    locale = accountActionResponseLocale responseContext
    requestContext = accountActionResponseRequestContext responseContext
    status = accountActionResponseStatus responseContext
    headers = accountActionResponseHeaders responseContext

profileResponse :: AccountActionRequest -> Http.Status -> PendingProfileForm -> AccountActionResponse
profileResponse actionRequest status form =
  HarchWeb.ClientActionResponse
    { HarchWeb.clientActionStatus = status,
      HarchWeb.clientActionPatches = replaceRegionPatch (pendingProfileRegion (HarchWeb.clientActionContext actionRequest) UpdateProfileTarget form),
      HarchWeb.clientActionFocusId = Nothing,
      HarchWeb.clientActionNavigation = HarchWeb.StayOnCurrentRoute,
      HarchWeb.clientActionStorageCleanup = HarchWeb.noClientStorageCleanup,
      HarchWeb.clientActionFailureDestinations = HarchWeb.noClientActionFailureDestinations,
      HarchWeb.clientActionHeaders = [],
      HarchWeb.clientActionObservabilityAttributes = [],
      HarchWeb.clientActionLogEntries = []
    }

-- | A single named binding for "no extra response headers," used at every
-- call site that would otherwise repeat an empty-list literal of this exact
-- type. Repeating the literal risks the CSE-sharing HPC gap this codebase
-- has already hit more than once (see the AC decision record in
-- docs/design-guidance.md): GHC can common up textually identical literals
-- into one CAF, silently leaving every call site but one permanently
-- unticked even though each is genuinely reached.
noHeaders :: Http.ResponseHeaders
noHeaders = []
