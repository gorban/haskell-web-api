{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page.Building
  ( buildPageModel,
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildProfilePageModel,
    buildUnavailableProfilePageModel,
    buildCallToActionHref,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Email qualified as Email
import HarchWeb.Username qualified as Username
import WebApi.Account (AccountProfile (..))
import WebApi.AccountPages.Actions.Contract (AccountActionTarget (..))
import WebApi.AccountPages.Forms
  ( LoginForm (..),
    MfaEnrollmentForm (..),
    VerificationForm (..),
    emptyRegistrationForm,
  )
import WebApi.Database (PageRepository, defaultPageRepository)
import WebApi.Localization (AppMessage (..), localizedMessage)
import WebApi.Page.Model
import WebApi.Profile (ProfileState (..))
import WebApi.Route
  ( AppRequestContext (..),
    AppRoute (..),
    renderRoutePath,
  )
import WebApi.RouteData
  ( RouteDataResult (..),
    SecondRouteData (..),
    selectRouteDataWithDatabase,
  )

buildPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> IO AppPageModel
buildPageModel = buildPageModelWithDatabase defaultPageRepository

buildPageModelWithDatabase :: PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO AppPageModel
buildPageModelWithDatabase pageRepository routeRequest =
  fmap
    (buildPageModelFromRouteData routeRequest)
    (selectRouteDataWithDatabase pageRepository routeRequest)

buildPageModelFromRouteData :: HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult -> AppPageModel
buildPageModelFromRouteData routeRequest routeData =
  case routeData of
    SecondRouteDataResult secondRouteDataResult ->
      buildSecondPageModel routeRequest secondRouteDataResult
    SpacesRouteDataResult ->
      SpacesPage
        SpacesPageModel
          { spacesHeading = localizedText routeRequest SiteUnderConstruction,
            spacesSummary = localizedText routeRequest FollowThisSpace
          }
    RegistrationRouteDataResult ->
      RegistrationPage
        RegisterAccountTarget
        emptyRegistrationForm
    EmailVerificationRouteDataResult ->
      EmailVerificationPage
        VerifyEmailTarget
        VerificationForm
          { verificationFormToken =
              fromMaybe Text.empty (lookup "token" (requestQueryParameters (HarchWeb.requestContext routeRequest))),
            verificationFormMessage = Nothing,
            verificationFormIsError = False
          }
    MfaEnrollmentRouteDataResult ->
      MfaEnrollmentPage
        EnrollMfaTarget
        (MfaEnrollmentForm Nothing [] Nothing False)
    LoginRouteDataResult ->
      LoginPage
        LoginAccountTarget
        (LoginForm Text.empty Nothing False)
    LogoutRouteDataResult ->
      LogoutPage
        LogoutAccountTarget
    ProfileRouteDataResult ->
      ProfilePage (buildProfilePageModel routeRequest ProfileUnauthenticated)
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = localizedText routeRequest NotFound,
            notFoundSummary = localizedText routeRequest NotFoundSummary,
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute (localizedText routeRequest ReturnHome)
          }

buildProfilePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> ProfileState -> ProfilePageModel
buildProfilePageModel routeRequest profileState =
  case profileState of
    ProfileUnauthenticated ->
      SignedOutProfilePage
        SignedOutProfilePageDetails
          { signedOutProfileHeading = localizedText routeRequest Profile,
            signedOutProfileSummary = localizedText routeRequest SignedOutProfileSummary,
            signedOutProfileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest SignIn),
            signedOutProfileRegistrationAction = buildCallToAction routeRequest RegistrationRoute (localizedText routeRequest CreateAccount)
          }
    ProfilePending profile ->
      PendingProfilePage
        PendingProfilePageDetails
          { pendingProfileHeading = localizedText routeRequest Profile,
            pendingProfileSummary = localizedText routeRequest VerifyEmailBeforeContinuing,
            pendingProfileEmail = Email.emailAddressText (accountProfileEmail profile),
            pendingProfileUsername = Username.usernameText <$> accountProfileUsername profile,
            pendingProfileDisplayName = accountProfileDisplayName profile,
            pendingProfileResendPath = UpdateProfileTarget,
            pendingProfileResendLabel = localizedText routeRequest ResendVerificationEmail,
            pendingProfileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest SignOut)
          }
    ProfileAuthenticated profile ->
      AuthenticatedProfilePage
        AuthenticatedProfilePageDetails
          { authenticatedProfileHeading = localizedText routeRequest Profile,
            authenticatedProfileSummary = localizedText routeRequest AuthenticatedProfileSummary,
            authenticatedProfileEmail = Email.emailAddressText (accountProfileEmail profile),
            authenticatedProfileUsername = Username.usernameText <$> accountProfileUsername profile,
            authenticatedProfileDisplayName = accountProfileDisplayName profile,
            authenticatedProfileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest SignOut)
          }

buildUnavailableProfilePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel
buildUnavailableProfilePageModel routeRequest =
  ProfilePage
    ( UnavailableProfilePage
        UnavailableProfilePageDetails
          { unavailableProfileHeading = localizedText routeRequest Profile,
            unavailableProfileSummary = localizedText routeRequest UnavailableProfileSummary,
            unavailableProfileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest SignIn)
          }
    )

buildSecondPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError SecondRouteData -> AppPageModel
buildSecondPageModel routeRequest secondRouteDataResult =
  let returnHome = buildCallToAction routeRequest HomeRoute (localizedText routeRequest ReturnHome)
   in case secondRouteDataResult of
        Right secondRouteData ->
          SecondPage
            SecondPageModel
              { secondHeading = localizedText routeRequest Second,
                secondSummary = secondRouteSummary secondRouteData,
                secondHighlights = secondRouteHighlights secondRouteData,
                secondErrorMessage = Nothing,
                secondPrimaryAction = returnHome
              }
        Left _ ->
          SecondPage
            SecondPageModel
              { secondHeading = localizedText routeRequest Second,
                secondSummary = localizedText routeRequest SecondPageUnavailable,
                secondHighlights = [],
                secondErrorMessage = Just (localizedText routeRequest SecondPageLoadFailed),
                secondPrimaryAction = returnHome
              }

buildCallToAction :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppRoute -> Text -> CallToAction
buildCallToAction routeRequest route label =
  CallToAction
    { callToActionLabel = label,
      callToActionRoute = route,
      -- Always a relative path built from this application's own typed
      -- route table, never from unvalidated caller text — a rejection here
      -- would mean a route itself renders an unsafe URL, a programming
      -- mistake in the route table, not a runtime condition this
      -- function's own callers need to handle. The failure path is extracted
      -- into 'buildCallToActionHref' (same shape as
      -- 'WebApi.Login.requiredPasswordHashOrDie') so a dedicated test can
      -- force it directly with deliberately unsafe text, rather than forcing
      -- the diagnostic message eagerly at this always-safe call site.
      callToActionHref = buildCallToActionHref renderedPath
    }
  where
    renderedPath =
      renderRoutePath
        HarchWeb.RouteRequest
          { HarchWeb.requestRoute = route,
            HarchWeb.requestContext = HarchWeb.requestContext routeRequest
          }

-- | Requires a route's rendered path to already be a safe relative URL,
-- so its failure diagnostic can be forced directly by a test — see
-- 'Unit.WebApiSpec' for the unsafe-path case.
buildCallToActionHref :: Text -> HarchWeb.SafeUrl
buildCallToActionHref renderedPath =
  HarchWeb.requiredSafeUrlOrDie
    ("buildCallToAction: rendered an unsafe URL: " <> renderedPath)
    (HarchWeb.mkSafeUrl renderedPath)

localizedText :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppMessage -> Text
localizedText routeRequest = localizedMessage (requestLocale (HarchWeb.requestContext routeRequest))
