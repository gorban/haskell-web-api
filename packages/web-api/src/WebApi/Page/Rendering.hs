{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page.Rendering
  ( renderPageBody,
    renderPageBodyForLocale,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.AccountPages.Forms (PendingProfileForm (..))
import WebApi.AccountPages.Rendering
  ( renderLoginPageHtml,
    renderLogoutPageHtml,
    renderMfaEnrollmentPageHtml,
    renderPendingProfileRegionHtml,
    renderRegistrationPageHtml,
    renderVerificationPageHtml,
  )
import WebApi.Page.Model
import WebApi.Route (AppLocale (..), AppRequestContext, defaultRequestContext)

renderPageBody :: AppPageModel -> Text
renderPageBody = HarchWeb.renderHtml . renderPageBodyForLocale defaultRequestContext English

renderPageBodyForLocale :: AppRequestContext -> AppLocale -> AppPageModel -> HarchWeb.Html
renderPageBodyForLocale context locale pageModel =
  case pageModel of
    SecondPage secondPage ->
      HarchWeb.element
        HarchWeb.sectionTag
        [HarchWeb.dataAttribute "page" "second"]
        [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text (secondHeading secondPage)],
          renderPageError (secondErrorMessage secondPage),
          HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (secondSummary secondPage)],
          renderSecondPageHighlights secondPage,
          renderCallToAction (secondPrimaryAction secondPage)
        ]
    SpacesPage spacesPage ->
      HarchWeb.element
        HarchWeb.sectionTag
        [HarchWeb.dataAttribute "page" "spaces"]
        [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text (spacesHeading spacesPage)],
          HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (spacesSummary spacesPage)]
        ]
    RegistrationPage _ registrationForm ->
      renderRegistrationPageHtml context locale registrationForm
    EmailVerificationPage _ verificationForm ->
      renderVerificationPageHtml context locale verificationForm
    MfaEnrollmentPage _ mfaEnrollmentForm ->
      renderMfaEnrollmentPageHtml context locale mfaEnrollmentForm
    LoginPage _ loginForm ->
      renderLoginPageHtml context locale loginForm
    LogoutPage _ ->
      renderLogoutPageHtml context locale
    ProfilePage profilePage ->
      renderProfilePageBody context profilePage
    NotFoundPage notFoundPage ->
      HarchWeb.element
        HarchWeb.sectionTag
        [HarchWeb.dataAttribute "page" "not-found"]
        [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text (notFoundHeading notFoundPage)],
          HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (notFoundSummary notFoundPage)],
          renderCallToAction (notFoundPrimaryAction notFoundPage)
        ]

-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on the pending form's @False@ literal is a last resort, confirmed
-- directly rather than assumed against this module's own test suite.
{-# ANN renderProfilePageBody ("HLint: ignore Redundant $!" :: String) #-}
renderProfilePageBody :: AppRequestContext -> ProfilePageModel -> HarchWeb.Html
renderProfilePageBody context profilePage =
  case profilePage of
    SignedOutProfilePage SignedOutProfilePageDetails {signedOutProfileHeading, signedOutProfileSummary, signedOutProfileSignInAction, signedOutProfileRegistrationAction} ->
      profilePageSection signedOutProfileHeading signedOutProfileSummary [renderCallToAction signedOutProfileSignInAction, renderCallToAction signedOutProfileRegistrationAction]
    PendingProfilePage PendingProfilePageDetails {pendingProfileHeading, pendingProfileSummary, pendingProfileEmail, pendingProfileUsername, pendingProfileDisplayName, pendingProfileResendPath, pendingProfileResendLabel, pendingProfileSignOutAction} ->
      profilePageSection pendingProfileHeading pendingProfileSummary [renderProfileIdentity pendingProfileUsername pendingProfileDisplayName, renderPendingProfileRegionHtml context pendingProfileResendPath ((PendingProfileForm pendingProfileEmail Nothing $! False) pendingProfileResendLabel), renderCallToAction pendingProfileSignOutAction]
    AuthenticatedProfilePage AuthenticatedProfilePageDetails {authenticatedProfileHeading, authenticatedProfileSummary, authenticatedProfileEmail, authenticatedProfileUsername, authenticatedProfileDisplayName, authenticatedProfileSignOutAction} ->
      profilePageSection authenticatedProfileHeading authenticatedProfileSummary [renderProfileIdentity authenticatedProfileUsername authenticatedProfileDisplayName, renderProfileEmail authenticatedProfileEmail, renderCallToAction authenticatedProfileSignOutAction]
    UnavailableProfilePage UnavailableProfilePageDetails {unavailableProfileHeading, unavailableProfileSummary, unavailableProfileSignInAction} ->
      profilePageSection unavailableProfileHeading unavailableProfileSummary [renderCallToAction unavailableProfileSignInAction]

profilePageSection :: Text -> Text -> [HarchWeb.Html] -> HarchWeb.Html
profilePageSection heading summary content =
  HarchWeb.element
    HarchWeb.sectionTag
    [HarchWeb.dataAttribute "page" "profile"]
    [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text heading],
      HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text summary],
      HarchWeb.fragment content
    ]

renderProfileEmail :: Text -> HarchWeb.Html
renderProfileEmail emailAddress =
  HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "profile-email" "true"] [HarchWeb.text emailAddress]

renderProfileIdentity :: Maybe Text -> Maybe Text -> HarchWeb.Html
renderProfileIdentity maybeUsername maybeDisplayName =
  HarchWeb.fragment
    [ maybe (HarchWeb.fragment []) (\username -> HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "profile-username" "true"] [HarchWeb.text username]) maybeUsername,
      maybe (HarchWeb.fragment []) (\displayName -> HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "profile-display-name" "true"] [HarchWeb.text displayName]) maybeDisplayName
    ]

renderHighlights :: [Text] -> HarchWeb.Html
renderHighlights highlights =
  case highlights of
    [] -> HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "empty-state" "true"] [HarchWeb.text "No highlights yet."]
    _ -> HarchWeb.element HarchWeb.listTag [] (map renderHighlight highlights)

renderPageError :: Maybe Text -> HarchWeb.Html
renderPageError maybeErrorMessage =
  case maybeErrorMessage of
    Nothing -> HarchWeb.fragment []
    Just errorMessage -> HarchWeb.element HarchWeb.paragraphTag [HarchWeb.dataAttribute "error-state" "true"] [HarchWeb.text errorMessage]

renderSecondPageHighlights :: SecondPageModel -> HarchWeb.Html
renderSecondPageHighlights secondPage =
  case secondErrorMessage secondPage of
    Nothing -> renderHighlights (secondHighlights secondPage)
    Just _ -> HarchWeb.fragment []

renderHighlight :: Text -> HarchWeb.Html
renderHighlight highlight =
  HarchWeb.element HarchWeb.listItemTag [] [HarchWeb.text highlight]

renderCallToAction :: CallToAction -> HarchWeb.Html
renderCallToAction callToAction =
  HarchWeb.element
    HarchWeb.paragraphTag
    []
    [ HarchWeb.element HarchWeb.anchorTag [HarchWeb.href (callToActionHref callToAction), HarchWeb.dataAttribute "page-link" "true"] [HarchWeb.text (callToActionLabel callToAction)]
    ]
