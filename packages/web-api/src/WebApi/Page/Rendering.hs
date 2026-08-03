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
import WebApi.Route (AppLocale (..))

renderPageBody :: AppPageModel -> Text
renderPageBody = HarchWeb.renderHtml . renderPageBodyForLocale English

renderPageBodyForLocale :: AppLocale -> AppPageModel -> HarchWeb.Html
renderPageBodyForLocale locale pageModel =
  case pageModel of
    HomePage homePage ->
      HarchWeb.element
        HarchWeb.sectionTag
        [HarchWeb.dataAttribute "page" "home"]
        [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text (homeHeading homePage)],
          renderPageError (homeErrorMessage homePage),
          HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (homeSummary homePage)],
          renderCallToAction (homePrimaryAction homePage)
        ]
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
    RegistrationPage registrationPath registrationForm ->
      renderRegistrationPageHtml locale registrationPath registrationForm
    EmailVerificationPage verificationPath verificationForm ->
      renderVerificationPageHtml locale verificationPath verificationForm
    MfaEnrollmentPage mfaEnrollmentPath mfaEnrollmentForm ->
      renderMfaEnrollmentPageHtml locale mfaEnrollmentPath mfaEnrollmentForm
    LoginPage loginPath loginForm ->
      renderLoginPageHtml locale loginPath loginForm
    LogoutPage logoutPath ->
      renderLogoutPageHtml locale logoutPath
    ProfilePage profilePage ->
      renderProfilePageBody profilePage
    NotFoundPage notFoundPage ->
      HarchWeb.element
        HarchWeb.sectionTag
        [HarchWeb.dataAttribute "page" "not-found"]
        [ HarchWeb.element HarchWeb.headingOneTag [HarchWeb.dataAttribute "page-title" "true"] [HarchWeb.text (notFoundHeading notFoundPage)],
          HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (notFoundSummary notFoundPage)],
          renderCallToAction (notFoundPrimaryAction notFoundPage)
        ]

renderProfilePageBody :: ProfilePageModel -> HarchWeb.Html
renderProfilePageBody profilePage =
  case profilePage of
    SignedOutProfilePage {profileHeading, profileSummary, profileSignInAction, profileRegistrationAction} ->
      profilePageSection profileHeading profileSummary [renderCallToAction profileSignInAction, renderCallToAction profileRegistrationAction]
    PendingProfilePage {profileHeading, profileSummary, profileEmail, profileUsername, profileDisplayName, profileResendPath, profileResendLabel, profileSignOutAction} ->
      profilePageSection profileHeading profileSummary [renderProfileIdentity profileUsername profileDisplayName, renderPendingProfileRegionHtml profileResendPath (PendingProfileForm profileEmail Nothing False profileResendLabel), renderCallToAction profileSignOutAction]
    AuthenticatedProfilePage {profileHeading, profileSummary, profileEmail, profileUsername, profileDisplayName, profileSignOutAction} ->
      profilePageSection profileHeading profileSummary [renderProfileIdentity profileUsername profileDisplayName, renderProfileEmail profileEmail, renderCallToAction profileSignOutAction]
    UnavailableProfilePage {profileHeading, profileSummary, profileSignInAction} ->
      profilePageSection profileHeading profileSummary [renderCallToAction profileSignInAction]

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
