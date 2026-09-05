{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page.Rendering
  ( renderPageBody,
    renderPageBodyForLocale,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.AccountPages.Forms (initialPendingProfileForm)
import WebApi.AccountPages.Rendering
  ( renderLoginPageHtml,
    renderLogoutPageHtml,
    renderMfaEnrollmentPageHtml,
    renderPendingProfileRegionHtml,
    renderRegistrationPageHtml,
    renderVerificationPageHtml,
  )
import WebApi.Components.AppControls (languageSelectionLinks)
import WebApi.Components.PageFrame
  ( PageFrameProps (..),
    PageKind (..),
    pageFrame,
  )
import WebApi.Components.Profile
  ( ProfileIdentityProps (..),
    profileIdentity,
  )
import WebApi.Page.Model
import WebApi.Route (AppLocale (..), AppRequestContext, defaultRequestContext)

renderPageBody :: AppPageModel -> Text
renderPageBody = HarchWeb.renderHtml . renderPageBodyForLocale defaultRequestContext English

renderPageBodyForLocale :: AppRequestContext -> AppLocale -> AppPageModel -> HarchWeb.Html
renderPageBodyForLocale context locale pageModel =
  case pageModel of
    SecondPage secondPage ->
      pageFrame
        PageFrameProps
          { pageFrameKind = SecondPageFrame,
            pageFrameHeading = secondHeading secondPage,
            pageFrameSummary = Just (secondSummary secondPage),
            pageFrameContent =
              [ renderPageError (secondErrorMessage secondPage),
                renderSecondPageHighlights secondPage,
                renderCallToAction (secondPrimaryAction secondPage)
              ]
          }
    SpacesPage spacesPage ->
      pageFrame
        PageFrameProps
          { pageFrameKind = SpacesPageFrame,
            pageFrameHeading = spacesHeading spacesPage,
            pageFrameSummary = Just (spacesSummary spacesPage),
            pageFrameContent = []
          }
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
    LanguagePage languagePage ->
      pageFrame
        PageFrameProps
          { pageFrameKind = LanguagePageFrame,
            pageFrameHeading = languageHeading languagePage,
            pageFrameSummary = Just (languageSummary languagePage),
            pageFrameContent = [languageSelectionLinks context]
          }
    HelpPage helpPage ->
      pageFrame
        PageFrameProps
          { pageFrameKind = HelpPageFrame,
            pageFrameHeading = helpHeading helpPage,
            pageFrameSummary = Just (helpSummary helpPage),
            pageFrameContent =
              [ HarchWeb.element HarchWeb.paragraphTag [] [HarchWeb.text (helpAccountGuidance helpPage)],
                renderCallToAction (helpSignInAction helpPage),
                renderCallToAction (helpRegistrationAction helpPage)
              ]
          }
    NotFoundPage notFoundPage ->
      pageFrame
        PageFrameProps
          { pageFrameKind = NotFoundPageFrame,
            pageFrameHeading = notFoundHeading notFoundPage,
            pageFrameSummary = Just (notFoundSummary notFoundPage),
            pageFrameContent = [renderCallToAction (notFoundPrimaryAction notFoundPage)]
          }

renderProfilePageBody :: AppRequestContext -> ProfilePageModel -> HarchWeb.Html
renderProfilePageBody context profilePage =
  case profilePage of
    SignedOutProfilePage SignedOutProfilePageDetails {signedOutProfileHeading, signedOutProfileSummary, signedOutProfileSignInAction, signedOutProfileRegistrationAction} ->
      profilePageSection signedOutProfileHeading signedOutProfileSummary [renderCallToAction signedOutProfileSignInAction, renderCallToAction signedOutProfileRegistrationAction]
    PendingProfilePage PendingProfilePageDetails {pendingProfileHeading, pendingProfileSummary, pendingProfileEmail, pendingProfileUsername, pendingProfileDisplayName, pendingProfileResendPath, pendingProfileResendLabel, pendingProfileSignOutAction} ->
      profilePageSection pendingProfileHeading pendingProfileSummary [profileIdentity (ProfileIdentityProps pendingProfileUsername pendingProfileDisplayName Nothing), renderPendingProfileRegionHtml context pendingProfileResendPath (initialPendingProfileForm pendingProfileEmail pendingProfileResendLabel), renderCallToAction pendingProfileSignOutAction]
    AuthenticatedProfilePage AuthenticatedProfilePageDetails {authenticatedProfileHeading, authenticatedProfileSummary, authenticatedProfileEmail, authenticatedProfileUsername, authenticatedProfileDisplayName, authenticatedProfileSignOutAction} ->
      profilePageSection authenticatedProfileHeading authenticatedProfileSummary [profileIdentity (ProfileIdentityProps authenticatedProfileUsername authenticatedProfileDisplayName (Just authenticatedProfileEmail)), renderCallToAction authenticatedProfileSignOutAction]
    UnavailableProfilePage UnavailableProfilePageDetails {unavailableProfileHeading, unavailableProfileSummary, unavailableProfileSignInAction} ->
      profilePageSection unavailableProfileHeading unavailableProfileSummary [renderCallToAction unavailableProfileSignInAction]

profilePageSection :: Text -> Text -> [HarchWeb.Html] -> HarchWeb.Html
profilePageSection heading summary content =
  pageFrame
    PageFrameProps
      { pageFrameKind = ProfilePageFrame,
        pageFrameHeading = heading,
        pageFrameSummary = Just summary,
        pageFrameContent = content
      }

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
