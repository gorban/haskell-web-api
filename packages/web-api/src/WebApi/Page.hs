{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page
  ( AppPageModel (..),
    CallToAction (..),
    HomePageModel (..),
    NotFoundPageModel (..),
    ProfilePageModel (..),
    SecondPageModel (..),
    SpacesPageModel (..),
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildPageModel,
    renderPageFromRouteData,
    renderUnavailableProfilePage,
    renderProfilePageWithState,
    renderPageWithDatabase,
    renderPage,
    renderPageBody,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Email qualified as Email
import WebApi.Account (AccountProfile (..))
import WebApi.AccountPages
  ( LoginForm (..),
    MfaEnrollmentForm (..),
    PendingProfileForm (..),
    RegistrationForm (..),
    VerificationForm (..),
    emptyRegistrationForm,
    renderLoginPage,
    renderLogoutPage,
    renderMfaEnrollmentPage,
    renderPendingProfileRegion,
    renderRegistrationPage,
    renderVerificationPage,
  )
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.Config (AppConfig (..))
import WebApi.Database
  ( DatabaseEffect,
    defaultDatabaseEffect,
  )
import WebApi.Profile (ProfileState (..))
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    RouteMetadata (routePageTitle),
    renderRoutePath,
    routeMetadata,
  )
import WebApi.RouteData
  ( HomeRouteData (..),
    RouteDataResult (..),
    SecondRouteData (..),
    selectRouteDataWithDatabase,
  )

data CallToAction = CallToAction
  { callToActionLabel :: Text,
    callToActionRoute :: AppRoute,
    callToActionHref :: Text
  }
  deriving (Eq, Show)

data HomePageModel = HomePageModel
  { homeHeading :: Text,
    homeSummary :: Text,
    homeErrorMessage :: Maybe Text,
    homePrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data SecondPageModel = SecondPageModel
  { secondHeading :: Text,
    secondSummary :: Text,
    secondHighlights :: [Text],
    secondErrorMessage :: Maybe Text,
    secondPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data SpacesPageModel = SpacesPageModel
  { spacesHeading :: Text,
    spacesSummary :: Text
  }
  deriving (Eq, Show)

data NotFoundPageModel = NotFoundPageModel
  { notFoundHeading :: Text,
    notFoundSummary :: Text,
    notFoundPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data ProfilePageModel
  = SignedOutProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileSignInAction :: CallToAction,
        profileRegistrationAction :: CallToAction
      }
  | PendingProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileEmail :: Text,
        profileResendPath :: Text,
        profileResendLabel :: Text,
        profileSignOutAction :: CallToAction
      }
  | AuthenticatedProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileEmail :: Text,
        profileSignOutAction :: CallToAction
      }
  | UnavailableProfilePage
      { profileHeading :: Text,
        profileSummary :: Text,
        profileSignInAction :: CallToAction
      }

data AppPageModel
  = HomePage HomePageModel
  | SecondPage SecondPageModel
  | SpacesPage SpacesPageModel
  | RegistrationPage Text RegistrationForm
  | EmailVerificationPage Text VerificationForm
  | MfaEnrollmentPage Text MfaEnrollmentForm
  | LoginPage Text LoginForm
  | LogoutPage Text
  | ProfilePage ProfilePageModel
  | NotFoundPage NotFoundPageModel

instance Eq AppPageModel where
  HomePage left == HomePage right = left == right
  SecondPage left == SecondPage right = left == right
  SpacesPage left == SpacesPage right = left == right
  RegistrationPage leftPath leftForm == RegistrationPage rightPath rightForm = leftPath == rightPath && leftForm == rightForm
  EmailVerificationPage leftPath leftForm == EmailVerificationPage rightPath rightForm = leftPath == rightPath && leftForm == rightForm
  MfaEnrollmentPage leftPath leftForm == MfaEnrollmentPage rightPath rightForm = leftPath == rightPath && leftForm == rightForm
  LoginPage leftPath leftForm == LoginPage rightPath rightForm = leftPath == rightPath && leftForm == rightForm
  LogoutPage leftPath == LogoutPage rightPath = leftPath == rightPath
  ProfilePage SignedOutProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileSignInAction = leftSignInAction, profileRegistrationAction = leftRegistrationAction}
    == ProfilePage SignedOutProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileSignInAction = rightSignInAction, profileRegistrationAction = rightRegistrationAction} =
      leftHeading == rightHeading
        && leftSummary == rightSummary
        && leftSignInAction == rightSignInAction
        && leftRegistrationAction == rightRegistrationAction
  ProfilePage PendingProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileEmail = leftEmail, profileResendPath = leftResendPath, profileResendLabel = leftResendLabel, profileSignOutAction = leftSignOutAction}
    == ProfilePage PendingProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileEmail = rightEmail, profileResendPath = rightResendPath, profileResendLabel = rightResendLabel, profileSignOutAction = rightSignOutAction} =
      leftHeading == rightHeading
        && leftSummary == rightSummary
        && leftEmail == rightEmail
        && leftResendPath == rightResendPath
        && leftResendLabel == rightResendLabel
        && leftSignOutAction == rightSignOutAction
  ProfilePage AuthenticatedProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileEmail = leftEmail, profileSignOutAction = leftSignOutAction}
    == ProfilePage AuthenticatedProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileEmail = rightEmail, profileSignOutAction = rightSignOutAction} =
      leftHeading == rightHeading
        && leftSummary == rightSummary
        && leftEmail == rightEmail
        && leftSignOutAction == rightSignOutAction
  ProfilePage UnavailableProfilePage {profileHeading = leftHeading, profileSummary = leftSummary, profileSignInAction = leftSignInAction}
    == ProfilePage UnavailableProfilePage {profileHeading = rightHeading, profileSummary = rightSummary, profileSignInAction = rightSignInAction} =
      leftHeading == rightHeading
        && leftSummary == rightSummary
        && leftSignInAction == rightSignInAction
  NotFoundPage left == NotFoundPage right = left == right
  _ == _ = False

instance Show AppPageModel where
  showsPrec precedence (HomePage homePage) =
    showParen (precedence > 10) (showString "HomePage " . showsPrec 11 homePage)
  showsPrec precedence (SecondPage secondPage) =
    showParen (precedence > 10) (showString "SecondPage " . showsPrec 11 secondPage)
  showsPrec precedence (SpacesPage spacesPage) =
    showParen (precedence > 10) (showString "SpacesPage " . showsPrec 11 spacesPage)
  showsPrec precedence (RegistrationPage registrationPath RegistrationForm {registrationFormEmail, registrationFormMessage, registrationFormIsError}) =
    showParen
      (precedence > 10)
      ( showString "RegistrationPage "
          . shows registrationPath
          . showChar ' '
          . showString "(RegistrationForm {registrationFormEmail = "
          . shows registrationFormEmail
          . showString ", registrationFormMessage = "
          . shows registrationFormMessage
          . showString ", registrationFormIsError = "
          . shows registrationFormIsError
          . showString "})"
      )
  showsPrec precedence (EmailVerificationPage verificationPath VerificationForm {verificationFormToken, verificationFormMessage, verificationFormIsError}) =
    showParen
      (precedence > 10)
      ( showString "EmailVerificationPage "
          . shows verificationPath
          . showChar ' '
          . showString "(VerificationForm {verificationFormToken = "
          . shows verificationFormToken
          . showString ", verificationFormMessage = "
          . shows verificationFormMessage
          . showString ", verificationFormIsError = "
          . shows verificationFormIsError
          . showString "})"
      )
  showsPrec precedence (MfaEnrollmentPage mfaEnrollmentPath MfaEnrollmentForm {mfaEnrollmentFormAccountId, mfaEnrollmentFormMessage, mfaEnrollmentFormIsError}) =
    showParen (precedence > 10) (showString "MfaEnrollmentPage " . shows mfaEnrollmentPath . showChar ' ' . shows mfaEnrollmentFormAccountId . showChar ' ' . shows mfaEnrollmentFormMessage . showChar ' ' . shows mfaEnrollmentFormIsError)
  showsPrec precedence (LoginPage loginPath LoginForm {loginFormEmail, loginFormMessage, loginFormIsError}) =
    showParen (precedence > 10) (showString "LoginPage " . shows loginPath . showChar ' ' . shows loginFormEmail . showChar ' ' . shows loginFormMessage . showChar ' ' . shows loginFormIsError)
  showsPrec precedence (LogoutPage logoutPath) =
    showParen (precedence > 10) (showString "LogoutPage " . shows logoutPath)
  showsPrec precedence (ProfilePage profilePage) =
    showProfilePage precedence profilePage
  showsPrec precedence (NotFoundPage notFoundPage) =
    showParen (precedence > 10) (showString "NotFoundPage " . showsPrec 11 notFoundPage)

showProfilePage :: Int -> ProfilePageModel -> ShowS
showProfilePage precedence profilePage =
  case profilePage of
    SignedOutProfilePage {profileHeading, profileSummary, profileSignInAction, profileRegistrationAction} ->
      showParen (precedence > 10) (showString "SignedOutProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileSignInAction . showChar ' ' . shows profileRegistrationAction)
    PendingProfilePage {profileHeading, profileSummary, profileEmail, profileResendPath, profileResendLabel, profileSignOutAction} ->
      showParen (precedence > 10) (showString "PendingProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileEmail . showChar ' ' . shows profileResendPath . showChar ' ' . shows profileResendLabel . showChar ' ' . shows profileSignOutAction)
    AuthenticatedProfilePage {profileHeading, profileSummary, profileEmail, profileSignOutAction} ->
      showParen (precedence > 10) (showString "AuthenticatedProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileEmail . showChar ' ' . shows profileSignOutAction)
    UnavailableProfilePage {profileHeading, profileSummary, profileSignInAction} ->
      showParen (precedence > 10) (showString "UnavailableProfilePage " . shows profileHeading . showChar ' ' . shows profileSummary . showChar ' ' . shows profileSignInAction)

renderPage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Page AppRoute AppRequestContext)
renderPage config =
  renderPageWithDatabase config defaultDatabaseEffect

renderPageWithDatabase :: AppConfig -> DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO (HarchWeb.Page AppRoute AppRequestContext)
renderPageWithDatabase config databaseEffect routeRequest =
  fmap
    (renderPageFromRouteData config routeRequest)
    (selectRouteDataWithDatabase databaseEffect routeRequest)

renderPageFromRouteData :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult -> HarchWeb.Page AppRoute AppRequestContext
renderPageFromRouteData config routeRequest routeData =
  renderPageModel config routeRequest (buildPageModelFromRouteData routeRequest routeData)

renderProfilePageWithState :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> ProfileState -> HarchWeb.Page AppRoute AppRequestContext
renderProfilePageWithState config routeRequest profileState =
  renderPageModel config routeRequest (ProfilePage (buildProfilePageModel routeRequest profileState))

renderUnavailableProfilePage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Page AppRoute AppRequestContext
renderUnavailableProfilePage config routeRequest =
  renderPageModel
    config
    routeRequest
    ( ProfilePage
        UnavailableProfilePage
          { profileHeading = localizedText routeRequest "Profile" "Perfil",
            profileSummary = localizedText routeRequest "Your profile is temporarily unavailable." "Tu perfil no está disponible temporalmente.",
            profileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest "Sign in" "Iniciar sesión")
          }
    )

renderPageModel :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel -> HarchWeb.Page AppRoute AppRequestContext
renderPageModel config routeRequest pageModel =
  HarchWeb.Page
    { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
      HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
      HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
      HarchWeb.pageBody = renderPageBodyForLocale (requestLocale (HarchWeb.requestContext routeRequest)) pageModel,
      HarchWeb.pageBootstrapHooks = pageEnhancementHooks (HarchWeb.requestRoute routeRequest)
    }

routeTitle :: AppRoute -> Text
routeTitle = routePageTitle . routeMetadata

buildPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> IO AppPageModel
buildPageModel = buildPageModelWithDatabase defaultDatabaseEffect

buildPageModelWithDatabase :: DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO AppPageModel
buildPageModelWithDatabase databaseEffect routeRequest =
  fmap
    (buildPageModelFromRouteData routeRequest)
    (selectRouteDataWithDatabase databaseEffect routeRequest)

buildPageModelFromRouteData :: HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult -> AppPageModel
buildPageModelFromRouteData routeRequest routeData =
  case routeData of
    HomeRouteDataResult homeRouteDataResult ->
      buildHomePageModel routeRequest homeRouteDataResult
    SecondRouteDataResult secondRouteDataResult ->
      buildSecondPageModel routeRequest secondRouteDataResult
    SpacesRouteDataResult ->
      SpacesPage
        SpacesPageModel
          { spacesHeading = localizedText routeRequest "Site under construction" "Sitio en construcción",
            spacesSummary = localizedText routeRequest "Follow this space." "Sigan este espacio."
          }
    RegistrationRouteDataResult ->
      RegistrationPage
        (renderRoutePath (HarchWeb.RouteRequest RegistrationRoute (HarchWeb.requestContext routeRequest)))
        emptyRegistrationForm
    EmailVerificationRouteDataResult ->
      EmailVerificationPage
        (renderRoutePath (HarchWeb.RouteRequest EmailVerificationRoute (HarchWeb.requestContext routeRequest)))
        VerificationForm
          { verificationFormToken =
              fromMaybe Text.empty (lookup "token" (requestQueryParameters (HarchWeb.requestContext routeRequest))),
            verificationFormMessage = Nothing,
            verificationFormIsError = False
          }
    MfaEnrollmentRouteDataResult ->
      MfaEnrollmentPage
        (renderRoutePath (HarchWeb.RouteRequest MfaEnrollmentRoute (HarchWeb.requestContext routeRequest)))
        (MfaEnrollmentForm (fromMaybe Text.empty (lookup "account" (requestQueryParameters (HarchWeb.requestContext routeRequest)))) Nothing [] Nothing False)
    LoginRouteDataResult ->
      LoginPage
        (renderRoutePath (HarchWeb.RouteRequest LoginRoute (HarchWeb.requestContext routeRequest)))
        (LoginForm Text.empty Nothing False)
    LogoutRouteDataResult ->
      LogoutPage
        (renderRoutePath (HarchWeb.RouteRequest LogoutRoute (HarchWeb.requestContext routeRequest)))
    ProfileRouteDataResult ->
      ProfilePage (buildProfilePageModel routeRequest ProfileUnauthenticated)
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = "Not Found",
            notFoundSummary = "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute "Return home"
          }

buildProfilePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> ProfileState -> ProfilePageModel
buildProfilePageModel routeRequest profileState =
  case profileState of
    ProfileUnauthenticated ->
      SignedOutProfilePage
        { profileHeading = localizedText routeRequest "Profile" "Perfil",
          profileSummary = localizedText routeRequest "Sign in to view and manage your profile." "Inicia sesión para ver y administrar tu perfil.",
          profileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest "Sign in" "Iniciar sesión"),
          profileRegistrationAction = buildCallToAction routeRequest RegistrationRoute (localizedText routeRequest "Create account" "Crear cuenta")
        }
    ProfilePending profile ->
      PendingProfilePage
        { profileHeading = localizedText routeRequest "Profile" "Perfil",
          profileSummary = localizedText routeRequest "Verify your email address before continuing." "Verifica tu dirección de correo antes de continuar.",
          profileEmail = Email.emailAddressText (accountProfileEmail profile),
          profileResendPath = renderRoutePath (HarchWeb.RouteRequest ProfileRoute (HarchWeb.requestContext routeRequest)),
          profileResendLabel = localizedText routeRequest "Resend verification email" "Reenviar correo de verificacion",
          profileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest "Sign out" "Cerrar sesión")
        }
    ProfileAuthenticated profile ->
      AuthenticatedProfilePage
        { profileHeading = localizedText routeRequest "Profile" "Perfil",
          profileSummary = localizedText routeRequest "You are signed in." "Has iniciado sesión.",
          profileEmail = Email.emailAddressText (accountProfileEmail profile),
          profileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest "Sign out" "Cerrar sesión")
        }

buildHomePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError HomeRouteData -> AppPageModel
buildHomePageModel routeRequest homeRouteDataResult =
  let browseSecond = buildCallToAction routeRequest SecondRoute (localizedText routeRequest "Browse the second page" "Ver la segunda página")
   in case homeRouteDataResult of
        Right homeRouteData ->
          HomePage
            HomePageModel
              { homeHeading = "Home",
                homeSummary = homeRouteSummary homeRouteData,
                homeErrorMessage = Nothing,
                homePrimaryAction = browseSecond
              }
        Left _ ->
          HomePage
            HomePageModel
              { homeHeading = "Home",
                homeSummary = "Home page content is temporarily unavailable.",
                homeErrorMessage = Just "Could not load home page data.",
                homePrimaryAction = browseSecond
              }

buildSecondPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError SecondRouteData -> AppPageModel
buildSecondPageModel routeRequest secondRouteDataResult =
  let returnHome = buildCallToAction routeRequest HomeRoute (localizedText routeRequest "Return home" "Volver al inicio")
   in case secondRouteDataResult of
        Right secondRouteData ->
          SecondPage
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = secondRouteSummary secondRouteData,
                secondHighlights = secondRouteHighlights secondRouteData,
                secondErrorMessage = Nothing,
                secondPrimaryAction = returnHome
              }
        Left _ ->
          SecondPage
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content is temporarily unavailable.",
                secondHighlights = [],
                secondErrorMessage = Just "Could not load second page data.",
                secondPrimaryAction = returnHome
              }

buildCallToAction :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppRoute -> Text -> CallToAction
buildCallToAction routeRequest route label =
  CallToAction
    { callToActionLabel = label,
      callToActionRoute = route,
      callToActionHref =
        renderRoutePath
          HarchWeb.RouteRequest
            { HarchWeb.requestRoute = route,
              HarchWeb.requestContext = HarchWeb.requestContext routeRequest
            }
    }

localizedText :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Text -> Text -> Text
localizedText routeRequest englishText spanishText =
  case requestLocale (HarchWeb.requestContext routeRequest) of
    English -> englishText
    Spanish -> spanishText

renderPageBody :: AppPageModel -> Text
renderPageBody = renderPageBodyForLocale English

renderPageBodyForLocale :: AppLocale -> AppPageModel -> Text
renderPageBodyForLocale locale pageModel =
  case pageModel of
    HomePage homePage ->
      Text.concat
        [ "<section data-page=\"home\">",
          "<h1 data-page-title=\"true\">",
          homeHeading homePage,
          "</h1>",
          renderPageError (homeErrorMessage homePage),
          "<p>",
          homeSummary homePage,
          "</p>",
          renderCallToAction (homePrimaryAction homePage),
          "</section>"
        ]
    SecondPage secondPage ->
      Text.concat
        [ "<section data-page=\"second\">",
          "<h1 data-page-title=\"true\">",
          secondHeading secondPage,
          "</h1>",
          renderPageError (secondErrorMessage secondPage),
          "<p>",
          secondSummary secondPage,
          "</p>",
          renderSecondPageHighlights secondPage,
          renderCallToAction (secondPrimaryAction secondPage),
          "</section>"
        ]
    SpacesPage spacesPage ->
      Text.concat
        [ "<section data-page=\"spaces\">",
          "<h1 data-page-title=\"true\">",
          spacesHeading spacesPage,
          "</h1><p>",
          spacesSummary spacesPage,
          "</p></section>"
        ]
    RegistrationPage registrationPath registrationForm ->
      renderRegistrationPage locale registrationPath registrationForm
    EmailVerificationPage verificationPath verificationForm ->
      renderVerificationPage locale verificationPath verificationForm
    MfaEnrollmentPage mfaEnrollmentPath mfaEnrollmentForm ->
      renderMfaEnrollmentPage locale mfaEnrollmentPath mfaEnrollmentForm
    LoginPage loginPath loginForm ->
      renderLoginPage locale loginPath loginForm
    LogoutPage logoutPath ->
      renderLogoutPage locale logoutPath
    ProfilePage profilePage ->
      renderProfilePageBody profilePage
    NotFoundPage notFoundPage ->
      Text.concat
        [ "<section data-page=\"not-found\">",
          "<h1 data-page-title=\"true\">",
          notFoundHeading notFoundPage,
          "</h1>",
          "<p>",
          notFoundSummary notFoundPage,
          "</p>",
          renderCallToAction (notFoundPrimaryAction notFoundPage),
          "</section>"
        ]

renderProfilePageBody :: ProfilePageModel -> Text
renderProfilePageBody profilePage =
  case profilePage of
    SignedOutProfilePage {profileHeading, profileSummary, profileSignInAction, profileRegistrationAction} ->
      profilePageSection profileHeading profileSummary [renderCallToAction profileSignInAction, renderCallToAction profileRegistrationAction]
    PendingProfilePage {profileHeading, profileSummary, profileEmail, profileResendPath, profileResendLabel, profileSignOutAction} ->
      profilePageSection profileHeading profileSummary [renderPendingProfileRegion profileResendPath (PendingProfileForm profileEmail Nothing False profileResendLabel), renderCallToAction profileSignOutAction]
    AuthenticatedProfilePage {profileHeading, profileSummary, profileEmail, profileSignOutAction} ->
      profilePageSection profileHeading profileSummary [renderProfileEmail profileEmail, renderCallToAction profileSignOutAction]
    UnavailableProfilePage {profileHeading, profileSummary, profileSignInAction} ->
      profilePageSection profileHeading profileSummary [renderCallToAction profileSignInAction]

profilePageSection :: Text -> Text -> [Text] -> Text
profilePageSection heading summary content =
  Text.concat
    [ "<section data-page=\"profile\">",
      "<h1 data-page-title=\"true\">",
      heading,
      "</h1><p>",
      summary,
      "</p>",
      Text.concat content,
      "</section>"
    ]

renderProfileEmail :: Text -> Text
renderProfileEmail emailAddress =
  Text.concat ["<p data-profile-email=\"true\">", emailAddress, "</p>"]

renderHighlights :: [Text] -> Text
renderHighlights highlights =
  case highlights of
    [] -> "<p data-empty-state=\"true\">No highlights yet.</p>"
    _ ->
      Text.concat
        [ "<ul>",
          Text.concat (map renderHighlight highlights),
          "</ul>"
        ]

renderPageError :: Maybe Text -> Text
renderPageError maybeErrorMessage =
  case maybeErrorMessage of
    Nothing -> Text.empty
    Just errorMessage ->
      Text.concat
        [ "<p data-error-state=\"true\">",
          errorMessage,
          "</p>"
        ]

renderSecondPageHighlights :: SecondPageModel -> Text
renderSecondPageHighlights secondPage =
  case secondErrorMessage secondPage of
    Nothing -> renderHighlights (secondHighlights secondPage)
    Just _ -> Text.empty

renderHighlight :: Text -> Text
renderHighlight highlight =
  Text.concat ["<li>", highlight, "</li>"]

renderCallToAction :: CallToAction -> Text
renderCallToAction callToAction =
  Text.concat
    [ "<p><a href=\"",
      callToActionHref callToAction,
      "\" data-page-link=\"true\">",
      callToActionLabel callToAction,
      "</a></p>"
    ]
