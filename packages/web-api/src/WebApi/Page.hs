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
    renderPageWithDatabase,
    renderPage,
    renderPageBody,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.AccountPages
  ( LoginForm (..),
    MfaEnrollmentForm (..),
    RegistrationForm (..),
    VerificationForm (..),
    emptyRegistrationForm,
    renderLoginPage,
    renderLogoutPage,
    renderMfaEnrollmentPage,
    renderRegistrationPage,
    renderVerificationPage,
  )
import WebApi.App.Enhancements (pageEnhancementHooks)
import WebApi.Config (AppConfig (..))
import WebApi.Database
  ( DatabaseEffect,
    defaultDatabaseEffect,
  )
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

data ProfilePageModel = ProfilePageModel
  { profileHeading :: Text,
    profileSummary :: Text,
    profileSignInAction :: CallToAction,
    profileRegistrationAction :: CallToAction
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
  ProfilePage left == ProfilePage right =
    profileHeading left == profileHeading right
      && profileSummary left == profileSummary right
      && profileSignInAction left == profileSignInAction right
      && profileRegistrationAction left == profileRegistrationAction right
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
    showParen
      (precedence > 10)
      ( showString "ProfilePage {profileHeading = "
          . shows (profileHeading profilePage)
          . showString ", profileSummary = "
          . shows (profileSummary profilePage)
          . showString ", profileSignInAction = "
          . shows (profileSignInAction profilePage)
          . showString ", profileRegistrationAction = "
          . shows (profileRegistrationAction profilePage)
          . showString "}"
      )
  showsPrec precedence (NotFoundPage notFoundPage) =
    showParen (precedence > 10) (showString "NotFoundPage " . showsPrec 11 notFoundPage)

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
  let pageModel = buildPageModelFromRouteData routeRequest routeData
   in HarchWeb.Page
        { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
          HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
          HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
          HarchWeb.pageBody = renderPageBody pageModel,
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
      ProfilePage
        ProfilePageModel
          { profileHeading = localizedText routeRequest "Profile" "Perfil",
            profileSummary = localizedText routeRequest "Sign in to view and manage your profile." "Inicia sesión para ver y administrar tu perfil.",
            profileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest "Sign in" "Iniciar sesión"),
            profileRegistrationAction = buildCallToAction routeRequest RegistrationRoute (localizedText routeRequest "Create account" "Crear cuenta")
          }
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = "Not Found",
            notFoundSummary = "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute "Return home"
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
renderPageBody pageModel =
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
      renderRegistrationPage registrationPath registrationForm
    EmailVerificationPage verificationPath verificationForm ->
      renderVerificationPage verificationPath verificationForm
    MfaEnrollmentPage mfaEnrollmentPath mfaEnrollmentForm ->
      renderMfaEnrollmentPage mfaEnrollmentPath mfaEnrollmentForm
    LoginPage loginPath loginForm ->
      renderLoginPage loginPath loginForm
    LogoutPage logoutPath ->
      renderLogoutPage logoutPath
    ProfilePage profilePage ->
      Text.concat
        [ "<section data-page=\"profile\">",
          "<h1 data-page-title=\"true\">",
          profileHeading profilePage,
          "</h1><p>",
          profileSummary profilePage,
          "</p>",
          renderCallToAction (profileSignInAction profilePage),
          renderCallToAction (profileRegistrationAction profilePage),
          "</section>"
        ]
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
