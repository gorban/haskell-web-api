{-# LANGUAGE OverloadedStrings #-}

module WebApi.Page.Building
  ( buildPageModel,
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildProfilePageModel,
    buildUnavailableProfilePageModel,
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
import WebApi.Page.Model
import WebApi.Profile (ProfileState (..))
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    renderRoutePath,
  )
import WebApi.RouteData
  ( HomeRouteData (..),
    RouteDataResult (..),
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
          { notFoundHeading = localizedText routeRequest "Not Found" "No encontrado",
            notFoundSummary = localizedText routeRequest "The requested page could not be found." "No se pudo encontrar la pagina solicitada.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute (localizedText routeRequest "Return home" "Volver al inicio")
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
          profileUsername = Username.usernameText <$> accountProfileUsername profile,
          profileDisplayName = accountProfileDisplayName profile,
          profileResendPath = UpdateProfileTarget,
          profileResendLabel = localizedText routeRequest "Resend verification email" "Reenviar correo de verificacion",
          profileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest "Sign out" "Cerrar sesión")
        }
    ProfileAuthenticated profile ->
      AuthenticatedProfilePage
        { profileHeading = localizedText routeRequest "Profile" "Perfil",
          profileSummary = localizedText routeRequest "You are signed in." "Has iniciado sesión.",
          profileEmail = Email.emailAddressText (accountProfileEmail profile),
          profileUsername = Username.usernameText <$> accountProfileUsername profile,
          profileDisplayName = accountProfileDisplayName profile,
          profileSignOutAction = buildCallToAction routeRequest LogoutRoute (localizedText routeRequest "Sign out" "Cerrar sesión")
        }

buildUnavailableProfilePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel
buildUnavailableProfilePageModel routeRequest =
  ProfilePage
    UnavailableProfilePage
      { profileHeading = localizedText routeRequest "Profile" "Perfil",
        profileSummary = localizedText routeRequest "Your profile is temporarily unavailable." "Tu perfil no está disponible temporalmente.",
        profileSignInAction = buildCallToAction routeRequest LoginRoute (localizedText routeRequest "Sign in" "Iniciar sesión")
      }

buildHomePageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError HomeRouteData -> AppPageModel
buildHomePageModel routeRequest homeRouteDataResult =
  let browseSecond = buildCallToAction routeRequest SecondRoute (localizedText routeRequest "Browse the second page" "Ver la segunda página")
   in case homeRouteDataResult of
        Right homeRouteData ->
          HomePage
            HomePageModel
              { homeHeading = localizedText routeRequest "Home" "Inicio",
                homeSummary = homeRouteSummary homeRouteData,
                homeErrorMessage = Nothing,
                homePrimaryAction = browseSecond
              }
        Left _ ->
          HomePage
            HomePageModel
              { homeHeading = localizedText routeRequest "Home" "Inicio",
                homeSummary = localizedText routeRequest "Home page content is temporarily unavailable." "El contenido de la pagina de inicio no esta disponible temporalmente.",
                homeErrorMessage = Just (localizedText routeRequest "Could not load home page data." "No se pudieron cargar los datos de la pagina de inicio."),
                homePrimaryAction = browseSecond
              }

buildSecondPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError SecondRouteData -> AppPageModel
buildSecondPageModel routeRequest secondRouteDataResult =
  let returnHome = buildCallToAction routeRequest HomeRoute (localizedText routeRequest "Return home" "Volver al inicio")
   in case secondRouteDataResult of
        Right secondRouteData ->
          SecondPage
            SecondPageModel
              { secondHeading = localizedText routeRequest "Second" "Segunda",
                secondSummary = secondRouteSummary secondRouteData,
                secondHighlights = secondRouteHighlights secondRouteData,
                secondErrorMessage = Nothing,
                secondPrimaryAction = returnHome
              }
        Left _ ->
          SecondPage
            SecondPageModel
              { secondHeading = localizedText routeRequest "Second" "Segunda",
                secondSummary = localizedText routeRequest "Second page content is temporarily unavailable." "El contenido de la segunda pagina no esta disponible temporalmente.",
                secondHighlights = [],
                secondErrorMessage = Just (localizedText routeRequest "Could not load second page data." "No se pudieron cargar los datos de la segunda pagina."),
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
