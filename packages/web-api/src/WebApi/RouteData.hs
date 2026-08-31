module WebApi.RouteData
  ( RouteDataResult (..),
    RouteDataSelection (..),
    SecondRouteData (..),
    selectRouteData,
    selectRouteDataSelectionWithDatabase,
    selectRouteDataWithDatabase,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Database
  ( DatabaseError,
    DatabaseOperation,
    PageRepository,
    SecondPageData,
    databaseResultOperations,
    databaseResultValue,
    defaultPageRepository,
    loadSecondPage,
    secondPageDataHighlights,
    secondPageDataSummary,
  )
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    PageRoute (..),
    requestLocale,
  )

data SecondRouteData = SecondRouteData
  { secondRouteSummary :: Text,
    secondRouteHighlights :: [Text]
  }
  deriving (Eq, Show)

data RouteDataResult
  = SecondRouteDataResult (Either DatabaseError SecondRouteData)
  | SpacesRouteDataResult
  | RegistrationRouteDataResult
  | EmailVerificationRouteDataResult
  | MfaEnrollmentRouteDataResult
  | LoginRouteDataResult
  | LogoutRouteDataResult
  | ProfileRouteDataResult
  | LanguageRouteDataResult
  | HelpRouteDataResult
  | NotFoundRouteDataResult
  deriving (Eq, Show)

data RouteDataSelection = RouteDataSelection
  { routeDataResult :: RouteDataResult,
    routeDataDatabaseOperations :: [DatabaseOperation]
  }
  deriving (Eq, Show)

selectRouteData :: HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataResult
selectRouteData =
  selectRouteDataWithDatabase defaultPageRepository

selectRouteDataSelectionWithDatabase :: PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataSelection
selectRouteDataSelectionWithDatabase pageRepository routeRequest =
  case routeDataPlan (HarchWeb.requestRoute routeRequest) of
    LoadSecondRouteData -> selectSecondRouteData pageRepository requestContext
    UseStaticRouteData result -> pure (emptyRouteDataSelection result)
  where
    requestContext = HarchWeb.requestContext routeRequest

data RouteDataPlan
  = LoadSecondRouteData
  | UseStaticRouteData RouteDataResult

selectSecondRouteData :: PageRepository -> AppRequestContext -> IO RouteDataSelection
selectSecondRouteData pageRepository requestContext = do
  secondPageDataResult <- loadSecondPage pageRepository (requestLocale requestContext)
  pure
    RouteDataSelection
      { routeDataResult = SecondRouteDataResult (toSecondRouteData <$> databaseResultValue secondPageDataResult),
        routeDataDatabaseOperations = databaseResultOperations secondPageDataResult
      }

toSecondRouteData :: SecondPageData -> SecondRouteData
toSecondRouteData pageData = SecondRouteData (secondPageDataSummary pageData) (secondPageDataHighlights pageData)

emptyRouteDataSelection :: RouteDataResult -> RouteDataSelection
emptyRouteDataSelection result = RouteDataSelection result []

routeDataPlan :: AppRoute -> RouteDataPlan
routeDataPlan route =
  case route of
    Page pageRoute ->
      case pageRoute of
        HomePage -> UseStaticRouteData NotFoundRouteDataResult
        SecondPage -> LoadSecondRouteData
        SpacesPage -> UseStaticRouteData SpacesRouteDataResult
        RegistrationPage -> UseStaticRouteData RegistrationRouteDataResult
        EmailVerificationPage -> UseStaticRouteData EmailVerificationRouteDataResult
        MfaEnrollmentPage -> UseStaticRouteData MfaEnrollmentRouteDataResult
        LoginPage -> UseStaticRouteData LoginRouteDataResult
        LogoutPage -> UseStaticRouteData LogoutRouteDataResult
        ProfilePage -> UseStaticRouteData ProfileRouteDataResult
        LanguagePage -> UseStaticRouteData LanguageRouteDataResult
        HelpPage -> UseStaticRouteData HelpRouteDataResult
        PageNotFound -> UseStaticRouteData NotFoundRouteDataResult
    Api _ -> UseStaticRouteData NotFoundRouteDataResult

selectRouteDataWithDatabase :: PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataResult
selectRouteDataWithDatabase pageRepository routeRequest =
  fmap routeDataResult (selectRouteDataSelectionWithDatabase pageRepository routeRequest)
