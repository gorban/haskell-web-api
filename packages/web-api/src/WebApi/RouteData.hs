module WebApi.RouteData
  ( HomeRouteData (..),
    RouteDataResult (..),
    RouteDataSelection (..),
    SecondRouteData (..),
    StatusApiData (..),
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
    homePageDataSummary,
    loadHomePage,
    loadSecondPage,
    secondPageDataHighlights,
    secondPageDataSummary,
  )
import WebApi.Route
  ( AppLocale,
    AppRequestContext,
    AppRoute (..),
    requestLocale,
  )

newtype HomeRouteData = HomeRouteData
  { homeRouteSummary :: Text
  }
  deriving (Eq, Show)

data SecondRouteData = SecondRouteData
  { secondRouteSummary :: Text,
    secondRouteHighlights :: [Text]
  }
  deriving (Eq, Show)

newtype StatusApiData = StatusApiData
  { statusApiLocale :: AppLocale
  }
  deriving (Eq, Show)

data RouteDataResult
  = HomeRouteDataResult (Either DatabaseError HomeRouteData)
  | SecondRouteDataResult (Either DatabaseError SecondRouteData)
  | SpacesRouteDataResult
  | RegistrationRouteDataResult
  | EmailVerificationRouteDataResult
  | MfaEnrollmentRouteDataResult
  | LoginRouteDataResult
  | LogoutRouteDataResult
  | ProfileRouteDataResult
  | StatusApiDataResult StatusApiData
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
    LoadHomeRouteData -> selectHomeRouteData pageRepository requestContext
    LoadSecondRouteData -> selectSecondRouteData pageRepository requestContext
    BuildStatusRouteData -> pure (emptyRouteDataSelection (StatusApiDataResult (StatusApiData (requestLocale requestContext))))
    UseStaticRouteData result -> pure (emptyRouteDataSelection result)
  where
    requestContext = HarchWeb.requestContext routeRequest

data RouteDataPlan
  = LoadHomeRouteData
  | LoadSecondRouteData
  | BuildStatusRouteData
  | UseStaticRouteData RouteDataResult

selectHomeRouteData :: PageRepository -> AppRequestContext -> IO RouteDataSelection
selectHomeRouteData pageRepository requestContext = do
  homePageDataResult <- loadHomePage pageRepository (requestLocale requestContext)
  pure
    RouteDataSelection
      { routeDataResult = HomeRouteDataResult (HomeRouteData . homePageDataSummary <$> databaseResultValue homePageDataResult),
        routeDataDatabaseOperations = databaseResultOperations homePageDataResult
      }

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
    HomeRoute -> LoadHomeRouteData
    SecondRoute -> LoadSecondRouteData
    SpacesRoute -> UseStaticRouteData SpacesRouteDataResult
    StatusApiRoute -> BuildStatusRouteData
    RegistrationRoute -> UseStaticRouteData RegistrationRouteDataResult
    EmailVerificationRoute -> UseStaticRouteData EmailVerificationRouteDataResult
    MfaEnrollmentRoute -> UseStaticRouteData MfaEnrollmentRouteDataResult
    LoginRoute -> UseStaticRouteData LoginRouteDataResult
    LogoutRoute -> UseStaticRouteData LogoutRouteDataResult
    ProfileRoute -> UseStaticRouteData ProfileRouteDataResult
    NotFoundRoute -> UseStaticRouteData NotFoundRouteDataResult

selectRouteDataWithDatabase :: PageRepository -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataResult
selectRouteDataWithDatabase pageRepository routeRequest =
  fmap routeDataResult (selectRouteDataSelectionWithDatabase pageRepository routeRequest)
