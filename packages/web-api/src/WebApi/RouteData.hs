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
  ( DatabaseEffect,
    DatabaseError,
    DatabaseOperation,
    databaseResultOperations,
    databaseResultValue,
    defaultDatabaseEffect,
    homePageDataSummary,
    loadHomePageDataWithObservability,
    loadSecondPageDataWithObservability,
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
  | RegistrationRouteDataResult
  | EmailVerificationRouteDataResult
  | MfaEnrollmentRouteDataResult
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
  selectRouteDataWithDatabase defaultDatabaseEffect

selectRouteDataSelectionWithDatabase :: DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataSelection
selectRouteDataSelectionWithDatabase databaseEffect routeRequest =
  case HarchWeb.requestRoute routeRequest of
    HomeRoute -> do
      homePageDataResult <- loadHomePageDataWithObservability databaseEffect (HarchWeb.requestContext routeRequest)
      pure
        RouteDataSelection
          { routeDataResult =
              HomeRouteDataResult
                ( fmap
                    ( \homePageData ->
                        HomeRouteData
                          { homeRouteSummary = homePageDataSummary homePageData
                          }
                    )
                    (databaseResultValue homePageDataResult)
                ),
            routeDataDatabaseOperations = databaseResultOperations homePageDataResult
          }
    SecondRoute -> do
      secondPageDataResult <- loadSecondPageDataWithObservability databaseEffect (HarchWeb.requestContext routeRequest)
      pure
        RouteDataSelection
          { routeDataResult =
              SecondRouteDataResult
                ( fmap
                    ( \secondPageData ->
                        SecondRouteData
                          { secondRouteSummary = secondPageDataSummary secondPageData,
                            secondRouteHighlights = secondPageDataHighlights secondPageData
                          }
                    )
                    (databaseResultValue secondPageDataResult)
                ),
            routeDataDatabaseOperations = databaseResultOperations secondPageDataResult
          }
    RegistrationRoute ->
      pure RouteDataSelection {routeDataResult = RegistrationRouteDataResult, routeDataDatabaseOperations = []}
    EmailVerificationRoute ->
      pure RouteDataSelection {routeDataResult = EmailVerificationRouteDataResult, routeDataDatabaseOperations = []}
    MfaEnrollmentRoute ->
      pure RouteDataSelection {routeDataResult = MfaEnrollmentRouteDataResult, routeDataDatabaseOperations = []}
    StatusApiRoute ->
      pure $
        RouteDataSelection
          { routeDataResult =
              StatusApiDataResult
                StatusApiData
                  { statusApiLocale = requestLocale (HarchWeb.requestContext routeRequest)
                  },
            routeDataDatabaseOperations = []
          }
    NotFoundRoute ->
      pure RouteDataSelection {routeDataResult = NotFoundRouteDataResult, routeDataDatabaseOperations = []}

selectRouteDataWithDatabase :: DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO RouteDataResult
selectRouteDataWithDatabase databaseEffect routeRequest =
  fmap routeDataResult (selectRouteDataSelectionWithDatabase databaseEffect routeRequest)
