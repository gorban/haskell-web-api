module WebApi.RouteData
  ( HomeRouteData (..),
    RouteDataResult (..),
    SecondRouteData (..),
    StatusApiData (..),
    selectRouteData,
    selectRouteDataWithDatabase,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Database
  ( DatabaseEffect,
    DatabaseError,
    defaultDatabaseEffect,
    loadSecondPageData,
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
  = HomeRouteDataResult HomeRouteData
  | SecondRouteDataResult (Either DatabaseError SecondRouteData)
  | StatusApiDataResult StatusApiData
  | NotFoundRouteDataResult
  deriving (Eq, Show)

selectRouteData :: HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult
selectRouteData =
  selectRouteDataWithDatabase defaultDatabaseEffect

selectRouteDataWithDatabase :: DatabaseEffect -> HarchWeb.RouteRequest AppRoute AppRequestContext -> RouteDataResult
selectRouteDataWithDatabase databaseEffect routeRequest =
  case HarchWeb.requestRoute routeRequest of
    HomeRoute ->
      HomeRouteDataResult
        HomeRouteData
          { homeRouteSummary = Text.pack "Server-rendered home page with stubbed content."
          }
    SecondRoute ->
      SecondRouteDataResult $
        fmap
          ( \secondPageData ->
              SecondRouteData
                { secondRouteSummary = secondPageDataSummary secondPageData,
                  secondRouteHighlights = secondPageDataHighlights secondPageData
                }
          )
          (loadSecondPageData databaseEffect (HarchWeb.requestContext routeRequest))
    StatusApiRoute ->
      StatusApiDataResult
        StatusApiData
          { statusApiLocale = requestLocale (HarchWeb.requestContext routeRequest)
          }
    NotFoundRoute ->
      NotFoundRouteDataResult
