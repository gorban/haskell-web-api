module WebApi.Page
  ( AppPageModel (..),
    CallToAction (..),
    HomePageModel (..),
    NotFoundPageModel (..),
    SecondPageModel (..),
    buildPageModelFromRouteData,
    buildPageModelWithDatabase,
    buildPageModel,
    renderPageFromRouteData,
    renderPageWithDatabase,
    renderPage,
    renderPageBody,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Config (AppConfig (..))
import WebApi.Database
  ( DatabaseEffect,
    defaultDatabaseEffect,
  )
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    renderRoutePath,
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

data NotFoundPageModel = NotFoundPageModel
  { notFoundHeading :: Text,
    notFoundSummary :: Text,
    notFoundPrimaryAction :: CallToAction
  }
  deriving (Eq, Show)

data AppPageModel
  = HomePage HomePageModel
  | SecondPage SecondPageModel
  | NotFoundPage NotFoundPageModel
  deriving (Eq, Show)

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
        { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, Text.pack ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
          HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
          HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
          HarchWeb.pageBody = renderPageBody pageModel
        }

routeTitle :: AppRoute -> Text
routeTitle route =
  case route of
    HomeRoute -> Text.pack "Home"
    SecondRoute -> Text.pack "Second"
    _ -> Text.pack "Not Found"

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
    HomeRouteDataResult homeRouteData ->
      HomePage
        HomePageModel
          { homeHeading = Text.pack "Home",
            homeSummary = homeRouteSummary homeRouteData,
            homePrimaryAction = buildCallToAction routeRequest SecondRoute (Text.pack "Browse the second page")
          }
    SecondRouteDataResult secondRouteDataResult ->
      buildSecondPageModel routeRequest secondRouteDataResult
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = Text.pack "Not Found",
            notFoundSummary = Text.pack "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
          }

buildSecondPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError SecondRouteData -> AppPageModel
buildSecondPageModel routeRequest secondRouteDataResult =
  let returnHome = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
   in case secondRouteDataResult of
        Right secondRouteData ->
          SecondPage
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = secondRouteSummary secondRouteData,
                secondHighlights = secondRouteHighlights secondRouteData,
                secondErrorMessage = Nothing,
                secondPrimaryAction = returnHome
              }
        Left _ ->
          SecondPage
            SecondPageModel
              { secondHeading = Text.pack "Second",
                secondSummary = Text.pack "Second page content is temporarily unavailable.",
                secondHighlights = [],
                secondErrorMessage = Just (Text.pack "Could not load second page data."),
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

renderPageBody :: AppPageModel -> Text
renderPageBody pageModel =
  case pageModel of
    HomePage homePage ->
      Text.concat
        [ Text.pack "<section data-page=\"home\">",
          Text.pack "<h1 data-page-title=\"true\">",
          homeHeading homePage,
          Text.pack "</h1>",
          Text.pack "<p>",
          homeSummary homePage,
          Text.pack "</p>",
          renderCallToAction (homePrimaryAction homePage),
          Text.pack "</section>"
        ]
    SecondPage secondPage ->
      Text.concat
        [ Text.pack "<section data-page=\"second\">",
          Text.pack "<h1 data-page-title=\"true\">",
          secondHeading secondPage,
          Text.pack "</h1>",
          renderSecondPageError (secondErrorMessage secondPage),
          Text.pack "<p>",
          secondSummary secondPage,
          Text.pack "</p>",
          renderSecondPageHighlights secondPage,
          renderCallToAction (secondPrimaryAction secondPage),
          Text.pack "</section>"
        ]
    NotFoundPage notFoundPage ->
      Text.concat
        [ Text.pack "<section data-page=\"not-found\">",
          Text.pack "<h1 data-page-title=\"true\">",
          notFoundHeading notFoundPage,
          Text.pack "</h1>",
          Text.pack "<p>",
          notFoundSummary notFoundPage,
          Text.pack "</p>",
          renderCallToAction (notFoundPrimaryAction notFoundPage),
          Text.pack "</section>"
        ]

renderHighlights :: [Text] -> Text
renderHighlights highlights =
  case highlights of
    [] -> Text.pack "<p data-empty-state=\"true\">No highlights yet.</p>"
    _ ->
      Text.concat
        [ Text.pack "<ul>",
          Text.concat (map renderHighlight highlights),
          Text.pack "</ul>"
        ]

renderSecondPageError :: Maybe Text -> Text
renderSecondPageError maybeErrorMessage =
  case maybeErrorMessage of
    Nothing -> Text.empty
    Just errorMessage ->
      Text.concat
        [ Text.pack "<p data-error-state=\"true\">",
          errorMessage,
          Text.pack "</p>"
        ]

renderSecondPageHighlights :: SecondPageModel -> Text
renderSecondPageHighlights secondPage =
  case secondErrorMessage secondPage of
    Nothing -> renderHighlights (secondHighlights secondPage)
    Just _ -> Text.empty

renderHighlight :: Text -> Text
renderHighlight highlight =
  Text.concat [Text.pack "<li>", highlight, Text.pack "</li>"]

renderCallToAction :: CallToAction -> Text
renderCallToAction callToAction =
  Text.concat
    [ Text.pack "<p><a href=\"",
      callToActionHref callToAction,
      Text.pack "\" data-page-link=\"true\">",
      callToActionLabel callToAction,
      Text.pack "</a></p>"
    ]
