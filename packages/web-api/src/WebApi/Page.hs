{-# LANGUAGE OverloadedStrings #-}

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
        { HarchWeb.pageTitle = Text.concat [appTitlePrefix config, ": ", routeTitle (HarchWeb.requestRoute routeRequest)],
          HarchWeb.pageRoute = HarchWeb.requestRoute routeRequest,
          HarchWeb.pageContext = HarchWeb.requestContext routeRequest,
          HarchWeb.pageBody = renderPageBody pageModel,
          HarchWeb.pageBootstrapHooks = bootstrapHooks pageModel
        }

routeTitle :: AppRoute -> Text
routeTitle route =
  case route of
    HomeRoute -> "Home"
    SecondRoute -> "Second"
    _ -> "Not Found"

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
          { homeHeading = "Home",
            homeSummary = homeRouteSummary homeRouteData,
            homePrimaryAction = buildCallToAction routeRequest SecondRoute "Browse the second page"
          }
    SecondRouteDataResult secondRouteDataResult ->
      buildSecondPageModel routeRequest secondRouteDataResult
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = "Not Found",
            notFoundSummary = "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute "Return home"
          }

buildSecondPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> Either databaseError SecondRouteData -> AppPageModel
buildSecondPageModel routeRequest secondRouteDataResult =
  let returnHome = buildCallToAction routeRequest HomeRoute "Return home"
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

renderPageBody :: AppPageModel -> Text
renderPageBody pageModel =
  case pageModel of
    HomePage homePage ->
      Text.concat
        [ "<section data-page=\"home\">",
          "<h1 data-page-title=\"true\">",
          homeHeading homePage,
          "</h1>",
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
          renderSecondPageError (secondErrorMessage secondPage),
          "<p>",
          secondSummary secondPage,
          "</p>",
          renderSecondPageHighlights secondPage,
          renderCallToAction (secondPrimaryAction secondPage),
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

renderSecondPageError :: Maybe Text -> Text
renderSecondPageError maybeErrorMessage =
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

bootstrapHooks :: AppPageModel -> [Text]
bootstrapHooks pageModel =
  case pageModel of
    HomePage _ -> []
    SecondPage _ -> ["second-page"]
    NotFoundPage _ -> []
