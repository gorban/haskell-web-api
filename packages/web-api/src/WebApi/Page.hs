module WebApi.Page
  ( AppPageModel (..),
    CallToAction (..),
    HomePageModel (..),
    NotFoundPageModel (..),
    SecondPageModel (..),
    buildPageModel,
    renderPage,
    renderPageBody,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import WebApi.Config (AppConfig (..))
import WebApi.Route
  ( AppRequestContext,
    AppRoute (..),
    renderRoutePath,
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

renderPage :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> HarchWeb.Page AppRoute AppRequestContext
renderPage config routeRequest =
  let pageModel = buildPageModel routeRequest
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

buildPageModel :: HarchWeb.RouteRequest AppRoute AppRequestContext -> AppPageModel
buildPageModel routeRequest =
  case HarchWeb.requestRoute routeRequest of
    HomeRoute ->
      HomePage
        HomePageModel
          { homeHeading = Text.pack "Home",
            homeSummary = Text.pack "Server-rendered home page with stubbed content.",
            homePrimaryAction = buildCallToAction routeRequest SecondRoute (Text.pack "Browse the second page")
          }
    SecondRoute ->
      SecondPage
        SecondPageModel
          { secondHeading = Text.pack "Second",
            secondSummary = Text.pack "Second page content with stubbed data ready for future loaders.",
            secondHighlights = [],
            secondPrimaryAction = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
          }
    _ ->
      NotFoundPage
        NotFoundPageModel
          { notFoundHeading = Text.pack "Not Found",
            notFoundSummary = Text.pack "The requested page could not be found.",
            notFoundPrimaryAction = buildCallToAction routeRequest HomeRoute (Text.pack "Return home")
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
          Text.pack "<p>",
          secondSummary secondPage,
          Text.pack "</p>",
          renderHighlights (secondHighlights secondPage),
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
