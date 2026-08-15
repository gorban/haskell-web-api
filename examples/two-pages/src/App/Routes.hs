{-# LANGUAGE OverloadedStrings #-}

module App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    PreviewSlug,
    TwoPageAction (..),
    TwoPageActionTarget,
    TwoPageNavigationTarget (..),
    TwoPageRoute (..),
    mkPreviewSlug,
    previewSlugText,
    routeCodec,
    routeHref,
    twoPageActionContext,
    twoPageActions,
    twoPageNavigationPath,
    twoPageActionPath,
  )
where

import App.Pages.Route.Generated
  ( PageRoute (..),
    pageRoutePath,
    parsePageRoute,
  )
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( RouteCodec (..),
    RouteMethod (..),
    RouteRequest (..),
    routeMethodPolicy,
  )
import HarchWeb.Action
  ( ActionCodec,
    actionPath,
    formField,
    post,
    singleActionCodec,
    singleOrDefault,
    textValue,
  )

data ApiRoute
  = LiveDataEvents
  deriving (Eq, Show)

type TwoPageActionTarget = ()

data TwoPageNavigationTarget
  = NavigationPage PageRoute
  | NavigationPreview PreviewSlug

newtype TwoPageAction = SubscribeAction Text

newtype PreviewSlug = PreviewSlug Text
  deriving (Eq, Show)

data CustomRoute
  = PreviewPage PreviewSlug
  | NativeSubscriptionFallback
  deriving (Eq, Show)

data TwoPageRoute
  = Page PageRoute
  | Api ApiRoute
  | Custom CustomRoute
  deriving (Eq, Show)

routeCodec :: RouteCodec TwoPageRoute ()
routeCodec =
  RouteCodec
    { parseRoute = \() path ->
        let normalizedPath = routePath path
         in case normalizedPath of
              "/live-data/events" ->
                Just
                  RouteRequest
                    { requestRoute = Api LiveDataEvents,
                      requestContext = ()
                    }
              "/native-subscribe" ->
                Just
                  RouteRequest
                    { requestRoute = Custom NativeSubscriptionFallback,
                      requestContext = ()
                    }
              _ ->
                case parsePreviewPath normalizedPath of
                  Just previewSlug ->
                    Just
                      RouteRequest
                        { requestRoute = Custom (PreviewPage previewSlug),
                          requestContext = ()
                        }
                  Nothing ->
                    (\page -> RouteRequest {requestRoute = Page page, requestContext = ()})
                      <$> parsePageRoute normalizedPath,
      renderRoute = routeHref . requestRoute,
      notFoundRequest = \() ->
        RouteRequest {requestRoute = Page PageNotFound, requestContext = ()},
      routeMethods = routeMethodPolicy . twoPageRouteMethods
    }

twoPageRouteMethods :: TwoPageRoute -> [RouteMethod]
twoPageRouteMethods route =
  case route of
    Page PageNotFound -> []
    Page _ -> [RouteGet]
    Api LiveDataEvents -> [RouteGet]
    Custom (PreviewPage _) -> [RouteGet]
    Custom NativeSubscriptionFallback -> [RouteGet, RoutePost]

routeHref :: TwoPageRoute -> Text
routeHref route =
  case route of
    Page page -> pageRoutePath page
    Api LiveDataEvents -> "/live-data/events"
    Custom (PreviewPage previewSlug) -> "/preview/" <> previewSlugText previewSlug
    Custom NativeSubscriptionFallback -> "/native-subscribe"

twoPageActionPath :: TwoPageActionTarget -> Text
twoPageActionPath = actionPath twoPageActions $! twoPageActionContext

twoPageActionContext :: ()
twoPageActionContext = ()

twoPageActions :: ActionCodec TwoPageActionTarget () TwoPageAction
twoPageActions =
  singleActionCodec
    ()
    (post "/actions/subscribe")
    (SubscribeAction <$> singleOrDefault "" (formField "email" textValue))

twoPageNavigationPath :: TwoPageNavigationTarget -> Text
twoPageNavigationPath navigationTarget =
  case navigationTarget of
    NavigationPage page -> pageRoutePath page
    NavigationPreview previewSlug -> "/preview/" <> previewSlugText previewSlug

mkPreviewSlug :: Text -> Maybe PreviewSlug
mkPreviewSlug value =
  if not (Text.null value) && Text.all validSlugCharacter value
    then Just (PreviewSlug value)
    else Nothing
  where
    validSlugCharacter character =
      isAsciiLower character || isDigit character || character == '-'

previewSlugText :: PreviewSlug -> Text
previewSlugText (PreviewSlug value) = value

parsePreviewPath :: Text -> Maybe PreviewSlug
parsePreviewPath path =
  Text.stripPrefix "/preview/" path >>= mkPreviewSlug

routePath :: Text -> Text
routePath =
  Text.takeWhile (/= '?')
