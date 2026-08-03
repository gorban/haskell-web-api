{-# LANGUAGE OverloadedStrings #-}

module App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    PreviewSlug,
    TwoPageRoute (..),
    mkPreviewSlug,
    previewSlugText,
    routeCodec,
    routeHref,
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
    RouteRequest (..),
  )

data ApiRoute
  = LiveDataEvents
  deriving (Eq, Show)

newtype PreviewSlug = PreviewSlug Text
  deriving (Eq, Show)

newtype CustomRoute
  = PreviewPage PreviewSlug
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
        RouteRequest {requestRoute = Page PageNotFound, requestContext = ()}
    }

routeHref :: TwoPageRoute -> Text
routeHref route =
  case route of
    Page page -> pageRoutePath page
    Api LiveDataEvents -> "/live-data/events"
    Custom (PreviewPage previewSlug) -> "/preview/" <> previewSlugText previewSlug

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
