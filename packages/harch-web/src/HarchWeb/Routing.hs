{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed route matching and rendering for HarchWeb applications.
--
-- The framework facade re-exports this module. Applications can keep using
-- 'HarchWeb' for the convenience API, while this focused module owns the
-- route codec boundary without depending on server or document machinery.
module HarchWeb.Routing
  ( RouteCodec (..),
    RouteDispatch (..),
    RouteMethod (..),
    RouteRequest (..),
    RouteFamily (..),
    matchRouteMethod,
    matchRoute,
    combineRouteCodecs,
    routeAllowHeaderValue,
    routeMethodText,
    routeHref,
  )
where

import Control.Applicative ((<|>))
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text

-- | HTTP methods the shared route boundary understands. @HEAD@ and @OPTIONS@
-- are derived from a route's declared methods rather than declared separately.
data RouteMethod
  = RouteGet
  | RoutePost
  | RoutePut
  | RoutePatch
  | RouteDelete
  deriving (Eq, Show)

routeMethodText :: RouteMethod -> Text
routeMethodText routeMethod =
  case routeMethod of
    RouteGet -> "GET"
    RoutePost -> "POST"
    RoutePut -> "PUT"
    RoutePatch -> "PATCH"
    RouteDelete -> "DELETE"

-- | The result of matching a request target and method through one route
-- codec. Method policy is evaluated only after the path is known to exist, so
-- an unknown path cannot be mistaken for a 405 response.
data RouteDispatch route context
  = RouteNotFound (RouteRequest route context)
  | RouteMethodNotAllowed (RouteRequest route context) (NonEmpty RouteMethod)
  | RouteMatched (RouteRequest route context)
  | RouteMatchedHead (RouteRequest route context)
  | RouteOptions (RouteRequest route context) (NonEmpty RouteMethod)
  deriving (Eq, Show)

data RouteRequest route context = RouteRequest
  { requestRoute :: route,
    requestContext :: context
  }
  deriving (Eq, Show)

data RouteCodec route context = RouteCodec
  { parseRoute :: context -> Text -> Maybe (RouteRequest route context),
    renderRoute :: RouteRequest route context -> Text,
    notFoundRequest :: context -> RouteRequest route context,
    routeMethods :: route -> [RouteMethod]
  }

routeHref :: RouteCodec route context -> context -> route -> Text
routeHref codec context route =
  renderRoute codec RouteRequest {requestRoute = route, requestContext = context}

-- | A closed choice between two route families sharing one 'RouteCodec', so
-- their combined table is the sole 404\/405\/'Allow'\/@HEAD@\/@OPTIONS@
-- authority instead of two independently composed dispatchers (a
-- 'Wai.Middleware' wrapping another 'Wai.Application', say) that can each
-- believe they own an overlapping path with no shared arbiter. Nest it to
-- combine more than two families.
data RouteFamily routeA routeB
  = RouteFamilyA routeA
  | RouteFamilyB routeB
  deriving (Eq, Show)

-- | Combine two route families into one 'RouteCodec'. 'parseRoute' tries the
-- first family before the second, so a path only one family recognizes
-- resolves to that family, and a path both families would otherwise
-- recognize deterministically belongs to the first — the same explicit,
-- declaration-order precedence 'matchRouteMethod' already applies to
-- multiple declarations of one route. Register the more specific family
-- first (e.g. a fixed API path table) and a catch-all family second (e.g.
-- ordinary pages), so a reserved path cannot be shadowed by the catch-all
-- and the combined codec's 'notFoundRequest' — used only for a path neither
-- family recognizes at all — is the catch-all family's own not-found route,
-- not the specific family's. A family that wants its own not-found
-- representation for a path shape it otherwise owns (an unmatched
-- @\/api\/*@ path rendering an API 404 instead of the page 404, say) states
-- that by having its own 'parseRoute' recognize that shape directly, the
-- same way a single hand-written 'RouteCodec' already would; this
-- combinator does not add a second, shape-based notion of family ownership
-- beyond what 'parseRoute' itself decides.
combineRouteCodecs :: RouteCodec routeA context -> RouteCodec routeB context -> RouteCodec (RouteFamily routeA routeB) context
combineRouteCodecs codecA codecB =
  RouteCodec
    { parseRoute = \context path ->
        (mapRouteRequest RouteFamilyA <$> parseRoute codecA context path)
          <|> (mapRouteRequest RouteFamilyB <$> parseRoute codecB context path),
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          RouteFamilyA routeA -> renderRoute codecA (routeRequest {requestRoute = routeA})
          RouteFamilyB routeB -> renderRoute codecB (routeRequest {requestRoute = routeB}),
      notFoundRequest = mapRouteRequest RouteFamilyB . notFoundRequest codecB,
      routeMethods = \case
        RouteFamilyA routeA -> routeMethods codecA routeA
        RouteFamilyB routeB -> routeMethods codecB routeB
    }
  where
    mapRouteRequest embed routeRequest = routeRequest {requestRoute = embed (requestRoute routeRequest)}

matchRoute :: RouteCodec route context -> context -> Text -> RouteRequest route context
matchRoute codec context path =
  fromMaybe (notFoundRequest codec context) (parseRoute codec context path)

-- | Match a route's path before evaluating its declared method policy. An
-- empty method declaration is an explicit typed not-found route. It retains
-- its parsed route so an API or page route family can render its own 404
-- representation, while a path that did not parse uses 'notFoundRequest'.
matchRouteMethod :: RouteCodec route context -> context -> Text -> Text -> RouteDispatch route context
matchRouteMethod codec context requestMethod path =
  case parseRoute codec context path of
    Nothing -> RouteNotFound (notFoundRequest codec context)
    Just routeRequest ->
      case List.nub (routeMethods codec (requestRoute routeRequest)) of
        [] -> RouteNotFound routeRequest
        firstMethod : remainingMethods ->
          matchDeclaredMethods routeRequest requestMethod (firstMethod :| remainingMethods)

matchDeclaredMethods :: RouteRequest route context -> Text -> NonEmpty RouteMethod -> RouteDispatch route context
matchDeclaredMethods routeRequest requestMethod declaredMethods
  | requestMethod == "HEAD", RouteGet `elem` declaredMethods = RouteMatchedHead routeRequest
  | requestMethod == "OPTIONS" = RouteOptions routeRequest declaredMethods
  | any ((== requestMethod) . routeMethodText) declaredMethods = RouteMatched routeRequest
  | otherwise = RouteMethodNotAllowed routeRequest declaredMethods

routeAllowHeaderValue :: NonEmpty RouteMethod -> Text
routeAllowHeaderValue declaredMethods =
  Text.intercalate
    ", "
    ( map routeMethodText declaredMethodList
        <> ["HEAD" | RouteGet `elem` declaredMethodList]
        <> ["OPTIONS"]
    )
  where
    declaredMethodList = List.nub (NonEmpty.toList declaredMethods)
