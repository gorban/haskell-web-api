{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed route matching and rendering for HarchWeb applications.
--
-- The framework facade re-exports this module. Applications can keep using
-- 'HarchWeb' for the convenience API, while this focused module owns the
-- route codec boundary without depending on server or document machinery.
--
-- === Design decision: 'RouteMethodPolicy' over parsed request methods
--
-- 'RouteCodec.routeMethods' reports a 'RouteMethodPolicy' (an opaque
-- \"hidden or these methods\" set) rather than a raw @['RouteMethod']@, and
-- method/path dispatch consumes opaque 'RequestMethod' \/ 'RequestPath'
-- newtypes rather than bare 'Text'. This extends the existing route codec
-- boundary instead of adding a parallel one, per
-- @docs/design-guidance.md@'s extend-vs-new-abstraction rule.
--
-- 'RequestMethod' deliberately stays a 'Text' wrapper instead of parsing
-- into 'RouteMethod': 'RouteMethod' only models the five methods a route can
-- declare (GET, POST, PUT, PATCH, DELETE), while 'matchRouteMethod' still
-- has to recognise HEAD and OPTIONS by string comparison to dispatch them
-- against a GET-declaring or any-declaring route. Parsing the incoming
-- method into 'RouteMethod' up front would have discarded that information
-- before dispatch could use it. This is the framework-capability-gap
-- protocol's middle tier: work around the gap in this module rather than
-- widening 'RouteMethod' to model methods no route ever declares.
--
-- 'RouteMethod' additionally derives 'Ord' so 'routeMethodPolicy' can store
-- declared methods in a 'Data.Set.Set', which subsumes the 'Data.List.nub'
-- deduplication this module used to do by hand. This is an observable
-- behavior change: the @Allow@ header (see 'routeAllowHeaderValue') now
-- lists methods in ascending @Ord@ order (GET, POST, PUT, PATCH, DELETE)
-- instead of the order a route family's 'RouteMethod' list declared them
-- in. RFC 9110 does not mandate an @Allow@-header method order, and a
-- repo-wide search found no route declaring its methods out of ascending
-- order, so the change is currently behavior-invisible; it is recorded here
-- because it is observable in principle.
module HarchWeb.Routing
  ( RouteCodec (..),
    RouteDispatch (..),
    RouteMethod (..),
    RouteMethodPolicy (..),
    RouteRequest (..),
    RouteFamily (..),
    RequestMethod,
    RequestPath,
    requestMethod,
    requestMethodText,
    requestPath,
    requestPathText,
    routeMethodPolicy,
    routeMethodPolicyMethods,
    matchRouteMethod,
    matchRoute,
    combineRouteCodecs,
    routeAllowHeaderValue,
    routeMethodText,
    routeHref,
  )
where

import Control.Applicative ((<|>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
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
  deriving (Eq, Ord, Show)

routeMethodText :: RouteMethod -> Text
routeMethodText routeMethodValue =
  case routeMethodValue of
    RouteGet -> "GET"
    RoutePost -> "POST"
    RoutePut -> "PUT"
    RoutePatch -> "PATCH"
    RouteDelete -> "DELETE"

-- | An incoming request's method, kept distinct from 'RouteMethod' — the
-- request domain includes @HEAD@, @OPTIONS@, and methods no route ever
-- declares, none of which belong in the declaration-only 'RouteMethod' ADT.
newtype RequestMethod = RequestMethod Text
  deriving (Eq, Show)

requestMethod :: Text -> RequestMethod
requestMethod = RequestMethod

requestMethodText :: RequestMethod -> Text
requestMethodText (RequestMethod value) = value

-- | An incoming request's target path, kept distinct from 'RequestMethod' so
-- 'matchRouteMethod' cannot compile with the two swapped.
newtype RequestPath = RequestPath Text
  deriving (Eq, Show)

requestPath :: Text -> RequestPath
requestPath = RequestPath

requestPathText :: RequestPath -> Text
requestPathText (RequestPath value) = value

-- | A route's declared method policy. 'RouteHidden' is its own case instead
-- of overloading an empty method list, and 'RouteAllows' holds a 'Set' so a
-- duplicate declaration cannot make 'matchRouteMethod' or
-- 'routeAllowHeaderValue' re-derive uniqueness themselves.
data RouteMethodPolicy
  = RouteHidden
  | RouteAllows (Set RouteMethod)
  deriving (Eq, Show)

-- | Build a 'RouteMethodPolicy' from the plain list a route declaration
-- naturally writes (e.g. @[RouteGet]@); an empty declaration is 'RouteHidden'.
routeMethodPolicy :: [RouteMethod] -> RouteMethodPolicy
routeMethodPolicy declaredMethods =
  case declaredMethods of
    [] -> RouteHidden
    _ -> RouteAllows (Set.fromList declaredMethods)

-- | The plain list a 'RouteMethodPolicy' declares, for a caller that derives
-- its own declaration from an existing 'RouteCodec' rather than the reverse.
routeMethodPolicyMethods :: RouteMethodPolicy -> [RouteMethod]
routeMethodPolicyMethods routeMethodPolicyValue =
  case routeMethodPolicyValue of
    RouteHidden -> []
    RouteAllows declaredMethods -> Set.toList declaredMethods

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
    routeMethods :: route -> RouteMethodPolicy
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

-- | Match a route's path before evaluating its declared method policy. A
-- 'RouteHidden' policy is an explicit typed not-found route. It retains its
-- parsed route so an API or page route family can render its own 404
-- representation, while a path that did not parse uses 'notFoundRequest'.
-- The request's method and path are distinct types (not two adjacent 'Text'
-- values), so passing them in the wrong order is a compile error rather than
-- a silently-swapped runtime bug.
matchRouteMethod :: RouteCodec route context -> context -> RequestMethod -> RequestPath -> RouteDispatch route context
matchRouteMethod codec context incomingMethod incomingPath =
  case parseRoute codec context (requestPathText incomingPath) of
    Nothing -> RouteNotFound (notFoundRequest codec context)
    Just routeRequest ->
      case routeMethods codec (requestRoute routeRequest) of
        RouteHidden -> RouteNotFound routeRequest
        RouteAllows declaredMethods ->
          case NonEmpty.nonEmpty (Set.toList declaredMethods) of
            Nothing -> RouteNotFound routeRequest
            Just nonEmptyDeclaredMethods ->
              matchDeclaredMethods routeRequest (requestMethodText incomingMethod) nonEmptyDeclaredMethods

matchDeclaredMethods :: RouteRequest route context -> Text -> NonEmpty RouteMethod -> RouteDispatch route context
matchDeclaredMethods routeRequest incomingMethodText declaredMethods
  | incomingMethodText == "HEAD", RouteGet `elem` declaredMethods = RouteMatchedHead routeRequest
  | incomingMethodText == "OPTIONS" = RouteOptions routeRequest declaredMethods
  | any ((== incomingMethodText) . routeMethodText) declaredMethods = RouteMatched routeRequest
  | otherwise = RouteMethodNotAllowed routeRequest declaredMethods

-- | The declared methods reaching this from 'matchRouteMethod' are already
-- deduplicated by 'RouteAllows'' 'Set'; a caller building its own
-- 'NonEmpty' by hand still gets a correct (if possibly redundant) value.
routeAllowHeaderValue :: NonEmpty RouteMethod -> Text
routeAllowHeaderValue declaredMethods =
  Text.intercalate
    ", "
    ( map routeMethodText declaredMethodList
        <> ["HEAD" | RouteGet `elem` declaredMethodList]
        <> ["OPTIONS"]
    )
  where
    declaredMethodList = NonEmpty.toList declaredMethods
