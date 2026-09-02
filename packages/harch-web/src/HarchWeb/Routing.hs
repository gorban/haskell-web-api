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
    RequestTarget,
    requestTarget,
    RouteLocation (..),
    RouteParseResult (..),
    RouteDecodeError (..),
    PathSegment,
    pathSegmentText,
    requiredPathSegment,
    QueryName,
    queryNameText,
    requiredQueryName,
    QueryValue,
    queryValueText,
    queryValue,
    RouteDispatch (..),
    RouteMethod (..),
    RouteMethodPolicy (..),
    RouteRequest (..),
    RouteFamily (..),
    RequestMethod,
    requestMethod,
    requestMethodText,
    decodeRouteLocation,
    encodeRouteLocation,
    mapRouteParseResult,
    prefixRouteLocation,
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

import Data.Bits (shiftR, (.&.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.Char (isAsciiLower, isAsciiUpper, isControl, isDigit, ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Markup (SafeUrl, mkSafeUrl, requiredSafeUrl)
import HarchWeb.PathPrefix (PathPrefix, pathPrefixText)

-- | Raw HTTP target bytes held only between the server adapter and
-- 'decodeRouteLocation'. A route codec never receives this representation.
data RequestTarget = RequestTarget ByteString ByteString

-- | Construct a target from the request adapter's already-bounded raw path
-- and query bytes. The first component excludes the query delimiter; the
-- second includes it when it was present in the HTTP request.
requestTarget :: ByteString -> ByteString -> RequestTarget
requestTarget = RequestTarget

-- | One decoded path component. An empty component is retained for paths
-- such as @/second/@, allowing the owning codec to keep its established
-- trailing-slash policy without reconstructing raw input.
newtype PathSegment = PathSegment Text
  deriving (Eq, Ord, Show)

pathSegmentText :: PathSegment -> Text
pathSegmentText (PathSegment value) = value

-- | Build a route-table-owned segment. A rejected value is an authored route
-- definition error, not a request outcome.
requiredPathSegment :: Text -> PathSegment
requiredPathSegment value
  | Text.any invalidPathCharacter value = error ("invalid route path segment: " <> show value)
  | otherwise = PathSegment value

-- | A decoded query-field name. It is distinct from a path component so a
-- route codec cannot accidentally treat a query key as part of its ownership
-- prefix.
newtype QueryName = QueryName Text
  deriving (Eq, Ord, Show)

queryNameText :: QueryName -> Text
queryNameText (QueryName value) = value

requiredQueryName :: Text -> QueryName
requiredQueryName value
  | Text.any invalidQueryCharacter value = error ("invalid route query name: " <> show value)
  | otherwise = QueryName value

-- | A decoded query-field value. Repeated names deliberately remain a list in
-- 'RouteLocation': multiplicity is an input fact for a route codec to model.
newtype QueryValue = QueryValue Text
  deriving (Eq, Ord, Show)

queryValueText :: QueryValue -> Text
queryValueText (QueryValue value) = value

queryValue :: Text -> Maybe QueryValue
queryValue value
  | Text.any invalidQueryCharacter value = Nothing
  | otherwise = Just (QueryValue value)

-- | The sole structured representation passed between the HTTP request-target
-- decoder and a typed route codec. Path and query parsing are deliberately
-- separate from action/form-field parsing.
data RouteLocation = RouteLocation
  { routePathSegments :: [PathSegment],
    routeQueryFields :: [(QueryName, QueryValue)]
  }
  deriving (Eq, Show)

-- | Stable reasons the one request-target decoder can reject before a route
-- family gets a chance to claim the location.
data RouteDecodeError
  = InvalidRouteTargetEncoding
  | InvalidRoutePercentEncoding
  | EncodedRouteSeparator
  | AmbiguousRouteDotSegment
  | InvalidRouteBackslash
  | InvalidRouteControlCharacter
  deriving (Eq, Show)

-- | Route parsing distinguishes an ordinary ownership miss from malformed
-- input. This prevents a malformed target from falling through to a later
-- mounted module as if it had merely not matched an earlier one.
data RouteParseResult route context
  = RouteNotMatched
  | RouteMalformed RouteDecodeError
  | RouteParsed (RouteRequest route context)
  deriving (Eq, Show)

-- | Decode one bounded raw HTTP target. Percent decoding happens exactly once
-- per structured component; a decoded separator, backslash, dot segment, or
-- control character is rejected before a route family can claim it.
decodeRouteLocation :: RequestTarget -> Either RouteDecodeError RouteLocation
decodeRouteLocation (RequestTarget rawPath rawQuery) = do
  pathSegments <- decodePath rawPath
  queryFields <- decodeQuery rawQuery
  pure RouteLocation {routePathSegments = pathSegments, routeQueryFields = queryFields}

-- | Render a decoded location as one safe relative URL, escaping each path
-- and query component exactly once.
encodeRouteLocation :: RouteLocation -> SafeUrl
encodeRouteLocation RouteLocation {routePathSegments, routeQueryFields} =
  requiredSafeUrl (mkSafeUrl ("/" <> renderedPath <> renderedQuery))
  where
    renderedPath = Text.intercalate "/" (map (percentEncode . pathSegmentText) routePathSegments)
    renderedQuery =
      case routeQueryFields of
        [] -> Text.empty
        fields -> "?" <> Text.intercalate "&" (map renderQueryField fields)
    renderQueryField (name, value) = percentEncode (queryNameText name) <> "=" <> percentEncode (queryValueText value)

-- | Add the already-validated, browser-visible forwarding prefix to a route
-- location without sending it back through the raw request-target decoder.
prefixRouteLocation :: PathPrefix -> RouteLocation -> RouteLocation
prefixRouteLocation pathPrefix location =
  location {routePathSegments = prefixSegments <> routePathSegments location}
  where
    prefixSegments =
      case pathPrefixText pathPrefix of
        "" -> []
        prefixText -> map PathSegment (Text.splitOn "/" (Text.drop 1 prefixText))

decodePath :: ByteString -> Either RouteDecodeError [PathSegment]
decodePath rawPath =
  case ByteString.uncons rawPath of
    Just (47, pathWithoutLeadingSlash) -> traverse decodePathSegment (rawPathSegments pathWithoutLeadingSlash)
    _ -> Left InvalidRouteTargetEncoding
  where
    -- 'ByteString.split' represents an empty input as no components.  At the
    -- HTTP boundary, however, @/@ is one empty path component: preserving it
    -- lets an owning codec distinguish its root route from a path which was
    -- never a valid absolute request target.
    rawPathSegments pathWithoutLeadingSlash =
      case ByteString.split 47 pathWithoutLeadingSlash of
        [] -> [ByteString.empty]
        segments -> segments
    decodePathSegment rawSegment = do
      decoded <- decodeComponent rawSegment
      if decoded == "." || decoded == ".."
        then Left AmbiguousRouteDotSegment
        else
          if Text.any invalidPathCharacter decoded
            then Left (routeCharacterError decoded)
            else Right (PathSegment decoded)

decodeQuery :: ByteString -> Either RouteDecodeError [(QueryName, QueryValue)]
decodeQuery rawQuery =
  case ByteString.uncons rawQuery of
    Nothing -> Right []
    Just (63, queryWithoutDelimiter) -> traverse decodeQueryField (ByteString.split 38 queryWithoutDelimiter)
    _ -> Left InvalidRouteTargetEncoding
  where
    decodeQueryField rawField = do
      let (rawName, rawValueWithDelimiter) = ByteString.break (== 61) rawField
          rawValue = maybe ByteString.empty snd (ByteString.uncons rawValueWithDelimiter)
      name <- decodeComponent rawName
      value <- decodeComponent rawValue
      if Text.any invalidQueryCharacter name || Text.any invalidQueryCharacter value
        then Left (routeCharacterError (name <> value))
        else Right (QueryName name, QueryValue value)

decodeComponent :: ByteString -> Either RouteDecodeError Text
decodeComponent rawComponent = do
  decodedBytes <- percentDecode rawComponent
  case TextEncoding.decodeUtf8' decodedBytes of
    Left _ -> Left InvalidRouteTargetEncoding
    Right decoded -> Right decoded

percentDecode :: ByteString -> Either RouteDecodeError ByteString
percentDecode = fmap ByteString.pack . go . ByteString.unpack
  where
    go [] = Right []
    go (37 : high : low : remaining) = do
      highValue <- hexValue high
      lowValue <- hexValue low
      decodedRemaining <- go remaining
      pure ((highValue * 16 + lowValue) : decodedRemaining)
    go (37 : _) = Left InvalidRoutePercentEncoding
    go (byte : remaining) = (byte :) <$> go remaining

    hexValue byte
      | byte >= 48 && byte <= 57 = Right (byte - 48)
      | byte >= 65 && byte <= 70 = Right (byte - 55)
      | byte >= 97 && byte <= 102 = Right (byte - 87)
      | otherwise = Left InvalidRoutePercentEncoding

percentEncode :: Text -> Text
percentEncode = Text.concatMap encodeCharacter
  where
    encodeCharacter character
      | isUnreserved character = Text.singleton character
      | otherwise = Text.concat (map encodeByte (ByteString.unpack (TextEncoding.encodeUtf8 (Text.singleton character))))
    encodeByte byte =
      "%"
        <> Text.singleton (hexDigit (byte `shiftR` 4))
        <> Text.singleton (hexDigit (byte .&. 15))
    hexDigit value
      | value < 10 = toEnum (ord '0' + fromIntegral value)
      | otherwise = toEnum (ord 'A' + fromIntegral value - 10)

isUnreserved :: Char -> Bool
isUnreserved character =
  isAsciiLower character
    || isAsciiUpper character
    || isDigit character
    || character `elem` ['-', '.', '_', '~']

invalidPathCharacter :: Char -> Bool
invalidPathCharacter character = character == '/' || character == '\\' || isControl character

invalidQueryCharacter :: Char -> Bool
invalidQueryCharacter character = character == '\\' || isControl character

routeCharacterError :: Text -> RouteDecodeError
routeCharacterError value
  | Text.any (== '/') value = EncodedRouteSeparator
  | Text.any (== '\\') value = InvalidRouteBackslash
  | otherwise = InvalidRouteControlCharacter

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
  { parseRoute :: context -> RouteLocation -> RouteParseResult route context,
    renderRoute :: RouteRequest route context -> RouteLocation,
    notFoundRequest :: context -> RouteRequest route context,
    routeMethods :: route -> RouteMethodPolicy
  }

routeHref :: RouteCodec route context -> context -> route -> SafeUrl
routeHref codec context route =
  encodeRouteLocation (renderRoute codec RouteRequest {requestRoute = route, requestContext = context})

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
    { parseRoute = \context location ->
        case parseRoute codecA context location of
          RouteNotMatched -> mapRouteParseResult RouteFamilyB (parseRoute codecB context location)
          parsed -> mapRouteParseResult RouteFamilyA parsed,
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

-- | Map only a successfully parsed route while preserving the two results
-- whose meaning belongs to the shared route boundary: an ordinary ownership
-- miss and a malformed request location.  Mounts and route adapters use this
-- instead of recreating a partial-looking case split that could accidentally
-- turn malformed input into a fallthrough.
mapRouteParseResult :: (route -> mappedRoute) -> RouteParseResult route context -> RouteParseResult mappedRoute context
mapRouteParseResult embed parseResult =
  case parseResult of
    RouteNotMatched -> RouteNotMatched
    RouteMalformed routeError -> RouteMalformed routeError
    RouteParsed routeRequest -> RouteParsed (routeRequest {requestRoute = embed (requestRoute routeRequest)})

matchRoute :: RouteCodec route context -> context -> RouteLocation -> RouteParseResult route context
matchRoute = parseRoute

-- | Match a route's path before evaluating its declared method policy. A
-- 'RouteHidden' policy is an explicit typed not-found route. It retains its
-- parsed route so an API or page route family can render its own 404
-- representation, while a path that did not parse uses 'notFoundRequest'.
-- The request's method and path are distinct types (not two adjacent 'Text'
-- values), so passing them in the wrong order is a compile error rather than
-- a silently-swapped runtime bug.
matchRouteMethod :: RouteCodec route context -> context -> RequestMethod -> RouteLocation -> Either RouteDecodeError (RouteDispatch route context)
matchRouteMethod codec context incomingMethod location =
  case parseRoute codec context location of
    RouteNotMatched -> Right (RouteNotFound (notFoundRequest codec context))
    RouteMalformed routeError -> Left routeError
    RouteParsed routeRequest ->
      case routeMethods codec (requestRoute routeRequest) of
        RouteHidden -> Right (RouteNotFound routeRequest)
        RouteAllows declaredMethods ->
          case NonEmpty.nonEmpty (Set.toList declaredMethods) of
            Nothing -> Right (RouteNotFound routeRequest)
            Just nonEmptyDeclaredMethods ->
              Right (matchDeclaredMethods routeRequest (requestMethodText incomingMethod) nonEmptyDeclaredMethods)

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
