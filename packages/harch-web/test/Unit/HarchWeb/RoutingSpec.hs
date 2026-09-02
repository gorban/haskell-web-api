{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception (ErrorCall (..), evaluate)
import Control.Monad ()
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Set qualified as Set (empty, fromList)
import Data.Text (Text)
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb
import HarchWeb.Action qualified as Action ()
import HarchWeb.ApplicationModule (ContextProjection (..), RouteMount (..), mountRouteCodec)
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestContext (testContextPathPrefix), TestRoute (KnownRoute), defaultContext, routeLocationText, sampleCodec, spanishContext, testRouteLocation)

data RoutingTestRoute
  = RoutingReadRoute
  | RoutingWriteRoute
  | RoutingEmptyRoute
  | RoutingSharedRoute
  | RoutingMissingRoute
  deriving (Eq, Show)

data SecondFamilyRoute
  = RoutingSecondOnlyRoute
  | RoutingSharedRouteB
  deriving (Eq, Show)

data RoutingTestContext = RoutingTestContext
  deriving (Eq, Show)

requiredQueryValue :: Text -> QueryValue
requiredQueryValue input =
  case queryValue input of
    Nothing -> error "test query value"
    Just queryResult -> queryResult

data MountedParentRoute
  = CatalogParentRoute
  | OtherParentRoute
  deriving (Eq, Show)

data MountedChildRoute = CatalogChildRoute
  deriving (Eq, Show)

mountedCatalogCodec :: RouteCodec MountedParentRoute Int
mountedCatalogCodec =
  mountRouteCodec
    RouteMount
      { routeMountName = requiredModuleName,
        routeMountPrefix = requiredPathSegment "catalog" :| [],
        embedChildRoute = const CatalogParentRoute,
        projectChildRoute = \case
          CatalogParentRoute -> Just CatalogChildRoute
          OtherParentRoute -> Nothing
      }
    (ContextProjection (const "catalog-context"))
    (RouteRequest OtherParentRoute)
    mountedChildCodec
  where
    requiredModuleName =
      case mkModuleName "root.catalog" of
        Left moduleNameError -> error ("invalid test module name: " <> show moduleNameError)
        Right moduleName -> moduleName

mountedChildCodec :: RouteCodec MountedChildRoute Text
mountedChildCodec =
  RouteCodec
    { parseRoute = \childContext location ->
        if routeLocationText location == "/items"
          then RouteParsed (RouteRequest CatalogChildRoute childContext)
          else RouteNotMatched,
      renderRoute = const (testRouteLocation "/items"),
      notFoundRequest = RouteRequest CatalogChildRoute,
      routeMethods = const (routeMethodPolicy [RouteGet])
    }

malformedCodec :: RouteCodec RoutingTestRoute RoutingTestContext
malformedCodec =
  RouteCodec
    { parseRoute = \_ _ -> RouteMalformed InvalidRouteTargetEncoding,
      renderRoute = const (testRouteLocation "/malformed"),
      notFoundRequest = RouteRequest RoutingMissingRoute,
      routeMethods = const RouteHidden
    }

existingSpec :: Spec
existingSpec = do
  describe "RouteLocation HTTP boundary" $ do
    it "decodes each path and query component once while preserving repeated and empty fields" $
      decodeRouteLocation (requestTarget "/caf%C3%A9/items" "?tag=one&tag=&=empty&flag")
        `shouldBe` Right
          RouteLocation
            { routePathSegments = [requiredPathSegment "café", requiredPathSegment "items"],
              routeQueryFields =
                [ (requiredQueryName "tag", requiredQueryValue "one"),
                  (requiredQueryName "tag", requiredQueryValue ""),
                  (requiredQueryName "", requiredQueryValue "empty"),
                  (requiredQueryName "flag", requiredQueryValue "")
                ]
            }

    it "rejects malformed, ambiguous, and unsafe request-target components" $
      expectAll
        ( (decodeRouteLocation (requestTarget "/%" "") `shouldBe` Left InvalidRoutePercentEncoding)
            :| [ decodeRouteLocation (requestTarget "/%2F" "") `shouldBe` Left EncodedRouteSeparator,
                 decodeRouteLocation (requestTarget "/%2e" "") `shouldBe` Left AmbiguousRouteDotSegment,
                 decodeRouteLocation (requestTarget "/a%5Cb" "") `shouldBe` Left InvalidRouteBackslash,
                 decodeRouteLocation (requestTarget "/safe" "?key=%00") `shouldBe` Left InvalidRouteControlCharacter,
                 decodeRouteLocation (requestTarget "/safe" "?key=%FF") `shouldBe` Left InvalidRouteTargetEncoding
               ]
        )

    it "renders a structured location with one percent-encoding layer" $
      safeUrlText
        ( encodeRouteLocation
            RouteLocation
              { routePathSegments = [requiredPathSegment "café", requiredPathSegment "a b"],
                routeQueryFields =
                  [ (requiredQueryName "return", requiredQueryValue "/orders/42?tab=one"),
                    (requiredQueryName "notice", requiredQueryValue "saved")
                  ]
              }
        )
        `shouldBe` "/caf%C3%A9/a%20b?return=%2Forders%2F42%3Ftab%3Done&notice=saved"

    it "keeps authored location declarations distinct and rejects invalid declarations before routing" $ do
      let firstSegment = requiredPathSegment "catalog"
          secondSegment = requiredPathSegment "orders"
          firstName = requiredQueryName "tab"
          secondName = requiredQueryName "view"
          firstValue = requiredQueryValue "details"
          secondValue = requiredQueryValue "summary"
          location = RouteLocation [firstSegment] [(firstName, firstValue)]
      expectAll
        ( (firstSegment < secondSegment `shouldBe` True)
            :| [ firstName < secondName `shouldBe` True,
                 firstValue < secondValue `shouldBe` True,
                 location `shouldBe` RouteLocation [firstSegment] [(firstName, firstValue)],
                 length (show firstSegment) + length (show firstName) + length (show firstValue) + length (show location) `shouldSatisfy` (> 0),
                 showList [firstSegment] "" <> showList [firstName] "" <> showList [firstValue] "" <> showList [location] "" `shouldSatisfy` (not . null),
                 queryValue "bad\\value" `shouldBe` Nothing,
                 routeMethodText RoutePatch `shouldBe` "PATCH",
                 requestMethodText (requestMethod "TRACE") `shouldBe` "TRACE",
                 routeMethodPolicyMethods (routeMethodPolicy []) `shouldBe` [],
                 routeMethodPolicyMethods (routeMethodPolicy [RouteDelete, RouteGet, RouteDelete]) `shouldBe` [RouteGet, RouteDelete]
               ]
        )
      assertOrderedPublicValue firstSegment secondSegment
      assertOrderedPublicValue firstName secondName
      assertOrderedPublicValue firstValue secondValue
      assertComparablePublicValue location (RouteLocation [secondSegment] [(secondName, secondValue)])
      assertComparablePublicValue InvalidRouteTargetEncoding InvalidRoutePercentEncoding
      assertComparablePublicValue
        (RouteParsed (RouteRequest RoutingReadRoute RoutingTestContext) :: RouteParseResult RoutingTestRoute RoutingTestContext)
        RouteNotMatched
      evaluate (requiredPathSegment "bad/path")
        `shouldThrow` \case
          ErrorCall message -> message == "invalid route path segment: \"bad/path\""
      evaluate (requiredQueryName "bad\\name")
        `shouldThrow` \case
          ErrorCall message -> message == "invalid route query name: \"bad\\\\name\""

    it "rejects malformed target envelopes and malformed percent octets before a codec runs" $
      expectAll
        ( (decodeRouteLocation (requestTarget "relative" "") `shouldBe` Left InvalidRouteTargetEncoding)
            :| [ decodeRouteLocation (requestTarget "/safe" "key=value") `shouldBe` Left InvalidRouteTargetEncoding,
                 decodeRouteLocation (requestTarget "/%G0" "") `shouldBe` Left InvalidRoutePercentEncoding,
                 decodeRouteLocation (requestTarget "/safe" "?key=%G0") `shouldBe` Left InvalidRoutePercentEncoding
               ]
        )

    it "adds a validated forwarding prefix without re-parsing decoded query fields" $ do
      prefix <-
        case parseRequestPathPrefix "/app/tenant" of
          Left prefixError -> expectationFailure (show prefixError) >> fail "could not construct test prefix"
          Right configuredPrefix -> pure configuredPrefix
      let location =
            RouteLocation
              { routePathSegments = [requiredPathSegment "orders", requiredPathSegment "42"],
                routeQueryFields = [(requiredQueryName "tab", requiredQueryValue "details")]
              }
      prefixRouteLocation prefix location
        `shouldBe` RouteLocation
          { routePathSegments = [requiredPathSegment "app", requiredPathSegment "tenant", requiredPathSegment "orders", requiredPathSegment "42"],
            routeQueryFields = [(requiredQueryName "tab", requiredQueryValue "details")]
          }
      prefixRouteLocation emptyPathPrefix location `shouldBe` location

  describe "mountRouteCodec" $ do
    it "projects context one way and owns only its typed structured prefix" $
      expectAll
        ( ( parseRoute mountedCatalogCodec 42 (testRouteLocation "/catalog/items")
              `shouldBe` RouteParsed (RouteRequest CatalogParentRoute 42)
          )
            :| [ parseRoute mountedCatalogCodec 42 (testRouteLocation "/orders/items") `shouldBe` RouteNotMatched,
                 routeLocationText (renderRoute mountedCatalogCodec (RouteRequest CatalogParentRoute 42)) `shouldBe` "/catalog/items",
                 routeMethods mountedCatalogCodec OtherParentRoute `shouldBe` RouteHidden
               ]
        )

  describe "combineRouteCodecs" $ do
    it "parses a path only the first family recognizes into that family" $
      parseRoute combinedCodec RoutingTestContext (testRouteLocation "/read")
        `shouldBe` RouteParsed (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext)

    it "falls through to the second family for a path the first does not recognize" $
      parseRoute combinedCodec RoutingTestContext (testRouteLocation "/second-only")
        `shouldBe` RouteParsed (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)

    it "resolves a path both families recognize to the first family, not the second" $
      parseRoute combinedCodec RoutingTestContext (testRouteLocation "/shared")
        `shouldBe` RouteParsed (RouteRequest (RouteFamilyA RoutingSharedRoute) RoutingTestContext)

    it "reports no match for a path neither family recognizes" $
      parseRoute combinedCodec RoutingTestContext (testRouteLocation "/missing") `shouldBe` RouteNotMatched

    it "renders through whichever family a route belongs to" $
      expectAll
        ( (routeLocationText (renderRoute combinedCodec (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext)) `shouldBe` "/read")
            :| [routeLocationText (renderRoute combinedCodec (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)) `shouldBe` "/second-only"]
        )

    it "uses the second, catch-all family's not-found route as the combined codec's not-found route" $
      notFoundRequest combinedCodec RoutingTestContext `shouldBe` RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext

    it "delegates routeMethods to whichever family a route belongs to" $
      expectAll
        ( (routeMethods combinedCodec (RouteFamilyA RoutingWriteRoute) `shouldBe` routeMethodPolicy [RoutePost, RoutePut])
            :| [routeMethods combinedCodec (RouteFamilyB RoutingSecondOnlyRoute) `shouldBe` routeMethodPolicy [RouteGet]]
        )

    it "derives one shared 404/405/HEAD/OPTIONS authority across both combined families" $
      expectAll
        ( (matchRouteMethod combinedCodec RoutingTestContext (requestMethod "GET") (testRouteLocation "/missing") `shouldBe` Right (RouteNotFound (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)))
            :| [ matchRouteMethod combinedCodec RoutingTestContext (requestMethod "DELETE") (testRouteLocation "/read")
                   `shouldBe` Right (RouteMethodNotAllowed (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext) (RouteGet :| [])),
                 matchRouteMethod combinedCodec RoutingTestContext (requestMethod "HEAD") (testRouteLocation "/second-only")
                   `shouldBe` Right (RouteMatchedHead (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)),
                 matchRouteMethod combinedCodec RoutingTestContext (requestMethod "GET") (testRouteLocation "/second-only")
                   `shouldBe` Right (RouteMatched (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext))
               ]
        )

    it "keeps every combined route value comparable and printable" $ do
      let routes :: [RouteFamily RoutingTestRoute SecondFamilyRoute]
          routes = [RouteFamilyA RoutingReadRoute, RouteFamilyA RoutingWriteRoute, RouteFamilyB RoutingSecondOnlyRoute]
      ((RouteFamilyA RoutingReadRoute :: RouteFamily RoutingTestRoute SecondFamilyRoute) == RouteFamilyA RoutingReadRoute) `shouldBe` True
      ((RouteFamilyA RoutingReadRoute :: RouteFamily RoutingTestRoute SecondFamilyRoute) /= RouteFamilyB RoutingSecondOnlyRoute) `shouldBe` True
      map show routes `shouldSatisfy` not . any null
      showsPrec 11 (RouteFamilyB RoutingSecondOnlyRoute :: RouteFamily RoutingTestRoute SecondFamilyRoute) "" `shouldSatisfy` not . null
      showList routes "" `shouldSatisfy` not . null

  describe "mapRouteParseResult" $ do
    it "changes only a parsed route and preserves miss and malformed outcomes" $ do
      let mapTestResult :: RouteParseResult RoutingTestRoute RoutingTestContext -> RouteParseResult (RouteFamily RoutingTestRoute SecondFamilyRoute) RoutingTestContext
          mapTestResult = mapRouteParseResult RouteFamilyA
      expectAll
        ( ( mapTestResult RouteNotMatched
              `shouldBe` RouteNotMatched
          )
            :| [ mapTestResult (RouteMalformed InvalidRouteTargetEncoding)
                   `shouldBe` RouteMalformed InvalidRouteTargetEncoding,
                 mapTestResult (RouteParsed readRequest)
                   `shouldBe` RouteParsed (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext)
               ]
        )

  describe "matchRouteMethod" $ do
    it "returns the malformed route error instead of deriving a dispatch result" $
      matchRouteMethod malformedCodec RoutingTestContext (requestMethod "GET") (testRouteLocation "/malformed")
        `shouldBe` Left InvalidRouteTargetEncoding

    it "keeps an unknown path as not found rather than manufacturing a 405" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "DELETE") (testRouteLocation "/missing")
        `shouldBe` Right (RouteNotFound missingRequest)

    it "derives a 405 and Allow value only after a path matches" $
      case matchRouteMethod testCodec RoutingTestContext (requestMethod "POST") (testRouteLocation "/read") of
        Right (RouteMethodNotAllowed routeRequest declaredMethods) -> do
          routeRequest `shouldBe` readRequest
          declaredMethods `shouldBe` RouteGet :| []
          routeAllowHeaderValue declaredMethods `shouldBe` "GET, HEAD, OPTIONS"
        routeDispatch -> expectationFailure ("expected a method mismatch, got " <> show routeDispatch)

    it "derives HEAD from GET without declaring another route" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "HEAD") (testRouteLocation "/read")
        `shouldBe` Right (RouteMatchedHead readRequest)

    it "synthesizes OPTIONS from every declared method in declaration order" $
      case matchRouteMethod testCodec RoutingTestContext (requestMethod "OPTIONS") (testRouteLocation "/write") of
        Right (RouteOptions routeRequest declaredMethods) -> do
          routeRequest `shouldBe` writeRequest
          declaredMethods `shouldBe` RoutePost :| [RoutePut]
          routeAllowHeaderValue declaredMethods `shouldBe` "POST, PUT, OPTIONS"
        routeDispatch -> expectationFailure ("expected options, got " <> show routeDispatch)

    it "matches a declared non-GET method directly" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "PUT") (testRouteLocation "/write")
        `shouldBe` Right (RouteMatched writeRequest)

    it "keeps a parsed zero-method route so its route family can render its own 404" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "GET") (testRouteLocation "/empty")
        `shouldBe` Right (RouteNotFound emptyRequest)

    it "also treats a route allowing an empty method set as not found" $
      matchRouteMethod
        (testCodec {routeMethods = const (RouteAllows Set.empty)})
        RoutingTestContext
        (requestMethod "GET")
        (testRouteLocation "/read")
        `shouldBe` Right (RouteNotFound readRequest)

    it "renders every declared method in the public protocol vocabulary" $
      map routeMethodText [RouteGet, RoutePost, RoutePut, RoutePatch, RouteDelete]
        `shouldBe` ["GET", "POST", "PUT", "PATCH", "DELETE"]

    it "keeps RequestMethod, RouteLocation, and RouteMethodPolicy comparable, printable, and total" $
      expectAll
        ( (requestMethod "GET" `shouldBe` requestMethod "GET")
            :| [ requestMethod "GET" `shouldNotBe` requestMethod "POST",
                 requestMethodText (requestMethod "GET") `shouldBe` "GET",
                 length (show (requestMethod "GET")) + length (showList [requestMethod "GET"] "") `shouldSatisfy` (> 0),
                 testRouteLocation "/read" `shouldBe` testRouteLocation "/read",
                 testRouteLocation "/read" `shouldNotBe` testRouteLocation "/write",
                 routeLocationText (testRouteLocation "/read") `shouldBe` "/read",
                 length (show (testRouteLocation "/read")) + length (showList [testRouteLocation "/read"] "") `shouldSatisfy` (> 0),
                 routeMethodPolicy [] `shouldBe` RouteHidden,
                 routeMethodPolicy [RouteGet, RouteGet] `shouldBe` RouteAllows (Set.fromList [RouteGet]),
                 RouteHidden `shouldNotBe` routeMethodPolicy [RouteGet],
                 length (show RouteHidden) + length (showList [RouteHidden] "") `shouldSatisfy` (> 0),
                 routeMethodPolicyMethods RouteHidden `shouldBe` [],
                 routeMethodPolicyMethods (routeMethodPolicy [RoutePost, RouteGet]) `shouldBe` [RouteGet, RoutePost],
                 compare RouteGet RoutePost `shouldBe` LT,
                 compare RoutePost RouteGet `shouldBe` GT,
                 compare RouteGet RouteGet `shouldBe` EQ,
                 (RouteGet < RoutePost) `shouldBe` True,
                 (RouteGet <= RouteGet) `shouldBe` True,
                 (RoutePost > RouteGet) `shouldBe` True,
                 (RouteGet >= RouteGet) `shouldBe` True,
                 max RouteGet RoutePost `shouldBe` RoutePost,
                 min RouteGet RoutePost `shouldBe` RouteGet
               ]
        )

    it "keeps every public dispatch value comparable and inspectable" $ do
      let methods = [RouteGet, RoutePost, RoutePut, RoutePatch, RouteDelete]
          dispatches =
            [ RouteNotFound missingRequest,
              RouteMethodNotAllowed readRequest (RouteGet :| []),
              RouteMatched writeRequest,
              RouteMatchedHead readRequest,
              RouteOptions writeRequest (RoutePost :| [RoutePut])
            ]
      methods `shouldBe` [RouteGet, RoutePost, RoutePut, RoutePatch, RouteDelete]
      (RouteGet /= RoutePost) `shouldBe` True
      dispatches
        `shouldBe` [ RouteNotFound missingRequest,
                     RouteMethodNotAllowed readRequest (RouteGet :| []),
                     RouteMatched writeRequest,
                     RouteMatchedHead readRequest,
                     RouteOptions writeRequest (RoutePost :| [RoutePut])
                   ]
      map show methods
        `shouldBe` ["RouteGet", "RoutePost", "RoutePut", "RoutePatch", "RouteDelete"]
      show RouteDelete `shouldBe` "RouteDelete"
      showsPrec 11 RouteDelete "" `shouldBe` "RouteDelete"
      showList [RoutePatch, RouteDelete] "" `shouldBe` "[RoutePatch,RouteDelete]"
      (RouteMatched readRequest == RouteMatched (RouteRequest RoutingReadRoute RoutingTestContext)) `shouldBe` True
      (RouteMatched readRequest /= RouteMatchedHead readRequest) `shouldBe` True
      show (RouteOptions writeRequest (RoutePost :| [RoutePut])) `shouldSatisfy` not . null
      showsPrec 11 (RouteMatchedHead readRequest) "" `shouldSatisfy` not . null
      showList dispatches "" `shouldSatisfy` not . null
      map show dispatches `shouldSatisfy` not . any null

testCodec :: RouteCodec RoutingTestRoute RoutingTestContext
testCodec =
  RouteCodec
    { parseRoute = \requestContextValue location ->
        case routeLocationText location of
          "/read" -> RouteParsed (readRequest {requestContext = requestContextValue})
          "/write" -> RouteParsed (writeRequest {requestContext = requestContextValue})
          "/empty" -> RouteParsed (emptyRequest {requestContext = requestContextValue})
          "/shared" -> RouteParsed (RouteRequest RoutingSharedRoute requestContextValue)
          _ -> RouteNotMatched,
      renderRoute = \routeRequest ->
        testRouteLocation $
          case requestRoute routeRequest of
            RoutingReadRoute -> "/read"
            RoutingWriteRoute -> "/write"
            RoutingEmptyRoute -> "/empty"
            RoutingSharedRoute -> "/shared"
            RoutingMissingRoute -> "/404",
      notFoundRequest = \requestContextValue -> missingRequest {requestContext = requestContextValue},
      routeMethods =
        routeMethodPolicy . \case
          RoutingReadRoute -> [RouteGet]
          RoutingWriteRoute -> [RoutePost, RoutePut]
          RoutingEmptyRoute -> []
          RoutingSharedRoute -> [RouteGet]
          RoutingMissingRoute -> []
    }

-- | A second, independent route family used only to exercise
-- 'combineRouteCodecs': it also recognizes @\/shared@, so the combined-codec
-- tests can confirm first-family precedence rather than an incidental
-- absence of overlap.
secondCodec :: RouteCodec SecondFamilyRoute RoutingTestContext
secondCodec =
  RouteCodec
    { parseRoute = \requestContextValue location ->
        case routeLocationText location of
          "/second-only" -> RouteParsed (RouteRequest RoutingSecondOnlyRoute requestContextValue)
          "/shared" -> RouteParsed (RouteRequest RoutingSharedRouteB requestContextValue)
          _ -> RouteNotMatched,
      renderRoute = \routeRequest ->
        testRouteLocation $
          case requestRoute routeRequest of
            RoutingSecondOnlyRoute -> "/second-only"
            RoutingSharedRouteB -> "/shared",
      notFoundRequest = RouteRequest RoutingSecondOnlyRoute,
      routeMethods =
        routeMethodPolicy . \case
          RoutingSecondOnlyRoute -> [RouteGet]
          RoutingSharedRouteB -> [RouteGet]
    }

combinedCodec :: RouteCodec (RouteFamily RoutingTestRoute SecondFamilyRoute) RoutingTestContext
combinedCodec = combineRouteCodecs testCodec secondCodec

readRequest :: RouteRequest RoutingTestRoute RoutingTestContext
readRequest = RouteRequest RoutingReadRoute RoutingTestContext

writeRequest :: RouteRequest RoutingTestRoute RoutingTestContext
writeRequest = RouteRequest RoutingWriteRoute RoutingTestContext

emptyRequest :: RouteRequest RoutingTestRoute RoutingTestContext
emptyRequest = RouteRequest RoutingEmptyRoute RoutingTestContext

missingRequest :: RouteRequest RoutingTestRoute RoutingTestContext
missingRequest = RouteRequest RoutingMissingRoute RoutingTestContext

movedSpec :: Spec
movedSpec = do
  describe "matchRoute" $ do
    it "returns parsed routes for supported paths" $
      matchRoute sampleCodec defaultContext (testRouteLocation "/known")
        `shouldBe` RouteParsed RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}

    it "can derive route context from the matched path" $
      matchRoute sampleCodec defaultContext (testRouteLocation "/es/known")
        `shouldBe` RouteParsed RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec defaultContext (testRouteLocation "/missing")
        `shouldBe` RouteNotMatched

  describe "renderRoute" $
    it "can include route context in generated paths" $ do
      routeLocationText (renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` "/known"
      routeLocationText (renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}))
        `shouldBe` "/es/known"

  describe "routeHref" $
    it "reuses route rendering for app-provided navigation targets" $ do
      safeUrlText (routeHref sampleCodec defaultContext KnownRoute) `shouldBe` "/known"
      safeUrlText (routeHref sampleCodec spanishContext KnownRoute) `shouldBe` "/es/known"
      safeUrlText (routeHref sampleCodec (defaultContext {testContextPathPrefix = "/app"}) KnownRoute) `shouldBe` "/app/known"

spec = do
  existingSpec
  movedSpec

assertOrderedPublicValue :: (Ord value, Show value) => value -> value -> Expectation
assertOrderedPublicValue firstValue secondValue =
  expectAll
    ( (firstValue /= secondValue `shouldBe` True)
        :| [ firstValue <= secondValue `shouldBe` True,
             secondValue > firstValue `shouldBe` True,
             secondValue >= firstValue `shouldBe` True,
             compare firstValue secondValue `shouldBe` LT,
             min firstValue secondValue `shouldBe` firstValue,
             max firstValue secondValue `shouldBe` secondValue,
             show firstValue `shouldSatisfy` not . null,
             showsPrec 11 firstValue "" `shouldSatisfy` not . null,
             showList [firstValue] "" `shouldSatisfy` not . null
           ]
    )

assertComparablePublicValue :: (Eq value, Show value) => value -> value -> Expectation
assertComparablePublicValue firstValue secondValue =
  expectAll
    ( (firstValue /= secondValue `shouldBe` True)
        :| [ show firstValue `shouldSatisfy` not . null,
             showsPrec 11 firstValue "" `shouldSatisfy` not . null,
             showList [firstValue] "" `shouldSatisfy` not . null
           ]
    )
