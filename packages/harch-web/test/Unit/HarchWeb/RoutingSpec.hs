{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
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
import Data.Text ()
import Data.Text qualified as Text ()
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb
import HarchWeb.Action qualified as Action ()
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
import Unit.HarchWeb.TestSupport (TestContext (testContextPathPrefix), TestRoute (KnownRoute, MissingRoute), defaultContext, sampleCodec, spanishContext)

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

existingSpec :: Spec
existingSpec = do
  describe "combineRouteCodecs" $ do
    it "parses a path only the first family recognizes into that family" $
      parseRoute combinedCodec RoutingTestContext "/read"
        `shouldBe` Just (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext)

    it "falls through to the second family for a path the first does not recognize" $
      parseRoute combinedCodec RoutingTestContext "/second-only"
        `shouldBe` Just (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)

    it "resolves a path both families recognize to the first family, not the second" $
      parseRoute combinedCodec RoutingTestContext "/shared"
        `shouldBe` Just (RouteRequest (RouteFamilyA RoutingSharedRoute) RoutingTestContext)

    it "reports no match for a path neither family recognizes" $
      parseRoute combinedCodec RoutingTestContext "/missing" `shouldBe` Nothing

    it "renders through whichever family a route belongs to" $
      expectAll
        ( (renderRoute combinedCodec (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext) `shouldBe` "/read")
            :| [renderRoute combinedCodec (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext) `shouldBe` "/second-only"]
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
        ( (matchRouteMethod combinedCodec RoutingTestContext (requestMethod "GET") (requestPath "/missing") `shouldBe` RouteNotFound (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext))
            :| [ matchRouteMethod combinedCodec RoutingTestContext (requestMethod "DELETE") (requestPath "/read")
                   `shouldBe` RouteMethodNotAllowed (RouteRequest (RouteFamilyA RoutingReadRoute) RoutingTestContext) (RouteGet :| []),
                 matchRouteMethod combinedCodec RoutingTestContext (requestMethod "HEAD") (requestPath "/second-only")
                   `shouldBe` RouteMatchedHead (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext),
                 matchRouteMethod combinedCodec RoutingTestContext (requestMethod "GET") (requestPath "/second-only")
                   `shouldBe` RouteMatched (RouteRequest (RouteFamilyB RoutingSecondOnlyRoute) RoutingTestContext)
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

  describe "matchRouteMethod" $ do
    it "keeps an unknown path as not found rather than manufacturing a 405" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "DELETE") (requestPath "/missing")
        `shouldBe` RouteNotFound missingRequest

    it "derives a 405 and Allow value only after a path matches" $
      case matchRouteMethod testCodec RoutingTestContext (requestMethod "POST") (requestPath "/read") of
        RouteMethodNotAllowed routeRequest declaredMethods -> do
          routeRequest `shouldBe` readRequest
          declaredMethods `shouldBe` RouteGet :| []
          routeAllowHeaderValue declaredMethods `shouldBe` "GET, HEAD, OPTIONS"
        routeDispatch -> expectationFailure ("expected a method mismatch, got " <> show routeDispatch)

    it "derives HEAD from GET without declaring another route" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "HEAD") (requestPath "/read")
        `shouldBe` RouteMatchedHead readRequest

    it "synthesizes OPTIONS from every declared method in declaration order" $
      case matchRouteMethod testCodec RoutingTestContext (requestMethod "OPTIONS") (requestPath "/write") of
        RouteOptions routeRequest declaredMethods -> do
          routeRequest `shouldBe` writeRequest
          declaredMethods `shouldBe` RoutePost :| [RoutePut]
          routeAllowHeaderValue declaredMethods `shouldBe` "POST, PUT, OPTIONS"
        routeDispatch -> expectationFailure ("expected options, got " <> show routeDispatch)

    it "matches a declared non-GET method directly" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "PUT") (requestPath "/write")
        `shouldBe` RouteMatched writeRequest

    it "keeps a parsed zero-method route so its route family can render its own 404" $
      matchRouteMethod testCodec RoutingTestContext (requestMethod "GET") (requestPath "/empty")
        `shouldBe` RouteNotFound emptyRequest

    it "also treats a route allowing an empty method set as not found" $
      matchRouteMethod
        (testCodec {routeMethods = const (RouteAllows Set.empty)})
        RoutingTestContext
        (requestMethod "GET")
        (requestPath "/read")
        `shouldBe` RouteNotFound readRequest

    it "renders every declared method in the public protocol vocabulary" $
      map routeMethodText [RouteGet, RoutePost, RoutePut, RoutePatch, RouteDelete]
        `shouldBe` ["GET", "POST", "PUT", "PATCH", "DELETE"]

    it "keeps RequestMethod, RequestPath, and RouteMethodPolicy comparable, printable, and total" $
      expectAll
        ( (requestMethod "GET" `shouldBe` requestMethod "GET")
            :| [ requestMethod "GET" `shouldNotBe` requestMethod "POST",
                 requestMethodText (requestMethod "GET") `shouldBe` "GET",
                 length (show (requestMethod "GET")) + length (showList [requestMethod "GET"] "") `shouldSatisfy` (> 0),
                 requestPath "/read" `shouldBe` requestPath "/read",
                 requestPath "/read" `shouldNotBe` requestPath "/write",
                 requestPathText (requestPath "/read") `shouldBe` "/read",
                 length (show (requestPath "/read")) + length (showList [requestPath "/read"] "") `shouldSatisfy` (> 0),
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
    { parseRoute = \requestContextValue path ->
        case path of
          "/read" -> Just (readRequest {requestContext = requestContextValue})
          "/write" -> Just (writeRequest {requestContext = requestContextValue})
          "/empty" -> Just (emptyRequest {requestContext = requestContextValue})
          "/shared" -> Just (RouteRequest RoutingSharedRoute requestContextValue)
          _ -> Nothing,
      renderRoute = \routeRequest ->
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
    { parseRoute = \requestContextValue path ->
        case path of
          "/second-only" -> Just (RouteRequest RoutingSecondOnlyRoute requestContextValue)
          "/shared" -> Just (RouteRequest RoutingSharedRouteB requestContextValue)
          _ -> Nothing,
      renderRoute = \routeRequest ->
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
      matchRoute sampleCodec defaultContext "/known"
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}

    it "can derive route context from the matched path" $
      matchRoute sampleCodec defaultContext "/es/known"
        `shouldBe` RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext}

    it "falls back to the stable not-found route for unsupported paths" $
      matchRoute sampleCodec defaultContext "/missing"
        `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}

  describe "renderRoute" $
    it "can include route context in generated paths" $ do
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})
        `shouldBe` "/known"
      renderRoute sampleCodec (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})
        `shouldBe` "/es/known"

  describe "routeHref" $
    it "reuses route rendering for app-provided navigation targets" $ do
      routeHref sampleCodec defaultContext KnownRoute `shouldBe` "/known"
      routeHref sampleCodec spanishContext KnownRoute `shouldBe` "/es/known"
      routeHref sampleCodec (defaultContext {testContextPathPrefix = "/app"}) KnownRoute `shouldBe` "/app/known"

spec = do
  existingSpec
  movedSpec
