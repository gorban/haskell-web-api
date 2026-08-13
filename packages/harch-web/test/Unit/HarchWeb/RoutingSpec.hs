{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.RoutingSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Routing
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

data TestRoute
  = ReadRoute
  | WriteRoute
  | EmptyRoute
  | SharedRoute
  | MissingRoute
  deriving (Eq, Show)

data SecondFamilyRoute
  = SecondOnlyRoute
  | SharedRouteB
  deriving (Eq, Show)

data TestContext = TestContext
  deriving (Eq, Show)

spec :: Spec
spec = do
  describe "combineRouteCodecs" $ do
    it "parses a path only the first family recognizes into that family" $
      parseRoute combinedCodec TestContext "/read"
        `shouldBe` Just (RouteRequest (RouteFamilyA ReadRoute) TestContext)

    it "falls through to the second family for a path the first does not recognize" $
      parseRoute combinedCodec TestContext "/second-only"
        `shouldBe` Just (RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext)

    it "resolves a path both families recognize to the first family, not the second" $
      parseRoute combinedCodec TestContext "/shared"
        `shouldBe` Just (RouteRequest (RouteFamilyA SharedRoute) TestContext)

    it "reports no match for a path neither family recognizes" $
      parseRoute combinedCodec TestContext "/missing" `shouldBe` Nothing

    it "renders through whichever family a route belongs to" $
      expectAll
        ( (renderRoute combinedCodec (RouteRequest (RouteFamilyA ReadRoute) TestContext) `shouldBe` "/read")
            :| [renderRoute combinedCodec (RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext) `shouldBe` "/second-only"]
        )

    it "uses the second, catch-all family's not-found route as the combined codec's not-found route" $
      notFoundRequest combinedCodec TestContext `shouldBe` RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext

    it "delegates routeMethods to whichever family a route belongs to" $
      expectAll
        ( (routeMethods combinedCodec (RouteFamilyA WriteRoute) `shouldBe` [RoutePost, RoutePut])
            :| [routeMethods combinedCodec (RouteFamilyB SecondOnlyRoute) `shouldBe` [RouteGet]]
        )

    it "derives one shared 404/405/HEAD/OPTIONS authority across both combined families" $
      expectAll
        ( (matchRouteMethod combinedCodec TestContext "GET" "/missing" `shouldBe` RouteNotFound (RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext))
            :| [ matchRouteMethod combinedCodec TestContext "DELETE" "/read"
                   `shouldBe` RouteMethodNotAllowed (RouteRequest (RouteFamilyA ReadRoute) TestContext) (RouteGet :| []),
                 matchRouteMethod combinedCodec TestContext "HEAD" "/second-only"
                   `shouldBe` RouteMatchedHead (RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext),
                 matchRouteMethod combinedCodec TestContext "GET" "/second-only"
                   `shouldBe` RouteMatched (RouteRequest (RouteFamilyB SecondOnlyRoute) TestContext)
               ]
        )

    it "keeps every combined route value comparable and printable" $ do
      let routes :: [RouteFamily TestRoute SecondFamilyRoute]
          routes = [RouteFamilyA ReadRoute, RouteFamilyA WriteRoute, RouteFamilyB SecondOnlyRoute]
      eqViaDictionary (RouteFamilyA ReadRoute :: RouteFamily TestRoute SecondFamilyRoute) (RouteFamilyA ReadRoute) `shouldBe` True
      neqViaDictionary (RouteFamilyA ReadRoute :: RouteFamily TestRoute SecondFamilyRoute) (RouteFamilyB SecondOnlyRoute) `shouldBe` True
      map showViaDictionary routes `shouldSatisfy` not . any null
      showsPrecViaDictionary 11 (RouteFamilyB SecondOnlyRoute :: RouteFamily TestRoute SecondFamilyRoute) "" `shouldSatisfy` not . null
      showListViaDictionary routes "" `shouldSatisfy` not . null

  describe "matchRouteMethod" $ do
    it "keeps an unknown path as not found rather than manufacturing a 405" $
      matchRouteMethod testCodec TestContext "DELETE" "/missing"
        `shouldBe` RouteNotFound missingRequest

    it "derives a 405 and Allow value only after a path matches" $
      case matchRouteMethod testCodec TestContext "POST" "/read" of
        RouteMethodNotAllowed routeRequest declaredMethods -> do
          routeRequest `shouldBe` readRequest
          declaredMethods `shouldBe` RouteGet :| []
          routeAllowHeaderValue declaredMethods `shouldBe` "GET, HEAD, OPTIONS"
        routeDispatch -> expectationFailure ("expected a method mismatch, got " <> show routeDispatch)

    it "derives HEAD from GET without declaring another route" $
      matchRouteMethod testCodec TestContext "HEAD" "/read"
        `shouldBe` RouteMatchedHead readRequest

    it "synthesizes OPTIONS from every declared method in declaration order" $
      case matchRouteMethod testCodec TestContext "OPTIONS" "/write" of
        RouteOptions routeRequest declaredMethods -> do
          routeRequest `shouldBe` writeRequest
          declaredMethods `shouldBe` RoutePost :| [RoutePut]
          routeAllowHeaderValue declaredMethods `shouldBe` "POST, PUT, OPTIONS"
        routeDispatch -> expectationFailure ("expected options, got " <> show routeDispatch)

    it "matches a declared non-GET method directly" $
      matchRouteMethod testCodec TestContext "PUT" "/write"
        `shouldBe` RouteMatched writeRequest

    it "keeps a parsed zero-method route so its route family can render its own 404" $
      matchRouteMethod testCodec TestContext "GET" "/empty"
        `shouldBe` RouteNotFound emptyRequest

    it "renders every declared method in the public protocol vocabulary" $
      map routeMethodText [RouteGet, RoutePost, RoutePut, RoutePatch, RouteDelete]
        `shouldBe` ["GET", "POST", "PUT", "PATCH", "DELETE"]

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
      eqViaDictionary RouteGet RouteGet `shouldBe` True
      neqViaDictionary RouteGet RoutePost `shouldBe` True
      dispatches
        `shouldBe` [ RouteNotFound missingRequest,
                     RouteMethodNotAllowed readRequest (RouteGet :| []),
                     RouteMatched writeRequest,
                     RouteMatchedHead readRequest,
                     RouteOptions writeRequest (RoutePost :| [RoutePut])
                   ]
      map show methods
        `shouldBe` ["RouteGet", "RoutePost", "RoutePut", "RoutePatch", "RouteDelete"]
      showViaDictionary RouteDelete `shouldBe` "RouteDelete"
      showsPrecViaDictionary 11 RouteDelete "" `shouldBe` "RouteDelete"
      showListViaDictionary [RoutePatch, RouteDelete] "" `shouldBe` "[RoutePatch,RouteDelete]"
      eqViaDictionary (RouteMatched readRequest) (RouteMatched readRequest) `shouldBe` True
      neqViaDictionary (RouteMatched readRequest) (RouteMatchedHead readRequest) `shouldBe` True
      showViaDictionary (RouteOptions writeRequest (RoutePost :| [RoutePut])) `shouldSatisfy` not . null
      showsPrecViaDictionary 11 (RouteMatchedHead readRequest) "" `shouldSatisfy` not . null
      showListViaDictionary dispatches "" `shouldSatisfy` not . null
      map show dispatches `shouldSatisfy` not . any null

testCodec :: RouteCodec TestRoute TestContext
testCodec =
  RouteCodec
    { parseRoute = \requestContextValue path ->
        case path of
          "/read" -> Just (readRequest {requestContext = requestContextValue})
          "/write" -> Just (writeRequest {requestContext = requestContextValue})
          "/empty" -> Just (emptyRequest {requestContext = requestContextValue})
          "/shared" -> Just (RouteRequest SharedRoute requestContextValue)
          _ -> Nothing,
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          ReadRoute -> "/read"
          WriteRoute -> "/write"
          EmptyRoute -> "/empty"
          SharedRoute -> "/shared"
          MissingRoute -> "/404",
      notFoundRequest = \requestContextValue -> missingRequest {requestContext = requestContextValue},
      routeMethods = \case
        ReadRoute -> [RouteGet]
        WriteRoute -> [RoutePost, RoutePut]
        EmptyRoute -> []
        SharedRoute -> [RouteGet]
        MissingRoute -> []
    }

-- | A second, independent route family used only to exercise
-- 'combineRouteCodecs': it also recognizes @\/shared@, so the combined-codec
-- tests can confirm first-family precedence rather than an incidental
-- absence of overlap.
secondCodec :: RouteCodec SecondFamilyRoute TestContext
secondCodec =
  RouteCodec
    { parseRoute = \requestContextValue path ->
        case path of
          "/second-only" -> Just (RouteRequest SecondOnlyRoute requestContextValue)
          "/shared" -> Just (RouteRequest SharedRouteB requestContextValue)
          _ -> Nothing,
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          SecondOnlyRoute -> "/second-only"
          SharedRouteB -> "/shared",
      notFoundRequest = RouteRequest SecondOnlyRoute,
      routeMethods = \case
        SecondOnlyRoute -> [RouteGet]
        SharedRouteB -> [RouteGet]
    }

combinedCodec :: RouteCodec (RouteFamily TestRoute SecondFamilyRoute) TestContext
combinedCodec = combineRouteCodecs testCodec secondCodec

readRequest :: RouteRequest TestRoute TestContext
readRequest = RouteRequest ReadRoute TestContext

writeRequest :: RouteRequest TestRoute TestContext
writeRequest = RouteRequest WriteRoute TestContext

emptyRequest :: RouteRequest TestRoute TestContext
emptyRequest = RouteRequest EmptyRoute TestContext

missingRequest :: RouteRequest TestRoute TestContext
missingRequest = RouteRequest MissingRoute TestContext

eqViaDictionary :: (Eq value) => value -> value -> Bool
eqViaDictionary = (==)
{-# NOINLINE eqViaDictionary #-}

neqViaDictionary :: (Eq value) => value -> value -> Bool
neqViaDictionary = (/=)
{-# NOINLINE neqViaDictionary #-}

showViaDictionary :: (Show value) => value -> String
showViaDictionary = show
{-# NOINLINE showViaDictionary #-}

showsPrecViaDictionary :: (Show value) => Int -> value -> ShowS
showsPrecViaDictionary = showsPrec
{-# NOINLINE showsPrecViaDictionary #-}

showListViaDictionary :: (Show value) => [value] -> ShowS
showListViaDictionary = showList
{-# NOINLINE showListViaDictionary #-}
