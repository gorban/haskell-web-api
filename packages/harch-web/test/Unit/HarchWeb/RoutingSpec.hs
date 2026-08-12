{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.RoutingSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Routing
import Test.Hspec

data TestRoute
  = ReadRoute
  | WriteRoute
  | EmptyRoute
  | MissingRoute
  deriving (Eq, Show)

data TestContext = TestContext
  deriving (Eq, Show)

spec :: Spec
spec = do
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
          _ -> Nothing,
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          ReadRoute -> "/read"
          WriteRoute -> "/write"
          EmptyRoute -> "/empty"
          MissingRoute -> "/404",
      notFoundRequest = \requestContextValue -> missingRequest {requestContext = requestContextValue},
      routeMethods = \case
        ReadRoute -> [RouteGet]
        WriteRoute -> [RoutePost, RoutePut]
        EmptyRoute -> []
        MissingRoute -> []
    }

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
