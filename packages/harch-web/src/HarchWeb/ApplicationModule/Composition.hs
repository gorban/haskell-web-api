-- | Same-algebra application-module composition and declaration validation.
module HarchWeb.ApplicationModule.Composition
  ( ModuleCompositionError (..),
    combineApplicationModules,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import HarchWeb.Action (ActionCodecError, combineActionCodecs, declaredActionEndpointMetadata)
import HarchWeb.ApplicationModule.Core (ApplicationModule (..))
import HarchWeb.EndpointMetadata (EndpointName, endpointName)
import HarchWeb.Routing (RouteCodec (..), RouteParseResult (..), RouteRequest (..))
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (ModuleName)
import HarchWeb.Site (RouteDefinition (routeMetadata))

-- | Rejected root composition declarations.  A duplicate is an authored
-- startup error, not an ordering rule that would let one module shadow
-- another's endpoint identity.
data ModuleCompositionError
  = DuplicateModuleName ModuleName
  | DuplicateModuleEndpointName EndpointName
  | ModuleDoesNotOwnDeclaredRoute ModuleName
  | OverlappingModuleRoute ModuleName ModuleName
  | InvalidComposedActionCodec ActionCodecError
  deriving (Eq, Show)

-- | Combine modules already expressed in the same root route/action/context
-- algebra.  Parsing retains declaration order only for ordinary route-owner
-- misses; malformed locations stop immediately.  Rendering, definitions, and
-- method policy select the sole module that declares ownership of the route.
combineApplicationModules ::
  NonEmpty (ApplicationModule route actionTarget action context authorization) ->
  Either ModuleCompositionError (ApplicationModule route actionTarget action context authorization)
combineApplicationModules modules =
  case compositionError of
    Just errorValue -> Left errorValue
    Nothing ->
      case combineActionCodecs (fmap moduleActionCodec modules) of
        Left actionError -> Left (InvalidComposedActionCodec actionError)
        Right combinedActions ->
          Right
            ApplicationModule
              { moduleName = moduleName firstModule,
                moduleOwnsRoute = \routeValue -> any (`moduleOwnsRoute` routeValue) moduleList,
                moduleRouteMountChain = \routeValue -> moduleRouteMountChain (selectModule id routeValue) routeValue,
                moduleRouteCodec = combinedRouteCodec,
                moduleDeclaredRoutes = concatMap moduleDeclaredRoutes moduleList,
                moduleEndpoints = \routeValue -> moduleEndpoints (selectModule id routeValue) routeValue,
                moduleActionCodec = combinedActions,
                moduleActionRoute = \requestContext actionTarget -> listToMaybe (mapMaybe (\applicationModule -> moduleActionRoute applicationModule requestContext actionTarget) moduleList),
                moduleHandleAction = handleAction,
                moduleGuards = concatMap moduleGuards moduleList
              }
  where
    firstModule NonEmpty.:| remainingModules = modules
    moduleList = firstModule : remainingModules
    declaredEndpointNames = routeEndpointNames <> actionEndpointNames
    routeEndpointNames =
      [ endpointName (routeMetadata (moduleEndpoints applicationModule routeValue))
      | applicationModule <- moduleList,
        routeValue <- moduleDeclaredRoutes applicationModule
      ]
    declaredRoutes =
      [ (applicationModule, routeValue)
      | applicationModule <- moduleList,
        routeValue <- moduleDeclaredRoutes applicationModule
      ]
    actionEndpointNames =
      [ endpointName metadata
      | applicationModule <- moduleList,
        metadata <- declaredActionEndpointMetadata (moduleActionCodec applicationModule)
      ]

    compositionError =
      firstCompositionError
        ( validateDistinct DuplicateModuleName (map moduleName moduleList)
            : map validateDeclaredRouteOwnership declaredRoutes
              <> [validateDistinct DuplicateModuleEndpointName declaredEndpointNames]
        )

    combinedRouteCodec =
      RouteCodec
        { parseRoute = parseModules,
          renderRoute = \routeRequest -> renderRoute (moduleRouteCodec (selectModule id (requestRoute routeRequest))) routeRequest,
          notFoundRequest = notFoundRequest (moduleRouteCodec firstModule),
          routeMethods = \routeValue -> Routing.routeMethods (moduleRouteCodec (selectModule id routeValue)) routeValue
        }

    parseModules context location = go moduleList
      where
        go [] = RouteNotMatched
        go (applicationModule : remaining) =
          case parseRoute (moduleRouteCodec applicationModule) context location of
            RouteNotMatched -> go remaining
            result -> result

    selectModule project routeValue =
      case [project applicationModule | applicationModule <- moduleList, moduleOwnsRoute applicationModule routeValue] of
        [selected] -> selected
        [] -> error "no application module owns the selected route"
        _ -> error "multiple application modules own the selected route"

    handleAction request = go moduleList
      where
        go [] = pure Nothing
        go (applicationModule : remaining) = do
          result <- moduleHandleAction applicationModule request
          case result of
            Just response -> pure (Just response)
            Nothing -> go remaining

    validateDeclaredRouteOwnership (declaringModule, routeValue)
      | not (moduleOwnsRoute declaringModule routeValue) = Just (ModuleDoesNotOwnDeclaredRoute (moduleName declaringModule))
      | otherwise =
          case [moduleName applicationModule | applicationModule <- moduleList, moduleName applicationModule /= moduleName declaringModule, moduleOwnsRoute applicationModule routeValue] of
            [] -> Nothing
            conflictingOwner : _ -> Just (OverlappingModuleRoute (moduleName declaringModule) conflictingOwner)

firstCompositionError :: [Maybe ModuleCompositionError] -> Maybe ModuleCompositionError
firstCompositionError = listToMaybe . catMaybes

validateDistinct :: (Eq value) => (value -> ModuleCompositionError) -> [value] -> Maybe ModuleCompositionError
validateDistinct errorFor values =
  case duplicate values of
    Nothing -> Nothing
    Just duplicateValue -> Just (errorFor duplicateValue)
  where
    duplicate [] = Nothing
    duplicate (value : remaining)
      | value `elem` remaining = Just value
      | otherwise = duplicate remaining
