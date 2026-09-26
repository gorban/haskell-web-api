{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | Explicit OpenAPI document construction over documented endpoint families.
--
-- Decision record (AHI-4E, 2026-09-22): the public @openapi3@ model is the
-- mutable document value applications transform, while this module owns the
-- two model gaps required by the selected OpenAPI 3.0.3 wire contract: its
-- upstream encoder emits @3.0.0@ and it has no representation for @x-*@
-- members.  'OpenApiDocument' therefore keeps the typed model and a small,
-- validated extension overlay together until encoding.  It does not parse a
-- completed 'Site', redispatch requests, or retain handlers: callers supply
-- the exact 'ApiEndpointFamily' and the same structural 'RouteMount' that
-- supplies runtime routing. It records each endpoint's declared request and
-- response media types. An application can add inline request or response
-- schemas, while the runtime declarations remain representation truth rather
-- than a body-shape inference.
--
-- Decision record (AHI-4E, 2026-09-23): 'OpenApiMountedFamily' also carries
-- the exact @ApiPath -> EndpointMetadata authorization@ function the
-- application already writes for real route mounting, plus an
-- @authorization -> [Text]@ scope projection; 'buildOpenApiDocument' takes a
-- document-level @Map AuthenticationProfileName OpenApiSecurityScheme@.
-- Every operation's @security@ is derived from that real, already-enforced
-- 'HarchWeb.EndpointMetadata.AccessRequirement' — never authored
-- independently in the extension — and construction fails
-- ('UndefinedOpenApiSecurityProfile', 'UnresolvedOpenApiSecurityProfile')
-- rather than guessing when a profile is undefined or unresolvable. Swagger
-- routes, and wiring this into a real application (@web-api@,
-- @composed-domains@), remain later AHI-4E slices.
-- An extension can carry a nonblank authored operation ID; otherwise the
-- existing family abstraction has no runtime 'EndpointMetadata' name per
-- method, so this slice falls back to a stable method/path identifier and
-- rejects collisions.  The fallback must be replaced by the validated endpoint
-- name when the API-family metadata boundary carries that identity; it must not
-- be presented as that later name-based guarantee.
--
-- Decision record (AHI-4E-MH, 2026-09-26): this facade keeps the public
-- document types and the assembly rail; typed path/operation construction
-- lives in 'HarchWeb.OpenApi.Document.Operation' (which also owns the
-- construction failures its builders raise) and the raw-JSON wire adapters
-- live in 'HarchWeb.OpenApi.Document.Encoding' — the split-by-ownership
-- pattern 'HarchWeb.Api.Endpoint.Internal'/'Family'/'Runtime' already uses.
-- The public export surface is unchanged: the facade re-exports the split
-- pieces.
module HarchWeb.OpenApi.Document
  ( OpenApiDocument,
    OpenApiDocumentDetails (..),
    OpenApiDocumentFailure (..),
    renderOpenApiDocumentFailure,
    OpenApiMountedFamily,
    openApiMountedFamily,
    buildOpenApiDocument,
    openApiDocumentModel,
    mapOpenApiDocumentModel,
    applyOpenApiOperationExtensions,
    applyOpenApiAnonymousSecurity,
    encodeOpenApiDocument,
  )
where

import Data.Aeson (encode, toJSON)
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrdHashMap
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.OpenApi
  ( Components (..),
    Info (..),
    OpenApi (..),
    PathItem (..),
    SecurityDefinitions (..),
  )
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api
  ( ApiEndpointFamily,
    ApiMethod (..),
    ApiPath,
    mapApiEndpointFamily,
  )
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.EndpointMetadata
  ( AuthenticationProfileName,
    EndpointMetadata,
    authenticationProfileNameText,
  )
import HarchWeb.OpenApi.Document.Encoding
  ( applyOpenApiAnonymousSecurity,
    applyOpenApiOperationExtensions,
  )
import HarchWeb.OpenApi.Document.Operation
  ( OpenApiDocumentFailure (..),
    OpenApiOperation (..),
    operationForEndpoint,
    renderOpenApiDocumentFailure,
  )
import HarchWeb.OpenApi.Metadata (OpenApiExtension, OpenApiSpecificationExtension)
import HarchWeb.OpenApi.Security (OpenApiSecurityScheme, openApiSecuritySchemeModel)

-- | Required, application-owned document identity.  It is separate from
-- endpoint documentation because one combined document has one title and
-- version regardless of how many independently packaged families it mounts.
data OpenApiDocumentDetails = OpenApiDocumentDetails
  { openApiDocumentTitle :: Text,
    openApiDocumentVersion :: Text
  }

-- | One documented family paired with the actual structural runtime mount
-- and the same access information the application already established for
-- real routing.  The existential route types are deliberately irrelevant to
-- documentation: only the mount's construction-owned prefix is observed.
-- Keeping the whole 'RouteMount' value prevents a second, stringly
-- documentation-prefix input.
--
-- Decision record (AHI-4E, 2026-09-23): security is derived from the exact
-- @ApiPath -> EndpointMetadata authorization@ function the application
-- already writes for
-- 'HarchWeb.Api.Endpoint.Family.apiRouteEndpointFamilyDefinition', not a
-- second, independently authored copy. This is the only way a documented
-- operation's declared security can be structurally guaranteed to match the
-- profile/access requirement that actually governs the endpoint, rather than
-- a docs-only override the task's own design forbids. The scope function
-- turns one endpoint's opaque @authorization@ requirement into the OpenAPI
-- scope names an application-owned 'RequireAuthorized' value demands;
-- 'RequireAuthenticated' always requires zero scopes and
-- 'AllowUnauthenticated' requires none of this at all.
data OpenApiMountedFamily context where
  OpenApiMountedFamily ::
    RouteMount parentRoute childRoute ->
    ApiEndpointFamily context OpenApiExtension ->
    (ApiPath -> EndpointMetadata authorization) ->
    (authorization -> [Text]) ->
    OpenApiMountedFamily context

-- | Select an API family for documentation at the composition root.  The
-- family is already validated by @harch-web@; this function merely records
-- the exact mount used when that family is installed for runtime routing,
-- plus the same endpoint-metadata function and a scope projection used to
-- derive each operation's real security (see the type's own Haddock).
openApiMountedFamily ::
  RouteMount parentRoute childRoute ->
  ApiEndpointFamily context OpenApiExtension ->
  (ApiPath -> EndpointMetadata authorization) ->
  (authorization -> [Text]) ->
  OpenApiMountedFamily context
openApiMountedFamily = OpenApiMountedFamily

-- | A typed OpenAPI model plus the valid @x-*@ values that its upstream model
-- cannot retain.  Use 'mapOpenApiDocumentModel' for an application-owned
-- transformation before 'encodeOpenApiDocument'.
data OpenApiDocument = OpenApiDocument
  { documentModel :: OpenApi,
    openApiDocumentOperationExtensions :: [(Text, ApiMethod, [OpenApiSpecificationExtension])],
    -- | Every anonymous operation's path/method, forced to an explicit empty
    -- @security@ array at encoding time; see 'applyOpenApiAnonymousSecurity'.
    openApiDocumentAnonymousOperations :: [(Text, ApiMethod)]
  }

-- | Build a valid basic OpenAPI operation for every endpoint available in the
-- supplied construction snapshot.  An extension may select a documented
-- response status; otherwise each operation truthfully uses @default@ rather
-- than inventing a @200@ response.  Its nonempty response-encoder list is the
-- runtime source of representation media types, so those keys are recorded in
-- OpenAPI @content@. An explicitly attached inline schema is shared by every
-- representation; no schema is inferred from an encoder, and examples remain
-- a later typed metadata slice.
buildOpenApiDocument ::
  OpenApiDocumentDetails ->
  -- | Every resolved AHI-4D authentication profile this document's mounted
  -- families may reference, keyed by the exact profile name real routing
  -- uses. A profile a documented endpoint names but this map omits fails
  -- construction ('UndefinedOpenApiSecurityProfile') rather than being
  -- silently treated as anonymous or skipped.
  Map AuthenticationProfileName OpenApiSecurityScheme ->
  context ->
  [OpenApiMountedFamily context] ->
  Either OpenApiDocumentFailure OpenApiDocument
buildOpenApiDocument details securitySchemes context mountedFamilies = do
  validatedDetails <- validateDocumentDetails details
  operations <- concatMapM (operationsForMountedFamily securitySchemes context) mountedFamilies
  distinctOperations <- validateDistinctOperations operations
  distinctOperationIds <- validateDistinctOperationIds distinctOperations
  pure
    OpenApiDocument
      { documentModel = modelFor validatedDetails securitySchemes distinctOperationIds,
        openApiDocumentOperationExtensions =
          [ (operationPath, operationMethod, operationExtensions)
          | OpenApiOperation {operationPath, operationMethod, operationExtensions} <- distinctOperationIds,
            not (null operationExtensions)
          ],
        openApiDocumentAnonymousOperations =
          [ (operationPath, operationMethod)
          | OpenApiOperation {operationPath, operationMethod, operationAnonymous} <- distinctOperationIds,
            operationAnonymous
          ]
      }

-- | Read the transformable public model.  Its eventual wire encoding is
-- performed by 'encodeOpenApiDocument' so the repository still emits OpenAPI
-- 3.0.3 and validated @x-*@ members.
openApiDocumentModel :: OpenApiDocument -> OpenApi
openApiDocumentModel = documentModel

-- | Apply an application-owned transformation to the typed public model
-- before validation/encoding continues at the document boundary.
mapOpenApiDocumentModel :: (OpenApi -> OpenApi) -> OpenApiDocument -> OpenApiDocument
mapOpenApiDocumentModel transform document =
  document {documentModel = transform (openApiDocumentModel document)}

-- | Encode the selected model as OpenAPI 3.0.3, apply each already validated
-- operation extension, and force an explicit empty @security@ array onto
-- every anonymous operation (see 'applyOpenApiAnonymousSecurity').  The
-- adapter modifies only the root @openapi@ member and existing selected
-- operations; it does not turn arbitrary application text into JSON member
-- names.
encodeOpenApiDocument :: OpenApiDocument -> ByteString
encodeOpenApiDocument document =
  encode
    ( applyOpenApiAnonymousSecurity
        (openApiDocumentAnonymousOperations document)
        (applyOpenApiOperationExtensions (openApiDocumentOperationExtensions document) (toJSON (openApiDocumentModel document)))
    )

validateDocumentDetails :: OpenApiDocumentDetails -> Either OpenApiDocumentFailure OpenApiDocumentDetails
validateDocumentDetails OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion}
  | Text.null openApiDocumentTitle = Left EmptyOpenApiDocumentTitle
  | Text.null openApiDocumentVersion = Left EmptyOpenApiDocumentVersion
  | otherwise = Right OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion}

operationsForMountedFamily :: Map AuthenticationProfileName OpenApiSecurityScheme -> context -> OpenApiMountedFamily context -> Either OpenApiDocumentFailure [OpenApiOperation]
operationsForMountedFamily securitySchemes context (OpenApiMountedFamily routeMount family endpointMetadataForPath requiredScopesFor) =
  catMaybes
    <$> sequenceA
      ( mapApiEndpointFamily
          (operationForEndpoint securitySchemes context (routeMountPrefix routeMount) endpointMetadataForPath requiredScopesFor)
          family
      )

validateDistinctOperations :: [OpenApiOperation] -> Either OpenApiDocumentFailure [OpenApiOperation]
validateDistinctOperations operations =
  case findDuplicate operationIdentity operations of
    Nothing -> Right operations
    Just duplicate -> Left (DuplicateOpenApiPathMethod (operationPath duplicate) (operationMethod duplicate))
  where
    operationIdentity operation = (operationPath operation, operationMethod operation)

validateDistinctOperationIds :: [OpenApiOperation] -> Either OpenApiDocumentFailure [OpenApiOperation]
validateDistinctOperationIds operations =
  case findDuplicate operationId operations of
    Nothing -> Right operations
    Just duplicate -> Left (DuplicateOpenApiOperationId (operationId duplicate))

findDuplicate :: (Eq identity) => (value -> identity) -> [value] -> Maybe value
findDuplicate identify = go []
  where
    go _ [] = Nothing
    go identities (value : remaining)
      | identify value `elem` identities = Just value
      | otherwise = go (identify value : identities) remaining

modelFor :: OpenApiDocumentDetails -> Map AuthenticationProfileName OpenApiSecurityScheme -> [OpenApiOperation] -> OpenApi
modelFor OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion} securitySchemes operations =
  (mempty :: OpenApi)
    { _openApiInfo =
        (mempty :: Info)
          { _infoTitle = openApiDocumentTitle,
            _infoVersion = openApiDocumentVersion
          },
      _openApiPaths =
        InsOrdHashMap.fromList
          [ (Text.unpack path, pathItemFor path operations)
          | path <- distinctPaths operations
          ],
      _openApiComponents =
        (mempty :: Components)
          { _componentsSecuritySchemes =
              SecurityDefinitions
                ( InsOrdHashMap.fromList
                    [ (authenticationProfileNameText profileName, openApiSecuritySchemeModel scheme)
                    | (profileName, scheme) <- Map.toList securitySchemes
                    ]
                )
          }
    }

distinctPaths :: [OpenApiOperation] -> [Text]
distinctPaths = foldr addPath []
  where
    addPath operation paths
      | operationPath operation `elem` paths = paths
      | otherwise = operationPath operation : paths

pathItemFor :: Text -> [OpenApiOperation] -> PathItem
pathItemFor path operations = foldr addOperation (mempty :: PathItem) (filter ((== path) . operationPath) operations)
  where
    addOperation operation item =
      case operationMethod operation of
        ApiGet -> item {_pathItemGet = Just (operationValue operation)}
        ApiPost -> item {_pathItemPost = Just (operationValue operation)}
        ApiPut -> item {_pathItemPut = Just (operationValue operation)}
        ApiPatch -> item {_pathItemPatch = Just (operationValue operation)}
        ApiDelete -> item {_pathItemDelete = Just (operationValue operation)}

concatMapM :: (value -> Either failure [result]) -> [value] -> Either failure [result]
concatMapM transform = fmap concat . traverse transform
