{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

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
-- supplies runtime routing.  It records each endpoint's declared response
-- media types, but leaves response schemas and examples absent: encoder
-- selection is representation truth, not a body-shape declaration.  Later
-- AHI-4E slices add schemas, resolved security, provider caching, and Swagger
-- routes.
-- An extension can carry a nonblank authored operation ID; otherwise the
-- existing family abstraction has no runtime 'EndpointMetadata' name per
-- method, so this slice falls back to a stable method/path identifier and
-- rejects collisions.  The fallback must be replaced by the validated endpoint
-- name when the API-family metadata boundary carries that identity; it must not
-- be presented as that later name-based guarantee.
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
    encodeOpenApiDocument,
  )
where

import Data.Aeson (Value (..), encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi
  ( Info (..),
    MediaTypeObject,
    OpenApi (..),
    Operation (..),
    PathItem (..),
    Referenced (Inline),
    Response (..),
    Responses (..),
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiAvailability (ApiAvailable),
    ApiEndpointContract (..),
    ApiEndpointFamily,
    ApiMediaType,
    ApiMethod (..),
    ApiResponseEncoder (..),
    ApiRouteEndpoint,
    ApiRouteEndpointDeclaration (..),
    apiContentTypeMediaType,
    apiEndpointContractExtension,
    apiMediaTypeText,
    apiMethodText,
    apiPathText,
    apiRouteEndpointAvailability,
    mapApiEndpointFamily,
    withApiRouteEndpointDeclaration,
  )
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.OpenApi.Metadata
  ( OpenApiExtension,
    OpenApiSpecificationExtension,
    openApiExtensionDeprecated,
    openApiExtensionDescription,
    openApiExtensionOperationId,
    openApiExtensionResponseStatus,
    openApiExtensionSpecificationExtensions,
    openApiExtensionSummary,
    openApiExtensionTags,
    openApiSpecificationExtensionName,
    openApiSpecificationExtensionValue,
  )
import HarchWeb.Routing (PathSegment, pathSegmentText)
import Network.HTTP.Media qualified as HttpMedia

-- | Required, application-owned document identity.  It is separate from
-- endpoint documentation because one combined document has one title and
-- version regardless of how many independently packaged families it mounts.
data OpenApiDocumentDetails = OpenApiDocumentDetails
  { openApiDocumentTitle :: Text,
    openApiDocumentVersion :: Text
  }

-- | Construction failures for an explicit, static document.  These name the
-- conflicting generated identity instead of silently overwriting one
-- operation in the OpenAPI path map.
data OpenApiDocumentFailure
  = EmptyOpenApiDocumentTitle
  | EmptyOpenApiDocumentVersion
  | InvalidOpenApiEndpointPath Text
  | DuplicateOpenApiPathMethod Text ApiMethod
  | DuplicateOpenApiOperationId Text

-- | Render a construction failure for application diagnostics. The ADT stays
-- the programmatic boundary; callers should branch on its constructors rather
-- than parse this text.
renderOpenApiDocumentFailure :: OpenApiDocumentFailure -> Text
renderOpenApiDocumentFailure failure =
  case failure of
    EmptyOpenApiDocumentTitle -> "OpenAPI document title must not be empty."
    EmptyOpenApiDocumentVersion -> "OpenAPI document version must not be empty."
    InvalidOpenApiEndpointPath path -> "OpenAPI endpoint path is invalid: " <> path
    DuplicateOpenApiPathMethod path method -> "OpenAPI path and method are duplicated: " <> Text.toLower (apiMethodText method) <> " " <> path
    DuplicateOpenApiOperationId operationId -> "OpenAPI operation identifier is duplicated: " <> operationId

-- | One documented family paired with the actual structural runtime mount.
-- The existential route types are deliberately irrelevant to documentation:
-- only the mount's construction-owned prefix is observed.  Keeping the whole
-- 'RouteMount' value prevents a second, stringly documentation-prefix input.
data OpenApiMountedFamily context where
  OpenApiMountedFamily ::
    RouteMount parentRoute childRoute ->
    ApiEndpointFamily context OpenApiExtension ->
    OpenApiMountedFamily context

-- | Select an API family for documentation at the composition root.  The
-- family is already validated by @harch-web@; this function merely records
-- the exact mount used when that family is installed for runtime routing.
openApiMountedFamily :: RouteMount parentRoute childRoute -> ApiEndpointFamily context OpenApiExtension -> OpenApiMountedFamily context
openApiMountedFamily = OpenApiMountedFamily

-- | A typed OpenAPI model plus the valid @x-*@ values that its upstream model
-- cannot retain.  Use 'mapOpenApiDocumentModel' for an application-owned
-- transformation before 'encodeOpenApiDocument'.
data OpenApiDocument = OpenApiDocument
  { documentModel :: OpenApi,
    openApiDocumentOperationExtensions :: [(Text, ApiMethod, [OpenApiSpecificationExtension])]
  }

data OpenApiOperation = OpenApiOperation
  { operationPath :: Text,
    operationMethod :: ApiMethod,
    operationId :: Text,
    operationValue :: Operation,
    operationExtensions :: [OpenApiSpecificationExtension]
  }

-- | Build a valid basic OpenAPI operation for every endpoint available in the
-- supplied construction snapshot.  An extension may select a documented
-- response status; otherwise each operation truthfully uses @default@ rather
-- than inventing a @200@ response.  Its nonempty response-encoder list is the
-- runtime source of representation media types, so those keys are recorded in
-- OpenAPI @content@ with empty media objects.  This declares neither a schema
-- nor examples, which need a distinct typed response-body contract.
buildOpenApiDocument :: OpenApiDocumentDetails -> context -> [OpenApiMountedFamily context] -> Either OpenApiDocumentFailure OpenApiDocument
buildOpenApiDocument details context mountedFamilies = do
  validatedDetails <- validateDocumentDetails details
  operations <- concatMapM (operationsForMountedFamily context) mountedFamilies
  distinctOperations <- validateDistinctOperations operations
  distinctOperationIds <- validateDistinctOperationIds distinctOperations
  pure
    OpenApiDocument
      { documentModel = modelFor validatedDetails distinctOperationIds,
        openApiDocumentOperationExtensions =
          [ (operationPath, operationMethod, operationExtensions)
          | OpenApiOperation {operationPath, operationMethod, operationExtensions} <- distinctOperationIds,
            not (null operationExtensions)
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

-- | Encode the selected model as OpenAPI 3.0.3 and apply each already
-- validated operation extension.  The adapter modifies only the root
-- @openapi@ member and existing selected operations; it does not turn
-- arbitrary application text into JSON member names.
encodeOpenApiDocument :: OpenApiDocument -> ByteString
encodeOpenApiDocument document = encode (applyOpenApiOperationExtensions (openApiDocumentOperationExtensions document) (toJSON (openApiDocumentModel document)))

-- | Apply validated @x-*@ operation members to a raw encoded OpenAPI value.
-- This is the narrow wire adapter required because @openapi3@ has no typed
-- representation for specification extensions.  It updates the OpenAPI
-- version to 3.0.3 only for an object value, and leaves a missing path,
-- non-object path item, or non-object operation unchanged.  Those cases make
-- the adapter safe for an application-transformed model whose selected
-- operation has been removed before encoding.
applyOpenApiOperationExtensions :: [(Text, ApiMethod, [OpenApiSpecificationExtension])] -> Value -> Value
applyOpenApiOperationExtensions extensions value =
  case value of
    Object root -> Object (KeyMap.insert "openapi" (String "3.0.3") (applyToPaths extensions root))
    _ -> value

validateDocumentDetails :: OpenApiDocumentDetails -> Either OpenApiDocumentFailure OpenApiDocumentDetails
validateDocumentDetails OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion}
  | Text.null openApiDocumentTitle = Left EmptyOpenApiDocumentTitle
  | Text.null openApiDocumentVersion = Left EmptyOpenApiDocumentVersion
  | otherwise = Right OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion}

operationsForMountedFamily :: context -> OpenApiMountedFamily context -> Either OpenApiDocumentFailure [OpenApiOperation]
operationsForMountedFamily context (OpenApiMountedFamily routeMount family) =
  catMaybes
    <$> sequenceA
      ( mapApiEndpointFamily
          (operationForEndpoint context (routeMountPrefix routeMount))
          family
      )

operationForEndpoint :: context -> NonEmpty.NonEmpty PathSegment -> ApiRouteEndpoint context OpenApiExtension fields body domainFailure response -> Either OpenApiDocumentFailure (Maybe OpenApiOperation)
operationForEndpoint context mountPrefix endpoint
  | apiRouteEndpointAvailability endpoint context /= ApiAvailable = Right Nothing
  | otherwise =
      withApiRouteEndpointDeclaration endpoint $ \declaration -> do
        fullPath <- mountedOperationPath mountPrefix (apiPathText (apiRouteEndpointDeclarationPath declaration))
        let contract = apiRouteEndpointDeclarationContract declaration
            extension = apiEndpointContractExtension contract
            method = apiEndpointContractMethod contract
        pure
          ( Just
              OpenApiOperation
                { operationPath = fullPath,
                  operationMethod = method,
                  operationId = operationIdForExtension fullPath method extension,
                  operationValue = operationForExtension fullPath method (apiEndpointContractEncoders contract) extension,
                  operationExtensions = openApiExtensionSpecificationExtensions extension
                }
          )

mountedOperationPath :: NonEmpty.NonEmpty PathSegment -> Text -> Either OpenApiDocumentFailure Text
mountedOperationPath mountPrefix localPath
  | Text.null localPath || not (Text.isPrefixOf "/" localPath) = Left (InvalidOpenApiEndpointPath localPath)
  | Text.isInfixOf "//" localPath || Text.any (`elem` ['?', '#', '\\']) localPath = Left (InvalidOpenApiEndpointPath localPath)
  | otherwise =
      Right
        ( "/"
            <> Text.intercalate "/" (map pathSegmentText (NonEmpty.toList mountPrefix))
            <> (if localPath == "/" then "" else localPath)
        )

operationForExtension :: Text -> ApiMethod -> NonEmpty.NonEmpty (ApiResponseEncoder response) -> OpenApiExtension fields body response -> Operation
operationForExtension path method encoders extension =
  (mempty :: Operation)
    { _operationTags = InsOrdHashSet.fromList (openApiExtensionTags extension),
      _operationSummary = openApiExtensionSummary extension,
      _operationDescription = openApiExtensionDescription extension,
      _operationOperationId = Just (operationIdForExtension path method extension),
      _operationDeprecated = Just (openApiExtensionDeprecated extension),
      _operationResponses =
        responsesForExtension encoders extension
    }

responsesForExtension :: NonEmpty.NonEmpty (ApiResponseEncoder response) -> OpenApiExtension fields body response -> Responses
responsesForExtension encoders extension =
  case openApiExtensionResponseStatus extension of
    Nothing ->
      (mempty :: Responses)
        { _responsesDefault = Just (Inline (responseFor encoders "Response"))
        }
    Just status ->
      (mempty :: Responses)
        { _responsesResponses =
            InsOrdHashMap.singleton
              status
              (Inline (responseFor encoders (responseDescriptionForStatus status)))
        }

responseFor :: NonEmpty.NonEmpty (ApiResponseEncoder response) -> Text -> Response
responseFor encoders description =
  (mempty :: Response)
    { _responseDescription = description,
      _responseContent =
        InsOrdHashMap.fromList
          ( map
              ( \encoder ->
                  ( openApiMediaType (apiContentTypeMediaType (apiResponseEncoderContentType encoder)),
                    mempty :: MediaTypeObject
                  )
              )
              (NonEmpty.toList encoders)
          )
    }

-- | 'ApiMediaType' is opaque and its declaration validation uses the same
-- media-name grammar as @http-media@, so splitting its normalized bare
-- @type/subtype@ form is total and the public constructor cannot fail.
openApiMediaType :: ApiMediaType -> HttpMedia.MediaType
openApiMediaType mediaType =
  TextEncoding.encodeUtf8 mainType HttpMedia.// TextEncoding.encodeUtf8 subtype
  where
    (mainType, slashAndSubtype) = Text.breakOn "/" (apiMediaTypeText mediaType)
    subtype = Text.drop 1 slashAndSubtype

responseDescriptionForStatus :: Int -> Text
responseDescriptionForStatus status =
  case status of
    200 -> "OK"
    201 -> "Created"
    202 -> "Accepted"
    204 -> "No Content"
    400 -> "Bad Request"
    401 -> "Unauthorized"
    403 -> "Forbidden"
    404 -> "Not Found"
    409 -> "Conflict"
    422 -> "Unprocessable Content"
    429 -> "Too Many Requests"
    500 -> "Internal Server Error"
    _ -> "Response"

operationIdFor :: Text -> ApiMethod -> Text
operationIdFor path method =
  Text.toLower (apiMethodText method)
    <> "-"
    <> Text.intercalate "-" (filter (not . Text.null) (Text.split (`elem` ['/', '-', '_']) path))

operationIdForExtension :: Text -> ApiMethod -> OpenApiExtension fields body response -> Text
operationIdForExtension path method extension =
  fromMaybe (operationIdFor path method) (openApiExtensionOperationId extension)

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

modelFor :: OpenApiDocumentDetails -> [OpenApiOperation] -> OpenApi
modelFor OpenApiDocumentDetails {openApiDocumentTitle, openApiDocumentVersion} operations =
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
          ]
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

applyToPaths :: [(Text, ApiMethod, [OpenApiSpecificationExtension])] -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
applyToPaths extensions root =
  case KeyMap.lookup "paths" root of
    Just (Object paths) -> KeyMap.insert "paths" (Object (foldr applyOne paths extensions)) root
    _ -> root
  where
    applyOne (extensionPath, extensionMethod, extensionValues) =
      mapKey (applyToPathItem extensionMethod extensionValues) (Key.fromText extensionPath)

applyToPathItem :: ApiMethod -> [OpenApiSpecificationExtension] -> Value -> Value
applyToPathItem method extensions value =
  case value of
    Object pathItem -> Object (mapKey (applyToOperation extensions) (Key.fromText (Text.toLower (apiMethodText method))) pathItem)
    _ -> value

mapKey :: (Value -> Value) -> Key.Key -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
mapKey transform key values =
  case KeyMap.lookup key values of
    Nothing -> values
    Just value -> KeyMap.insert key (transform value) values

applyToOperation :: [OpenApiSpecificationExtension] -> Value -> Value
applyToOperation extensions value =
  case value of
    Object operation ->
      Object
        ( foldr
            (\extension -> KeyMap.insert (Key.fromText (openApiSpecificationExtensionName extension)) (openApiSpecificationExtensionValue extension))
            operation
            extensions
        )
    _ -> value

concatMapM :: (value -> Either failure [result]) -> [value] -> Either failure [result]
concatMapM transform = fmap concat . traverse transform
