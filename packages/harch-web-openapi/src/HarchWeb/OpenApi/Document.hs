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

import Data.Aeson (Value (..), encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString.Lazy (ByteString)
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.OpenApi
  ( Components (..),
    ExternalDocs (..),
    Info (..),
    MediaTypeObject (..),
    OpenApi (..),
    Operation (..),
    PathItem (..),
    Referenced (Inline),
    RequestBody (..),
    Response (..),
    Responses (..),
    Schema,
    SecurityDefinitions (..),
    SecurityRequirement (..),
    URL (..),
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiAvailability (ApiAvailable),
    ApiBodyDecoder (..),
    ApiEndpointContract (..),
    ApiEndpointFamily,
    ApiMediaType,
    ApiMethod (..),
    ApiPath,
    ApiRequestBody (..),
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
    requireApiMediaType,
    urlEncodedFormMediaType,
    withApiRouteEndpointDeclaration,
  )
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (..),
    AuthenticationProfileName,
    EndpointMetadata (..),
    authenticationProfileNameText,
  )
import HarchWeb.OpenApi.Metadata
  ( OpenApiExtension,
    OpenApiExternalDocs (..),
    OpenApiSpecificationExtension,
    openApiExtensionDeprecated,
    openApiExtensionDescription,
    openApiExtensionExternalDocs,
    openApiExtensionOperationId,
    openApiExtensionRequestExample,
    openApiExtensionRequestSchema,
    openApiExtensionResponseExample,
    openApiExtensionResponseSchema,
    openApiExtensionResponseStatus,
    openApiExtensionSpecificationExtensions,
    openApiExtensionSummary,
    openApiExtensionTags,
    openApiSpecificationExtensionName,
    openApiSpecificationExtensionValue,
  )
import HarchWeb.OpenApi.Security (OpenApiSecurityScheme, openApiSecuritySchemeModel)
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
  | OpenApiRequestSchemaWithoutDeclaredMediaType Text ApiMethod
  | OpenApiRequestExampleWithoutDeclaredMediaType Text ApiMethod
  | -- | A non-anonymous endpoint names an authentication profile with no
    -- entry in the document's supplied profile-to-scheme map. Never resolved
    -- as anonymous or an arbitrary scheme: the map is the single authority.
    UndefinedOpenApiSecurityProfile Text
  | -- | A non-anonymous endpoint's real 'HarchWeb.EndpointMetadata.EndpointMetadata'
    -- has no authentication profile of its own (one would be inherited from
    -- an outer default this boundary cannot see). Never silently documented
    -- as either anonymous or secured: that would be a docs-only guess about
    -- a real access requirement.
    UnresolvedOpenApiSecurityProfile Text ApiMethod

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
    OpenApiRequestSchemaWithoutDeclaredMediaType path method -> "OpenAPI request schema has no declared request media type: " <> Text.toLower (apiMethodText method) <> " " <> path
    OpenApiRequestExampleWithoutDeclaredMediaType path method -> "OpenAPI request example has no declared request media type: " <> Text.toLower (apiMethodText method) <> " " <> path
    UndefinedOpenApiSecurityProfile profileName -> "OpenAPI security profile is not defined in the supplied scheme map: " <> profileName
    UnresolvedOpenApiSecurityProfile path method -> "OpenAPI operation has no resolvable authentication profile: " <> Text.toLower (apiMethodText method) <> " " <> path

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

data OpenApiOperation = OpenApiOperation
  { operationPath :: Text,
    operationMethod :: ApiMethod,
    operationId :: Text,
    operationValue :: Operation,
    operationExtensions :: [OpenApiSpecificationExtension],
    -- | Whether this operation resolved to 'AllowUnauthenticated'. Tracked
    -- separately from 'operationValue' because @openapi3@'s generic encoder
    -- omits an empty @_operationSecurity@ list entirely rather than emitting
    -- @security: []@; see 'applyOpenApiAnonymousSecurity'.
    operationAnonymous :: Bool
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

-- | Force an explicit empty @security@ array onto every anonymous
-- operation. @openapi3@'s generic encoder treats an empty list as that
-- field's default value and omits it entirely from the encoded JSON (its
-- @AesonDefaultValue [a]@ instance), which would otherwise leave an
-- anonymous operation with no @security@ member at all — silently inheriting
-- any top-level security declaration instead of explicitly requiring none.
-- This mirrors 'applyOpenApiOperationExtensions': the same narrow,
-- already-established raw-JSON adapter for a typed-model gap, not a second
-- encoding path. It updates only the named path/method pairs and leaves a
-- missing path, non-object path item, or non-object operation unchanged.
applyOpenApiAnonymousSecurity :: [(Text, ApiMethod)] -> Value -> Value
applyOpenApiAnonymousSecurity anonymousOperations value =
  case value of
    Object root -> Object (applyAnonymousToPaths anonymousOperations root)
    _ -> value

applyAnonymousToPaths :: [(Text, ApiMethod)] -> KeyMap.KeyMap Value -> KeyMap.KeyMap Value
applyAnonymousToPaths anonymousOperations root =
  case KeyMap.lookup "paths" root of
    Just (Object paths) -> KeyMap.insert "paths" (Object (foldr applyOne paths anonymousOperations)) root
    _ -> root
  where
    applyOne (anonymousPath, anonymousMethod) =
      mapKey (applyAnonymousToPathItem anonymousMethod) (Key.fromText anonymousPath)

applyAnonymousToPathItem :: ApiMethod -> Value -> Value
applyAnonymousToPathItem method value =
  case value of
    Object pathItem -> Object (mapKey applyAnonymousToOperation (Key.fromText (Text.toLower (apiMethodText method))) pathItem)
    _ -> value

applyAnonymousToOperation :: Value -> Value
applyAnonymousToOperation value =
  case value of
    Object operation -> Object (KeyMap.insert "security" (Array mempty) operation)
    _ -> value

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

-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- on 'endpointMetadataForPath's argument below is a confirmed, reproducible
-- fix, not a guess. Every construction path here is genuinely exercised, but
-- 'apiPath' is a bare local binding used as a direct argument to an
-- already-HPC-instrumented call, the documented pattern where HPC
-- permanently leaves the occurrence unticked despite real execution.
{-# ANN operationForEndpoint ("HLint: ignore Redundant $!" :: String) #-}
operationForEndpoint ::
  Map AuthenticationProfileName OpenApiSecurityScheme ->
  context ->
  NonEmpty.NonEmpty PathSegment ->
  (ApiPath -> EndpointMetadata authorization) ->
  (authorization -> [Text]) ->
  ApiRouteEndpoint context OpenApiExtension fields body domainFailure response ->
  Either OpenApiDocumentFailure (Maybe OpenApiOperation)
operationForEndpoint securitySchemes context mountPrefix endpointMetadataForPath requiredScopesFor endpoint
  | apiRouteEndpointAvailability endpoint context /= ApiAvailable = Right Nothing
  | otherwise =
      withApiRouteEndpointDeclaration endpoint $ \declaration -> do
        let apiPath = apiRouteEndpointDeclarationPath declaration
        fullPath <- mountedOperationPath mountPrefix (apiPathText apiPath)
        let contract = apiRouteEndpointDeclarationContract declaration
            extension = apiEndpointContractExtension contract
            method = apiEndpointContractMethod contract
            metadata = endpointMetadataForPath $! apiPath
        (security, anonymous) <- operationSecurityFor securitySchemes fullPath method requiredScopesFor metadata
        operation <- operationForExtension fullPath method (apiEndpointContractBody contract) (apiEndpointContractEncoders contract) extension security
        pure
          ( Just
              OpenApiOperation
                { operationPath = fullPath,
                  operationMethod = method,
                  operationId = operationIdForExtension fullPath method extension,
                  operationValue = operation,
                  operationExtensions = openApiExtensionSpecificationExtensions extension,
                  operationAnonymous = anonymous
                }
          )

-- | Derive one operation's OpenAPI @security@ requirement from the same
-- 'EndpointMetadata' that governs its real runtime access, never from
-- docs-only authoring. Returns whether the resolved requirement is anonymous
-- so 'buildOpenApiDocument' can force an explicit empty array at encoding
-- time (see 'applyOpenApiAnonymousSecurity').
operationSecurityFor ::
  Map AuthenticationProfileName OpenApiSecurityScheme ->
  Text ->
  ApiMethod ->
  (authorization -> [Text]) ->
  EndpointMetadata authorization ->
  Either OpenApiDocumentFailure ([SecurityRequirement], Bool)
operationSecurityFor securitySchemes path method requiredScopesFor metadata =
  case endpointAccess metadata of
    AllowUnauthenticated -> Right ([], True)
    RequireAuthenticated -> resolvedSecurity []
    RequireAuthorized authorizationValue -> resolvedSecurity (requiredScopesFor authorizationValue)
  where
    resolvedSecurity scopes =
      case endpointAuthenticationProfile metadata of
        Nothing -> Left (UnresolvedOpenApiSecurityProfile path method)
        Just profileName ->
          case Map.lookup profileName securitySchemes of
            Nothing -> Left (UndefinedOpenApiSecurityProfile (authenticationProfileNameText profileName))
            Just _scheme -> Right ([SecurityRequirement (InsOrdHashMap.singleton (authenticationProfileNameText profileName) scopes)], False)

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

operationForExtension :: Text -> ApiMethod -> ApiRequestBody body -> NonEmpty.NonEmpty (ApiResponseEncoder response) -> OpenApiExtension fields body response -> [SecurityRequirement] -> Either OpenApiDocumentFailure Operation
operationForExtension path method requestBody encoders extension security = do
  requestBodyValue <- requestBodyFor path method requestBody (openApiExtensionRequestSchema extension) (openApiExtensionRequestExample extension)
  pure
    (mempty :: Operation)
      { _operationTags = InsOrdHashSet.fromList (openApiExtensionTags extension),
        _operationSummary = openApiExtensionSummary extension,
        _operationDescription = openApiExtensionDescription extension,
        _operationExternalDocs =
          (\externalDocs -> ExternalDocs (openApiExternalDocsDescription externalDocs) (URL (openApiExternalDocsUrl externalDocs)))
            <$> openApiExtensionExternalDocs extension,
        _operationOperationId = Just (operationIdForExtension path method extension),
        _operationRequestBody = requestBodyValue,
        _operationDeprecated = Just (openApiExtensionDeprecated extension),
        _operationResponses =
          responsesForExtension encoders extension,
        _operationSecurity = security
      }

requestBodyFor :: Text -> ApiMethod -> ApiRequestBody body -> Maybe Schema -> Maybe Value -> Either OpenApiDocumentFailure (Maybe (Referenced RequestBody))
requestBodyFor path method requestBody schema example =
  case requestMediaTypes requestBody of
    [] ->
      case (schema, example) of
        (Nothing, Nothing) -> Right Nothing
        (Just _, _) -> Left (OpenApiRequestSchemaWithoutDeclaredMediaType path method)
        (Nothing, Just _) -> Left (OpenApiRequestExampleWithoutDeclaredMediaType path method)
    mediaTypes ->
      Right
        ( Just
            ( Inline
                ( (mempty :: RequestBody)
                    { _requestBodyContent =
                        InsOrdHashMap.fromList
                          [ (openApiMediaType mediaType, mediaTypeObjectFor schema example)
                          | mediaType <- mediaTypes
                          ]
                    }
                )
            )
        )

requestMediaTypes :: ApiRequestBody body -> [ApiMediaType]
requestMediaTypes requestBody =
  case requestBody of
    ApiNoRequestBody -> []
    ApiBufferedRequestBody _ _ decoders -> map apiBodyDecoderMediaType decoders
    ApiUrlEncodedFormRequestBody {} -> [urlEncodedFormMediaType]
    ApiStreamingRequestBody _ -> []
    ApiMultipartRequestBody _ _ -> [requireApiMediaType "multipart/form-data"]

responsesForExtension :: NonEmpty.NonEmpty (ApiResponseEncoder response) -> OpenApiExtension fields body response -> Responses
responsesForExtension encoders extension =
  case openApiExtensionResponseStatus extension of
    Nothing ->
      (mempty :: Responses)
        { _responsesDefault = Just (Inline (responseFor encoders extension "Response"))
        }
    Just status ->
      (mempty :: Responses)
        { _responsesResponses =
            InsOrdHashMap.singleton
              status
              (Inline (responseFor encoders extension (responseDescriptionForStatus status)))
        }

responseFor :: NonEmpty.NonEmpty (ApiResponseEncoder response) -> OpenApiExtension fields body response -> Text -> Response
responseFor encoders extension description =
  (mempty :: Response)
    { _responseDescription = description,
      _responseContent =
        InsOrdHashMap.fromList
          ( map
              ( \encoder ->
                  ( openApiMediaType (apiContentTypeMediaType (apiResponseEncoderContentType encoder)),
                    mediaTypeObjectFor (openApiExtensionResponseSchema extension) (openApiExtensionResponseExample extension)
                  )
              )
              (NonEmpty.toList encoders)
          )
    }

mediaTypeObjectFor :: Maybe Schema -> Maybe Value -> MediaTypeObject
mediaTypeObjectFor schema example =
  (mempty :: MediaTypeObject)
    { _mediaTypeObjectSchema = Inline <$> schema,
      _mediaTypeObjectExample = example
    }

-- | 'ApiMediaType' is opaque and accepts only concrete media names that
-- @http-media@ accepts, so splitting its normalized bare @type/subtype@ form
-- is total and the public constructor cannot fail.
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
