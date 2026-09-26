{-# LANGUAGE OverloadedStrings #-}

-- | Typed path/operation construction for 'HarchWeb.OpenApi.Document'
-- (AHI-4E-MH). One module owns the construction rail: each available
-- endpoint becomes one typed @openapi3@ 'Operation' whose request, response,
-- and security shapes come from the same declarations runtime dispatch
-- executes. 'OpenApiDocumentFailure' lives here because these builders are
-- what fail with it; the document facade re-exports it unchanged.
--
-- Split from 'HarchWeb.OpenApi.Document' by ownership (mirroring
-- 'HarchWeb.Api.Endpoint.Internal'/'Family'/'Runtime'): the facade keeps the
-- public document types and assembly, 'HarchWeb.OpenApi.Document.Encoding'
-- keeps the raw-JSON adapters, and this module keeps operation construction.
module HarchWeb.OpenApi.Document.Operation
  ( OpenApiDocumentFailure (..),
    renderOpenApiDocumentFailure,
    OpenApiOperation (..),
    operationForEndpoint,
  )
where

import Data.Aeson (Value)
import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.OpenApi
  ( ExternalDocs (..),
    MediaTypeObject (..),
    Operation (..),
    Referenced (Inline),
    RequestBody (..),
    Response (..),
    Responses (..),
    Schema,
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
    requireApiMediaType,
    urlEncodedFormMediaType,
    withApiRouteEndpointDeclaration,
  )
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
  )
import HarchWeb.OpenApi.Security (OpenApiSecurityScheme)
import HarchWeb.Routing (PathSegment, pathSegmentText)
import Network.HTTP.Media qualified as HttpMedia

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

-- | One constructed operation plus the validated overlay members its typed
-- model cannot carry and its anonymous-security marker.
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
        operation <- operationForExtension fullPath method contract security
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
-- so 'HarchWeb.OpenApi.Document.buildOpenApiDocument' can force an explicit
-- empty array at encoding time (see 'HarchWeb.OpenApi.Document.Encoding').
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

-- | Render one operation's request/response/security shapes. The path and
-- method name the operation; the endpoint's own contract is the cohesive
-- \"how to render\" input (its request body, response encoders, and typed
-- extension), paired with the already-resolved security requirement.
operationForExtension :: Text -> ApiMethod -> ApiEndpointContract OpenApiExtension fields body response -> [SecurityRequirement] -> Either OpenApiDocumentFailure Operation
operationForExtension path method contract security = do
  let extension = apiEndpointContractExtension contract
  requestBodyValue <- requestBodyFor path method (apiEndpointContractBody contract) (openApiExtensionRequestSchema extension) (openApiExtensionRequestExample extension)
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
          responsesForExtension (apiEndpointContractEncoders contract) extension,
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
