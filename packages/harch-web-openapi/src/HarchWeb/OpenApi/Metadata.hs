{-# LANGUAGE OverloadedStrings #-}

-- | Typed, optional documentation metadata carried by an API endpoint.
--
-- This belongs to the optional @harch-web-openapi@ package: the generic
-- extension slot remains in @harch-web@, so applications that never select
-- this value do not acquire an OpenAPI dependency or a documentation route.
-- The metadata is parameterized by the endpoint's existing request and
-- response types so it can inhabit 'HarchWeb.ApiEndpointContract' without an
-- untyped side channel.  Later interpretation consumes these values from an
-- explicitly supplied family; it does not inspect a completed site.
module HarchWeb.OpenApi.Metadata
  ( OpenApiExtension,
    OpenApiExtensionError (..),
    OpenApiSpecificationExtension,
    emptyOpenApiExtension,
    withOpenApiExtension,
    mkOpenApiExtension,
    withOpenApiOperationId,
    withOpenApiResponseStatus,
    withOpenApiResponseSchema,
    mkOpenApiSpecificationExtension,
    openApiExtensionSummary,
    openApiExtensionOperationId,
    openApiExtensionResponseStatus,
    openApiExtensionResponseSchema,
    openApiExtensionDescription,
    openApiExtensionTags,
    openApiExtensionDeprecated,
    openApiExtensionSpecificationExtensions,
    openApiSpecificationExtensionName,
    openApiSpecificationExtensionValue,
  )
where

import Data.Aeson (Value)
import Data.Char (isAlphaNum, isAscii)
import Data.OpenApi (Schema)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api (ApiEndpointContract, withApiEndpointExtension)

-- | Metadata an application deliberately attaches to one documented API
-- endpoint.  The phantom endpoint parameters preserve the generic extension
-- relationship owned by @harch-web@ without making documentation alter the
-- endpoint handler, codecs, authorization, or availability decision.
data OpenApiExtension fields body response = OpenApiExtension
  { openApiExtensionOperationId :: Maybe Text,
    openApiExtensionResponseStatus :: Maybe Int,
    openApiExtensionResponseSchema :: Maybe Schema,
    openApiExtensionSummary :: Maybe Text,
    openApiExtensionDescription :: Maybe Text,
    openApiExtensionTags :: [Text],
    openApiExtensionDeprecated :: Bool,
    openApiExtensionSpecificationExtensions :: [OpenApiSpecificationExtension]
  }
  deriving (Eq, Show)

-- | A validated OpenAPI @x-*@ extension.  Its JSON value is authored data;
-- validation of the key prevents accidental emission of an ordinary or
-- malformed specification member as an extension.
data OpenApiSpecificationExtension = OpenApiSpecificationExtension Text Value
  deriving (Eq, Show)

-- | Construction failures that are objective specification errors.
data OpenApiExtensionError
  = InvalidOpenApiSpecificationExtensionName Text
  | InvalidOpenApiOperationId Text
  | InvalidOpenApiResponseStatus Int
  | DuplicateOpenApiSpecificationExtension Text
  deriving (Eq, Show)

-- | Metadata with no optional prose, tags, deprecation marker, or extensions.
emptyOpenApiExtension :: OpenApiExtension fields body response
emptyOpenApiExtension = OpenApiExtension Nothing Nothing Nothing Nothing Nothing [] False []

-- | Attach documentation to the same typed endpoint contract that owns its
-- codecs and runtime behavior.  This replaces only the generic extension;
-- it cannot duplicate a route or change its handler, authorization, or
-- availability policy.
withOpenApiExtension :: OpenApiExtension fields body response -> ApiEndpointContract extension fields body response -> ApiEndpointContract OpenApiExtension fields body response
withOpenApiExtension = withApiEndpointExtension

-- | Construct endpoint documentation metadata after rejecting duplicate
-- extension names. Absent prose, tags, and custom extensions are intentionally
-- not errors: documentation remains optional for an otherwise runnable
-- endpoint.
mkOpenApiExtension :: Maybe Text -> Maybe Text -> [Text] -> Bool -> [OpenApiSpecificationExtension] -> Either OpenApiExtensionError (OpenApiExtension fields body response)
mkOpenApiExtension summary description tags deprecated specificationExtensions =
  case duplicateExtensionName specificationExtensions of
    Just duplicate -> Left (DuplicateOpenApiSpecificationExtension duplicate)
    Nothing -> Right (OpenApiExtension Nothing Nothing Nothing summary description tags deprecated specificationExtensions)

-- | Replace the derived method/path operation identity with an authored,
-- portable identifier. An empty or whitespace-only value cannot name an
-- operation; punctuation remains allowed because OpenAPI treats the value as
-- an opaque identifier.
withOpenApiOperationId :: Text -> OpenApiExtension fields body response -> Either OpenApiExtensionError (OpenApiExtension fields body response)
withOpenApiOperationId operationId extension
  | Text.null (Text.strip operationId) = Left (InvalidOpenApiOperationId operationId)
  | otherwise = Right extension {openApiExtensionOperationId = Just operationId}

-- | Replace the conservative @default@ response with one explicitly authored
-- HTTP status.  The status has no schema or body claim: those require a later
-- typed schema/response slice.  OpenAPI permits only three-digit HTTP status
-- codes here, so reject values outside the registered range before document
-- construction.
withOpenApiResponseStatus :: Int -> OpenApiExtension fields body response -> Either OpenApiExtensionError (OpenApiExtension fields body response)
withOpenApiResponseStatus status extension
  | status < 100 || status > 599 = Left (InvalidOpenApiResponseStatus status)
  | otherwise = Right extension {openApiExtensionResponseStatus = Just status}

-- | Declare the schema for every representation of this endpoint's successful
-- response. This is deliberately inline: component references need
-- document-owned component construction and validation, which is a later
-- OpenAPI slice. It cannot change runtime encoding or handler behavior.
withOpenApiResponseSchema :: Schema -> OpenApiExtension fields body response -> OpenApiExtension fields body response
withOpenApiResponseSchema schema extension =
  extension {openApiExtensionResponseSchema = Just schema}

-- | Validate one custom OpenAPI extension name.  OpenAPI reserves the
-- @x-@ prefix; after it, accept only nonempty ASCII letters, digits, dots,
-- underscores, and hyphens so generated JSON members remain portable.
mkOpenApiSpecificationExtension :: Text -> Value -> Either OpenApiExtensionError OpenApiSpecificationExtension
mkOpenApiSpecificationExtension name value
  | isOpenApiExtensionName name = Right (OpenApiSpecificationExtension name value)
  | otherwise = Left (InvalidOpenApiSpecificationExtensionName name)

openApiSpecificationExtensionName :: OpenApiSpecificationExtension -> Text
openApiSpecificationExtensionName (OpenApiSpecificationExtension name _) = name

openApiSpecificationExtensionValue :: OpenApiSpecificationExtension -> Value
openApiSpecificationExtensionValue (OpenApiSpecificationExtension _ value) = value

isOpenApiExtensionName :: Text -> Bool
isOpenApiExtensionName name =
  case Text.stripPrefix "x-" name of
    Just suffix -> not (Text.null suffix) && Text.all validExtensionCharacter suffix
    Nothing -> False

validExtensionCharacter :: Char -> Bool
validExtensionCharacter character =
  character `elem` ("._-" :: String) || (isAscii character && isAlphaNum character)

duplicateExtensionName :: [OpenApiSpecificationExtension] -> Maybe Text
duplicateExtensionName = go []
  where
    go _ [] = Nothing
    go names (extension : rest)
      | openApiSpecificationExtensionName extension `elem` names = Just (openApiSpecificationExtensionName extension)
      | otherwise = go (openApiSpecificationExtensionName extension : names) rest
