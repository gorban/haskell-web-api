{-# LANGUAGE OverloadedStrings #-}

-- | Pure, validated endpoint declarations shared by route and action codecs.
-- Keeping this layer below response execution lets every declared endpoint
-- carry the same metadata without making action decoding depend on guards.
module HarchWeb.EndpointMetadata
  ( AccessRequirement (..),
    EndpointMetadata (..),
    EndpointMetadataError (..),
    EndpointName,
    EndpointProtocol (..),
    RouteTemplate,
    endpointNameText,
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
    routeTemplateText,
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text

data EndpointProtocol
  = HtmlEndpoint
  | ApiEndpoint
  | ActionEndpoint
  | AssetEndpoint
  deriving (Eq, Show)

data AccessRequirement authorization
  = AllowUnauthenticated
  | RequireAuthenticated
  | RequireAuthorized authorization
  deriving (Eq, Show)

newtype EndpointName = EndpointName Text
  deriving (Eq, Ord, Show)

endpointNameText :: EndpointName -> Text
endpointNameText (EndpointName value) = value

newtype RouteTemplate = RouteTemplate Text
  deriving (Eq, Ord, Show)

routeTemplateText :: RouteTemplate -> Text
routeTemplateText (RouteTemplate value) = value

data EndpointMetadataError
  = EmptyEndpointName
  | EndpointNameTooLong
  | InvalidEndpointName
  | EmptyRouteTemplate
  | RouteTemplateTooLong
  | InvalidRouteTemplate
  deriving (Eq, Show)

mkEndpointName :: Text -> Either EndpointMetadataError EndpointName
mkEndpointName value
  | Text.null value = Left EmptyEndpointName
  | Text.length value > 128 = Left EndpointNameTooLong
  | Text.all endpointNameCharacter value = Right (EndpointName value)
  | otherwise = Left InvalidEndpointName
  where
    endpointNameCharacter character =
      isAsciiLower character || isDigit character || character == '.' || character == '-'

-- | Construct an endpoint name for a program-owned static declaration.
--
-- Request values must use 'mkEndpointName' and handle its error rail. This
-- helper makes a malformed literal an immediate programming error, with the
-- same explicit boundary used by 'HarchWeb.requiredSafeUrlOrDie'.
requiredEndpointNameOrDie :: Text -> EndpointName
requiredEndpointNameOrDie value =
  case mkEndpointName value of
    Right endpointNameValue -> endpointNameValue
    Left metadataError -> error ("HarchWeb.EndpointMetadata: invalid endpoint name " <> show value <> ": " <> show metadataError)

mkRouteTemplate :: Text -> Either EndpointMetadataError RouteTemplate
mkRouteTemplate value
  | Text.null value = Left EmptyRouteTemplate
  | Text.length value > 256 = Left RouteTemplateTooLong
  | validRouteTemplate value = Right (RouteTemplate value)
  | otherwise = Left InvalidRouteTemplate

validRouteTemplate :: Text -> Bool
validRouteTemplate value =
  Text.head value == '/'
    && Text.all routeTemplateCharacter value
  where
    routeTemplateCharacter character =
      character /= '?' && character /= '#' && character /= '\\' && character >= ' '

-- | Construct a route template for a program-owned static declaration.
--
-- Request values must use 'mkRouteTemplate' and handle its error rail. This
-- helper keeps invalid literals at the declaration boundary instead of
-- spreading local partial wrappers through every application route table.
requiredRouteTemplateOrDie :: Text -> RouteTemplate
requiredRouteTemplateOrDie value =
  case mkRouteTemplate value of
    Right routeTemplateValue -> routeTemplateValue
    Left metadataError -> error ("HarchWeb.EndpointMetadata: invalid route template " <> show value <> ": " <> show metadataError)

data EndpointMetadata authorization = EndpointMetadata
  { endpointName :: EndpointName,
    endpointRouteTemplate :: RouteTemplate,
    endpointProtocol :: EndpointProtocol,
    endpointAccess :: AccessRequirement authorization
  }
  deriving (Eq, Show)

mkEndpointMetadata :: EndpointName -> RouteTemplate -> EndpointProtocol -> AccessRequirement authorization -> EndpointMetadata authorization
mkEndpointMetadata = EndpointMetadata
