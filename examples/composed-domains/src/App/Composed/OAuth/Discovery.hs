-- | Public JWKS and OAuth authorization-server/resource metadata responses.
module App.Composed.OAuth.Discovery
  ( authorizationServerMetadataRouteDefinition,
    protectedResourceMetadataRouteDefinition,
    publicJwksRouteDefinition,
  )
where

import App.Composed.Auth (composedJwtPublicJwkSet)
import App.Composed.Model (ComposedContext, RootAuthorization, RootRoute)
import App.Composed.OAuth.Configuration (ComposedOAuthDependencies (..), ComposedOAuthUrl (..))
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api
  ( ApiEndpointContract (..),
    ApiFieldFailurePolicy (ApiUseGenericFieldFailure),
    ApiMethod (ApiGet),
    ApiRequestBody (ApiNoRequestBody),
    NoApiExtension (..),
    apiContentType,
    apiResponse,
    apiRouteDefinitionWithContextNeverFailing,
    bytesResponseEncoder,
    jsonMediaType,
    noRequestFields,
  )
import HarchWeb.Authentication (oauth2ScopeText)
import HarchWeb.EndpointMetadata (EndpointMetadata)
import HarchWeb.Site (RouteDefinition)
import Network.URI (URI (..), uriToString)

publicJwksRouteDefinition :: ComposedOAuthDependencies -> EndpointMetadata RootAuthorization -> RouteDefinition RootRoute ComposedContext RootAuthorization
publicJwksRouteDefinition dependencies metadata =
  staticJsonRouteDefinition metadata (Aeson.toJSON (composedJwtPublicJwkSet (composedOAuthRuntime dependencies)))

authorizationServerMetadataRouteDefinition :: ComposedOAuthDependencies -> EndpointMetadata RootAuthorization -> RouteDefinition RootRoute ComposedContext RootAuthorization
authorizationServerMetadataRouteDefinition dependencies metadata =
  staticJsonRouteDefinition metadata (authorizationServerMetadata dependencies)

protectedResourceMetadataRouteDefinition :: ComposedOAuthDependencies -> EndpointMetadata RootAuthorization -> RouteDefinition RootRoute ComposedContext RootAuthorization
protectedResourceMetadataRouteDefinition dependencies metadata =
  staticJsonRouteDefinition metadata (protectedResourceMetadata dependencies)

staticJsonRouteDefinition :: EndpointMetadata RootAuthorization -> Value -> RouteDefinition RootRoute ComposedContext RootAuthorization
staticJsonRouteDefinition metadata jsonValue =
  apiRouteDefinitionWithContextNeverFailing contract metadata (\_ _ -> pure (apiResponse responseBytes))
  where
    contract =
      ApiEndpointContract
        ApiGet
        noRequestFields
        ApiNoRequestBody
        (bytesResponseEncoder (apiContentType jsonMediaType) :| [])
        ApiUseGenericFieldFailure
        NoApiExtension
    responseBytes = jsonBytes jsonValue

authorizationServerMetadata :: ComposedOAuthDependencies -> Value
authorizationServerMetadata dependencies =
  Aeson.object
    [ "issuer" .= composedOAuthIssuer dependencies,
      "token_endpoint" .= endpointAbsoluteUrl (composedOAuthIssuerUrl dependencies) "/oauth/token",
      "jwks_uri" .= endpointAbsoluteUrl (composedOAuthIssuerUrl dependencies) "/oauth/jwks.json",
      "grant_types_supported" .= ["client_credentials" :: Text],
      "response_types_supported" .= ([] :: [Text]),
      "token_endpoint_auth_methods_supported" .= ["client_secret_basic" :: Text],
      "scopes_supported" .= (oauth2ScopeText <$> composedOAuthExampleScopes dependencies)
    ]

protectedResourceMetadata :: ComposedOAuthDependencies -> Value
protectedResourceMetadata dependencies =
  Aeson.object
    [ "resource" .= composedOAuthResource dependencies,
      "authorization_servers" .= [composedOAuthIssuer dependencies],
      "scopes_supported" .= (oauth2ScopeText <$> composedOAuthExampleScopes dependencies),
      "bearer_methods_supported" .= ["header" :: Text]
    ]

endpointAbsoluteUrl :: ComposedOAuthUrl -> Text -> Text
endpointAbsoluteUrl url suffix =
  Text.pack
    ( uriToString
        id
        ((composedOAuthParsedUrl url) {uriPath = Text.unpack (composedOAuthBasePath url <> suffix), uriQuery = "", uriFragment = ""})
        ""
    )

jsonBytes :: Value -> ByteString
jsonBytes = LazyByteString.toStrict . Aeson.encode
