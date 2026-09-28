{-# LANGUAGE OverloadedStrings #-}

-- | Validated issuer/resource settings shared by the composed OAuth owners.
module App.Composed.OAuth.Configuration
  ( ComposedOAuthConfigurationError (..),
    ComposedOAuthDependencies (..),
    ComposedOAuthUrl (..),
    mkComposedOAuthDependencies,
  )
where

import App.Composed.ApiClient (composedExampleApiClientScopes)
import App.Composed.ApiClientToken (ComposedApiClientTokenEnvironment (..))
import App.Composed.Auth (ComposedJwtRuntime, composedJwtApiAudienceText, composedJwtIssuerText)
import Data.Bifunctor (first)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Authentication (OAuth2Scope, OAuth2ScopeError)
import HarchWeb.Routing (decodeRouteLocation, pathSegmentText, requestTarget, routePathSegments)
import Network.URI (URI (..), URIAuth (..), parseURI)

data ComposedOAuthConfigurationError
  = ComposedOAuthIssuerMustBeHttpsUrl
  | ComposedOAuthResourceMustBeHttpsUrl
  | ComposedOAuthExampleScopesInvalid OAuth2ScopeError
  deriving (Eq, Show)

data ComposedOAuthUrl = ComposedOAuthUrl
  { composedOAuthParsedUrl :: URI,
    composedOAuthBasePath :: Text,
    composedOAuthBaseSegments :: [Text]
  }

data ComposedOAuthDependencies = ComposedOAuthDependencies
  { composedOAuthTokenEnvironment :: ComposedApiClientTokenEnvironment,
    composedOAuthRuntime :: ComposedJwtRuntime,
    composedOAuthIssuer :: Text,
    composedOAuthResource :: Text,
    composedOAuthIssuerUrl :: ComposedOAuthUrl,
    composedOAuthResourceUrl :: ComposedOAuthUrl,
    composedOAuthExampleScopes :: NonEmpty OAuth2Scope
  }

mkComposedOAuthDependencies :: ComposedApiClientTokenEnvironment -> Either ComposedOAuthConfigurationError ComposedOAuthDependencies
mkComposedOAuthDependencies tokenEnvironment = do
  let jwtRuntime = composedApiClientTokenJwtRuntime tokenEnvironment
      issuer = composedJwtIssuerText jwtRuntime
      resource = composedJwtApiAudienceText jwtRuntime
  issuerUrl <- maybe (Left ComposedOAuthIssuerMustBeHttpsUrl) Right (parseComposedOAuthUrl issuer)
  resourceUrl <- maybe (Left ComposedOAuthResourceMustBeHttpsUrl) Right (parseComposedOAuthUrl resource)
  exampleScopes <- first ComposedOAuthExampleScopesInvalid composedExampleApiClientScopes
  pure
    ComposedOAuthDependencies
      { composedOAuthTokenEnvironment = tokenEnvironment,
        composedOAuthRuntime = jwtRuntime,
        composedOAuthIssuer = issuer,
        composedOAuthResource = resource,
        composedOAuthIssuerUrl = issuerUrl,
        composedOAuthResourceUrl = resourceUrl,
        composedOAuthExampleScopes = exampleScopes
      }

parseComposedOAuthUrl :: Text -> Maybe ComposedOAuthUrl
parseComposedOAuthUrl value = do
  parsedUrl <- parseURI (Text.unpack value)
  authority <- uriAuthority parsedUrl
  if uriScheme parsedUrl /= "https:" || null (uriRegName authority) || not (null (uriUserInfo authority)) || not (null (uriQuery parsedUrl)) || not (null (uriFragment parsedUrl))
    then Nothing
    else do
      let rawBasePath = Text.pack (uriPath parsedUrl)
          basePath = stripOneTrailingSlash rawBasePath
          rawRoutePath = TextEncoding.encodeUtf8 (if Text.null basePath then "/" else basePath)
      decodedPath <- either (const Nothing) Just (decodeRouteLocation (requestTarget rawRoutePath ""))
      let allSegments = pathSegmentText <$> routePathSegments decodedPath
          baseSegments = trimTrailingEmptySegments allSegments
      if any Text.null baseSegments
        then Nothing
        else
          Just
            ComposedOAuthUrl
              { composedOAuthParsedUrl = parsedUrl,
                composedOAuthBasePath = basePath,
                composedOAuthBaseSegments = baseSegments
              }

stripOneTrailingSlash :: Text -> Text
stripOneTrailingSlash value =
  if Text.isSuffixOf "/" value
    then Text.dropEnd 1 value
    else value

trimTrailingEmptySegments :: [Text] -> [Text]
trimTrailingEmptySegments = reverse . dropWhile Text.null . reverse
