{-# LANGUAGE OverloadedStrings #-}

-- | The closed OpenAPI security-scheme vocabulary an application may attach
-- to a documented family.
--
-- Decision record (AHI-4E, 2026-09-23): this type is deliberately closed to
-- the AHI-4D flow kinds @TASKS/ahi-4e-openapi-and-swagger.md@ names (browser
-- session awareness, and OAuth 2.0 client credentials), not an open escape
-- hatch onto the full @openapi3@ 'Data.OpenApi.SecurityScheme'. Every value a
-- smart constructor here accepts is already a valid scheme, so
-- 'HarchWeb.OpenApi.Document' never has to re-validate a scheme it is handed
-- and cannot represent a malformed one.
--
-- Follow-up decision (AHI-4E, 2026-09-23, while documenting @web-api@'s real
-- endpoints): the task file's "browser-session awareness" prose describes a
-- single credential source, but @web-api@'s actual cookie-or-bearer AHI-4D
-- profiles accept one JWT through either an @__Host-@ session cookie or a
-- bearer @Authorization@ header at the same transport boundary (see
-- @HarchWeb.Authentication.Transport.JwtProofSource@) — a plain OpenAPI
-- @apiKey@/cookie scheme cannot describe that, and it is not the OAuth2
-- client-credentials shape either. 'OpenApiHttpBearerSecurityScheme' adds the
-- third, still-closed case this requires: a standard HTTP bearer scheme,
-- which is also the idiomatic way Swagger's raw-@Authorization@-header
-- control (a supported case per the task file's authentication section)
-- exercises a JWT-bearing endpoint, cookie or no cookie.
module HarchWeb.OpenApi.Security
  ( OpenApiSecurityScheme,
    OpenApiSecuritySchemeError (..),
    mkOpenApiCookieSessionSecurityScheme,
    mkOpenApiOAuth2ClientCredentialsSecurityScheme,
    mkOpenApiHttpBearerSecurityScheme,
    openApiSecuritySchemeModel,
  )
where

import Data.HashMap.Strict.InsOrd.Compat qualified as InsOrdHashMap
import Data.OpenApi
  ( ApiKeyLocation (ApiKeyCookie),
    ApiKeyParams (..),
    HttpSchemeType (HttpSchemeBearer),
    OAuth2ClientCredentialsFlow (..),
    OAuth2Flow (..),
    OAuth2Flows (..),
    SecurityScheme (..),
    SecuritySchemeType (SecuritySchemeApiKey, SecuritySchemeHttp, SecuritySchemeOAuth2),
  )
import Data.Text (Text)
import Data.Text qualified as Text
import Network.URI (URI (..), URIAuth (..), parseURI)

-- | One documented security scheme. A cookie-session scheme names the
-- @HttpOnly@ cookie itself only so the rendered document can describe it;
-- Swagger never reads or copies its value (see the task file's "Security
-- schemes and authentication assistance" section). An OAuth2 client-
-- credentials scheme carries its token URL and the scheme's complete
-- declared scope set (a distinct, generally larger set than what any one
-- operation requires). An HTTP bearer scheme documents a profile that
-- accepts a JWT via a bearer @Authorization@ header (optionally alongside a
-- cookie, at the application's transport boundary, not this scheme) — the
-- shape @web-api@'s own cookie-or-bearer AHI-4D profiles actually need; it
-- is distinct from the pure browser-session cookie case above.
data OpenApiSecurityScheme
  = OpenApiCookieSessionSecurityScheme Text
  | OpenApiOAuth2ClientCredentialsSecurityScheme Text [(Text, Text)]
  | OpenApiHttpBearerSecurityScheme (Maybe Text)
  deriving (Eq, Show)

-- | Construction failures for one security scheme value.
data OpenApiSecuritySchemeError
  = EmptyOpenApiCookieSessionName
  | InvalidOpenApiOAuth2TokenUrl Text
  deriving (Eq, Show)

-- | Name the @HttpOnly@ session cookie a browser-session profile relies on.
-- An empty name cannot describe a real cookie.
mkOpenApiCookieSessionSecurityScheme :: Text -> Either OpenApiSecuritySchemeError OpenApiSecurityScheme
mkOpenApiCookieSessionSecurityScheme cookieName
  | Text.null cookieName = Left EmptyOpenApiCookieSessionName
  | otherwise = Right (OpenApiCookieSessionSecurityScheme cookieName)

-- | Declare an HTTP bearer-token scheme. The optional bearer format is
-- display-only documentation (e.g. @Just "JWT"@); there is no invalid input
-- to reject here, so this never fails.
mkOpenApiHttpBearerSecurityScheme :: Maybe Text -> OpenApiSecurityScheme
mkOpenApiHttpBearerSecurityScheme = OpenApiHttpBearerSecurityScheme

-- | Declare an OAuth 2.0 client-credentials scheme. The token URL must be an
-- absolute @https@ URL with an authority: @client_secret_basic@ sends a
-- confidential client secret, so an unencrypted or relative endpoint would
-- describe a scheme Swagger cannot safely exercise. This intentionally does
-- not accept @http@, including for local development; a reviewed reverse
-- proxy or a documented @https@-only local certificate remains the
-- application's existing TLS story rather than a special case here.
mkOpenApiOAuth2ClientCredentialsSecurityScheme :: Text -> [(Text, Text)] -> Either OpenApiSecuritySchemeError OpenApiSecurityScheme
mkOpenApiOAuth2ClientCredentialsSecurityScheme tokenUrl scopes
  | isHttpsUrl tokenUrl = Right (OpenApiOAuth2ClientCredentialsSecurityScheme tokenUrl scopes)
  | otherwise = Left (InvalidOpenApiOAuth2TokenUrl tokenUrl)

isHttpsUrl :: Text -> Bool
isHttpsUrl value =
  case parseURI (Text.unpack value) of
    Just URI {uriScheme, uriAuthority = Just URIAuth {uriRegName}}
      | not (null uriRegName) && uriScheme == "https:" -> True
    _ -> False

-- | Lower a validated scheme to the real @openapi3@ model. A cookie-session
-- scheme renders as a standard OpenAPI @apiKey@/@cookie@ scheme, the
-- idiomatic representation for a same-origin session cookie; there is no
-- dedicated OpenAPI "browser session" scheme type.
openApiSecuritySchemeModel :: OpenApiSecurityScheme -> SecurityScheme
openApiSecuritySchemeModel scheme =
  case scheme of
    OpenApiCookieSessionSecurityScheme cookieName ->
      SecurityScheme
        { _securitySchemeType = SecuritySchemeApiKey (ApiKeyParams cookieName ApiKeyCookie),
          _securitySchemeDescription = Nothing
        }
    OpenApiOAuth2ClientCredentialsSecurityScheme tokenUrl scopes ->
      SecurityScheme
        { _securitySchemeType =
            SecuritySchemeOAuth2
              (mempty :: OAuth2Flows)
                { _oAuth2FlowsClientCredentials =
                    Just
                      OAuth2Flow
                        { _oAuth2Params = OAuth2ClientCredentialsFlow tokenUrl,
                          _oAath2RefreshUrl = Nothing,
                          _oAuth2Scopes = InsOrdHashMap.fromList scopes
                        }
                },
          _securitySchemeDescription = Nothing
        }
    OpenApiHttpBearerSecurityScheme bearerFormat ->
      SecurityScheme
        { _securitySchemeType = SecuritySchemeHttp (HttpSchemeBearer bearerFormat),
          _securitySchemeDescription = Nothing
        }
