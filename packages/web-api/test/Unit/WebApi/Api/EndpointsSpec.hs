{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import HarchWeb.Account (mkAccountId)
import HarchWeb.Api (ApiResponse (..), MissingContentTypePolicy (RejectMissingContentType), at)
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.Authentication (ScopeRequirement (RequireAllScopes, RequireAnyScope), encodedJwtFromBytes)
import HarchWeb.Email (mkEmailAddress)
import HarchWeb.EndpointSecurity (requiredAuthenticationProfileNameOrDie)
import HarchWeb.OpenApi (OpenApiDocumentFailure (EmptyOpenApiDocumentTitle), OpenApiDocumentProvider (..), mkOpenApiExtension, mkOpenApiHttpBearerSecurityScheme, withOpenApiOperationId)
import HarchWeb.Routing (requiredPathSegment)
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Site (RouteDefinition (routeMetadata))
import HarchWeb.Username (mkUsername)
import Network.HTTP.Types qualified as HttpTypes
import WebApi.Account (AccountProfile (..), AccountStoreError (AccountStoreUnavailable))
import WebApi.Api.Endpoints (MeApiFailure (..), TokenApiFailure (..), appAuthorizationScopes, docsOpenApiSpecRouteDefinition, meApiFailureResponse, meApiOutcomeResponse, requireOpenApiExtension, requireWebApiOpenApiDocumentProvider, requiredApiHeaderNameOrDie, requiredApiHeaderValueOrDie, tokenApiFailureResponse, tokenApiMissingContentTypePolicy, tokenApiOutcomeResponse, webApiApiRouteMount, webApiOpenApiEndpointMetadataForPath, webApiOpenApiSecuritySchemes)
import WebApi.ApiClientToken (ApiClientTokenOutcome (..))
import WebApi.Profile (ProfileLoadError (..), ProfileState (..))
import WebApi.Route (AppRoute (..), accountAuthenticationProfileName, defaultRequestContext, endpointMetadata, requiredOAuth2ScopeOrDie, resourceAuthenticationProfileName, resourceReadScope)

spec =
  describe "WebApi.Api.Endpoints" $ do
    it "reports the token endpoint unavailable rather than emitting an unencodable access token" $
      case tokenApiOutcomeResponse (ApiClientTokenIssued (encodedJwtFromBytes (ByteString.singleton 255)) [] 900) of
        Left TokenApiUnavailable -> pure ()
        Left _ -> expectationFailure "expected the generic unavailable outcome for an unencodable signed token"
        Right _ -> expectationFailure "expected malformed EncodedJwt bytes to be rejected, not rendered as a token"

    it "collapses every non-protocol issuance failure into the same generic unavailable outcome" $
      mapM_
        ( \case
            Left TokenApiUnavailable -> pure ()
            Left _ -> expectationFailure "expected the generic unavailable outcome"
            Right _ -> expectationFailure "expected an unavailable outcome, not a token"
        )
        [ tokenApiOutcomeResponse ApiClientTokenStoreUnavailable,
          tokenApiOutcomeResponse ApiClientTokenWorkBudgetExhausted,
          tokenApiOutcomeResponse ApiClientTokenIssueFailed
        ]

    it "renders the generic unavailable outcome as a safe 503 body" $ do
      let response = tokenApiFailureResponse TokenApiUnavailable
      apiEndpointResponseStatus response `shouldBe` HttpTypes.status503
      apiEndpointResponseValue response `shouldBe` "{\"error\":\"token-issuance-unavailable\"}"

    it "declares the RFC 6749 form-body content-type policy exactly once" $
      tokenApiMissingContentTypePolicy `shouldBe` RejectMissingContentType

    it "fails immediately when a program-owned header literal is invalid" $ do
      requiredApiHeaderNameOrDie "WWW-Authenticate" `shouldBe` requiredApiHeaderNameOrDie "WWW-Authenticate"
      requiredApiHeaderValueOrDie "Basic" `shouldBe` requiredApiHeaderValueOrDie "Basic"
      -- 'isInfixOf' on only the literal prefix would never force the
      -- appended invalid value, leaving that concatenation genuinely
      -- untested; comparing the exact message forces the whole thunk.
      evaluate (requiredApiHeaderNameOrDie "" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "invalid API header name literal: "
      evaluate (requiredApiHeaderValueOrDie "not a valid header value\n" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "invalid API header value literal: not a valid header value\n"

    it "reports the account-self resource unavailable for a durable-store failure or a lookup that disagrees with an already-established session" $
      mapM_
        ( \outcome -> case meApiOutcomeResponse outcome of
            Left MeApiUnavailable -> pure ()
            Right _ -> expectationFailure "expected the generic unavailable outcome, not a rendered profile"
        )
        [ Left (ProfileAccountStoreError (AccountStoreUnavailable "test")),
          Right ProfileUnauthenticated
        ]

    it "renders the account-self unavailable outcome as a safe 503 body" $ do
      let response = meApiFailureResponse MeApiUnavailable
      apiEndpointResponseStatus response `shouldBe` HttpTypes.status503
      apiEndpointResponseValue response `shouldBe` "{\"error\":\"profile-unavailable\"}"

    it "renders an authenticated (email-verified) account's own username and email, and a pending account's the same way" $
      mapM_
        ( \profileState -> case meApiOutcomeResponse (Right profileState) of
            Right response -> apiEndpointResponseValue response `shouldBe` "{\"username\":\"account-holder\",\"email\":\"me-spec@example.test\"}"
            Left _ -> expectationFailure "expected a rendered profile, not the generic unavailable outcome"
        )
        [ProfileAuthenticated testAccountProfile, ProfilePending testAccountProfile]

    -- AHI-4E: the documentation boundary below is where authored OpenAPI
    -- metadata, real endpoint security, and the served specification meet.

    it "keeps an authored documentation extension total, with its failure rail directly testable" $ do
      let validExtension = requireOpenApiExtension (mkOpenApiExtension (Just "summary") Nothing [] False [])
      validExtension `seq` pure ()
      evaluate (requireOpenApiExtension (withOpenApiOperationId "" validExtension) `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "web-api authored an invalid OpenAPI extension: InvalidOpenApiOperationId \"\""

    it "resolves every documented family path back to the ONE real endpoint metadata dispatch uses" $
      expectAll
        ( (webApiOpenApiEndpointMetadataForPath (at "/status") `shouldBe` endpointMetadata StatusApiRoute)
            :| [ webApiOpenApiEndpointMetadataForPath (at "/second") `shouldBe` endpointMetadata SecondApiRoute,
                 webApiOpenApiEndpointMetadataForPath (at "/me") `shouldBe` endpointMetadata MeApiRoute,
                 webApiOpenApiEndpointMetadataForPath (at "/oauth/token") `shouldBe` endpointMetadata TokenApiRoute
               ]
        )

    it "fails document construction for a family path no real endpoint owns" $
      evaluate (webApiOpenApiEndpointMetadataForPath (at "/unknown") `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "web-api documents no API endpoint at family path /api/unknown"

    it "projects both real scope-requirement forms into their declared scope names" $
      expectAll
        ( (appAuthorizationScopes (RequireAnyScope (resourceReadScope NonEmpty.:| [])) `shouldBe` ["resource:read"])
            :| [ appAuthorizationScopes
                   (RequireAllScopes (resourceReadScope NonEmpty.:| [requiredOAuth2ScopeOrDie "orders:write"]))
                   `shouldBe` ["resource:read", "orders:write"],
                 appAuthorizationScopes (RequireAnyScope (requiredOAuth2ScopeOrDie "resource:read" NonEmpty.:| [])) `shouldBe` ["resource:read"]
               ]
        )

    it "maps both real authentication profiles to the JWT bearer scheme those endpoints actually accept" $
      expectAll
        ( (Map.size webApiOpenApiSecuritySchemes `shouldBe` 2)
            :| [ Map.lookup accountAuthenticationProfileName webApiOpenApiSecuritySchemes
                   `shouldBe` Just (mkOpenApiHttpBearerSecurityScheme (Just "JWT")),
                 Map.lookup resourceAuthenticationProfileName webApiOpenApiSecuritySchemes
                   `shouldBe` Just (mkOpenApiHttpBearerSecurityScheme (Just "JWT")),
                 Map.lookup (requiredAuthenticationProfileNameOrDie "unknown") webApiOpenApiSecuritySchemes
                   `shouldBe` Nothing
               ]
        )

    it "records the one structural documented-family mount with an identity prism over the closed route type" $
      expectAll
        ( (routeMountName webApiApiRouteMount `shouldBe` requiredModuleNameOrDie "web-api")
            :| [ routeMountPrefix webApiApiRouteMount `shouldBe` (requiredPathSegment "api" NonEmpty.:| []),
                 embedChildRoute webApiApiRouteMount HomeRoute `shouldBe` HomeRoute,
                 projectChildRoute webApiApiRouteMount HomeRoute `shouldBe` Just HomeRoute,
                 projectChildRoute webApiApiRouteMount DocsOpenApiSpecRoute `shouldBe` Just DocsOpenApiSpecRoute
               ]
        )

    it "passes a valid provider through unchanged and names a construction failure instead of serving it" $ do
      let deliberatelyFailingProvider =
            OpenApiDocumentProvider (\_ -> pure (Left EmptyOpenApiDocumentTitle))
      prepared <- prepareOpenApiDocument (requireWebApiOpenApiDocumentProvider (Right deliberatelyFailingProvider)) defaultRequestContext
      case prepared of
        Left EmptyOpenApiDocumentTitle -> pure ()
        Left _ -> expectationFailure "expected exactly the supplied typed failure, not another construction error"
        Right _ -> expectationFailure "expected the provider's own typed failure to pass through unchanged"
      evaluate (requireWebApiOpenApiDocumentProvider (Left EmptyOpenApiDocumentTitle) `seq` ())
        `shouldThrow` \case
          ErrorCall message -> message == "OpenAPI document title must not be empty."

    it "carries the specification route's own endpoint metadata into its route definition" $ do
      let inertProvider = OpenApiDocumentProvider (\_ -> pure (Left EmptyOpenApiDocumentTitle))
      routeMetadata (docsOpenApiSpecRouteDefinition inertProvider)
        `shouldBe` endpointMetadata DocsOpenApiSpecRoute

testAccountProfile :: AccountProfile
testAccountProfile =
  AccountProfile
    { accountProfileId = required "account id" (mkAccountId "account_01"),
      accountProfileEmail = required "email address" (mkEmailAddress "me-spec@example.test"),
      accountProfileUsername = Just (required "username" (mkUsername "account-holder")),
      accountProfileDisplayName = Nothing,
      accountProfileEmailVerified = True
    }

required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected a valid test " <> label))
