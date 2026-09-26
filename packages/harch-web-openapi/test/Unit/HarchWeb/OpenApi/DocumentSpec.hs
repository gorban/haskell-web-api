{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Aeson (Object, Value (..), eitherDecode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Map.Strict qualified as Map
import Data.Maybe (isJust)
import Data.OpenApi (Schema)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api qualified as Api
import HarchWeb.Api.Multipart (InMemoryUpload, defaultMultipartLimits, inMemoryMultipartStorage)
import HarchWeb.ApplicationModule (RouteMount (..))
import HarchWeb.EndpointMetadata (AccessRequirement (AllowUnauthenticated, RequireAuthenticated, RequireAuthorized), AuthenticationProfileName, EndpointMetadata, EndpointProtocol (ApiEndpoint), mkEndpointMetadata, requiredAuthenticationProfileNameOrDie, requiredEndpointNameOrDie, requiredRouteTemplateOrDie, withAuthenticationProfile)
import HarchWeb.OpenApi
import HarchWeb.Routing (requiredPathSegment)
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)

spec =
  describe "OpenAPI document construction" $ do
    it "uses the supplied structural mount, prunes hidden endpoints, and encodes the 3.0.3 extension overlay" $ do
      extension <- requireRight (mkOpenApiExtension (Just "List catalog items") (Just "The visible catalog collection.") ["catalog"] False =<< traverse (uncurry mkOpenApiSpecificationExtension) [("x-harch-preview", String "enabled")])
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension, hiddenEndpoint "/unlisted" extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      encoded <- decodeDocument document
      let visibleGet = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get"
      expectAll
        ( ((visibleGet >>= lookupText "summary") `shouldBe` Just "List catalog items")
            :| [ (lookupObject "paths" encoded >>= lookupObject "/api/catalog/unlisted") `shouldBe` Nothing,
                 lookupText "openapi" encoded `shouldBe` Just "3.0.3",
                 (visibleGet >>= lookupText "operationId") `shouldBe` Just "get-api-catalog-items",
                 (visibleGet >>= lookupText "x-harch-preview") `shouldBe` Just "enabled",
                 (visibleGet >>= lookupObject "responses" >>= lookupObject "default" >>= lookupText "description") `shouldBe` Just "Response",
                 (visibleGet >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content" >>= lookupObject "text/plain") `shouldBe` Just mempty
               ]
        )

    it "rejects a duplicate mounted path and method before producing a document" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      let endpointFamily = family [visibleEndpoint "/items" Api.ApiGet extension]
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount endpointFamily anonymousEndpointMetadata noRequiredScopes, openApiMountedFamily catalogMount endpointFamily anonymousEndpointMetadata noRequiredScopes])
        (DuplicateOpenApiPathMethod "/api/catalog/items" Api.ApiGet)

    it "rejects generated operation identifiers that would collide" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      expectDocumentFailure
        ( buildOpenApiDocument
            (OpenApiDocumentDetails "Catalog API" "1.0")
            Map.empty
            False
            [openApiMountedFamily catalogMount (family [visibleEndpoint "/one-two" Api.ApiGet extension, visibleEndpoint "/one/two" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes]
        )
        (DuplicateOpenApiOperationId "get-api-catalog-one-two")

    it "uses authored operation identifiers and rejects their collisions" $ do
      baseExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      extension <- requireRight (withOpenApiOperationId "catalog-lookup" baseExtension)
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
      encoded <- decodeDocument document
      (lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupText "operationId") `shouldBe` Just "catalog-lookup"
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension, visibleEndpoint "/summary" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
        (DuplicateOpenApiOperationId "catalog-lookup")

    it "uses an authored response status with its safe standard description" $ do
      extension <- requireRight (withOpenApiResponseStatus 201 emptyOpenApiExtension)
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiPost extension]) anonymousEndpointMetadata noRequiredScopes])
      encoded <- decodeDocument document
      let responses = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "post" >>= lookupObject "responses"
      expectAll
        ( ((responses >>= lookupObject "201" >>= lookupText "description") `shouldBe` Just "Created")
            :| [ (responses >>= lookupObject "default") `shouldBe` Nothing
               ]
        )

    it "encodes validated operation external documentation" $ do
      extension <- requireRight (withOpenApiExternalDocs "https://docs.example.test/catalog/items" (Just "Catalog item guide") emptyOpenApiExtension)
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
      encoded <- decodeDocument document
      let externalDocs = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "externalDocs"
      expectAll
        ( ((externalDocs >>= lookupText "url") `shouldBe` Just "https://docs.example.test/catalog/items")
            :| [ (externalDocs >>= lookupText "description") `shouldBe` Just "Catalog item guide"
               ]
        )

    it "documents every declared response representation without inventing a body shape" $ do
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [visibleEndpointWithEncoders "/items" Api.ApiGet (Api.jsonResponseEncoder :| [Api.textResponseEncoder]) emptyOpenApiExtension]) anonymousEndpointMetadata noRequiredScopes]
          )
      encoded <- decodeDocument document
      let content = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content"
      expectAll
        ( ((content >>= lookupObject "application/json") `shouldBe` Just mempty)
            :| [ (content >>= lookupObject "text/plain") `shouldBe` Just mempty
               ]
        )

    it "applies explicit inline response schema and example metadata to every declared representation" $ do
      let extension = withOpenApiResponseExample (String "visible item") (withOpenApiResponseSchema (mempty :: Schema) emptyOpenApiExtension)
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [visibleEndpointWithEncoders "/items" Api.ApiGet (Api.jsonResponseEncoder :| [Api.textResponseEncoder]) extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      encoded <- decodeDocument document
      let content = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content"
      expectAll
        ( ((content >>= lookupObject "application/json" >>= lookupObject "schema") `shouldBe` Just mempty)
            :| [ (content >>= lookupObject "text/plain" >>= lookupObject "schema") `shouldBe` Just mempty,
                 (content >>= lookupObject "application/json" >>= lookupText "example") `shouldBe` Just "visible item",
                 (content >>= lookupObject "text/plain" >>= lookupText "example") `shouldBe` Just "visible item"
               ]
        )

    it "documents an authored response example without inventing a schema" $ do
      let extension = withOpenApiResponseExample (String "visible item") emptyOpenApiExtension
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      encoded <- decodeDocument document
      let mediaType = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content" >>= lookupObject "text/plain"
      expectAll
        ( ((mediaType >>= lookupText "example") `shouldBe` Just "visible item")
            :| [ (mediaType >>= lookupObject "schema") `shouldBe` Nothing
               ]
        )

    it "documents an explicit request schema at every concrete typed request representation" $ do
      let extension = withOpenApiRequestExample (String "new item") (withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension)
      bufferedDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [bufferedEndpoint "/items" extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      urlEncodedDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [urlEncodedEndpoint "/form" extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      multipartDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [multipartEndpoint "/uploads" extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      bufferedContent <- requestContentFor bufferedDocument "/api/catalog/items"
      urlEncodedContent <- requestContentFor urlEncodedDocument "/api/catalog/form"
      multipartContent <- requestContentFor multipartDocument "/api/catalog/uploads"
      expectAll
        ( ((bufferedContent >>= lookupObject "text/plain" >>= lookupObject "schema") `shouldBe` Just mempty)
            :| [ (bufferedContent >>= lookupObject "text/plain" >>= lookupText "example") `shouldBe` Just "new item",
                 (urlEncodedContent >>= lookupObject "application/x-www-form-urlencoded" >>= lookupObject "schema") `shouldBe` Just mempty,
                 (urlEncodedContent >>= lookupObject "application/x-www-form-urlencoded" >>= lookupText "example") `shouldBe` Just "new item",
                 (multipartContent >>= lookupObject "multipart/form-data" >>= lookupObject "schema") `shouldBe` Just mempty,
                 (multipartContent >>= lookupObject "multipart/form-data" >>= lookupText "example") `shouldBe` Just "new item"
               ]
        )

    it "rejects a request schema when the runtime body declares no media type" $ do
      let extension = withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiPost extension]) anonymousEndpointMetadata noRequiredScopes])
        (OpenApiRequestSchemaWithoutDeclaredMediaType "/api/catalog/items" Api.ApiPost)

    it "rejects a request schema for a streaming body without a media type" $ do
      let extension = withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [streamingEndpoint "/events" extension]) anonymousEndpointMetadata noRequiredScopes])
        (OpenApiRequestSchemaWithoutDeclaredMediaType "/api/catalog/events" Api.ApiPost)

    it "rejects a request example when the runtime body declares no media type" $ do
      let extension = withOpenApiRequestExample (String "new item") emptyOpenApiExtension
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiPost extension]) anonymousEndpointMetadata noRequiredScopes])
        (OpenApiRequestExampleWithoutDeclaredMediaType "/api/catalog/items" Api.ApiPost)

    it "documents every supported standard response status and keeps an unknown status neutral" $ do
      let expectations =
            [ (200, "OK"),
              (201, "Created"),
              (202, "Accepted"),
              (204, "No Content"),
              (400, "Bad Request"),
              (401, "Unauthorized"),
              (403, "Forbidden"),
              (404, "Not Found"),
              (409, "Conflict"),
              (422, "Unprocessable Content"),
              (429, "Too Many Requests"),
              (500, "Internal Server Error"),
              (418, "Response")
            ]
      descriptions <- traverse documentedResponseDescription expectations
      descriptions `shouldBe` map (Just . snd) expectations

    it "rejects an API declaration path that cannot be an OpenAPI path" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
        (InvalidOpenApiEndpointPath "items")

    it "requires a non-empty document title and version" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      let mounted = [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes]
      expectAll
        ( expectDocumentFailure (buildOpenApiDocument (OpenApiDocumentDetails "" "1.0") Map.empty False mounted) EmptyOpenApiDocumentTitle
            :| [ expectDocumentFailure (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "") Map.empty False mounted) EmptyOpenApiDocumentVersion,
                 map renderOpenApiDocumentFailure [EmptyOpenApiDocumentTitle, EmptyOpenApiDocumentVersion, InvalidOpenApiEndpointPath "/items", DuplicateOpenApiPathMethod "/items" Api.ApiGet, DuplicateOpenApiOperationId "get-items", OpenApiRequestSchemaWithoutDeclaredMediaType "/items" Api.ApiPost, OpenApiRequestExampleWithoutDeclaredMediaType "/items" Api.ApiPost, UndefinedOpenApiSecurityProfile "catalog-web", UnresolvedOpenApiSecurityProfile "/items" Api.ApiPost] `shouldBe` ["OpenAPI document title must not be empty.", "OpenAPI document version must not be empty.", "OpenAPI endpoint path is invalid: /items", "OpenAPI path and method are duplicated: get /items", "OpenAPI operation identifier is duplicated: get-items", "OpenAPI request schema has no declared request media type: post /items", "OpenAPI request example has no declared request media type: post /items", "OpenAPI security profile is not defined in the supplied scheme map: catalog-web", "OpenAPI operation has no resolvable authentication profile: post /items"]
               ]
        )

    it "renders every supported method once on a shared mounted path, including the mount root" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              Map.empty
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/" Api.ApiGet extension, visibleEndpoint "/events" Api.ApiGet extension, visibleEndpoint "/events" Api.ApiPost extension, visibleEndpoint "/events" Api.ApiPut extension, visibleEndpoint "/events" Api.ApiPatch extension, visibleEndpoint "/events" Api.ApiDelete extension]) anonymousEndpointMetadata noRequiredScopes]
          )
      encoded <- decodeDocument document
      let paths = lookupObject "paths" encoded
          events = paths >>= lookupObject "/api/catalog/events"
      expectAll
        ( (((paths >>= lookupObject "/api/catalog") >>= lookupObject "get") `shouldSatisfy` isJust)
            :| [ (events >>= lookupObject "get") `shouldSatisfy` isJust,
                 (events >>= lookupObject "post") `shouldSatisfy` isJust,
                 (events >>= lookupObject "put") `shouldSatisfy` isJust,
                 (events >>= lookupObject "patch") `shouldSatisfy` isJust,
                 (events >>= lookupObject "delete") `shouldSatisfy` isJust
               ]
        )

    it "rejects path separators and URI syntax that cannot describe an OpenAPI path" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      let documentFor path = buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint path Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes]
      expectAll
        ( expectDocumentFailure (documentFor "/items//recent") (InvalidOpenApiEndpointPath "/items//recent")
            :| [ expectDocumentFailure (documentFor "/items?preview") (InvalidOpenApiEndpointPath "/items?preview"),
                 expectDocumentFailure (documentFor "/items#fragment") (InvalidOpenApiEndpointPath "/items#fragment"),
                 expectDocumentFailure (documentFor "/items\\unsafe") (InvalidOpenApiEndpointPath "/items\\unsafe")
               ]
        )

    it "applies public model transformations before encoding and never adds extensions to removed operations" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False =<< traverse (uncurry mkOpenApiSpecificationExtension) [("x-harch-preview", String "enabled")])
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
      transformed <- decodeDocument (mapOpenApiDocumentModel (const mempty) document)
      unmodified <- decodeDocument (mapOpenApiDocumentModel id document)
      expectAll
        ( (lookupObject "paths" transformed `shouldBe` Just mempty)
            :| [ lookupObject "paths" unmodified `shouldSatisfy` isJust
               ]
        )

    it "preserves unsupported raw JSON shapes while applying validated extensions to OpenAPI objects" $ do
      extension <- requireRight (mkOpenApiSpecificationExtension "x-harch-preview" (String "enabled"))
      let extensions = [("/items", Api.ApiGet, [extension])]
          root = KeyMap.singleton "paths" (Object (KeyMap.singleton "/items" (Object (KeyMap.singleton "get" (String "not-an-operation")))))
          nonObjectPathItem = KeyMap.singleton "paths" (Object (KeyMap.singleton "/items" (String "not-a-path-item")))
          missingPaths = KeyMap.empty
          expectedRoot values = Object (KeyMap.insert "openapi" (String "3.0.3") values)
      expectAll
        ( (applyOpenApiOperationExtensions extensions (String "not-a-document") `shouldBe` String "not-a-document")
            :| [ applyOpenApiOperationExtensions extensions (Object missingPaths) `shouldBe` expectedRoot missingPaths,
                 applyOpenApiOperationExtensions extensions (Object nonObjectPathItem) `shouldBe` expectedRoot nonObjectPathItem,
                 applyOpenApiOperationExtensions extensions (Object root) `shouldBe` expectedRoot root
               ]
        )

    it "leaves a missing path, non-object path item, or non-object operation unchanged when forcing anonymous security" $ do
      let anonymousOperations = [("/items", Api.ApiGet)]
          root = KeyMap.singleton "paths" (Object (KeyMap.singleton "/items" (Object (KeyMap.singleton "get" (String "not-an-operation")))))
          nonObjectPathItem = KeyMap.singleton "paths" (Object (KeyMap.singleton "/items" (String "not-a-path-item")))
          missingPaths = KeyMap.empty
      expectAll
        ( (applyOpenApiAnonymousSecurity anonymousOperations (String "not-a-document") `shouldBe` String "not-a-document")
            :| [ applyOpenApiAnonymousSecurity anonymousOperations (Object missingPaths) `shouldBe` Object missingPaths,
                 applyOpenApiAnonymousSecurity anonymousOperations (Object nonObjectPathItem) `shouldBe` Object nonObjectPathItem,
                 applyOpenApiAnonymousSecurity anonymousOperations (Object root) `shouldBe` Object root
               ]
        )

    describe "security-scheme derivation" $ do
      it "forces an explicit empty security array for an anonymous operation" $ do
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
        encoded <- decodeDocument document
        (lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= KeyMap.lookup (Key.fromText "security")) `shouldBe` Just (Array mempty)

      it "documents a cookie-session profile's scheme and per-operation requirement" $ do
        scheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.singleton webProfile scheme)
                False
                [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) webAuthenticatedEndpointMetadata noRequiredScopes]
            )
        encoded <- decodeDocument document
        let operation = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get"
        expectAll
          ( ((operation >>= KeyMap.lookup (Key.fromText "security")) `shouldBe` Just (Array (pure (Object (KeyMap.singleton (Key.fromText "catalog-web") (Array mempty))))))
              :| [ (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "type") `shouldBe` Just "apiKey",
                   (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "in") `shouldBe` Just "cookie",
                   (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "name") `shouldBe` Just "__Host-harch-session"
                 ]
          )

      it "documents an HTTP bearer profile's scheme and per-operation requirement" $ do
        let scheme = mkOpenApiHttpBearerSecurityScheme (Just "JWT")
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.singleton webProfile scheme)
                False
                [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) webAuthenticatedEndpointMetadata noRequiredScopes]
            )
        encoded <- decodeDocument document
        let operation = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get"
        expectAll
          ( ((operation >>= KeyMap.lookup (Key.fromText "security")) `shouldBe` Just (Array (pure (Object (KeyMap.singleton (Key.fromText "catalog-web") (Array mempty))))))
              :| [ (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "type") `shouldBe` Just "http",
                   (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "scheme") `shouldBe` Just "bearer",
                   (lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-web" >>= lookupText "bearerFormat") `shouldBe` Just "JWT"
                 ]
          )

      it "documents an OAuth2 client-credentials profile's scheme and required scopes" $ do
        scheme <- requireRight (mkOpenApiOAuth2ClientCredentialsSecurityScheme "https://api.example.test/oauth/token" [("catalog:read", "Read the catalog")])
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.singleton apiProfile scheme)
                False
                [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) (apiAuthorizedEndpointMetadata ["catalog:read"]) requiredScopeNames]
            )
        encoded <- decodeDocument document
        let operation = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get"
            scheme' = lookupObject "components" encoded >>= lookupObject "securitySchemes" >>= lookupObject "catalog-api"
        expectAll
          ( ((operation >>= KeyMap.lookup (Key.fromText "security")) `shouldBe` Just (Array (pure (Object (KeyMap.singleton (Key.fromText "catalog-api") (Array (pure (String "catalog:read"))))))))
              :| [ (scheme' >>= lookupText "type") `shouldBe` Just "oauth2",
                   (scheme' >>= lookupObject "flows" >>= lookupObject "clientCredentials" >>= lookupText "tokenUrl") `shouldBe` Just "https://api.example.test/oauth/token",
                   (scheme' >>= lookupObject "flows" >>= lookupObject "clientCredentials" >>= lookupObject "scopes" >>= lookupText "catalog:read") `shouldBe` Just "Read the catalog"
                 ]
          )

      it "only requires the scopes an operation names, not the scheme's full declared set" $ do
        scheme <- requireRight (mkOpenApiOAuth2ClientCredentialsSecurityScheme "https://api.example.test/oauth/token" [("catalog:read", "Read the catalog"), ("catalog:write", "Write the catalog")])
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.singleton apiProfile scheme)
                False
                [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) (apiAuthorizedEndpointMetadata ["catalog:read"]) requiredScopeNames]
            )
        encoded <- decodeDocument document
        (lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= KeyMap.lookup (Key.fromText "security"))
          `shouldBe` Just (Array (pure (Object (KeyMap.singleton (Key.fromText "catalog-api") (Array (pure (String "catalog:read")))))))

      it "shares one securitySchemes component across mounted families using the same profile" $ do
        scheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
        catalogExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        ordersExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.singleton webProfile scheme)
                False
                [ openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet catalogExtension]) webAuthenticatedEndpointMetadata noRequiredScopes,
                  openApiMountedFamily ordersMount (family [visibleEndpoint "/summary" Api.ApiGet ordersExtension]) webAuthenticatedEndpointMetadata noRequiredScopes
                ]
            )
        encoded <- decodeDocument document
        (lookupObject "components" encoded >>= lookupObject "securitySchemes") `shouldSatisfy` (\schemes -> fmap (length . KeyMap.toList) schemes == Just 1)

      it "gives two mounted families using two distinct profiles their own distinct scheme" $ do
        webScheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
        apiScheme <- requireRight (mkOpenApiOAuth2ClientCredentialsSecurityScheme "https://api.example.test/oauth/token" [("catalog:read", "Read the catalog")])
        webExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        apiExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        document <-
          requireRight
            ( buildOpenApiDocument
                (OpenApiDocumentDetails "Catalog API" "1.0")
                (Map.fromList [(webProfile, webScheme), (apiProfile, apiScheme)])
                False
                [ openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet webExtension]) webAuthenticatedEndpointMetadata noRequiredScopes,
                  openApiMountedFamily ordersMount (family [visibleEndpoint "/summary" Api.ApiGet apiExtension]) (apiAuthorizedEndpointMetadata ["catalog:read"]) requiredScopeNames
                ]
            )
        encoded <- decodeDocument document
        let schemes = lookupObject "components" encoded >>= lookupObject "securitySchemes"
        expectAll
          ( ((schemes >>= lookupObject "catalog-web" >>= lookupText "type") `shouldBe` Just "apiKey")
              :| [(schemes >>= lookupObject "catalog-api" >>= lookupText "type") `shouldBe` Just "oauth2"]
          )

      it "rejects an operation naming a profile absent from the supplied scheme map" $ do
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        expectDocumentFailure
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) webAuthenticatedEndpointMetadata noRequiredScopes])
          (UndefinedOpenApiSecurityProfile "catalog-web")

      it "rejects a non-anonymous operation whose real metadata names no profile to resolve" $ do
        scheme <- requireRight (mkOpenApiCookieSessionSecurityScheme "__Host-harch-session")
        extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
        expectDocumentFailure
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              (Map.singleton webProfile scheme)
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) unresolvedProfileEndpointMetadata noRequiredScopes]
          )
          (UnresolvedOpenApiSecurityProfile "/api/catalog/items" Api.ApiGet)

catalogMount :: RouteMount () ()
catalogMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "root.catalog-api",
      routeMountPrefix = requiredPathSegment "api" :| [requiredPathSegment "catalog"],
      embedChildRoute = id,
      projectChildRoute = Just
    }

ordersMount :: RouteMount () ()
ordersMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "root.orders-api",
      routeMountPrefix = requiredPathSegment "api" :| [requiredPathSegment "orders"],
      embedChildRoute = id,
      projectChildRoute = Just
    }

family :: [Api.ApiRouteEndpoint Bool OpenApiExtension fields body domainFailure response] -> Api.ApiEndpointFamily Bool OpenApiExtension
family endpoints = Api.requireApiEndpointFamily (map Api.SomeApiRouteEndpoint endpoints)

anonymousEndpointMetadata :: Api.ApiPath -> EndpointMetadata ()
anonymousEndpointMetadata _ =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.item")
    (requiredRouteTemplateOrDie "/items")
    ApiEndpoint
    AllowUnauthenticated

noRequiredScopes :: () -> [Text]
noRequiredScopes _ = []

webProfile :: AuthenticationProfileName
webProfile = requiredAuthenticationProfileNameOrDie "catalog-web"

apiProfile :: AuthenticationProfileName
apiProfile = requiredAuthenticationProfileNameOrDie "catalog-api"

webAuthenticatedEndpointMetadata :: Api.ApiPath -> EndpointMetadata ()
webAuthenticatedEndpointMetadata _ =
  withAuthenticationProfile
    webProfile
    ( mkEndpointMetadata
        (requiredEndpointNameOrDie "catalog.item")
        (requiredRouteTemplateOrDie "/items")
        ApiEndpoint
        RequireAuthenticated
    )

unresolvedProfileEndpointMetadata :: Api.ApiPath -> EndpointMetadata ()
unresolvedProfileEndpointMetadata _ =
  mkEndpointMetadata
    (requiredEndpointNameOrDie "catalog.item")
    (requiredRouteTemplateOrDie "/items")
    ApiEndpoint
    RequireAuthenticated

apiAuthorizedEndpointMetadata :: [Text] -> Api.ApiPath -> EndpointMetadata [Text]
apiAuthorizedEndpointMetadata scopes _ =
  withAuthenticationProfile
    apiProfile
    ( mkEndpointMetadata
        (requiredEndpointNameOrDie "catalog.item")
        (requiredRouteTemplateOrDie "/items")
        ApiEndpoint
        (RequireAuthorized scopes)
    )

requiredScopeNames :: [Text] -> [Text]
requiredScopeNames = id

visibleEndpoint :: Text -> Api.ApiMethod -> OpenApiExtension () () Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () () () Text
visibleEndpoint path method =
  visibleEndpointWithEncoders path method (Api.textResponseEncoder :| [])

visibleEndpointWithEncoders :: Text -> Api.ApiMethod -> NonEmpty (Api.ApiResponseEncoder Text) -> OpenApiExtension () () Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () () () Text
visibleEndpointWithEncoders path method encoders extension =
  Api.apiRouteEndpointNeverFailing
    (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract method Api.noRequestFields Api.ApiNoRequestBody encoders Api.ApiUseGenericFieldFailure extension))
    (const (pure (Api.apiResponse "visible")))

bufferedEndpoint :: Text -> OpenApiExtension () Text Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () Text () Text
bufferedEndpoint path extension =
  Api.apiRouteEndpointNeverFailing
    (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract Api.ApiPost Api.noRequestFields (Api.ApiBufferedRequestBody Api.RejectMissingContentType (Api.requireApiRequestBodyByteLimit 64) [Api.textBodyDecoder]) (Api.textResponseEncoder :| []) Api.ApiUseGenericFieldFailure extension))
    (const (pure (Api.apiResponse "visible")))

urlEncodedEndpoint :: Text -> OpenApiExtension () Api.ApiForm Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () Api.ApiForm () Text
urlEncodedEndpoint path extension =
  Api.apiRouteEndpointNeverFailing
    (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract Api.ApiPost Api.noRequestFields (Api.ApiUrlEncodedFormRequestBody Api.RejectMissingContentType (Api.requireApiRequestBodyByteLimit 64) 8) (Api.textResponseEncoder :| []) Api.ApiUseGenericFieldFailure extension))
    (const (pure (Api.apiResponse "visible")))

multipartEndpoint :: Text -> OpenApiExtension () (Api.ApiMultipartRequest InMemoryUpload) Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () (Api.ApiMultipartRequest InMemoryUpload) () Text
multipartEndpoint path extension =
  Api.apiRouteEndpointNeverFailing
    (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract Api.ApiPost Api.noRequestFields (Api.ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits) (Api.textResponseEncoder :| []) Api.ApiUseGenericFieldFailure extension))
    (const (pure (Api.apiResponse "visible")))

streamingEndpoint :: Text -> OpenApiExtension () Api.ApiStreamingRequest Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () Api.ApiStreamingRequest () Text
streamingEndpoint path extension =
  Api.apiRouteEndpointNeverFailing
    (Api.ApiRouteEndpointDeclaration (Api.at path) (Api.ApiEndpointContract Api.ApiPost Api.noRequestFields (Api.ApiStreamingRequestBody (Api.requireApiRequestBodyByteLimit 64)) (Api.textResponseEncoder :| []) Api.ApiUseGenericFieldFailure extension))
    (const (pure (Api.apiResponse "visible")))

requestContentFor :: OpenApiDocument -> Text -> IO (Maybe Object)
requestContentFor document path = do
  encoded <- decodeDocument document
  pure (lookupObject "paths" encoded >>= lookupObject path >>= lookupObject "post" >>= lookupObject "requestBody" >>= lookupObject "content")

hiddenEndpoint :: Text -> OpenApiExtension () () Text -> Api.ApiRouteEndpoint Bool OpenApiExtension () () () Text
hiddenEndpoint path extension =
  Api.withApiEndpointAvailabilityFromContext
    (\enabled -> if enabled then Api.ApiAvailable else Api.ApiHidden)
    (visibleEndpoint path Api.ApiGet extension)

documentedResponseDescription :: (Int, Text) -> IO (Maybe Text)
documentedResponseDescription (status, _) = do
  extension <- requireRight (withOpenApiResponseStatus status emptyOpenApiExtension)
  document <-
    requireRight
      (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") Map.empty False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension]) anonymousEndpointMetadata noRequiredScopes])
  encoded <- decodeDocument document
  pure (lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject (Text.pack (show status)) >>= lookupText "description")

decodeDocument :: OpenApiDocument -> IO (KeyMap.KeyMap Value)
decodeDocument document =
  case eitherDecode (encodeOpenApiDocument document) of
    Right (Object object) -> pure object
    Right value -> expectationFailure ("expected JSON object, got " <> show value) >> fail "expected JSON object"
    Left decodeError -> expectationFailure decodeError >> fail "could not decode encoded OpenAPI document"

lookupObject :: Text -> KeyMap.KeyMap Value -> Maybe (KeyMap.KeyMap Value)
lookupObject key object =
  case KeyMap.lookup (fromStringKey key) object of
    Just (Object nestedObject) -> Just nestedObject
    _ -> Nothing

lookupText :: Text -> KeyMap.KeyMap Value -> Maybe Text
lookupText key object =
  case KeyMap.lookup (fromStringKey key) object of
    Just (String value) -> Just value
    _ -> Nothing

fromStringKey :: Text -> Key.Key
fromStringKey = Key.fromText

requireRight :: Either errorValue value -> IO value
requireRight result =
  case result of
    Left _ -> expectationFailure "expected Right" >> fail "expected Right"
    Right value -> pure value

expectDocumentFailure :: Either OpenApiDocumentFailure value -> OpenApiDocumentFailure -> Expectation
expectDocumentFailure result expectedFailure =
  case result of
    Left actualFailure -> renderOpenApiDocumentFailure actualFailure `shouldBe` renderOpenApiDocumentFailure expectedFailure
    Right _ -> expectationFailure "expected document construction to fail"
