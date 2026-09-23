{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Aeson (Object, Value (..), eitherDecode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (isJust)
import Data.OpenApi (Schema)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Api qualified as Api
import HarchWeb.Api.Multipart (InMemoryUpload, defaultMultipartLimits, inMemoryMultipartStorage)
import HarchWeb.ApplicationModule (RouteMount (..))
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
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension, hiddenEndpoint "/unlisted" extension])]
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
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount endpointFamily, openApiMountedFamily catalogMount endpointFamily])
        (DuplicateOpenApiPathMethod "/api/catalog/items" Api.ApiGet)

    it "rejects generated operation identifiers that would collide" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      expectDocumentFailure
        ( buildOpenApiDocument
            (OpenApiDocumentDetails "Catalog API" "1.0")
            False
            [openApiMountedFamily catalogMount (family [visibleEndpoint "/one-two" Api.ApiGet extension, visibleEndpoint "/one/two" Api.ApiGet extension])]
        )
        (DuplicateOpenApiOperationId "get-api-catalog-one-two")

    it "uses authored operation identifiers and rejects their collisions" $ do
      baseExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      extension <- requireRight (withOpenApiOperationId "catalog-lookup" baseExtension)
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension])])
      encoded <- decodeDocument document
      (lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupText "operationId") `shouldBe` Just "catalog-lookup"
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension, visibleEndpoint "/summary" Api.ApiGet extension])])
        (DuplicateOpenApiOperationId "catalog-lookup")

    it "uses an authored response status with its safe standard description" $ do
      extension <- requireRight (withOpenApiResponseStatus 201 emptyOpenApiExtension)
      document <-
        requireRight
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiPost extension])])
      encoded <- decodeDocument document
      let responses = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "post" >>= lookupObject "responses"
      expectAll
        ( ((responses >>= lookupObject "201" >>= lookupText "description") `shouldBe` Just "Created")
            :| [ (responses >>= lookupObject "default") `shouldBe` Nothing
               ]
        )

    it "documents every declared response representation without inventing a body shape" $ do
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [visibleEndpointWithEncoders "/items" Api.ApiGet (Api.jsonResponseEncoder :| [Api.textResponseEncoder]) emptyOpenApiExtension])]
          )
      encoded <- decodeDocument document
      let content = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content"
      expectAll
        ( ((content >>= lookupObject "application/json") `shouldBe` Just mempty)
            :| [ (content >>= lookupObject "text/plain") `shouldBe` Just mempty
               ]
        )

    it "applies an explicit inline response schema to every declared representation" $ do
      let extension = withOpenApiResponseSchema (mempty :: Schema) emptyOpenApiExtension
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [visibleEndpointWithEncoders "/items" Api.ApiGet (Api.jsonResponseEncoder :| [Api.textResponseEncoder]) extension])]
          )
      encoded <- decodeDocument document
      let content = lookupObject "paths" encoded >>= lookupObject "/api/catalog/items" >>= lookupObject "get" >>= lookupObject "responses" >>= lookupObject "default" >>= lookupObject "content"
      expectAll
        ( ((content >>= lookupObject "application/json" >>= lookupObject "schema") `shouldBe` Just mempty)
            :| [ (content >>= lookupObject "text/plain" >>= lookupObject "schema") `shouldBe` Just mempty
               ]
        )

    it "documents an explicit request schema at every concrete typed request representation" $ do
      let extension = withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension
      bufferedDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [bufferedEndpoint "/items" extension])]
          )
      urlEncodedDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [urlEncodedEndpoint "/form" extension])]
          )
      multipartDocument <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [multipartEndpoint "/uploads" extension])]
          )
      bufferedContent <- requestContentFor bufferedDocument "/api/catalog/items"
      urlEncodedContent <- requestContentFor urlEncodedDocument "/api/catalog/form"
      multipartContent <- requestContentFor multipartDocument "/api/catalog/uploads"
      expectAll
        ( ((bufferedContent >>= lookupObject "text/plain" >>= lookupObject "schema") `shouldBe` Just mempty)
            :| [ (urlEncodedContent >>= lookupObject "application/x-www-form-urlencoded" >>= lookupObject "schema") `shouldBe` Just mempty,
                 (multipartContent >>= lookupObject "multipart/form-data" >>= lookupObject "schema") `shouldBe` Just mempty
               ]
        )

    it "rejects a request schema when the runtime body declares no media type" $ do
      let extension = withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiPost extension])])
        (OpenApiRequestSchemaWithoutDeclaredMediaType "/api/catalog/items" Api.ApiPost)

    it "rejects a request schema for a streaming body without a media type" $ do
      let extension = withOpenApiRequestSchema (mempty :: Schema) emptyOpenApiExtension
      expectDocumentFailure
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [streamingEndpoint "/events" extension])])
        (OpenApiRequestSchemaWithoutDeclaredMediaType "/api/catalog/events" Api.ApiPost)

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
        (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "items" Api.ApiGet extension])])
        (InvalidOpenApiEndpointPath "items")

    it "requires a non-empty document title and version" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      let mounted = [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension])]
      expectAll
        ( expectDocumentFailure (buildOpenApiDocument (OpenApiDocumentDetails "" "1.0") False mounted) EmptyOpenApiDocumentTitle
            :| [ expectDocumentFailure (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "") False mounted) EmptyOpenApiDocumentVersion,
                 map renderOpenApiDocumentFailure [EmptyOpenApiDocumentTitle, EmptyOpenApiDocumentVersion, InvalidOpenApiEndpointPath "/items", DuplicateOpenApiPathMethod "/items" Api.ApiGet, DuplicateOpenApiOperationId "get-items", OpenApiRequestSchemaWithoutDeclaredMediaType "/items" Api.ApiPost] `shouldBe` ["OpenAPI document title must not be empty.", "OpenAPI document version must not be empty.", "OpenAPI endpoint path is invalid: /items", "OpenAPI path and method are duplicated: get /items", "OpenAPI operation identifier is duplicated: get-items", "OpenAPI request schema has no declared request media type: post /items"]
               ]
        )

    it "renders every supported method once on a shared mounted path, including the mount root" $ do
      extension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [])
      document <-
        requireRight
          ( buildOpenApiDocument
              (OpenApiDocumentDetails "Catalog API" "1.0")
              False
              [openApiMountedFamily catalogMount (family [visibleEndpoint "/" Api.ApiGet extension, visibleEndpoint "/events" Api.ApiGet extension, visibleEndpoint "/events" Api.ApiPost extension, visibleEndpoint "/events" Api.ApiPut extension, visibleEndpoint "/events" Api.ApiPatch extension, visibleEndpoint "/events" Api.ApiDelete extension])]
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
      let documentFor path = buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint path Api.ApiGet extension])]
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
          (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension])])
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

catalogMount :: RouteMount () ()
catalogMount =
  RouteMount
    { routeMountName = requiredModuleNameOrDie "root.catalog-api",
      routeMountPrefix = requiredPathSegment "api" :| [requiredPathSegment "catalog"],
      embedChildRoute = id,
      projectChildRoute = Just
    }

family :: [Api.ApiRouteEndpoint Bool OpenApiExtension fields body domainFailure response] -> Api.ApiEndpointFamily Bool OpenApiExtension
family endpoints = Api.requireApiEndpointFamily (map Api.SomeApiRouteEndpoint endpoints)

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
      (buildOpenApiDocument (OpenApiDocumentDetails "Catalog API" "1.0") False [openApiMountedFamily catalogMount (family [visibleEndpoint "/items" Api.ApiGet extension])])
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
