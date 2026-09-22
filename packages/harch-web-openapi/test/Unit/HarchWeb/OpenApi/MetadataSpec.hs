{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.Aeson (Value (String))
import Data.List.NonEmpty (NonEmpty ((:|)))
import HarchWeb.Api qualified as Api
import HarchWeb.OpenApi.Metadata

spec =
  describe "OpenApiExtension" $ do
    it "provides empty metadata for endpoints that have no documentation details" $ do
      let extension = emptyOpenApiExtension
      expectAll
        ( (openApiExtensionSummary extension `shouldBe` Nothing)
            :| [ openApiExtensionDescription extension `shouldBe` Nothing,
                 openApiExtensionTags extension `shouldBe` [],
                 openApiExtensionDeprecated extension `shouldBe` False,
                 openApiExtensionSpecificationExtensions extension `shouldBe` []
               ]
        )

    it "keeps optional documentation metadata separate from a runnable endpoint contract" $ do
      extension <-
        requireRight
          ( mkOpenApiExtension
              (Just "List items")
              Nothing
              ["catalog"]
              False
              []
          )
      expectAll
        ( (openApiExtensionSummary extension `shouldBe` Just "List items")
            :| [ openApiExtensionDescription extension `shouldBe` Nothing,
                 openApiExtensionTags extension `shouldBe` ["catalog"],
                 openApiExtensionDeprecated extension `shouldBe` False,
                 openApiExtensionSpecificationExtensions extension `shouldBe` []
               ]
        )

    it "adds only a validated authored operation identifier" $ do
      extension <- requireRight (withOpenApiOperationId "catalog-list" emptyOpenApiExtension)
      expectAll
        ( (openApiExtensionOperationId extension `shouldBe` Just "catalog-list")
            :| [ withOpenApiOperationId " \t" emptyOpenApiExtension `shouldBe` Left (InvalidOpenApiOperationId " \t"),
                 withOpenApiOperationId "" emptyOpenApiExtension `shouldBe` Left (InvalidOpenApiOperationId "")
               ]
        )

    it "adds only a valid HTTP response status" $ do
      extension <- requireRight (withOpenApiResponseStatus 201 emptyOpenApiExtension)
      expectAll
        ( (openApiExtensionResponseStatus extension `shouldBe` Just 201)
            :| [ withOpenApiResponseStatus 99 emptyOpenApiExtension `shouldBe` Left (InvalidOpenApiResponseStatus 99),
                 withOpenApiResponseStatus 600 emptyOpenApiExtension `shouldBe` Left (InvalidOpenApiResponseStatus 600)
               ]
        )

    it "accepts only portable x-* specification extension names" $ do
      extension <- requireRight (mkOpenApiSpecificationExtension "x-harch-preview.v1" (String "enabled"))
      portableCharacterExtension <- requireRight (mkOpenApiSpecificationExtension "x-harch_A-2" (String "enabled"))
      expectAll
        ( (openApiSpecificationExtensionName extension `shouldBe` "x-harch-preview.v1")
            :| [ openApiSpecificationExtensionValue extension `shouldBe` String "enabled",
                 openApiSpecificationExtensionName portableCharacterExtension `shouldBe` "x-harch_A-2"
               ]
        )
      mkOpenApiSpecificationExtension "harch-preview" (String "enabled")
        `shouldBe` Left (InvalidOpenApiSpecificationExtensionName "harch-preview")
      mkOpenApiSpecificationExtension "x-" (String "enabled")
        `shouldBe` Left (InvalidOpenApiSpecificationExtensionName "x-")
      mkOpenApiSpecificationExtension "x-harch-caf\233" (String "enabled")
        `shouldBe` Left (InvalidOpenApiSpecificationExtensionName "x-harch-caf\233")
      mkOpenApiSpecificationExtension "x-harch!preview" (String "enabled")
        `shouldBe` Left (InvalidOpenApiSpecificationExtensionName "x-harch!preview")

    it "rejects duplicate custom extension names before interpretation" $ do
      extension <- requireRight (mkOpenApiSpecificationExtension "x-harch-preview" (String "enabled"))
      secondExtension <- requireRight (mkOpenApiSpecificationExtension "x-harch-build" (String "stable"))
      uniqueExtension <- requireRight (mkOpenApiExtension Nothing Nothing [] False [extension, secondExtension])
      openApiExtensionSpecificationExtensions uniqueExtension `shouldBe` [extension, secondExtension]
      mkOpenApiExtension Nothing Nothing [] False [extension, extension]
        `shouldBe` Left (DuplicateOpenApiSpecificationExtension "x-harch-preview")
      mkOpenApiExtension Nothing Nothing [] False [extension, secondExtension, secondExtension]
        `shouldBe` Left (DuplicateOpenApiSpecificationExtension "x-harch-build")

    it "renders public metadata and construction errors for diagnostics" $ do
      extension <- requireRight (mkOpenApiSpecificationExtension "x-harch-preview" (String "enabled"))
      differentExtension <- requireRight (mkOpenApiSpecificationExtension "x-harch-build" (String "stable"))
      metadata <- requireRight (mkOpenApiExtension Nothing Nothing [] False [extension])
      expectAll
        ( (show metadata `shouldSatisfy` (not . null))
            :| [ showList [metadata] "" `shouldSatisfy` (not . null),
                 metadata /= emptyOpenApiExtension `shouldBe` True,
                 show extension `shouldSatisfy` (not . null),
                 showList [extension] "" `shouldSatisfy` (not . null),
                 extension /= differentExtension `shouldBe` True,
                 show (InvalidOpenApiSpecificationExtensionName "harch-preview") `shouldSatisfy` (not . null),
                 showList [InvalidOpenApiSpecificationExtensionName "harch-preview"] "" `shouldSatisfy` (not . null),
                 InvalidOpenApiSpecificationExtensionName "harch-preview" /= DuplicateOpenApiSpecificationExtension "x-harch-preview" `shouldBe` True,
                 show (DuplicateOpenApiSpecificationExtension "x-harch-preview") `shouldSatisfy` (not . null)
               ]
        )

    it "replaces only an endpoint contract's typed extension" $ do
      extension <- requireRight (mkOpenApiExtension (Just "List items") Nothing [] False [])
      let contract =
            Api.ApiEndpointContract
              Api.ApiGet
              Api.noRequestFields
              Api.ApiNoRequestBody
              (Api.textResponseEncoder :| [])
              Api.ApiUseGenericFieldFailure
              Api.NoApiExtension
          documentedContract = withOpenApiExtension extension contract
      expectAll
        ( (Api.apiEndpointContractMethod documentedContract `shouldBe` Api.ApiGet)
            :| [ expectGenericFieldFailure (Api.apiEndpointContractFieldFailurePolicy documentedContract),
                 Api.apiEndpointContractExtension documentedContract `shouldBe` extension,
                 expectNoRequestFields (Api.runRequestCodec (Api.apiEndpointContractFields documentedContract) (Api.ApiRequestData [] [] [] []))
               ]
        )

requireRight :: (Show errorValue) => Either errorValue value -> IO value
requireRight result =
  case result of
    Left errorValue -> expectationFailure (show errorValue) >> fail "expected Right"
    Right value -> pure value

expectGenericFieldFailure :: Api.ApiFieldFailurePolicy response -> Expectation
expectGenericFieldFailure fieldFailurePolicy =
  case fieldFailurePolicy of
    Api.ApiUseGenericFieldFailure -> pure ()
    Api.ApiRenderFieldFailures _ -> expectationFailure "expected the original generic field-failure policy"

expectNoRequestFields :: Api.ApiRequestDecodeResult () -> Expectation
expectNoRequestFields result =
  case result of
    Api.ApiRequestDecoded () -> pure ()
    Api.ApiRequestRejected _ -> expectationFailure "expected the original no-request-fields codec"
    Api.ApiRequestCodecInvalid -> expectationFailure "expected the original no-request-fields codec"
