{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.ByteString qualified as ByteString
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.RequestId
import Network.Wai qualified as Wai

spec = describe "HarchWeb.RequestId" $ do
  it "accepts only canonical lower-case UUIDv4 values" $
    expectAll
      ( (requestIdText <$> mkRequestId canonicalRequestId `shouldBe` Just canonicalRequestId)
          :| [ mkRequestId "550E8400-E29B-41D4-A716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-51d4-a716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-c716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-a716-44665544000" `shouldBe` Nothing,
               mkRequestId "550e8400xe29b-41d4-a716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-a716-44665544000g" `shouldBe` Nothing,
               mkRequestId "٥50e8400-e29b-41d4-a716-446655440000" `shouldBe` Nothing
             ]
      )

  it "generates fresh canonical UUIDv4 values from CSPRNG entropy" $ do
    first <- newRequestId
    second <- newRequestId
    let canonical = parseRequestId canonicalRequestId
    expectAll
      ( (mkRequestId (requestIdText first) `shouldBe` Just first)
          :| [ mkRequestId (requestIdText second) `shouldBe` Just second,
               first `shouldNotBe` second,
               show canonical `shouldBe` "RequestId \"550e8400-e29b-41d4-a716-446655440000\"",
               show [canonical] `shouldBe` "[RequestId \"550e8400-e29b-41d4-a716-446655440000\"]"
             ]
      )

  it "inherits one strict ID only for a service with propagation capability" $ do
    (upstreamRequestId, _) <- resolveRequestIdIngress freshRequestIdIngress Wai.defaultRequest
    let trustedIngress =
          authenticatedServiceRequestIdIngress
            ( \request ->
                pure $
                  case lookup "X-Test-Service" (Wai.requestHeaders request) of
                    Just "service" -> Just False
                    Just "propagating-service" -> Just True
                    _ -> Nothing
            )
            ( \hasPropagationCapability ->
                if hasPropagationCapability
                  then Just requestIdPropagationCapability
                  else Nothing
            )
        trustedRequest =
          Wai.defaultRequest
            { Wai.requestHeaders =
                [ requestIdHeader upstreamRequestId,
                  ("X-Test-Service", "propagating-service")
                ]
            }
        authenticatedWithoutCapability =
          trustedRequest {Wai.requestHeaders = [requestIdHeader upstreamRequestId, ("X-Test-Service", "service")]}
        malformedTrustedRequest =
          trustedRequest {Wai.requestHeaders = [("X-Request-ID", "not-a-request-id"), ("X-Test-Service", "propagating-service")]}
        repeatedTrustedRequest =
          trustedRequest {Wai.requestHeaders = [requestIdHeader upstreamRequestId, requestIdHeader upstreamRequestId, ("X-Test-Service", "propagating-service")]}
        oversizedTrustedRequest =
          trustedRequest {Wai.requestHeaders = [("X-Request-ID", snd (requestIdHeader upstreamRequestId) <> "x"), ("X-Test-Service", "propagating-service")]}
        nonUtf8TrustedRequest =
          trustedRequest {Wai.requestHeaders = [("X-Request-ID", ByteString.replicate 36 0xff), ("X-Test-Service", "propagating-service")]}
    (inheritedRequestId, inheritedResult) <- resolveRequestIdIngress trustedIngress trustedRequest
    (unpropagatedRequestId, unpropagatedResult) <- resolveRequestIdIngress trustedIngress authenticatedWithoutCapability
    (malformedRequestId, malformedResult) <- resolveRequestIdIngress trustedIngress malformedTrustedRequest
    (_, repeatedResult) <- resolveRequestIdIngress trustedIngress repeatedTrustedRequest
    (_, oversizedResult) <- resolveRequestIdIngress trustedIngress oversizedTrustedRequest
    (_, nonUtf8Result) <- resolveRequestIdIngress trustedIngress nonUtf8TrustedRequest
    expectAll
      ( (inheritedRequestId `shouldBe` upstreamRequestId)
          :| [ inheritedResult `shouldBe` InheritedRequestId,
               unpropagatedRequestId `shouldNotBe` upstreamRequestId,
               unpropagatedResult `shouldBe` FreshRequestId,
               malformedRequestId `shouldNotBe` upstreamRequestId,
               malformedResult `shouldBe` RejectedInheritedRequestId,
               repeatedResult `shouldBe` RejectedInheritedRequestId,
               oversizedResult `shouldBe` RejectedInheritedRequestId,
               nonUtf8Result `shouldBe` RejectedInheritedRequestId,
               FreshRequestId == FreshRequestId `shouldBe` True,
               FreshRequestId /= InheritedRequestId `shouldBe` True,
               show FreshRequestId `shouldBe` "FreshRequestId",
               show InheritedRequestId `shouldBe` "InheritedRequestId",
               show RejectedInheritedRequestId `shouldBe` "RejectedInheritedRequestId",
               show [FreshRequestId, InheritedRequestId, RejectedInheritedRequestId]
                 `shouldBe` "[FreshRequestId,InheritedRequestId,RejectedInheritedRequestId]"
             ]
      )

canonicalRequestId :: Text
canonicalRequestId = "550e8400-e29b-41d4-a716-446655440000"

parseRequestId :: Text -> RequestId
parseRequestId value =
  case mkRequestId value of
    Nothing -> error "canonical request id did not parse"
    Just parsedRequestId -> parsedRequestId
