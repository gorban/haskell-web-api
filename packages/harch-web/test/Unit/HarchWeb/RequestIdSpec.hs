{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.RequestId

spec = describe "HarchWeb.RequestId" $ do
  it "accepts only canonical lower-case UUIDv4 values" $
    expectAll
      ( (requestIdText <$> mkRequestId canonicalRequestId `shouldBe` Just canonicalRequestId)
          :| [ mkRequestId "550E8400-E29B-41D4-A716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-51d4-a716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-c716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-a716-44665544000" `shouldBe` Nothing,
               mkRequestId "550e8400xe29b-41d4-a716-446655440000" `shouldBe` Nothing,
               mkRequestId "550e8400-e29b-41d4-a716-44665544000g" `shouldBe` Nothing
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
               show canonical `shouldBe` "RequestId \"550e8400-e29b-41d4-a716-446655440000\""
             ]
      )

canonicalRequestId :: Text
canonicalRequestId = "550e8400-e29b-41d4-a716-446655440000"

parseRequestId :: Text -> RequestId
parseRequestId value =
  case mkRequestId value of
    Nothing -> error "canonical request id did not parse"
    Just parsedRequestId -> parsedRequestId
