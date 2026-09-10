{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.ClientActionFailure
import HarchWeb.RequestId (RequestId, mkRequestId)

spec = describe "HarchWeb.ClientActionFailure" $ do
  it "round-trips only the closed browser-failure vocabulary" $
    expectAll
      ( (map (parseHarchClientFailure . harchClientFailureCode) allFailures `shouldBe` map Just allFailures)
          :| [ parseHarchClientFailure "unknown-browser-failure" `shouldBe` Nothing,
               harchClientFailureCode StorageCleanupFailed `shouldBe` "storage-cleanup-failed",
               harchClientFailureCode ActionResponseProtocolFailed `shouldBe` "action-response-protocol-failed",
               harchClientFailureCode ResponseApplicationFailed `shouldBe` "response-application-failed"
             ]
      )

  it "accepts only an existing canonical UUIDv4 as a display-only reference" $ do
    let validRequestId = requiredRequestId "3a99e441-7c35-4b87-91bf-c3583c008a0f"
        reference = failureReference validRequestId
    expectAll
      ( (failureReferenceRequestId reference `shouldBe` validRequestId)
          :| [ failureReferenceText reference `shouldBe` "3a99e441-7c35-4b87-91bf-c3583c008a0f",
               parseFailureReference (failureReferenceText reference) `shouldBe` Just reference,
               parseFailureReference "3A99E441-7C35-4B87-91BF-C3583C008A0F" `shouldBe` Nothing,
               parseFailureReference "not-a-request-id" `shouldBe` Nothing
             ]
      )

  it "keeps public values inspectable" $
    expectAll
      ( (hasDerivedContract allFailures `shouldBe` True)
          :| [ hasDerivedContract [failureReference (requiredRequestId "3a99e441-7c35-4b87-91bf-c3583c008a0f")] `shouldBe` True
             ]
      )

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show value) + length (showList [value] "") | value <- values] > 0

allFailures :: [HarchClientFailure]
allFailures = [StorageCleanupFailed, ActionResponseProtocolFailed, ResponseApplicationFailed]

requiredRequestId :: Text -> RequestId
requiredRequestId value =
  case mkRequestId value of
    Nothing -> error "expected valid request identifier fixture"
    Just requestId -> requestId
