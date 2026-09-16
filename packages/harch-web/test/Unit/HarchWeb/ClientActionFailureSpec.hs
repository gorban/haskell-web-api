{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text (isInfixOf)
import HarchWeb (ClientActionFailurePresentation (..), ClientActionTerminalFailure (clientActionTerminalFailurePage))
import HarchWeb.ClientActionFailure
import HarchWeb.Document (Page (pageBody, pageBootstrapHooks, pageContext, pageRoute, pageTitle))
import HarchWeb.Markup (renderHtml)
import HarchWeb.RequestId (RequestId, mkRequestId)
import HarchWeb.Routing (RouteRequest (RouteRequest))

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

  it "renders the opaque reference in the complete default fallback page" $ do
    let reference = failureReference (requiredRequestId "3a99e441-7c35-4b87-91bf-c3583c008a0f")
        page = defaultClientActionFailurePage reference (RouteRequest ("failure" :: Text) ())
        rendered = renderHtml (pageBody page)
    expectAll
      ( (pageTitle page `shouldBe` "Request could not be completed")
          :| [ pageRoute page `shouldBe` "failure",
               pageContext page `shouldBe` (),
               rendered `shouldSatisfy` ("Request could not be completed" `Text.isInfixOf`),
               rendered `shouldSatisfy` (failureReferenceText reference `Text.isInfixOf`),
               pageBootstrapHooks page `shouldBe` [],
               harchClientFailureCode StorageCleanupFailed `Text.isInfixOf` rendered `shouldBe` False
             ]
      )

  it "renders the default server terminal failure without exposing an application value" $ do
    let requestId = requiredRequestId "3a99e441-7c35-4b87-91bf-c3583c008a0f"
        terminalPage =
          clientActionTerminalFailurePage
            defaultClientActionTerminalFailure
            ClientActionFailurePresentation
              { clientActionFailureRequestId = requestId,
                clientActionFailureRoute = RouteRequest ("failure" :: Text) ()
              }
        rendered = renderHtml (pageBody terminalPage)
    expectAll
      ( (pageTitle terminalPage `shouldBe` "Request could not be completed")
          :| [ rendered `shouldSatisfy` ("Request could not be completed" `Text.isInfixOf`),
               rendered `shouldSatisfy` ("3a99e441-7c35-4b87-91bf-c3583c008a0f" `Text.isInfixOf`),
               pageBootstrapHooks terminalPage `shouldBe` []
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
