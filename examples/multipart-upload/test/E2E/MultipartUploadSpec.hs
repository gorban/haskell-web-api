{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App (multipartUploadApplication)
import App.MultipartUpload (NativeUploadState, nativeUploadDiscardCount, newNativeUploadState)
import HarchWeb (LocalTestServer (..), withLocalTestServer)

spec =
  describe "multipart-upload real-browser behavior" $ do
    it "submits a native multipart upload as SSR with scripts enabled" $
      withBrowserAndUploadServer $ \browser server uploadState ->
        withTempFile "multipart-upload-e2e" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
          writeFile filePath "e2e file contents"
          let uploadUrl = localServerBaseUrl server <> "/native-upload"
          runBrowserSpec browser do
            visit uploadUrl
            setInputFiles (css "#native-upload-file") filePath
            submit (byRole Form `named` "Upload a file")
            assertAllObserved do
              textContent (byRole Heading `named` "Upload received") `matches` (`shouldBe` "Upload received")
              browserMetrics `matches` \metrics ->
                $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1, mutationRequestCount = 0}|])
          nativeUploadDiscardCount uploadState `shouldReturn` 1

    it "submits the same native multipart upload as SSR with scripts disabled" $
      withBrowserAndUploadServer $ \browser server uploadState ->
        withTempFile "multipart-upload-e2e-no-js" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
          writeFile filePath "e2e file contents, no scripts"
          let uploadUrl = localServerBaseUrl server <> "/native-upload"
          runBrowserSpec browser do
            visitWithoutScripts uploadUrl
            setInputFiles (css "#native-upload-file") filePath
            submit (byRole Form `named` "Upload a file")
            assertAllObserved do
              textContent (byRole Heading `named` "Upload received") `matches` (`shouldBe` "Upload received")
          nativeUploadDiscardCount uploadState `shouldReturn` 1

withBrowserAndUploadServer :: (BrowserConfig -> LocalTestServer -> NativeUploadState -> IO a) -> IO a
withBrowserAndUploadServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  uploadState <- newNativeUploadState
  withLocalTestServer (multipartUploadApplication uploadState) (\server -> action browser server uploadState)
