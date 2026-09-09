{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.App (multipartUploadApplication)
import App.MultipartUpload (NativeUploadState, nativeUploadDiscardCount, newNativeUploadState)
import HarchWeb (LocalTestServer (..), withLocalTestServer)

-- | Upload state owns one outstanding token and a per-fixture discard count.
-- Share only configuration; aroundWith creates fresh server/state scopes so
-- cleanup assertions remain local. Each example owns its temporary upload file.
-- Sharing this server across clients also needs the follow-up "multipart
-- concurrent-client CSRF ownership": one client's GET currently replaces the
-- outstanding token of another. Fresh fixtures do not establish that capability.
spec =
  beforeAll requirePlaywrightBrowserConfig $
    aroundWith withBrowserAndUploadServer $
      parallel $
        describe "multipart-upload real-browser behavior" $ do
          it "submits a native multipart upload as SSR with scripts enabled" $ \(browser, server, uploadState) ->
            withTempFile "multipart-upload-e2e" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
              writeFile filePath "e2e file contents"
              let uploadUrl = localServerBaseUrl server <> "/native-upload"
              runBrowserSpec browser do
                visit uploadUrl
                setInputFiles (css "#native-upload-file") filePath
                submit (byRole Form `named` "Upload a file")
                assertAllObserved do
                  textContent (byRole Heading `named` "Upload received") `shouldEqual` "Upload received"
                  $([|browserMetrics|] `matchesPattern` [p|BrowserMetrics {enhancedNavigationFetchCount = 0, hardNavigationCount = 1, mutationRequestCount = 0}|])
              nativeUploadDiscardCount uploadState `shouldReturn` 1

          it "submits the same native multipart upload as SSR with scripts disabled" $ \(browser, server, uploadState) ->
            withTempFile "multipart-upload-e2e-no-js" [] "attachment.txt" $ \(_tempRoot, filePath) -> do
              writeFile filePath "e2e file contents, no scripts"
              let uploadUrl = localServerBaseUrl server <> "/native-upload"
              runBrowserSpec browser do
                visitWithoutScripts uploadUrl
                setInputFiles (css "#native-upload-file") filePath
                submit (byRole Form `named` "Upload a file")
                assertAllObserved do
                  textContent (byRole Heading `named` "Upload received") `shouldEqual` "Upload received"
              nativeUploadDiscardCount uploadState `shouldReturn` 1

withBrowserAndUploadServer :: ((BrowserConfig, LocalTestServer, NativeUploadState) -> IO a) -> BrowserConfig -> IO a
withBrowserAndUploadServer action browser = do
  uploadState <- newNativeUploadState
  withLocalTestServer (multipartUploadApplication uploadState) (\server -> action (browser, server, uploadState))
