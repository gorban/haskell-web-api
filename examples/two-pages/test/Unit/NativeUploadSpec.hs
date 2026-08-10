{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.NativeUploadSpec (spec) where

import App.NativeUpload (handleNativeUpload, nativeUploadEndpoints, nativeUploadPath, newNativeUploadState)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LazyByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api qualified as Api
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

spec :: Spec
spec =
  describe "Unit.App.NativeUpload" $ do
    describe "GET /native-upload" $
      it "renders a native, script-free multipart form with a fresh CSRF field" $ do
        application <- newUploadApplication
        response <- performWaiRequest application getRequest
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status200)
              :| [ Text.isInfixOf "<form action=\"/native-upload\" method=\"POST\" enctype=\"multipart/form-data\" aria-label=\"Upload a file\">" body `shouldBe` True,
                   Text.isInfixOf "<input type=\"hidden\" name=\"_harch_csrf\" value=\"" body `shouldBe` True,
                   Text.isInfixOf "<input id=\"native-upload-file\" name=\"upload\" type=\"file\" required>" body `shouldBe` True,
                   Text.isInfixOf "data-harch-action" body `shouldBe` False
                 ]
          )

    describe "POST /native-upload" $ do
      it "accepts a valid CSRF token and file, spooling it and reporting its name and size" $ do
        application <- newUploadApplication
        csrfToken <- currentCsrfToken application
        response <- performRequest application (multipartRequest (csrfPart csrfToken <> filePart "hello.txt" "file bytes" <> closingBoundary))
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status200)
              :| [Text.isInfixOf "hello.txt (10 bytes) was received." body `shouldBe` True]
          )

      it "ignores an unrecognized field between the CSRF field and the file" $ do
        application <- newUploadApplication
        csrfToken <- currentCsrfToken application
        response <-
          performRequest
            application
            (multipartRequest (csrfPart csrfToken <> fieldPart "note" "hi" <> filePart "a.txt" "abc" <> closingBoundary))
        body <- readResponseBody response
        Text.isInfixOf "a.txt (3 bytes) was received." body `shouldBe` True

      it "rejects a mismatched CSRF token before any file part is ever read" $ do
        application <- newUploadApplication
        _ <- currentCsrfToken application
        response <-
          performRequest
            application
            (multipartRequest (csrfPart (Text.replicate 32 "x") <> filePart "hello.txt" "file bytes" <> closingBoundary))
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status403)
              :| [Text.isInfixOf "Your upload form had expired." body `shouldBe` True]
          )

      it "rejects a file that arrives with no CSRF field at all, even though it was already spooled" $ do
        application <- newUploadApplication
        _ <- currentCsrfToken application
        response <- performRequest application (multipartRequest (filePart "hello.txt" "file bytes" <> closingBoundary))
        Wai.responseStatus response `shouldBe` HttpTypes.status403

      it "reports a missing file for an otherwise-valid submission" $ do
        application <- newUploadApplication
        csrfToken <- currentCsrfToken application
        response <- performRequest application (multipartRequest (csrfPart csrfToken <> closingBoundary))
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status422)
              :| [Text.isInfixOf "Choose a file before submitting." body `shouldBe` True]
          )

      it "reports a missing multipart boundary as an invalid upload" $ do
        application <- newUploadApplication
        response <- performRequest application (requestWithContentType "multipart/form-data" "some body")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status400)
              :| [Text.isInfixOf "This upload was invalid." body `shouldBe` True]
          )

      it "accepts a quoted boundary parameter" $ do
        application <- newUploadApplication
        csrfToken <- currentCsrfToken application
        response <-
          performRequest
            application
            (requestWithContentType ("multipart/form-data; boundary=\"" <> boundaryToken <> "\"") (csrfPart csrfToken <> filePart "hello.txt" "file bytes" <> closingBoundary))
        body <- readResponseBody response
        Text.isInfixOf "hello.txt (10 bytes) was received." body `shouldBe` True

      it "reports an invalid body unrelated to CSRF as a generic malformed upload" $ do
        application <- newUploadApplication
        response <- performRequest application (multipartRequest "not multipart at all")
        body <- readResponseBody response
        expectAll
          ( (Wai.responseStatus response `shouldBe` HttpTypes.status400)
              :| [Text.isInfixOf "This upload was invalid." body `shouldBe` True]
          )

      it "invalidates an earlier token once a newer form has been issued" $ do
        application <- newUploadApplication
        staleToken <- currentCsrfToken application
        _freshToken <- currentCsrfToken application
        response <-
          performRequest
            application
            (multipartRequest (csrfPart staleToken <> filePart "hello.txt" "file bytes" <> closingBoundary))
        Wai.responseStatus response `shouldBe` HttpTypes.status403

boundaryToken :: ByteString
boundaryToken = "unit-native-upload-boundary"

csrfPart :: Text -> ByteString
csrfPart tokenText =
  "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; name=\"_harch_csrf\"\r\n\r\n" <> TextEncoding.encodeUtf8 tokenText <> "\r\n"

fieldPart :: ByteString -> ByteString -> ByteString
fieldPart fieldName fieldValue =
  "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; name=\"" <> fieldName <> "\"\r\n\r\n" <> fieldValue <> "\r\n"

filePart :: ByteString -> ByteString -> ByteString
filePart filename fileContent =
  "--" <> boundaryToken <> "\r\nContent-Disposition: form-data; name=\"upload\"; filename=\"" <> filename <> "\"\r\n\r\n" <> fileContent <> "\r\n"

closingBoundary :: ByteString
closingBoundary = "--" <> boundaryToken <> "--\r\n"

newUploadApplication :: IO Wai.Application
newUploadApplication = do
  state <- newNativeUploadState
  let fallback _request respond = respond (Wai.responseLBS HttpTypes.status404 [] "not found")
  pure (Api.apiEndpointMiddleware nativeUploadEndpoints (handleNativeUpload state) fallback)

currentCsrfToken :: Wai.Application -> IO Text
currentCsrfToken application = do
  response <- performWaiRequest application getRequest
  extractCsrfToken <$> readResponseBody response

extractCsrfToken :: Text -> Text
extractCsrfToken body =
  Text.takeWhile (/= '"') (Text.drop (Text.length marker) (snd (Text.breakOn marker body)))
  where
    marker = "name=\"_harch_csrf\" value=\""

getRequest :: Wai.Request
getRequest = Wai.defaultRequest {Wai.requestMethod = "GET", Wai.rawPathInfo = TextEncoding.encodeUtf8 nativeUploadPath}

requestWithContentType :: ByteString -> ByteString -> IO Wai.Request
requestWithContentType contentTypeValue bodyBytes = do
  chunksReference <- newIORef [bodyBytes]
  pure
    ( Wai.setRequestBodyChunks
        (nextRequestBodyChunk chunksReference)
        Wai.defaultRequest
          { Wai.requestMethod = "POST",
            Wai.rawPathInfo = TextEncoding.encodeUtf8 nativeUploadPath,
            Wai.requestHeaders = [(HttpTypes.hContentType, contentTypeValue)]
          }
    )

multipartRequest :: ByteString -> IO Wai.Request
multipartRequest = requestWithContentType ("multipart/form-data; boundary=" <> boundaryToken)

nextRequestBodyChunk :: IORef [ByteString] -> IO ByteString
nextRequestBodyChunk chunksReference =
  atomicModifyIORef' chunksReference $ \case
    [] -> ([], ByteString.empty)
    chunk : remainingChunks -> (remainingChunks, chunk)

performRequest :: Wai.Application -> IO Wai.Request -> IO Wai.Response
performRequest application requestAction = requestAction >>= performWaiRequest application

performWaiRequest :: Wai.Application -> Wai.Request -> IO Wai.Response
performWaiRequest webApplication request = do
  responseReference <- newIORef Nothing
  _ <- webApplication request (\response -> writeIORef responseReference (Just response) >> pure WaiInternal.ResponseReceived)
  maybeResponse <- readIORef responseReference
  maybe (ioError (userError "expected WAI application to produce a response")) pure maybeResponse

readResponseBody :: Wai.Response -> IO Text
readResponseBody response = do
  let (_, _, withStreamingBody) = Wai.responseToStream response
  chunksReference <- newIORef []
  withStreamingBody $ \streamingBody ->
    streamingBody
      (\builder -> atomicModifyIORef' chunksReference (\chunks -> (chunks <> [Builder.toLazyByteString builder], ())))
      (pure ())
  chunks <- readIORef chunksReference
  pure (TextEncoding.decodeUtf8 (LazyByteString.toStrict (LazyByteString.concat chunks)))
