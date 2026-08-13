{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A CSRF-protected, JS-optional native file-upload form using the typed
-- endpoint boundary (see 'HarchWeb.Api.Endpoint's route-family registry).
-- This dedicated example owns the raw, incremental request body a native
-- @POST@ needs. AD owns the upload-storage lifecycle policy.
--
-- CSRF policy: the form carries a single-use, server-held token rather than
-- a double-submit cookie, so no framework change is needed to let a plain
-- page response set a cookie header. 'NativeUploadState' holds at most one
-- outstanding token; issuing a fresh one (every @GET@) invalidates any
-- earlier, unsubmitted one. 'withApiMultipartRequest' validates the CSRF
-- field via its per-part callback -- which runs as soon as that field's
-- part finishes, before any later part is read -- so a request whose file
-- part follows an invalid or absent CSRF field is rejected before that
-- file reaches the in-memory adapter. The CSRF field must still appear before the
-- file field in the form markup below for the common, well-formed case to
-- reject before retaining it at all, rather than merely before the response
-- claims success; see 'uploadFormBody'.
--
-- This form carries no @data-harch-action@ attribute, so the inline capture
-- kernel's @form[data-harch-action="true"]@ selector never matches it and
-- the browser submits it natively -- no file bytes are ever read by client
-- script, with or without JavaScript enabled.
module App.MultipartUpload
  ( NativeUploadState,
    NativeUploadTarget (..),
    nativeUploadDiscardCount,
    nativeUploadEndpoints,
    nativeUploadPath,
    newNativeUploadState,
  )
where

import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Api
  ( ApiEndpointRequest (..),
    ApiMethod (ApiGet, ApiPost),
    ApiMultipartRequest,
    ApiMultipartRequestError (..),
    ApiRequestBody (..),
    ApiResponse (..),
    ApiResponseEncoder,
    ApiRouteEndpoint,
    SomeApiRouteEndpoint (..),
    apiResponse,
    apiRouteEndpointAt,
    apiUtf8ContentType,
    at,
    bytesResponseEncoder,
    htmlMediaType,
    withApiMultipartRequest,
  )
import HarchWeb.Api.Multipart
  ( InMemoryUpload,
    MultipartConsumeError (..),
    MultipartScopedPart (..),
    defaultMultipartLimits,
    discardMultipartUpload,
    inMemoryMultipartStorage,
  )
import HarchWeb.Markup qualified as Markup
import HarchWeb.Session (CsrfToken, csrfTokenText, generateCsrfToken, mkCsrfToken, validateCsrfToken)
import Network.HTTP.Types qualified as HttpTypes

data NativeUploadTarget
  = ShowUploadForm
  | SubmitUpload

-- | Holds at most one outstanding, server-issued CSRF token. See the module
-- header for why this is the CSRF transport instead of a double-submit
-- cookie. It also records deliberate disposal of accepted in-memory uploads,
-- so the native example's ownership boundary remains observable in its
-- real-browser tests.
data NativeUploadState = NativeUploadState
  { nativeUploadTokenReference :: IORef (Maybe CsrfToken),
    nativeUploadDiscardCountReference :: IORef Int
  }

-- The `$!` on an already-WHNF `Nothing` has no runtime effect; it exists so
-- HPC ticks this call on every invocation instead of treating the closed
-- literal as a once-shared CAF reference.
{-# ANN newNativeUploadState ("HLint: ignore Redundant $!" :: String) #-}
newNativeUploadState :: IO NativeUploadState
newNativeUploadState = NativeUploadState <$> (newIORef $! Nothing) <*> (newIORef $! 0)

-- | Returns how many accepted uploads this example deliberately discarded.
-- Production applications can instead promote an upload through their chosen
-- durable adapter; this example has no durable ownership requirement.
nativeUploadDiscardCount :: NativeUploadState -> IO Int
nativeUploadDiscardCount = readIORef . nativeUploadDiscardCountReference

nativeUploadPath :: Text
nativeUploadPath = "/native-upload"

-- | Declared once per running server so the single-use CSRF state is shared
-- by its form GET and POST requests; composed via
-- 'HarchWeb.Api.apiRouteEndpointFamilyCodec'/'apiRouteEndpointFamilyDefinition'
-- rather than the removed compatibility @apiEndpointMiddleware@.
nativeUploadEndpoints :: NativeUploadState -> [SomeApiRouteEndpoint]
nativeUploadEndpoints state =
  [ SomeApiRouteEndpoint (showUploadFormEndpoint state),
    SomeApiRouteEndpoint (submitUploadEndpoint state)
  ]

htmlResponseEncoders :: NonEmpty (ApiResponseEncoder ByteString.ByteString)
htmlResponseEncoders = bytesResponseEncoder (apiUtf8ContentType htmlMediaType) :| []

showUploadFormEndpoint :: NativeUploadState -> ApiRouteEndpoint () () () ByteString.ByteString
showUploadFormEndpoint state =
  apiRouteEndpointAt
    ApiGet
    (at nativeUploadPath)
    (pure ())
    ApiNoRequestBody
    htmlResponseEncoders
    (\_endpointRequest -> Right <$> (issueUploadToken state >>= renderUploadFormPage))
    (const (apiResponse ByteString.empty))

submitUploadEndpoint :: NativeUploadState -> ApiRouteEndpoint () (ApiMultipartRequest InMemoryUpload) () ByteString.ByteString
submitUploadEndpoint state =
  apiRouteEndpointAt
    ApiPost
    (at nativeUploadPath)
    (pure ())
    (ApiMultipartRequestBody inMemoryMultipartStorage defaultMultipartLimits)
    htmlResponseEncoders
    (\endpointRequest -> Right <$> handleUploadSubmission state (apiEndpointRequestBody endpointRequest))
    (const (apiResponse ByteString.empty))

issueUploadToken :: NativeUploadState -> IO CsrfToken
issueUploadToken state = do
  freshToken <- generateCsrfToken
  freshToken <$ atomicModifyIORef' (nativeUploadTokenReference state) (const (Just freshToken, ()))

-- | Consumes and invalidates the outstanding token if @suppliedTokenText@
-- matches it; a mismatched or absent supplied token leaves any outstanding
-- token in place so a legitimate retry after a transient failure (e.g. no
-- file selected) can still succeed against the same still-open form.
claimUploadToken :: NativeUploadState -> Text -> IO Bool
claimUploadToken state suppliedTokenText =
  atomicModifyIORef' (nativeUploadTokenReference state) $ \maybeOutstandingToken ->
    case (maybeOutstandingToken, mkCsrfToken suppliedTokenText) of
      (Just outstandingToken, Just suppliedToken)
        | validateCsrfToken outstandingToken suppliedToken ->
            (Nothing, True)
      _ -> (maybeOutstandingToken, False)

handleUploadSubmission :: NativeUploadState -> ApiMultipartRequest InMemoryUpload -> IO (ApiResponse ByteString.ByteString)
handleUploadSubmission state multipartRequestBody = do
  outcome <- consumeUpload state multipartRequestBody
  case outcome of
    UploadAccepted filename byteCount -> successPage filename byteCount
    UploadCsrfRejected -> errorPage HttpTypes.status403 "Your upload form had expired. Go back and try again."
    UploadMissingFile -> errorPage HttpTypes.status422 "Choose a file before submitting."
    -- Every 'MultipartConsumeError' (media type, declared size limits,
    -- malformed structure, truncation) renders the same way here: the
    -- distinction is exercised by "HarchWeb.Api.Multipart"'s unit suite.
    UploadRejected _consumeError -> errorPage HttpTypes.status400 "This upload was invalid."

data UploadOutcome
  = UploadAccepted Text Int
  | UploadCsrfRejected
  | UploadMissingFile
  | UploadRejected MultipartConsumeError

-- | Drives the endpoint's already-opened scoped multipart consumer with a
-- callback that rejects the whole body -- before any later part, including a
-- later file part, is read -- unless a valid, unexpired CSRF field already
-- arrived. A file part's bytes are only ever considered "accepted" once this
-- callback returns 'Right' for it. The built-in adapter retains such bytes in
-- memory only for this request; this example explicitly discards accepted
-- uploads. An application that needs durable ownership must promote an
-- upload through its selected storage adapter instead.
--
-- The `$!` applications below (on already-WHNF constructor arguments like
-- 'MultipartMalformedBody') exist so HPC ticks each on every invocation
-- instead of treating it as a once-shared reference; they have no runtime
-- effect.
{-# ANN consumeUpload ("HLint: ignore Redundant $!" :: String) #-}
consumeUpload :: NativeUploadState -> ApiMultipartRequest InMemoryUpload -> IO UploadOutcome
consumeUpload state multipartRequestBody = do
  csrfValidatedReference <- newIORef False
  acceptedReference <- newIORef Nothing
  csrfRejectedReference <- newIORef False
  consumeResult <-
    withApiMultipartRequest multipartRequestBody $ \case
      MultipartScopedFieldPart "_harch_csrf" suppliedTokenText -> do
        claimed <- claimUploadToken state suppliedTokenText
        if claimed
          then Right () <$ atomicModifyIORef' csrfValidatedReference (const (True, ()))
          else (Left $! MultipartMalformedBody) <$ atomicModifyIORef' csrfRejectedReference (const (True, ()))
      MultipartScopedFilePart _fieldName filename upload byteCount -> do
        csrfValidated <- atomicModifyIORef' csrfValidatedReference (\validated -> (validated, validated))
        if csrfValidated
          then do
            discardMultipartUpload upload
            atomicModifyIORef' (nativeUploadDiscardCountReference state) (\count -> (count + 1, ()))
            Right () <$ atomicModifyIORef' acceptedReference (const (Just (UploadAccepted filename byteCount), ()))
          else do
            atomicModifyIORef' csrfRejectedReference (const (True, ()))
            pure (Left $! MultipartMalformedBody)
      MultipartScopedFieldPart _ _ -> pure (Right ())
  case consumeResult of
    Left (ApiMultipartRequestFailed multipartError) -> do
      csrfRejected <- atomicModifyIORef' csrfRejectedReference (\rejected -> (rejected, rejected))
      pure (if csrfRejected then UploadCsrfRejected else UploadRejected $! multipartError)
    -- Unreachable: this handler calls 'withApiMultipartRequest' exactly once
    -- per request. Treated the same as a rejected body rather than crashing,
    -- since a caller-observable duplicate-consumption bug should still fail
    -- the request safely.
    Left ApiMultipartRequestAlreadyConsumed -> pure (UploadRejected MultipartMalformedBody)
    Right () -> do
      maybeAccepted <- atomicModifyIORef' acceptedReference (\accepted -> (accepted, accepted))
      pure (fromMaybe UploadMissingFile maybeAccepted)

successPage :: Text -> Int -> IO (ApiResponse ByteString.ByteString)
successPage filename byteCount =
  renderNativeUploadPage
    HttpTypes.status200
    "Upload received"
    ( Markup.element
        Markup.divTag
        [Markup.dataAttribute "page" "native-upload-success"]
        [ Markup.element Markup.headingOneTag [] [Markup.text "Upload received"],
          Markup.element
            Markup.paragraphTag
            []
            [Markup.text (filename <> " (" <> Text.pack (show byteCount) <> " bytes) was received.")]
        ]
    )

errorPage :: HttpTypes.Status -> Text -> IO (ApiResponse ByteString.ByteString)
errorPage statusCode message =
  renderNativeUploadPage
    statusCode
    "Upload failed"
    ( Markup.element
        Markup.divTag
        [Markup.dataAttribute "page" "native-upload-error"]
        [ Markup.element Markup.headingOneTag [] [Markup.text "Upload failed"],
          Markup.element Markup.paragraphTag [] [Markup.text message],
          Markup.element
            Markup.formTag
            [Markup.formAction nativeUploadPath, Markup.method "GET"]
            [Markup.element Markup.buttonTag [Markup.inputType "submit"] [Markup.text "Try again"]]
        ]
    )

renderUploadFormPage :: CsrfToken -> IO (ApiResponse ByteString.ByteString)
renderUploadFormPage csrfToken =
  renderNativeUploadPage HttpTypes.status200 "Upload a file" (uploadFormBody csrfToken)

-- | The CSRF field must precede the file field: 'consumeUpload' rejects
-- before opening a later part, so this ordering is what lets an invalid or
-- absent token skip retaining the file at all, not merely skip
-- treating it as accepted afterward.
uploadFormBody :: CsrfToken -> Markup.Html
uploadFormBody csrfToken =
  Markup.element
    Markup.formTag
    [Markup.formAction nativeUploadPath, Markup.method "POST", Markup.enctype "multipart/form-data", Markup.ariaLabel "Upload a file"]
    [ Markup.element Markup.headingOneTag [] [Markup.text "Upload a file"],
      Markup.voidElement Markup.inputTag [Markup.inputType "hidden", Markup.name "_harch_csrf", Markup.value (csrfTokenText csrfToken)],
      Markup.element Markup.labelTag [Markup.labelFor (Markup.literalElementId "native-upload-file")] [Markup.text "File"],
      Markup.voidElement
        Markup.inputTag
        [Markup.elementId (Markup.literalElementId "native-upload-file"), Markup.name "upload", Markup.inputType "file", Markup.required],
      Markup.element Markup.buttonTag [Markup.inputType "submit"] [Markup.text "Upload"]
    ]

-- The `$!` on the content type (already-WHNF) exists so HPC ticks this call
-- on every invocation instead of treating the closed literal as a once-shared
-- reference; it has no runtime effect.
{-# ANN renderNativeUploadPage ("HLint: ignore Redundant $!" :: String) #-}
renderNativeUploadPage :: HttpTypes.Status -> Text -> Markup.Html -> IO (ApiResponse ByteString.ByteString)
renderNativeUploadPage statusCode pageTitleText pageBodyHtml = do
  let renderedHtml =
        "<!doctype html><html><head><title>"
          <> Markup.renderHtml (Markup.text pageTitleText)
          <> "</title></head><body><main id=\"app-main\">"
          <> Markup.renderHtml pageBodyHtml
          <> "</main></body></html>"
  pure ((apiResponse $! TextEncoding.encodeUtf8 renderedHtml) {apiEndpointResponseStatus = statusCode})
