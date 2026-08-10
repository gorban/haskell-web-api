{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A CSRF-protected, JS-optional native file-upload form dispatched
-- through "HarchWeb.Api".'HarchWeb.Api.apiEndpointMiddleware', composed in
-- front of the site's own application (see 'App.App.buildNativeUploadMiddleware')
-- rather than through 'App.Routes.routeCodec': a native @POST@ needs the raw,
-- incremental request body that path-only route matching does not have.
--
-- CSRF policy: the form carries a single-use, server-held token rather than
-- a double-submit cookie, so no framework change is needed to let a plain
-- page response set a cookie header. 'NativeUploadState' holds at most one
-- outstanding token; issuing a fresh one (every @GET@) invalidates any
-- earlier, unsubmitted one. 'consumeMultipartRequestBodyWith' validates the
-- CSRF field via its per-part callback -- which runs as soon as that field's
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
module App.NativeUpload
  ( NativeUploadState,
    NativeUploadTarget (..),
    handleNativeUpload,
    nativeUploadEndpoints,
    nativeUploadPath,
    newNativeUploadState,
  )
where

import App.Components.Layout (twoPageShell)
import App.Pages.Route.Generated (PageRoute (HomePage, LiveDataPage, SecondPage))
import App.Routes (CustomRoute (NativeSubscriptionFallback), TwoPageRoute (Custom), routeCodec)
import App.Routes qualified as Routes
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb
  ( NavigationItem (..),
    Page (..),
    PageShell (..),
    buildPageShell,
    generateRuntimeNonce,
    renderDocumentWithNonce,
  )
import HarchWeb.Api
  ( ApiEndpoint,
    ApiMethod (ApiGet, ApiPost),
    ApiResponseBody,
    apiBytesResponse,
    apiEndpoint,
    apiResponseStatus,
    at,
  )
import HarchWeb.Api.Multipart
  ( MultipartConsumeError (..),
    MultipartLimits,
    MultipartPartWith (..),
    consumeMultipartRequestBodyWith,
    defaultMultipartLimits,
  )
import HarchWeb.Markup qualified as Markup
import HarchWeb.Session (CsrfToken, csrfTokenText, generateCsrfToken, mkCsrfToken, validateCsrfToken)
import Network.HTTP.Types qualified as HttpTypes
import Network.Wai qualified as Wai

data NativeUploadTarget
  = ShowUploadForm
  | SubmitUpload

-- | Holds at most one outstanding, server-issued CSRF token. See the module
-- header for why this is the CSRF transport instead of a double-submit
-- cookie.
newtype NativeUploadState = NativeUploadState (IORef (Maybe CsrfToken))

-- The `$!` on an already-WHNF `Nothing` has no runtime effect; it exists so
-- HPC ticks this call on every invocation instead of treating the closed
-- literal as a once-shared CAF reference.
{-# ANN newNativeUploadState ("HLint: ignore Redundant $!" :: String) #-}
newNativeUploadState :: IO NativeUploadState
newNativeUploadState = NativeUploadState <$> (newIORef $! Nothing)

nativeUploadPath :: Text
nativeUploadPath = "/native-upload"

nativeUploadEndpoints :: [ApiEndpoint NativeUploadTarget]
nativeUploadEndpoints =
  [ apiEndpoint ShowUploadForm ApiGet (at nativeUploadPath),
    apiEndpoint SubmitUpload ApiPost (at nativeUploadPath)
  ]

handleNativeUpload :: NativeUploadState -> Wai.Request -> NativeUploadTarget -> IO ApiResponseBody
handleNativeUpload state _request ShowUploadForm =
  issueUploadToken state >>= renderUploadFormPage
handleNativeUpload state request SubmitUpload =
  handleUploadSubmission state request

issueUploadToken :: NativeUploadState -> IO CsrfToken
issueUploadToken (NativeUploadState tokenReference) = do
  freshToken <- generateCsrfToken
  freshToken <$ atomicModifyIORef' tokenReference (const (Just freshToken, ()))

-- | Consumes and invalidates the outstanding token if @suppliedTokenText@
-- matches it; a mismatched or absent supplied token leaves any outstanding
-- token in place so a legitimate retry after a transient failure (e.g. no
-- file selected) can still succeed against the same still-open form.
claimUploadToken :: NativeUploadState -> Text -> IO Bool
claimUploadToken (NativeUploadState tokenReference) suppliedTokenText =
  atomicModifyIORef' tokenReference $ \maybeOutstandingToken ->
    case (maybeOutstandingToken, mkCsrfToken suppliedTokenText) of
      (Just outstandingToken, Just suppliedToken)
        | validateCsrfToken outstandingToken suppliedToken ->
            (Nothing, True)
      _ -> (maybeOutstandingToken, False)

handleUploadSubmission :: NativeUploadState -> Wai.Request -> IO ApiResponseBody
handleUploadSubmission state request =
  case lookup HttpTypes.hContentType (Wai.requestHeaders request) >>= multipartBoundary of
    Nothing -> errorPage 400 "This upload request had no multipart boundary."
    Just boundary -> do
      outcome <- consumeUpload state defaultMultipartLimits boundary request
      case outcome of
        UploadAccepted filename byteCount -> successPage filename byteCount
        UploadCsrfRejected -> errorPage 403 "Your upload form had expired. Go back and try again."
        UploadMissingFile -> errorPage 422 "Choose a file before submitting."
        -- Every 'MultipartConsumeError' (size limits, malformed structure,
        -- truncation) renders the same way here: the distinction between
        -- them is already exercised at the library level in
        -- "HarchWeb.Api.Multipart"'s own test suite.
        UploadRejected _consumeError -> errorPage 400 "This upload was invalid."

data UploadOutcome
  = UploadAccepted Text Int
  | UploadCsrfRejected
  | UploadMissingFile
  | UploadRejected MultipartConsumeError

-- | Drives 'consumeMultipartRequestBodyWith' with a callback that rejects
-- the whole body -- before any later part, including a later file part, is
-- read -- unless a valid, unexpired CSRF field already arrived. A file
-- part's bytes are only ever considered "accepted" once this callback
-- returns 'Right' for it. The built-in adapter retains such bytes in memory
-- for this request only; AD tracks explicit durable adoption and cleanup
-- when an application selects persistent storage.
--
-- The `$!` applications below (on already-WHNF constructor arguments like
-- 'MultipartMalformedBody') exist so HPC ticks each on every invocation
-- instead of treating it as a once-shared reference; they have no runtime
-- effect.
{-# ANN consumeUpload ("HLint: ignore Redundant $!" :: String) #-}
consumeUpload :: NativeUploadState -> MultipartLimits -> ByteString -> Wai.Request -> IO UploadOutcome
consumeUpload state limits boundary request = do
  csrfValidatedReference <- newIORef False
  acceptedReference <- newIORef Nothing
  csrfRejectedReference <- newIORef False
  consumeResult <-
    consumeMultipartRequestBodyWith limits boundary request $ \case
      MultipartFieldPart "_harch_csrf" suppliedTokenText -> do
        claimed <- claimUploadToken state suppliedTokenText
        if claimed
          then Right () <$ atomicModifyIORef' csrfValidatedReference (const (True, ()))
          else (Left $! MultipartMalformedBody) <$ atomicModifyIORef' csrfRejectedReference (const (True, ()))
      MultipartFilePart _fieldName filename _storedUpload byteCount -> do
        csrfValidated <- atomicModifyIORef' csrfValidatedReference (\validated -> (validated, validated))
        if csrfValidated
          then Right () <$ atomicModifyIORef' acceptedReference (const (Just (UploadAccepted filename byteCount), ()))
          else do
            void (atomicModifyIORef' csrfRejectedReference (const (True, ())))
            pure (Left $! MultipartMalformedBody)
      MultipartFieldPart _ _ -> pure (Right ())
  case consumeResult of
    Left multipartError -> do
      csrfRejected <- atomicModifyIORef' csrfRejectedReference (\rejected -> (rejected, rejected))
      pure (if csrfRejected then UploadCsrfRejected else UploadRejected $! multipartError)
    Right () -> do
      maybeAccepted <- atomicModifyIORef' acceptedReference (\accepted -> (accepted, accepted))
      pure (fromMaybe UploadMissingFile maybeAccepted)

successPage :: Text -> Int -> IO ApiResponseBody
successPage filename byteCount =
  renderNativeUploadPage
    200
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

errorPage :: Int -> Text -> IO ApiResponseBody
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

renderUploadFormPage :: CsrfToken -> IO ApiResponseBody
renderUploadFormPage csrfToken =
  renderNativeUploadPage 200 "Upload a file" (uploadFormBody csrfToken)

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

-- The `$!` on `Custom NativeSubscriptionFallback` (already-WHNF) exists so
-- HPC ticks it on every invocation instead of treating the closed literal
-- as a once-shared reference; it has no runtime effect.
{-# ANN renderNativeUploadPage ("HLint: ignore Redundant $!" :: String) #-}
renderNativeUploadPage :: Int -> Text -> Markup.Html -> IO ApiResponseBody
renderNativeUploadPage statusCode pageTitleText pageBodyHtml = do
  nonce <- generateRuntimeNonce
  let page =
        Page
          { pageTitle = pageTitleText,
            -- A structural identity only: this page is never dispatched
            -- through 'routeCodec' (see the module header), and no route in
            -- 'App.Routes.siteNavigationRoutes' equals it, so it never
            -- affects active-navigation highlighting.
            pageRoute = Custom $! NativeSubscriptionFallback,
            pageContext = (),
            pageBody = pageBodyHtml,
            pageBootstrapHooks = []
          }
      -- This app's 'App.Routes.routeCodec' never reads a page's context
      -- (it is always `()`), so nothing downstream ever forces this
      -- field on its own; force it explicitly here (via BangPatterns, so
      -- optimization can't prove the forcing is unobservable and elide
      -- it) purely so HPC ticks 'pageContext's initializer above on every
      -- call instead of reporting genuinely-dead code.
      !_pageContextForced = pageContext page
      -- Set directly rather than through 'HarchWeb.Site's automatic
      -- navigation injection (this page bypasses 'routeCodec' dispatch;
      -- see the module header), so a visitor can still reach the rest of
      -- the site from here.
      shell = (twoPageShell page) {shellNavigationItems = siteNavigationItems}
      document = (buildPageShell $! routeCodec) shell page
      renderedHtml = (renderDocumentWithNonce $! nonce) document
  pure ((apiBytesResponse $! "text/html; charset=utf-8") (TextEncoding.encodeUtf8 renderedHtml)) {apiResponseStatus = statusCode}

siteNavigationItems :: [NavigationItem TwoPageRoute]
siteNavigationItems =
  [ NavigationItem "Home" (Routes.Page HomePage),
    NavigationItem "Second" (Routes.Page SecondPage),
    NavigationItem "Live updates" (Routes.Page LiveDataPage)
  ]

multipartBoundary :: ByteString -> Maybe ByteString
multipartBoundary contentTypeValue =
  case ByteString.breakSubstring "boundary=" contentTypeValue of
    (_, suffix)
      | ByteString.null suffix -> Nothing
      | otherwise -> Just (unquote (ByteString.takeWhile (/= 59) (ByteString.drop 9 suffix)))
  where
    unquote quotedValue
      | ByteString.length quotedValue >= 2,
        ByteString.head quotedValue == 34,
        ByteString.last quotedValue == 34 =
          ByteString.init (ByteString.tail quotedValue)
      | otherwise = quotedValue
