{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( multipartUploadApplication,
    newMultipartUploadApplication,
  )
where

import App.MultipartUpload (NativeUploadState, handleNativeUpload, nativeUploadEndpoints, newNativeUploadState)
import HarchWeb.Api qualified as Api
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | Build the complete WAI application once per running server so the
-- single-use CSRF state is shared by its form GET and POST requests.
newMultipartUploadApplication :: IO Wai.Application
newMultipartUploadApplication = multipartUploadApplication <$> newNativeUploadState

multipartUploadApplication :: NativeUploadState -> Wai.Application
multipartUploadApplication state =
  Api.apiEndpointMiddleware nativeUploadEndpoints (handleNativeUpload state) notFoundApplication

notFoundApplication :: Wai.Application
notFoundApplication _request respond = respond (Wai.responseLBS Http.status404 [] "Not found")
