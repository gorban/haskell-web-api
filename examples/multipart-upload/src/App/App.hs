{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( multipartUploadApplication,
    multipartUploadSite,
    newMultipartUploadApplication,
  )
where

import App.MultipartUpload (NativeUploadState, nativeUploadEndpoints, newNativeUploadState)
import HarchWeb qualified
import HarchWeb.Api (ApiPath, apiRouteEndpointFamilyCodec, apiRouteEndpointFamilyDefinition)
import HarchWeb.Site qualified as Site
import Network.Wai qualified as Wai

-- | Build the complete WAI application once per running server so the
-- single-use CSRF state is shared by its form GET and POST requests.
newMultipartUploadApplication :: IO Wai.Application
newMultipartUploadApplication = newNativeUploadState >>= multipartUploadWaiApplication

-- | Build the typed application for one upload-state lifetime.
--
-- Decision record (local test policy, 2026-08-17): preserve the typed
-- 'HarchWeb.Application' through real-server tests instead of adding request
-- policy metadata to an opaque 'Wai.Application'. A WAI function cannot
-- carry a 'HarchWeb.RequestPolicyConfig'; 'HarchWeb.withLocalTestServer'
-- already owns applying it consistently with deployed listeners.
multipartUploadApplication :: NativeUploadState -> HarchWeb.Application ApiPath () ()
multipartUploadApplication state =
  Site.buildSiteApplication (multipartUploadSite state)

multipartUploadWaiApplication :: NativeUploadState -> IO Wai.Application
multipartUploadWaiApplication = HarchWeb.toWaiApplication . multipartUploadApplication

multipartUploadSite :: NativeUploadState -> Site.Site ApiPath () ()
multipartUploadSite state =
  Site.apiOnlySite
    "multipart-upload-example"
    ()
    (apiRouteEndpointFamilyCodec endpoints)
    (apiRouteEndpointFamilyDefinition endpoints)
  where
    endpoints = nativeUploadEndpoints state
