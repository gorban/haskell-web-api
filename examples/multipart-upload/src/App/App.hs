{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( multipartUploadApplication,
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
newMultipartUploadApplication = multipartUploadApplication <$> newNativeUploadState

multipartUploadApplication :: NativeUploadState -> Wai.Application
multipartUploadApplication state =
  HarchWeb.toWaiApplication (Site.buildSiteApplication (multipartUploadSite state))

multipartUploadSite :: NativeUploadState -> Site.Site ApiPath () ()
multipartUploadSite state =
  Site.simpleSite
    "multipart-upload-example"
    ()
    (apiRouteEndpointFamilyCodec endpoints)
    (const multipartUploadUnusedPageShell)
    []
    (apiRouteEndpointFamilyDefinition endpoints)
  where
    endpoints = nativeUploadEndpoints state

-- | No declared endpoint ever renders a 'HarchWeb.Page', so no route
-- reaches this shell; it exists only to satisfy 'Site.simpleSite's type.
multipartUploadUnusedPageShell :: HarchWeb.PageShell ApiPath ()
multipartUploadUnusedPageShell =
  HarchWeb.PageShell
    { HarchWeb.shellBodyAttributes = [],
      HarchWeb.shellNavigationAttributes = [],
      HarchWeb.shellNavigationItems = [],
      HarchWeb.shellMainId = "main",
      HarchWeb.shellMainAttributes = [],
      HarchWeb.shellStylesheets = [],
      HarchWeb.shellRuntimeDescriptors = []
    }
