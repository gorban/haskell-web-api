{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module HarchWeb
  ( module HarchWeb.Acme,
    module HarchWeb.Document,
    ConnectionObservability (..),
    HttpServerMetrics (..),
    ObservabilityAttribute (ObservabilityAttribute),
    ObservabilityAttributeValue (..),
    RequestTraceContext (..),
    RequestObservability (..),
    RequestSpan (..),
    ResponseKind (..),
    buildConnectionObservability,
    buildRequestObservability,
    forceConnectionObservability,
    forceRequestObservability,
    requestObservabilityAttributes,
    requestSpanName,
    withRequestTraceContext,
    module HarchWeb.Routing,
    module HarchWeb.Security,
    module HarchWeb.Server,
    module HarchWeb.StaticAssets,
    LocalTestServer (..),
    ObservabilityStartupPlan (..),
    ReloadingTlsCredentials,
    exportConnectionObservabilityToOtlp,
    exportRequestObservabilityToOtlp,
    navigationRuntimeScriptSource,
    planObservabilityStartup,
    reloadTlsCredentialsIfChanged,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    startManualTlsRuntimeServerWithStarter,
    runServer,
    startWarpRuntimeServerOnSocket,
    withLocalTestServer,
  )
where

import Data.Text (Text)
import HarchWeb.Acme
import HarchWeb.Document
  ( Document (..),
    HtmlAttribute (..),
    LiveRegion (..),
    NavigationItem (..),
    NavigationRuntime (..),
    Page (..),
    PageShell (..),
    ResolvedNavigationItem (..),
    RuntimeDescriptor (..),
    RuntimeNonce (..),
    buildDocument,
    buildNavigation,
    buildPageShell,
    defaultCaptureKernel,
    defaultCaptureKernelScript,
    defaultNavigationRuntime,
    defaultNavigationRuntimeScript,
    generateRuntimeNonce,
    liveRegionAttributes,
    renderDocument,
    renderDocumentWithNonce,
  )
import HarchWeb.Observability
import HarchWeb.Routing (RouteCodec (..), RouteRequest (..), matchRoute, routeHref)
import HarchWeb.Security
import HarchWeb.Server
import HarchWeb.Server.LocalTest (LocalTestServer (..), withLocalTestServer)
import HarchWeb.Server.Runtime (runServer)
import HarchWeb.Server.Transport
  ( ReloadingTlsCredentials,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
  )
import HarchWeb.StaticAssets
  ( AssetPath (..),
    CssClass (..),
    CssScope (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    Stylesheet (..),
    cssClassText,
    cssScope,
    defaultStaticAssetContentTypes,
    staticAssetHref,
    staticAssetHrefWithPrefix,
    stylesheet,
  )

navigationRuntimeScriptSource :: Text -> NavigationRuntime -> Text
navigationRuntimeScriptSource pathPrefix runtime =
  applyRequestPathPrefix pathPrefix (navigationRuntimePath runtime)
