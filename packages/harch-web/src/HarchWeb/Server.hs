{-# LANGUAGE DuplicateRecordFields #-}

-- | Typed application, request, response, and middleware contracts.
--
-- The framework facade re-exports this module. Private focused modules own
-- request execution, rendering, and transport implementation.
module HarchWeb.Server
  ( module HarchWeb.Server.Config,
    Application (..),
    RouteExecutionPolicy (..),
    ClientActionDecodeResult (..),
    ClientActionIdempotencyKey,
    ClientActionPayload (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch,
    RequestMiddleware (..),
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ProtocolResponse (..),
    ProtocolResponseBody (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
    application,
    applyResponseHeaders,
    clientActionResponseBody,
    eventStreamResponse,
    isClientActionRequest,
    LocalTestServer (..),
    ManualTlsServerStarter,
    ReloadingTlsCredentials,
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    parseClientActionFields,
    redirectResponse,
    RequestBodyReadFailure (..),
    readRequestBodyUpTo,
    newRequestBodyChunkReader,
    renderServerSentEvent,
    runEarlyRequestStages,
    responseDiagnostics,
    responseKind,
    responsePolicyHeaders,
    responseStatusCode,
    renderResponse,
    unboundedRouteExecutionPolicy,
    reportEarlyRequestObservability,
    serverSentEventContentType,
    runRequestMiddlewarePipeline,
    serverSentEventSourceFromList,
    navigationRuntimeResponse,
    runtimeAssetResponse,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    runServer,
    runServerWithWaiMiddleware,
    waitForShutdownSignalWith,
    startHttpRuntimeServerWithStarter,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
    toWaiApplication,
    toWaiBodyResponse,
    toWaiResponse,
    withLocalTestServer,
    withLocalTestServerForApplication,
  )
where

import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.Config
import HarchWeb.Server.LocalTest
import HarchWeb.Server.RequestBody
import HarchWeb.Server.RequestExecution
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering
import HarchWeb.Server.Runtime
import HarchWeb.Server.Sse
import HarchWeb.Server.Transport
  ( ManualTlsServerStarter,
    ReloadingTlsCredentials,
    TlsCertificateFilePath,
    TlsPrivateKeyFilePath,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    startHttpRuntimeServerWithStarter,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
    tlsCertificateFilePath,
    tlsCertificateFilePathValue,
    tlsPrivateKeyFilePath,
    tlsPrivateKeyFilePathValue,
  )
