-- | Typed application, request, response, and middleware contracts.
--
-- The framework facade re-exports this module. Private focused modules own
-- request execution, rendering, and transport implementation.
module HarchWeb.Server
  ( module HarchWeb.Server.Config,
    Application (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    MiddlewareResult (..),
    RegionPatch (..),
    RequestMiddleware (..),
    Response (..),
    ResponseBody (..),
    ResponseDiagnostics (..),
    ServerSentEvent (..),
    ServerSentEventSource (..),
    application,
    applyResponseHeaders,
    clientActionResponseBody,
    eventStreamResponse,
    isClientActionRequest,
    LocalTestServer (..),
    ReloadingTlsCredentials,
    parseClientActionFields,
    redirectResponse,
    renderServerSentEvent,
    runEarlyRequestStages,
    responseDiagnostics,
    responseKind,
    responsePolicyHeaders,
    responseStatusCode,
    reportEarlyRequestObservability,
    serverSentEventContentType,
    runRequestMiddlewarePipeline,
    serverSentEventSourceFromList,
    navigationRuntimeResponse,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    runServer,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
    toWaiApplication,
    toWaiBodyResponse,
    toWaiResponse,
    withLocalTestServer,
  )
where

import HarchWeb.Server.Application
import HarchWeb.Server.ClientAction
import HarchWeb.Server.Config
import HarchWeb.Server.LocalTest
import HarchWeb.Server.RequestExecution
import HarchWeb.Server.Response
import HarchWeb.Server.ResponseRendering
import HarchWeb.Server.Runtime
import HarchWeb.Server.Sse
import HarchWeb.Server.Transport
  ( ReloadingTlsCredentials,
    loadReloadingTlsCredentials,
    loadTlsCredentialSnapshotOrThrowWithLoader,
    reloadTlsCredentialsIfChanged,
    startManualTlsRuntimeServerWithStarter,
    startWarpRuntimeServerOnSocket,
  )
