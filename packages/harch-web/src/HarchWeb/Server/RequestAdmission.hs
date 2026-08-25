{-# LANGUAGE OverloadedStrings #-}

-- | Private admission control for complete WAI request lifecycles.
--
-- This module owns the concurrent-request gate because its state and lifetime
-- are independent of typed routing: a slot begins before middleware or body
-- reads and is released only after the WAI application completes. Keeping
-- that boundary separate lets runtime and local listeners share exactly the
-- same admission policy while 'HarchWeb.Server.RequestExecution' remains the
-- public request-execution facade.
module HarchWeb.Server.RequestAdmission
  ( concurrencyLimitedMiddleware,
    RouteConcurrencyGateCache,
    newRouteConcurrencyGateCache,
    routeConcurrencyMiddleware,
  )
where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Control.Exception (finally, mask)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import HarchWeb.Security (RequestConcurrencyLimit, requestConcurrencyLimitValue)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

-- | Compose an opt-in concurrent-in-flight-request gate in front of a
-- caller-supplied middleware. 'Nothing' preserves the framework's
-- established unbounded behaviour: the runtime forks a worker per accepted
-- connection with no admission control of its own, matching Warp 3.4.12's
-- own lack of a concurrent-request or connection-count setting. Every
-- caller that renders a real listener — 'HarchWeb.Server.Runtime' and
-- 'HarchWeb.Server.LocalTest' alike — builds this gate from the same
-- 'RequestPolicyConfig' field, so a real-socket test against a local
-- listener observes the same admission behaviour a deployed runtime would.
concurrencyLimitedMiddleware :: Maybe RequestConcurrencyLimit -> Wai.Middleware -> IO Wai.Middleware
concurrencyLimitedMiddleware maybeLimit waiMiddleware =
  case maybeLimit of
    Nothing -> pure waiMiddleware
    Just limit -> do
      gate <- newRequestConcurrencyGate limit
      pure (concurrencyGateMiddleware gate . waiMiddleware)

-- | Per-public-WAI-adapter gate state for route-local execution limits. A
-- gate is installed only when a bounded route is first selected, and then
-- reused for that route's later requests. This cache is deliberately below
-- the global gate: it cannot affect listener admission, request-head
-- validation, or middleware that already ran to provide route context.
newtype RouteConcurrencyGateCache route = RouteConcurrencyGateCache (MVar [(route, Wai.Middleware)])

newRouteConcurrencyGateCache :: IO (RouteConcurrencyGateCache route)
newRouteConcurrencyGateCache = RouteConcurrencyGateCache <$> newMVar []

routeConcurrencyMiddleware :: (Eq route) => RouteConcurrencyGateCache route -> route -> Maybe RequestConcurrencyLimit -> IO Wai.Middleware
routeConcurrencyMiddleware _ _ Nothing = pure id
routeConcurrencyMiddleware (RouteConcurrencyGateCache cache) routeValue (Just limit) =
  modifyMVar cache $ \entries ->
    case lookup routeValue entries of
      Just middleware -> pure (entries, middleware)
      Nothing -> do
        middleware <- concurrencyLimitedMiddleware (Just limit) id
        pure ((routeValue, middleware) : entries, middleware)

data RequestConcurrencyGate = RequestConcurrencyGate
  { concurrencyGateLimit :: Int,
    concurrencyGateInFlight :: IORef Int
  }

newRequestConcurrencyGate :: RequestConcurrencyLimit -> IO RequestConcurrencyGate
newRequestConcurrencyGate limit =
  RequestConcurrencyGate (requestConcurrencyLimitValue limit) <$> newIORef 0

-- | Admit at most the configured number of requests at once, across every
-- listener sharing this gate. Admission is a non-blocking, immediate
-- accept-or-reject rather than a queue: an exceeded gate returns a stable
-- '503' before route parsing, middleware, observability, or body reads,
-- rather than making a caller wait for a slot. A slot is held for the
-- request's whole lifetime, including a streamed response, and always
-- released — on ordinary completion or any exception. Acquisition and
-- installing that cleanup happen under 'mask', so an async exception cannot
-- arrive in between and permanently consume capacity. In particular, an SSE
-- response holds its slot for the lifetime of its stream: a finite
-- 'requestConcurrencyLimit' therefore reserves that many connections across
-- ordinary requests and streams together.
concurrencyGateMiddleware :: RequestConcurrencyGate -> Wai.Middleware
concurrencyGateMiddleware gate app request respond =
  mask $ \restore -> do
    admitted <- acquireConcurrencySlot gate
    if admitted
      then restore (app request respond) `finally` releaseConcurrencySlot gate
      else restore (respond concurrencyLimitResponse)

acquireConcurrencySlot :: RequestConcurrencyGate -> IO Bool
acquireConcurrencySlot gate =
  atomicModifyIORef' (concurrencyGateInFlight gate) $ \inFlight ->
    if inFlight < concurrencyGateLimit gate
      then (inFlight + 1, True)
      else (inFlight, False)

releaseConcurrencySlot :: RequestConcurrencyGate -> IO ()
releaseConcurrencySlot gate =
  atomicModifyIORef' (concurrencyGateInFlight gate) (\inFlight -> (inFlight - 1, ()))

concurrencyLimitResponse :: Wai.Response
concurrencyLimitResponse =
  Wai.responseLBS
    Http.status503
    [(Http.hContentType, "text/plain; charset=utf-8")]
    "Too many concurrent requests."
