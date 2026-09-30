-- | Internal peer-handoff validation hooks.
--
-- This module is exposed only so the package's real socket tests can exercise
-- Warp lifecycle-contract failures. It is not a supported framework API.
module HarchWeb.Server.Transport.Internal
  ( ActiveConnectionAddresses,
    acceptTrackedConnection,
    clearPendingAddressForAcceptLoopFailure,
    forkTrackedConnection,
    newActiveConnectionAddresses,
    openLoopbackSocket,
    recordAcceptLoopThread,
    socketPort,
  )
where

import HarchWeb.Server.Transport
  ( ActiveConnectionAddresses,
    acceptTrackedConnection,
    clearPendingAddressForAcceptLoopFailure,
    forkTrackedConnection,
    newActiveConnectionAddresses,
    openLoopbackSocket,
    recordAcceptLoopThread,
    socketPort,
  )
