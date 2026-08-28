# ADR-DQ: Attribute peer addresses before TLS setup

- Status: **Accepted and approved — pending required published Warp hook**
- Task: [DQ — connection-address attribution](../../TASKS/pr-3-request-pipeline-transport-and-static-assets.md)
- Date: 2026-08-26

## Executive problem statement

TLS connection-failure telemetry can report the previous connection's IP address as the current
peer. Warp accepts a socket before it forks its worker, while this application records the address
later in `onOpen`; the FIFO intended to bridge those events is therefore one connection behind.
Wrong identity is worse than missing identity for incident response, privacy, and rate-abuse
analysis. The correct fix needs the accepted `SockAddr` in the worker before WarpTLS performs the
handshake. Warp has that accepted address at its worker exception boundary, but its public
settings API does not expose it to applications.

Decision made: obtain a pre-handshake accepted-peer hook without taking ownership of WarpTLS's
complete TLS and HTTP/2 server loop, through Warp's public API.

## Design guidance that constrains the decision

- **Extend the existing boundary.** `HarchWeb.Server.Transport` already owns listener startup and
  connection telemetry. The fix belongs at that boundary, not in a second reporter or server.
- **Add a small general primitive when possible.** The missing-capability protocol prefers an
  additive Warp hook over an application workaround or a parallel TLS implementation.
- **Flag materially different behavior.** Inventing an address or correlating independent queues
  changes the correctness property and is not an acceptable fallback.
- **Keep observability truthful and bounded.** Stable event codes may remain; peer attributes must
  be present only when tied to the actual accepted socket. More diagnostic logging cannot repair
  bad correlation.
- **Do not call a partial slice complete.** Omitting the peer address is a safe mitigation for false
  attribution, but it does not complete DQ's pre-handshake attribution requirement.

## Current evidence

- The workspace resolves Warp 3.4.12 and warp-tls 3.4.9.
- [`Transport.hs`](../../packages/harch-web/src/HarchWeb/Server/Transport.hs) calls `setFork` to
  claim from a FIFO before `setOnOpen` appends the accepted address.
- A real `127.0.0.2` plaintext connection to the TLS listener proved WarpTLS rejects the connection
  before `onOpen`; a thread map populated only there cannot report that rejected peer.
- [Warp 3.4.12](https://github.com/yesodweb/wai/blob/warp-3.4.12/warp/Network/Wai/Handler/Warp/Run.hs)
  accepts the socket address and catches connection setup/serving failures in the same worker, but
  invokes only the address-less public `setOnException` callback. WarpTLS 3.4.14 continues to use
  that runner; changing `tls` or `warp-tls` versions therefore does not fix this ordering.
- Rechecked 2026-08-28 against upstream Warp `main` (commit `62a8b49b62a4`): `Settings` still
  exposes address-less `settingsOnException` / `setOnException` and the peer-bearing lifecycle
  hooks remain `onOpen` and `onClose`. There is no compatible published release or open upstream
  proposal for a peer-aware worker exception hook. DQ consequently remains blocked by the required
  external release; no local source pin, fork, or TCP/TLS reimplementation is permitted as a
  substitute.

## Options and consequences

### Option A — Add a peer-aware connection-exception hook upstream in Warp

Propose an additive `setOnConnectionException :: (SockAddr -> SomeException -> IO ()) -> Settings
-> Settings` (or equivalent) in Warp. Warp invokes it once from its existing outer worker handler
when setup or serving fails; existing `setOnException` behavior remains unchanged. Adopt only a
published Hackage Warp release carrying that hook.

Consequences:

- Preserves WarpTLS ownership of TLS negotiation, HTTP/2 ALPN, session management, timeouts, socket
  cleanup, and future compatibility.
- Gives Harch Web the accepted peer directly for rejected handshakes without an address queue.
- Avoids a WarpTLS fork, local connection maker, source pin, and forced WarpTLS version pin.
- The hook contract must cover asynchronous exceptions and run exactly once per escaped connection
  failure.

### Option B — Carry a permanent narrow fork or source pin

Maintain the same small hook as a repository-owned fork and pin it indefinitely.

Consequences:

- Completes DQ without waiting for an upstream release.
- Makes this project responsible for tracking every WarpTLS security and protocol update.
- Creates a durable supply-chain and release-maintenance obligation for one observability feature.

### Option C — Reimplement the WarpTLS connection maker in Harch Web

Call Warp's low-level `runSettingsConnectionMakerSecure` directly and reproduce WarpTLS's private
credential, TLS, HTTP/2, buffer, timeout, and socket-cleanup logic locally.

Consequences:

- Provides full control and no forked package.
- Duplicates a security-sensitive server loop and couples Harch Web to Warp internals.
- Requires ongoing parity tests and security maintenance; this is a new transport implementation,
  not a small extension of the existing boundary.

### Option D — Remove the FIFO and omit pre-handshake peer attributes

Use a worker-thread map populated by `onOpen` for connections that reach that hook. Report rejected
pre-handshake events without `client.address` or `network.peer.address`.

Consequences:

- Immediately stops false attribution and removes the FIFO's quadratic behavior.
- Preserves the event and exception diagnostics without fabricating identity.
- Loses the peer attributes on the failures DQ specifically needs to attribute, so DQ remains open.

### Option E — Keep the FIFO and add more logging

Consequences:

- Retains demonstrably false peer identity.
- Additional events cannot establish causality between the accept and worker hooks.
- Rejected.

## Recommendation

Adopt **Option A**. All implementation work below is approved. DQ awaits only the required
external prerequisite: a compatible published Warp release containing the hook. There is no source
pin, fork, local TLS connection maker, or temporary fallback that claims a peer address. The current
FIFO is known incorrect and must not be treated as a completed solution.

This recommendation keeps TLS mechanics in WarpTLS and gives the framework a small, general Warp
primitive for accurate connection-level telemetry without a permanent fork.

## Required approved execution order

1. Specify and submit the focused Warp worker-exception hook with its exactly-once semantics.
2. Wait for a compatible published Warp release; do not pin an unreleased revision.
3. Wire the hook into `HarchWeb.Server.Transport`, remove the FIFO/list tracker and `setFork` /
   `setOnOpen` / `setOnClose` bridge, without changing event names or public response
   behavior.
4. Prove `127.0.0.1` and `127.0.0.2` cannot cross-attribute under sequential and concurrent TLS,
   plaintext-on-TLS, premature-close, and asynchronous-exception cases.
5. Complete DT's TLS/cipher configuration and real transport proof as specified in
   [ADR-DT](dt-configurable-modern-tls-server-policy.md).
6. Run the complete CI-equivalent and module-health gates before committing and pushing, then close
   the affected task groups.

No additional architectural approval is required. The published Warp release is a technical
prerequisite, not a decision gate.
