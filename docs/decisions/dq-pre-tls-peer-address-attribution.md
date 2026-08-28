# ADR-DQ: Attribute peer addresses before TLS setup

- Status: **Implemented — local CI-equivalent validation passed; GitHub Actions pending**
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

Decision made: pair Warp's existing public `setAccept` and `setFork` hooks. `setAccept` records the
kernel-provided peer immediately after accept; `setFork` transfers that single peer to the worker
before WarpTLS runs its connection maker. This extends the existing transport boundary without
forking, pinning, or reimplementing WarpTLS's TLS and HTTP/2 server loop.

## Design guidance that constrains the decision

- **Extend the existing boundary.** `HarchWeb.Server.Transport` already owns listener startup and
  connection telemetry. The fix belongs at that boundary, not in a second reporter or server.
- **Extend the existing boundary first.** Warp's documented accept and worker-factory hooks already
  provide the required ordering, so an upstream API addition or a parallel TLS implementation is
  unnecessary.
- **Flag materially different behavior.** Inventing an address or correlating independent queues
  changes the correctness property and is not an acceptable fallback.
- **Keep observability truthful and bounded.** Stable event codes may remain; peer attributes must
  be present only when tied to the actual accepted socket. More diagnostic logging cannot repair
  bad correlation.
- **Do not call a partial slice complete.** Omitting the peer address is a safe mitigation for false
  attribution, but it does not complete DQ's pre-handshake attribution requirement.

## Current evidence

- The supported workspace resolves Warp 3.4.12 and warp-tls 3.4.9. Warp 3.4.12's public
  `setAccept` is invoked before `setFork`; Warp's masked accept loop does not begin another accept
  until the worker factory returns.
- `setAccept` records exactly one accepted `SockAddr` in a non-blocking, one-place handoff.
  `setFork` claims it before launching the worker and registers `ThreadId -> SockAddr` before
  WarpTLS can perform TLS setup. A full or empty handoff is a clear lifecycle-contract failure and
  never creates an event with a guessed peer.
- The normal exception callback clears an unclaimed handoff only when it is running on the recorded
  accept-loop thread. A worker failure cannot clear a later connection's handoff.
- Real source-bound `127.0.0.1`/`127.0.0.2` sequential and concurrent plaintext-on-TLS and
  premature-close tests, preceded by an injected asynchronous worker failure, prove every
  recognized event uses that connection's accepted TCP peer.

## Options and consequences

### Option A — Use public accept and worker-factory hooks

Use `setAccept` to record the accepted peer and `setFork` to attach that peer to the worker before
the existing WarpTLS connection maker runs. Keep the association in a one-place, non-blocking
handoff plus a worker-thread map.

Consequences:

- Preserves WarpTLS ownership of TLS negotiation, HTTP/2 ALPN, session management, timeouts, socket
  cleanup, and future compatibility.
- Gives Harch Web the accepted peer directly for rejected handshakes without an address queue.
- Avoids a WarpTLS fork, local connection maker, source pin, forced WarpTLS version pin, and
  upstream coordination.
- Detects an upstream lifecycle-order change safely rather than correlating independent queues.

### Option B — Carry a permanent narrow Warp fork or source pin

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

Adopt **Option A**. It uses the stable public Warp surface at the point the kernel peer is known,
keeps TLS mechanics in WarpTLS, and has no third-party source ownership. The FIFO is deleted rather
than retained as a fallback: false peer identity is worse than no peer identity.

## Required approved execution order

1. Wire `setAccept`/`setFork` into `HarchWeb.Server.Transport`, remove the FIFO/list tracker and
   `setOnOpen` / `setOnClose` bridge, without changing event names or public response behavior.
2. Prove `127.0.0.1` and `127.0.0.2` cannot cross-attribute under sequential and concurrent TLS,
   plaintext-on-TLS, premature-close, and asynchronous-exception cases.
3. Complete DT's TLS/cipher configuration and real transport proof as specified in
   [ADR-DT](dt-configurable-modern-tls-server-policy.md).
4. Run the complete CI-equivalent and module-health gates before committing and pushing, then close
   the affected task groups.

No additional architectural approval is required.
