# ADR-DT: Configurable modern TLS server policy

- Status: **Accepted and approved — completes after DQ integration**
- Task: [DT — configurable modern TLS policy](../../TASKS/pr-3-correctness-and-security-defects.md)
- Date: 2026-08-26

## Executive problem statement

`tls-1.9.0` defaults allow TLS 1.0/1.1 and compatibility cipher suites that a modern web listener
does not need. Operators must be secure by default while retaining an explicit, validated escape
hatch for a deployment that genuinely requires an older client.

## Design guidance

- Extend the existing listener `TlsConfig` and its bind plans; do not add a second runtime policy.
- Parse untrusted environment values before listener startup into closed ADTs. Invalid values must
  fail with their configuration key and value, never fall back to a weaker package default.
- Apply one policy to manual, shared-certificate, and ACME TLS paths. Certificate reload and SNI
  remain certificate-lifecycle concerns, not policy selectors.

## Options and consequences

### Option A — Safe defaults with explicit versions and cipher overrides

Default to TLS 1.2/1.3 and a browser-oriented AEAD/PFS suite list. Expose closed, listener-scoped
version and IANA cipher-suite lists, validating that each selected version has a compatible cipher.

- Secure normal deployments without relying on library defaults.
- Allows legacy compatibility only by deliberate selection of both an old protocol and old suite.
- Adds a documented, versioned configuration surface.

### Option B — Rely on `tls` defaults

- Requires no configuration work.
- Retains TLS 1.0/1.1 and legacy suite exposure until a package upgrade; rejected.

### Option C — Hard-code modern policy with no override

- Smallest surface and strongest guarantee.
- Prevents a deployment owner from supporting a known legacy client; rejected.

## Recommendation

Adopt Option A. `LISTENER_<n>_TLS_ALLOWED_VERSIONS` defaults to `1.2,1.3`;
`LISTENER_<n>_TLS_CIPHER_SUITES` defaults to TLS 1.3 AES-GCM/ChaCha20 and TLS 1.2 ECDHE
RSA/ECDSA AES-GCM/ChaCha20. Both are non-empty closed lists. Explicit TLS 1.0/1.1 requires an
explicit compatible legacy cipher. Package upgrades remain separately HSEC-driven work.

## Verification

Configuration tests prove defaults, a compatible legacy override, and empty/duplicate/unknown/
incompatible rejection. Runtime transport settings are derived exclusively from the validated bind
plan. Future transport tests must prove allowed TLS 1.2/1.3 handshakes and default rejection of
legacy protocol/CBC attempts with a real client.

## Required cross-task ordering

All design and implementation work is approved. Follow [ADR-DQ](dq-pre-tls-peer-address-attribution.md)
in this required order: the peer-aware Warp hook reaches a compatible published release, DQ is
integrated and regression-proven, this TLS/cipher policy receives its real transport proof, then the
normal gates run and the affected task groups close. The Warp release is a technical prerequisite,
not a remaining architectural approval.
