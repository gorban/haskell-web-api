# middleware-auth-jwt

**Status:** Implemented guide

Show the implemented application model for:

- middleware with an allowlist of public paths,
- a protected page backed by an opaque, server-side session,
- host-only secure session cookies and synchronizer CSRF tokens,
- application-owned session lookup and invalidation.

This repository deliberately uses opaque server-side sessions rather than JWT
cookies: browser cookies contain only a random identifier, while principal,
expiry, invalidation, and CSRF state remain server-side.

Current repo alignment:

- `HarchWeb.Session` supplies typed opaque session identifiers, expiry validation, invalidation
  seams, strict cookie rendering, same-origin return paths, and CSRF token comparison;
- the application supplies cryptographically secure token generation, PostgreSQL-backed session
  persistence, credential verification, Argon2id hashing, bounded login throttling/rate limiting,
  and audit hooks;
- `packages/web-api` provides the working localized registration, login, logout, and session-backed
  profile flow, including browser coverage for authenticated profile rendering and logout.

See the working composition points:

- [session primitives](../../packages/harch-web/src/HarchWeb/Session.hs)
- [session-backed profile resolution](../../packages/web-api/src/WebApi/Profile.hs)
- [account actions and safe response handling](../../packages/web-api/src/WebApi/AccountPages.hs)
