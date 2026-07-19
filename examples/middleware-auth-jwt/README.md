# middleware-auth-jwt

**Status:** In progress

Show the desired app authoring model for:

- middleware with an allowlist of public paths,
- a protected page backed by an opaque, server-side session,
- host-only secure session cookies and synchronizer CSRF tokens,
- application-owned session lookup and invalidation.

Current repo alignment:

- `HarchWeb.Session` supplies typed opaque session identifiers, expiry validation, invalidation
  seams, strict cookie rendering, same-origin return paths, and CSRF token comparison;
- the application supplies a cryptographically secure token generator and durable session store;
- credential verification, password hashing, throttling, rate limiting, and audit hooks remain
  follow-up work before this becomes a complete login example.

Suggested snippet:

- [src/App/Middleware/Auth.hs.md](src/App/Middleware/Auth.hs.md)
