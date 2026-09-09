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

## Session expiry and confidential-page locking

Session expiry, logout, and revocation prevent the server from authorizing a
subsequent protected request. They cannot retract a protected document that is
already rendered in a browser tab, copied to the clipboard, captured in a
screenshot, or otherwise accessed on an unlocked device.

An application that needs a shorter visible-content policy can add its own
screen lock: hide confidential regions, make their underlying controls
inaccessible, and require the application's ordinary reauthentication flow
before restoring the regions. The application owns the trigger (for example,
an idle timeout, a visibility change, or a server-authentication challenge),
the presentation, accessibility focus handling, and the decision to reload or
refresh its protected data. Harch's bounded retained-action reauthentication
mechanism is not a screen lock and does not automatically clear rendered
content.

This is documented guidance, **not an implemented screen-lock feature in any
example**. It does not protect against a person with access to the browser or
device, cached/copied content, screenshots, browser extensions, or an already
compromised client. Treat it as a product-level confidentiality measure layered
on top of server-side authentication, not a replacement for it.
