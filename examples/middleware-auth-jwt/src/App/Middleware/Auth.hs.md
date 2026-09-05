# src/App/Middleware/Auth.hs

```hs
module App.Middleware.Auth where

authMiddleware :: Middleware
authMiddleware =
  requireAuth
    { allowlist = ["/", "/login", "/assets"]
    , sessionLookup = postgresSessionLookup
    , sessionCookiePolicy = defaultSessionCookiePolicy
    }
```

`postgresSessionLookup` is application code that persists `OpaqueSession` records by their
unpredictable `SessionId` and invalidates them on logout. A successful login generates a fresh
session identifier and a separate synchronizer CSRF token, then renders the session identifier
through `renderSessionCookie`. The CSRF token stays in server-side session state and is rendered
only into same-origin forms.

The final framework story should make this kind of middleware declarative, while leaving session
storage, credential verification, and audit policy in application code.
