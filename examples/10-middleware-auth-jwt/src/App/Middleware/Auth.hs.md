# src/App/Middleware/Auth.hs

```hs
module App.Middleware.Auth where

authMiddleware :: Middleware
authMiddleware =
  requireAuth
    { allowlist = ["/", "/login", "/assets"]
    , loginCookieName = "session"
    , cookieSecurity =
        CookieSecurity
          { httpOnly = True
          , secure = True
          , sameSite = Strict
          , path = "/"
          }
    }
```

The final framework story should make this kind of middleware declarative, while still leaving
custom auth policy in application code.
