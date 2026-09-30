-- | Password and MFA login facade.
--
-- Decision (FQ6, 2026-08-29): callers build one
-- 'PasswordLoginEnvironment' for the password stage and extend it with a
-- 'SecondFactorContext' for a proof. Private password, MFA, and reservation
-- modules own their stable lifecycle responsibilities; this facade remains
-- the single application-facing protocol and does not add a second
-- interpretation boundary. See @docs/design-guidance.md@ for the decision.
module WebApi.Login
  ( module WebApi.Login.Types,
    beginPasswordLogin,
    beginPasswordLoginWithIdentifier,
    completePasswordLogin,
    completePasswordLoginWithIdentifier,
    requiredPasswordHashOrDie,
  )
where

import WebApi.Login.Mfa (completePasswordLogin, completePasswordLoginWithIdentifier)
import WebApi.Login.Password (beginPasswordLogin, beginPasswordLoginWithIdentifier, requiredPasswordHashOrDie)
import WebApi.Login.Types
