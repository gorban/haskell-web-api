-- | The application-owned principal established only after a JWT has been
-- verified and its referenced durable account session is still active.
--
-- AHI-4C deliberately carries the session identifier only inside this
-- post-guard value.  Parsing a browser cookie into request context would make
-- an unverified bearer credential look like an application grant before the
-- authentication rail had run.
module WebApi.AccountPrincipal
  ( AccountPrincipal,
    accountPrincipalAccountId,
    accountPrincipalSessionExpiresAtNanoseconds,
    accountPrincipalSessionId,
    mkAccountPrincipal,
  )
where

import HarchWeb.Account (AccountId)
import HarchWeb.Session (SessionId)
import HarchWeb.Time (UnixTimeNanoseconds)

data AccountPrincipal = AccountPrincipal
  { accountPrincipalAccountId :: AccountId,
    accountPrincipalSessionId :: SessionId,
    accountPrincipalSessionExpiresAtNanoseconds :: UnixTimeNanoseconds
  }
  deriving (Eq)

-- | Account and session identifiers are security-sensitive correlation data.
-- Do not make a request-context diagnostic reveal either one.
instance Show AccountPrincipal where
  show _ = "AccountPrincipal <redacted>"

mkAccountPrincipal :: AccountId -> SessionId -> UnixTimeNanoseconds -> AccountPrincipal
mkAccountPrincipal = AccountPrincipal
