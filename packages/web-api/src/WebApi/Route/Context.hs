{-# LANGUAGE OverloadedStrings #-}

-- | Trusted application request-context construction.
--
-- Decision (AHI-4D-MH2, 2026-09-19): this module owns the explicit context
-- value and its WAI ingress enrichment. 'WebApi.Route' retains the one closed
-- route codec, parsing folds, and endpoint declarations. Keeping those folds
-- together avoids a second route matcher; keeping ingress enrichment here
-- prevents route declarations from owning cookie decoding or peer attribution.
module WebApi.Route.Context
  ( AppAuthorization,
    AppLocale (..),
    AppRequestContext (..),
    RequestAuthenticationTransport (..),
    accountAuthenticationProfileName,
    resourceAuthenticationProfileName,
    resourceReadScope,
    requiredOAuth2ScopeOrDie,
    defaultRequestContext,
    requestContextFromWaiRequest,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb qualified
import HarchWeb.Session
  ( SessionId,
    mkSessionId,
    sessionCookieName,
    sessionCookieNameText,
  )
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import WebApi.AccountPrincipal (AccountPrincipal)
import WebApi.Session (mfaEnrollmentSessionCookiePolicy)

-- | The one @authorization@ payload every 'AppRoute' endpoint declaration,
-- application, and authentication pipeline in this module shares.
-- 'HarchWeb.RequireAuthorized' is not used by any route yet — every existing
-- declaration still resolves through 'HarchWeb.AllowUnauthenticated' or
-- 'HarchWeb.RequireAuthenticated', so widening this one shared type
-- parameter from @()@ is a pure type-signature change with no behavior
-- difference. It exists ahead of its first user (the AHI-4D combined
-- account-or-API-client-bearer profile securing @\/api\/second@) so that
-- follow-up work extends one already-published type instead of widening it
-- and every call site a second time; see the AHI-4D decision record in
-- @docs\/design-guidance.md@.
type AppAuthorization = HarchWeb.ScopeRequirement HarchWeb.OAuth2Scope

-- | The production account profile owns cookie-or-bearer JWT admission. Only
-- account-protected declarations select it; public declarations inherit the
-- public root profile.
accountAuthenticationProfileName :: HarchWeb.AuthenticationProfileName
accountAuthenticationProfileName = HarchWeb.requiredAuthenticationProfileNameOrDie "account"

-- | The AHI-4D combined account-or-API-client-bearer profile
-- ('WebApi.ResourceAuthentication.resourceAuthenticationPipeline'), declared
-- here rather than in that module: the pipeline itself never needs its own
-- registered name (mirroring 'accountAuthenticationProfileName', which
-- 'WebApi.AccountJwt' likewise never references), and 'WebApi.ResourceAuthentication'
-- already imports this module for 'AppRoute'\/'AppRequestContext'\/
-- 'AppAuthorization' — defining the name there too would import this module
-- back into it.
resourceAuthenticationProfileName :: HarchWeb.AuthenticationProfileName
resourceAuthenticationProfileName = HarchWeb.requiredAuthenticationProfileNameOrDie "resource"

-- | The scope an API-client bearer token must carry (directly, or via a
-- current durable allowance that still intersects it; see
-- 'WebApi.ApiClient.intersectEstablishedApiClientScopes') to reach
-- @GET \/api\/second@. An authenticated account principal needs no scope at
-- all; see 'WebApi.ResourceAuthentication's authorization interpreter.
resourceReadScope :: HarchWeb.OAuth2Scope
resourceReadScope = requiredOAuth2ScopeOrDie "resource:read"

-- | Unwrap a statically-known-valid OAuth scope literal, or crash naming the
-- offending declaration. Exported (like 'HarchWeb.requiredEndpointNameOrDie')
-- so its error rail can be exercised directly against a genuinely invalid
-- literal instead of only through 'resourceReadScope', which is reviewed to
-- never trigger it.
requiredOAuth2ScopeOrDie :: Text -> HarchWeb.OAuth2Scope
requiredOAuth2ScopeOrDie value =
  either (\scopeError -> error ("invalid OAuth scope declaration " <> show value <> ": " <> show scopeError)) id (HarchWeb.mkOAuth2Scope value)

data AppLocale
  = English
  | Spanish
  deriving (Eq, Show)

data AppRequestContext = AppRequestContext
  { requestLocale :: AppLocale,
    requestLocaleIsExplicit :: Bool,
    requestCorrelationId :: Maybe HarchWeb.RequestId,
    -- | Trusted attribution attached only after the root has selected a route
    -- and endpoint declaration.  Request paths and action input cannot supply
    -- facts for the application-owned audit projection.
    requestRouteObservation :: Maybe HarchWeb.RouteObservation,
    requestClientAddress :: HarchWeb.ClientAddress,
    requestPathPrefix :: HarchWeb.PathPrefix,
    requestQueryParameters :: [(Text, Text)],
    requestAccountPrincipal :: Maybe AccountPrincipal,
    -- | The established account-JWT transport, never raw credential bytes.
    -- It is set only by the post-match authentication rail and lets the
    -- action lifecycle choose CSRF from the credential actually accepted.
    requestAuthenticationTransport :: RequestAuthenticationTransport,
    requestMfaEnrollmentSessionId :: Maybe SessionId
  }
  deriving (Eq, Show)

-- | The source(s) of a successfully established account credential. The
-- cookie-participating cases remain distinct from bearer-only so an action
-- cannot suppress CSRF merely because the same JWT was also sent explicitly.
data RequestAuthenticationTransport
  = NoRequestAuthentication
  | AccountJwtFromCookie
  | AccountJwtFromBearer
  | AccountJwtFromCookieAndBearer
  deriving (Eq, Show)

defaultRequestContext :: AppRequestContext
defaultRequestContext =
  AppRequestContext
    { requestLocale = English,
      requestLocaleIsExplicit = False,
      requestCorrelationId = Nothing,
      requestRouteObservation = Nothing,
      requestClientAddress = HarchWeb.defaultClientAddress,
      requestPathPrefix = HarchWeb.emptyPathPrefix,
      requestQueryParameters = [],
      requestAccountPrincipal = Nothing,
      requestAuthenticationTransport = NoRequestAuthentication,
      requestMfaEnrollmentSessionId = Nothing
    }

requestContextFromWaiRequest :: HarchWeb.RequestPolicyConfig -> Wai.Request -> HarchWeb.RequestId -> AppRequestContext -> AppRequestContext
requestContextFromWaiRequest requestPolicyConfig request requestId requestContext =
  requestContext
    { requestPathPrefix =
        HarchWeb.requestPathPrefix requestPolicyConfig request,
      requestCorrelationId = Just requestId,
      requestClientAddress = HarchWeb.requestClientAddress requestPolicyConfig request,
      requestMfaEnrollmentSessionId = sessionIdFromCookieHeaders (sessionCookieNameText (sessionCookieName mfaEnrollmentSessionCookiePolicy)) (Wai.requestHeaders request)
    }

sessionIdFromCookieHeaders :: Text -> Http.RequestHeaders -> Maybe SessionId
sessionIdFromCookieHeaders cookieName headers = do
  cookieHeader <- lookup "Cookie" headers
  cookieText <- either (const Nothing) Just (TextEncoding.decodeUtf8' cookieHeader)
  cookieValue <- lookup cookieName (map parseCookiePair (Text.splitOn ";" cookieText))
  mkSessionId cookieValue
  where
    parseCookiePair value =
      let (name, rawValue) = Text.breakOn "=" (Text.strip value)
       in (name, Text.drop 1 rawValue)
