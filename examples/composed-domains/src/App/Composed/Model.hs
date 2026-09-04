{-# LANGUAGE OverloadedStrings #-}

-- | Closed root values and locale policy owned by the composed-domains
-- application. Domain packages depend on none of these values: the root maps
-- their local routes, actions, contexts, and policy values at an explicit
-- mount boundary.
module App.Composed.Model
  ( AdmissionPrincipal,
    AdmissionReturnTarget (..),
    ComposedContext,
    LocalePolicy (..),
    LocaleResolutionInput (..),
    LocalizedRoute (..),
    PublicRoute (..),
    RootAction (..),
    RootActionTarget (..),
    RootAuthorization (..),
    RootClient (..),
    RootLocal (..),
    RootPrincipal (..),
    RootRoute (..),
    allowedLocale,
    admissionReturnTargetRoute,
    defaultComposedContext,
    defaultComposedStaticAssets,
    defaultLocalePolicy,
    principalLocalePreference,
    principalScopes,
    mkAdmissionReturnTarget,
    rootClientAddress,
    resolveLocale,
  )
where

import App.Composed.Admission.Types (AdmissionLoginName, AdmissionPrincipal)
import Catalog.Domain
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Localization (Locale, locale, localeText)
import HarchWeb.RequestContext
  ( CoreRequestContext (..),
    RequestContext (..),
    RequestIdentity (..),
    correlationContext,
    requiredCanonicalOriginOrDie,
  )
import HarchWeb.Security (ClientAddress, defaultClientAddress, emptyPathPrefix)
import HarchWeb.StaticAssets
  ( StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultStaticAssetContentTypes,
  )
import HarchWeb.StaticAssets.Route (StaticAssetRoute)
import HarchWeb.Totp (TotpCode)
import Orders.Domain

data PublicRoute
  = PublicAdmission
  | PublicAdmissionNativeFallback
  | PublicLogin
  | PublicAsset StaticAssetRoute
  | PublicNotFound
  deriving (Eq, Show)

-- | The admission form accepts a closed return target, never a browser URL.
-- Its present vocabulary intentionally has one target; a later public route
-- must extend this ADT and its pure rendering rather than reintroduce an
-- unvalidated redirect string.
data AdmissionReturnTarget = ReturnToAccountLogin
  deriving (Eq, Show)

mkAdmissionReturnTarget :: Text -> Maybe AdmissionReturnTarget
mkAdmissionReturnTarget value =
  case value of
    "login" -> Just ReturnToAccountLogin
    _ -> Nothing

data LocalizedRoute
  = Public PublicRoute
  | Catalog CatalogRoute
  | Orders OrdersRoute
  deriving (Eq, Show)

admissionReturnTargetRoute :: AdmissionReturnTarget -> LocalizedRoute
admissionReturnTargetRoute returnTarget =
  case returnTarget of
    ReturnToAccountLogin -> Public PublicLogin

data RootRoute = Localized Locale LocalizedRoute
  deriving (Eq, Show)

data RootActionTarget
  = AdmissionActionTarget
  | CatalogActionTarget CatalogActionTarget
  | OrdersActionTarget OrdersActionTarget
  deriving (Eq, Show)

data RootAction
  = SubmitAdmission AdmissionLoginName TotpCode AdmissionReturnTarget
  | CatalogAction CatalogAction
  | OrdersAction OrdersAction
  deriving (Eq, Show)

data RootAuthorization
  = RootMayReadCatalog
  | RootMayRefreshCatalog
  | RootMayReadOrders
  | RootMaySubmitOrders
  deriving (Eq, Show)

data RootPrincipal = RootPrincipal
  { rootPrincipalLocalePreference :: Maybe Locale,
    rootPrincipalScopes :: [Text]
  }
  deriving (Eq, Show)

data RootClient
  = BrowserClient
  | OtherClient
  | TrustedNetworkClient RootClient ClientAddress
  deriving (Eq)

instance Show RootClient where
  show rootClient =
    case rootClient of
      BrowserClient -> "BrowserClient"
      OtherClient -> "OtherClient"
      TrustedNetworkClient client _ -> "TrustedNetworkClient " <> show client <> " <redacted>"

-- | The trusted forwarding resolver runs once when the site constructs the
-- request context. Synthetic unit contexts intentionally use Harch's
-- loopback test identity; application code never turns a request header into
-- a peer budget directly.
rootClientAddress :: RootClient -> ClientAddress
rootClientAddress rootClient =
  case rootClient of
    TrustedNetworkClient _ clientAddress -> clientAddress
    BrowserClient -> defaultClientAddress
    OtherClient -> defaultClientAddress

-- | Account identity remains in 'requestIdentity'.  A successful admission
-- guard enriches only this separate local axis, so it cannot grant account
-- scopes or be mistaken for an account-MFA/session result.
data RootLocal
  = RootLocal
  | AdmissionEstablished AdmissionPrincipal
  deriving (Eq)

instance Show RootLocal where
  show rootLocal =
    case rootLocal of
      RootLocal -> "RootLocal"
      AdmissionEstablished _ -> "AdmissionEstablished <redacted>"

type ComposedContext = RequestContext (RequestIdentity RootPrincipal) RootClient RootLocal

data LocalePolicy = LocalePolicy
  { supportedLocales :: NonEmpty Locale,
    defaultLocale :: Locale
  }
  deriving (Eq, Show)

data LocaleResolutionInput = LocaleResolutionInput
  { localeExplicitPrefix :: Maybe Locale,
    localeCookieValue :: Maybe Text,
    localeAcceptLanguage :: Maybe Text,
    localeIdentity :: RequestIdentity RootPrincipal
  }
  deriving (Eq, Show)

defaultLocalePolicy :: LocalePolicy
defaultLocalePolicy = LocalePolicy (locale "en" :| [locale "es"]) (locale "en")

defaultComposedStaticAssets :: StaticAssetsConfig
defaultComposedStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [StaticAssetRoot "/public/assets" "public-assets"],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Just 300
    }

defaultComposedContext :: ComposedContext
defaultComposedContext =
  RequestContext
    { requestCore =
        CoreRequestContext
          { requestLocale = defaultLocale defaultLocalePolicy,
            requestLocaleFallbacks = [defaultLocale defaultLocalePolicy],
            requestRouteObservation = Nothing,
            requestCorrelation = correlationContext Nothing,
            requestCanonicalOrigin = requiredCanonicalOriginOrDie "https://composed.example.test",
            requestPathPrefix = emptyPathPrefix
          },
      requestIdentity = AnonymousIdentity,
      requestClient = BrowserClient,
      requestLocal = RootLocal
    }

-- | Explicit URL locale wins. On an unprefixed request, a durable principal
-- preference wins over the cookie/header/default chain before a child context
-- projection receives the root's safe core.
resolveLocale :: LocalePolicy -> LocaleResolutionInput -> Locale
resolveLocale localePolicy input =
  fromMaybe
    ( fromMaybe
        ( fromMaybe
            (fromMaybe (defaultLocale localePolicy) (localeAcceptLanguage input >>= acceptedLocale localePolicy))
            (localeCookieValue input >>= allowedLocale localePolicy)
        )
        (principalLocalePreference (localeIdentity input))
    )
    (localeExplicitPrefix input)

principalLocalePreference :: RequestIdentity RootPrincipal -> Maybe Locale
principalLocalePreference identity =
  case identity of
    AnonymousIdentity -> Nothing
    AuthenticatedIdentity principal -> rootPrincipalLocalePreference principal

principalScopes :: RequestIdentity RootPrincipal -> [Text]
principalScopes identity =
  case identity of
    AnonymousIdentity -> []
    AuthenticatedIdentity principal -> rootPrincipalScopes principal

allowedLocale :: LocalePolicy -> Text -> Maybe Locale
allowedLocale localePolicy value =
  listToMaybe [candidate | candidate <- NonEmpty.toList (supportedLocales localePolicy), localeText candidate == value]

acceptedLocale :: LocalePolicy -> Text -> Maybe Locale
acceptedLocale localePolicy acceptLanguage =
  listToMaybe
    [ selectedLocale
    | languageRange <- Text.splitOn "," acceptLanguage,
      let primaryLanguage = Text.takeWhile (\character -> character /= ';' && character /= '-') (Text.strip languageRange),
      Just selectedLocale <- [allowedLocale localePolicy primaryLanguage]
    ]
