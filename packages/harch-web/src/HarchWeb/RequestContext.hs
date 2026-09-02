{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Safe request facts shared between a root and its mounted application
-- modules.  The context deliberately has no transport request, raw headers,
-- credentials, query text, services, or mutable merge operation: those
-- capabilities belong either to the HTTP boundary or to explicit package
-- constructor dependencies.
module HarchWeb.RequestContext
  ( CanonicalOrigin,
    CanonicalOriginError (..),
    ContextProjection (..),
    CoreRequestContext (..),
    CorrelationContext,
    RequestContext (..),
    RequestIdentity (..),
    canonicalOriginErrorCode,
    canonicalOriginText,
    correlationContext,
    correlationTraceContext,
    defaultContextProjection,
    mkCanonicalOrigin,
    requiredCanonicalOriginOrDie,
  )
where

import Data.Char (isControl, isSpace)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Localization (Locale)
import HarchWeb.Observability (RequestTraceContext)
import HarchWeb.PathPrefix (PathPrefix)
import HarchWeb.SecurityEvent (RouteObservation)

-- | A configured origin used for same-origin checks and browser-visible root
-- links.  It has no path, query, fragment, credentials, or whitespace, so a
-- request-supplied URL cannot be transposed into an origin configuration.
newtype CanonicalOrigin = CanonicalOrigin Text
  deriving (Eq)

instance Show CanonicalOrigin where
  showsPrec precedence (CanonicalOrigin origin) =
    showParen (precedence > 10) $
      showString "CanonicalOrigin " . shows origin

-- | Stable configuration failures for 'mkCanonicalOrigin'.  The rejected
-- origin text is deliberately not retained in this result.
data CanonicalOriginError
  = CanonicalOriginMissingScheme
  | CanonicalOriginEmptyAuthority
  | CanonicalOriginUnsafeCharacter
  deriving (Eq)

-- | Stable low-cardinality configuration-error code suitable for a private
-- log or a deployment failure. The rejected origin is intentionally absent so
-- callers do not accidentally expose it.
canonicalOriginErrorCode :: CanonicalOriginError -> Text
canonicalOriginErrorCode = \case
  CanonicalOriginMissingScheme -> "missing-scheme"
  CanonicalOriginEmptyAuthority -> "empty-authority"
  CanonicalOriginUnsafeCharacter -> "unsafe-character"

canonicalOriginText :: CanonicalOrigin -> Text
canonicalOriginText (CanonicalOrigin origin) = origin

mkCanonicalOrigin :: Text -> Either CanonicalOriginError CanonicalOrigin
mkCanonicalOrigin origin
  | Text.any (\character -> isControl character || isSpace character) origin = Left CanonicalOriginUnsafeCharacter
  | otherwise =
      case (Text.stripPrefix "https://" origin, Text.stripPrefix "http://" origin) of
        (Just authority, _) -> canonicalAuthority "https://" authority
        (_, Just authority) -> canonicalAuthority "http://" authority
        _ -> Left CanonicalOriginMissingScheme
  where
    canonicalAuthority scheme authority
      | Text.null authority = Left CanonicalOriginEmptyAuthority
      | Text.any (`elem` ['/', '?', '#', '@', '\\']) authority = Left CanonicalOriginUnsafeCharacter
      | otherwise = Right (CanonicalOrigin (scheme <> authority))

-- | Construct a statically configured canonical origin. Runtime input must
-- use 'mkCanonicalOrigin' so validation failures stay on the ordinary result
-- rail; an invalid literal is a deployment configuration defect.
requiredCanonicalOriginOrDie :: Text -> CanonicalOrigin
requiredCanonicalOriginOrDie value =
  either (error . ("invalid canonical origin declaration: " <>) . Text.unpack . canonicalOriginErrorCode) id (mkCanonicalOrigin value)

-- | The correlation data already validated by the observability boundary.
-- A missing incoming trace is an honest state, not a synthetic identifier.
newtype CorrelationContext = CorrelationContext
  { correlationTraceContext :: Maybe RequestTraceContext
  }
  deriving (Eq, Show)

correlationContext :: Maybe RequestTraceContext -> CorrelationContext
correlationContext = CorrelationContext

data RequestIdentity principal
  = AnonymousIdentity
  | AuthenticatedIdentity principal
  deriving (Eq, Show)

-- | Core facts constructed by the root.  'requestRouteObservation' is absent
-- only before route matching; post-match guards and module handlers receive
-- the same context with a derived observation.
data CoreRequestContext = CoreRequestContext
  { requestLocale :: Locale,
    requestLocaleFallbacks :: [Locale],
    requestRouteObservation :: Maybe RouteObservation,
    requestCorrelation :: CorrelationContext,
    requestCanonicalOrigin :: CanonicalOrigin,
    requestPathPrefix :: PathPrefix
  }
  deriving (Eq, Show)

-- | The root's shared safe core plus application-selected identity, client,
-- and local values.  A package can only receive a smaller context through
-- 'ContextProjection'; there is intentionally no reverse projection.
data RequestContext identity client local = RequestContext
  { requestCore :: CoreRequestContext,
    requestIdentity :: identity,
    requestClient :: client,
    requestLocal :: local
  }
  deriving (Eq, Show)

-- | A one-way parent-to-child context capability.
newtype ContextProjection parent child = ContextProjection
  { projectRequestContext :: parent -> child
  }

-- | Preserve the root-owned core while allowing a composition root to select
-- exactly which identity/client/local views a child receives.  It cannot merge
-- any child result back into its parent.
defaultContextProjection ::
  (parentIdentity -> childIdentity) ->
  (parentClient -> childClient) ->
  (parentLocal -> childLocal) ->
  ContextProjection
    (RequestContext parentIdentity parentClient parentLocal)
    (RequestContext childIdentity childClient childLocal)
defaultContextProjection projectIdentity projectClient projectLocal =
  ContextProjection $ \parentContext ->
    RequestContext
      { requestCore = requestCore parentContext,
        requestIdentity = projectIdentity (requestIdentity parentContext),
        requestClient = projectClient (requestClient parentContext),
        requestLocal = projectLocal (requestLocal parentContext)
      }
