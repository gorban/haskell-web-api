{-# LANGUAGE OverloadedStrings #-}

-- | Trusted security-event contracts shared by endpoint admission and future
-- application-module composition.
--
-- Decision record (AHI-4A, 2026-09-01): route observation and event meaning
-- belong to the framework security boundary, while application audit storage
-- remains application-owned. 'RouteObservation' is constructed from declared
-- route/module data, never a request path or a child-supplied string. The
-- deliberately small 'TelemetryEvent' projection contains only validated
-- declaration identifiers and closed outcome constructors; there is no
-- generic conversion to an application's durable audit event. AHI-4B extends
-- construction from 'rootRouteObservation' to mounted route chains without
-- changing these event meanings.
module HarchWeb.SecurityEvent
  ( AuthenticationEvent (..),
    AuthenticationEventOutcome (..),
    AuthorizationEvent (..),
    EventDeliveryFailure,
    EventDeliveryRequirement (..),
    ModuleName,
    ModuleNameError (..),
    RouteObservation (..),
    SecurityEvent (..),
    SecurityEventDelivery (..),
    SecurityEventDeliveryResult (..),
    SecurityEventRoot (..),
    SecurityEventSink (..),
    SecurityEventEnvelope (..),
    SessionEvent (..),
    SessionEventOutcome (..),
    TelemetryEvent (..),
    mkEventDeliveryFailure,
    mkModuleName,
    requiredModuleNameOrDie,
    moduleNameText,
    projectTelemetryEvent,
    rootSecurityEventSink,
    rootSecurityEventSinkWithMountChain,
    rootRouteObservation,
  )
where

import Data.Char (isAsciiLower, isDigit)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.EndpointMetadata (EndpointName, RouteTemplate)
import HarchWeb.Localization (Locale)
import HarchWeb.SecurityFailureCode (SecurityFailureCode)

-- | A construction-time application-module identity. It is a declaration,
-- not an HTTP path, account identity, or request-supplied label.
newtype ModuleName = ModuleName Text
  deriving (Eq, Ord, Show)

moduleNameText :: ModuleName -> Text
moduleNameText (ModuleName value) = value

data ModuleNameError
  = EmptyModuleName
  | ModuleNameTooLong
  | InvalidModuleName
  deriving (Eq, Show)

mkModuleName :: Text -> Either ModuleNameError ModuleName
mkModuleName value
  | Text.null value = Left EmptyModuleName
  | Text.length value > 128 = Left ModuleNameTooLong
  | Text.all validCharacter value = Right (ModuleName value)
  | otherwise = Left InvalidModuleName
  where
    validCharacter character = isAsciiLower character || isDigit character || character == '.' || character == '-'

-- | Construct a module name from a program-owned declaration. Runtime input
-- must use 'mkModuleName' and retain its validation failure; a literal that
-- fails here is an authored framework configuration defect.
requiredModuleNameOrDie :: Text -> ModuleName
requiredModuleNameOrDie = either (error . ("invalid module name declaration: " <>) . show) id . mkModuleName

-- | Route facts owned by the root matcher. The locale is retained for a
-- future application-owned audit projection, but is deliberately excluded
-- from 'TelemetryEvent': it is not needed as a metric dimension.
data RouteObservation = RouteObservation
  { observedEndpointName :: EndpointName,
    observedMountChain :: NonEmpty ModuleName,
    observedRouteTemplate :: RouteTemplate,
    observedLocale :: Locale
  }
  deriving (Eq, Show)

rootRouteObservation :: ModuleName -> Locale -> EndpointName -> RouteTemplate -> RouteObservation
rootRouteObservation moduleName localeValue endpointName routeTemplate =
  RouteObservation
    { observedEndpointName = endpointName,
      observedMountChain = moduleName :| [],
      observedRouteTemplate = routeTemplate,
      observedLocale = localeValue
    }

data AuthenticationEventOutcome
  = AuthenticationAnonymous
  | AuthenticationEstablished
  | AuthenticationMissing
  | AuthenticationRejected
  | AuthenticationDependencyUnavailable
  deriving (Eq, Show)

-- | The authentication outcome only. Proofs, JWTs, claims, headers, cookies,
-- account IDs, and error text never enter this framework event.
data AuthenticationEvent = AuthenticationEvent
  { authenticationEventOutcome :: AuthenticationEventOutcome,
    authenticationEventFailureCode :: Maybe SecurityFailureCode
  }
  deriving (Eq, Show)

newtype AuthorizationEvent = AuthorizationEvent
  { authorizationEventFailureCode :: SecurityFailureCode
  }
  deriving (Eq, Show)

data SessionEventOutcome
  = SessionEstablished
  | SessionRejected
  deriving (Eq, Show)

-- | A session-state fact contains no session identifier. Applications may
-- correlate it privately in their explicitly authored audit event.
data SessionEvent = SessionEvent
  { sessionEventOutcome :: SessionEventOutcome,
    sessionEventFailureCode :: Maybe SecurityFailureCode
  }
  deriving (Eq, Show)

data SecurityEvent
  = AuthenticationEvaluated AuthenticationEvent
  | AuthorizationDenied AuthorizationEvent
  | SessionStateChanged SessionEvent
  deriving (Eq, Show)

data EventDeliveryRequirement
  = TelemetryBestEffort
  | AuditRequired
  deriving (Eq, Show)

newtype EventDeliveryFailure = EventDeliveryFailure SecurityFailureCode
  deriving (Eq, Show)

mkEventDeliveryFailure :: SecurityFailureCode -> EventDeliveryFailure
mkEventDeliveryFailure = EventDeliveryFailure

data SecurityEventEnvelope = SecurityEventEnvelope
  { securityEventRoute :: RouteObservation,
    securityEventBody :: SecurityEvent,
    securityEventRequirement :: EventDeliveryRequirement
  }
  deriving (Eq, Show)

data SecurityEventDeliveryResult
  = SecurityEventDelivered
  | SecurityEventUndelivered EventDeliveryFailure
  deriving (Eq, Show)

-- | Application-owned delivery. A caller chooses how an undelivered
-- 'AuditRequired' event affects its transaction; this framework contract does
-- not falsely report it as delivered or turn best-effort telemetry failure
-- into an access decision.
newtype SecurityEventDelivery = SecurityEventDelivery
  { deliverSecurityEvent :: SecurityEventEnvelope -> IO SecurityEventDeliveryResult
  }

-- | Root-owned facts required to attach a sink to a matched endpoint. The
-- matcher supplies the endpoint declaration and current context; child guards
-- receive only the resulting 'SecurityEventSink', so they cannot substitute a
-- request path, module, locale, or endpoint identity into an event.
data SecurityEventRoot context = SecurityEventRoot
  { securityEventRootModule :: ModuleName,
    securityEventRootLocale :: context -> Locale,
    securityEventRootDelivery :: SecurityEventDelivery,
    securityEventRootUndelivered :: EventDeliveryFailure -> IO ()
  }

-- | A root-attached sink accepts only the closed event body and its delivery
-- requirement. It reports every undelivered result through the root's bounded
-- health hook before returning the truthful delivery result.
newtype SecurityEventSink = SecurityEventSink
  { emitSecurityEvent :: EventDeliveryRequirement -> SecurityEvent -> IO SecurityEventDeliveryResult
  }

rootSecurityEventSink :: SecurityEventRoot context -> EndpointName -> RouteTemplate -> context -> SecurityEventSink
rootSecurityEventSink eventRoot =
  rootSecurityEventSinkWithMountChain eventRoot (securityEventRootModule eventRoot :| [])

-- | Attach a security-event sink using the selected module chain supplied by
-- the root's constructed route table.  The chain is passed only by the shared
-- dispatcher after matching; child guards still receive just the resulting
-- sink and cannot replace its route attribution with request or child data.
rootSecurityEventSinkWithMountChain :: SecurityEventRoot context -> NonEmpty ModuleName -> EndpointName -> RouteTemplate -> context -> SecurityEventSink
rootSecurityEventSinkWithMountChain eventRoot mountChain endpointName routeTemplate requestContext =
  SecurityEventSink $ \requirement eventBody -> do
    deliveryResult <-
      deliverSecurityEvent
        (securityEventRootDelivery eventRoot)
        SecurityEventEnvelope
          { securityEventRoute =
              RouteObservation
                { observedEndpointName = endpointName,
                  observedMountChain = mountChain,
                  observedRouteTemplate = routeTemplate,
                  observedLocale = securityEventRootLocale eventRoot requestContext
                },
            securityEventBody = eventBody,
            securityEventRequirement = requirement
          }
    case deliveryResult of
      SecurityEventDelivered -> pure SecurityEventDelivered
      SecurityEventUndelivered deliveryFailure -> do
        securityEventRootUndelivered eventRoot deliveryFailure
        pure (SecurityEventUndelivered deliveryFailure)

-- | The bounded telemetry-safe projection. It intentionally omits locale,
-- account/session IDs, raw HTTP values, proof material, claim values, and
-- arbitrary application audit payloads.
data TelemetryEvent = TelemetryEvent
  { telemetryEventEndpointName :: EndpointName,
    telemetryEventRouteTemplate :: RouteTemplate,
    telemetryEventKind :: Text,
    telemetryEventOutcome :: Text
  }
  deriving (Eq, Show)

projectTelemetryEvent :: SecurityEventEnvelope -> TelemetryEvent
projectTelemetryEvent eventEnvelope =
  TelemetryEvent
    { telemetryEventEndpointName = observedEndpointName routeObservation,
      telemetryEventRouteTemplate = observedRouteTemplate routeObservation,
      telemetryEventKind = eventKind eventBody,
      telemetryEventOutcome = eventOutcome eventBody
    }
  where
    routeObservation = securityEventRoute eventEnvelope
    eventBody = securityEventBody eventEnvelope

eventKind :: SecurityEvent -> Text
eventKind securityEvent =
  case securityEvent of
    AuthenticationEvaluated _ -> "authentication"
    AuthorizationDenied _ -> "authorization"
    SessionStateChanged _ -> "session"

eventOutcome :: SecurityEvent -> Text
eventOutcome securityEvent =
  case securityEvent of
    AuthenticationEvaluated authenticationEvent ->
      case authenticationEventOutcome authenticationEvent of
        AuthenticationAnonymous -> "anonymous"
        AuthenticationEstablished -> "established"
        AuthenticationMissing -> "missing"
        AuthenticationRejected -> "rejected"
        AuthenticationDependencyUnavailable -> "dependency-unavailable"
    AuthorizationDenied _ -> "denied"
    SessionStateChanged sessionEvent ->
      case sessionEventOutcome sessionEvent of
        SessionEstablished -> "established"
        SessionRejected -> "rejected"
