{-# LANGUAGE OverloadedStrings #-}

-- | Application-owned durable account-audit vocabulary.
--
-- Decision record (AHI-5, 2026-09-05): Harch Web owns only the trusted route
-- observation and telemetry-safe security event. This module owns the closed
-- account activity catalog and converts a trusted observation into bounded
-- audit columns; it accepts neither a request path nor arbitrary payload text.
-- The PostgreSQL repository and transactional account-operation integration
-- remain the next AHI-5 slices, so this module deliberately does not claim an
-- append is yet atomic with account state.
module WebApi.ActivityAudit
  ( AccountActivity (..),
    AccountAuditEvent (..),
    AccountAuditPayload (..),
    ActivityAuditStore (..),
    ActivityAuditStoreError (..),
    ActivityId,
    AuditAuthenticationMethod (..),
    AuditAuthenticationStage (..),
    AuditRegistrationDeliveryStage (..),
    AuditRouteObservation,
    AuditRouteObservationError (..),
    AuditSessionEndReason (..),
    accountAuditEventPayload,
    activityIdFromDatabase,
    auditRouteEndpointName,
    auditRouteLocale,
    auditRouteMountChain,
    auditRouteObservationFromTrusted,
    auditRouteTemplate,
  )
where

import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Word (Word16, Word64)
import HarchWeb.Account (AccountId)
import HarchWeb.EndpointMetadata (endpointNameText, routeTemplateText)
import HarchWeb.Localization (localeText)
import HarchWeb.SecurityEvent (RouteObservation (..), moduleNameText)

data AccountActivity = AccountActivity
  { activitySubject :: AccountId,
    activityEvent :: AccountAuditEvent,
    activityRoute :: Maybe AuditRouteObservation
  }

-- | The initial operator questions are deliberately finite: account delivery,
-- verification, admitted authentication outcome, MFA enrollment, and session
-- lifecycle. In particular, unknown-identifier failures and every later
-- observation of an existing throttle stay out of this account ledger.
data AccountAuditEvent
  = PendingRegistrationDelivered AuditRegistrationDeliveryStage
  | VerificationResendDelivered
  | EmailVerified
  | AuthenticationRejected AuditAuthenticationStage
  | AuthenticationThrottled AuditAuthenticationStage
  | MfaEnrolled
  | AccountSessionIssued AuditAuthenticationMethod
  | AccountSessionEnded AuditSessionEndReason

data AuditAuthenticationStage
  = PasswordAuthenticationStage
  | SecondFactorAuthenticationStage

data AuditRegistrationDeliveryStage
  = RegistrationCreated
  | RegistrationRetried

data AuditAuthenticationMethod
  = PasswordAuthenticationMethod
  | TotpAuthenticationMethod
  | RecoveryCodeAuthenticationMethod

data AuditSessionEndReason
  = ExplicitLogout
  | SessionRevoked

-- | The typed database payload: all text is selected by exhaustive folds over
-- 'AccountAuditEvent'; no caller-provided message, JSON, request value, or
-- secret can enter these columns.
data AccountAuditPayload = AccountAuditPayload
  { accountAuditEventCode :: Text,
    accountAuditPayloadVersion :: Word16,
    accountAuditPayloadDetail :: Maybe Text
  }

accountAuditEventPayload :: AccountAuditEvent -> AccountAuditPayload
accountAuditEventPayload accountAuditEvent =
  case accountAuditEvent of
    PendingRegistrationDelivered deliveryStage ->
      AccountAuditPayload "pending-registration-delivered" 1 (Just (deliveryStageCode deliveryStage))
    VerificationResendDelivered ->
      AccountAuditPayload "verification-resend-delivered" 1 Nothing
    EmailVerified ->
      AccountAuditPayload "email-verified" 1 Nothing
    AuthenticationRejected stage ->
      AccountAuditPayload "authentication-rejected" 1 (Just (authenticationStageCode stage))
    AuthenticationThrottled stage ->
      AccountAuditPayload "authentication-throttled" 1 (Just (authenticationStageCode stage))
    MfaEnrolled ->
      AccountAuditPayload "mfa-enrolled" 1 Nothing
    AccountSessionIssued method ->
      AccountAuditPayload "account-session-issued" 1 (Just (authenticationMethodCode method))
    AccountSessionEnded reason ->
      AccountAuditPayload "account-session-ended" 1 (Just (sessionEndReasonCode reason))

deliveryStageCode :: AuditRegistrationDeliveryStage -> Text
deliveryStageCode deliveryStage =
  case deliveryStage of
    RegistrationCreated -> "created"
    RegistrationRetried -> "retried"

authenticationStageCode :: AuditAuthenticationStage -> Text
authenticationStageCode authenticationStage =
  case authenticationStage of
    PasswordAuthenticationStage -> "password"
    SecondFactorAuthenticationStage -> "second-factor"

authenticationMethodCode :: AuditAuthenticationMethod -> Text
authenticationMethodCode authenticationMethod =
  case authenticationMethod of
    PasswordAuthenticationMethod -> "password"
    TotpAuthenticationMethod -> "totp"
    RecoveryCodeAuthenticationMethod -> "recovery-code"

sessionEndReasonCode :: AuditSessionEndReason -> Text
sessionEndReasonCode sessionEndReason =
  case sessionEndReason of
    ExplicitLogout -> "explicit-logout"
    SessionRevoked -> "revoked"

-- | Bounded projection of a root-owned route observation. The constructor is
-- private so a request, action, or application module cannot substitute raw
-- path/query text into durable audit storage.
data AuditRouteObservation = AuditRouteObservation
  { auditRouteEndpointName :: Text,
    auditRouteMountChain :: Text,
    auditRouteTemplate :: Text,
    auditRouteLocale :: Text
  }

data AuditRouteObservationError
  = AuditRouteMountChainTooLong
  | AuditRouteLocaleTooLong

auditRouteObservationFromTrusted :: RouteObservation -> Either AuditRouteObservationError AuditRouteObservation
auditRouteObservationFromTrusted routeObservation = do
  let endpointName = endpointNameText (observedEndpointName routeObservation)
  mountChain <- bounded AuditRouteMountChainTooLong 512 (Text.intercalate "/" (NonEmpty.toList (fmap moduleNameText (observedMountChain routeObservation))))
  let routeTemplate = routeTemplateText (observedRouteTemplate routeObservation)
  locale <- bounded AuditRouteLocaleTooLong 16 (localeText (observedLocale routeObservation))
  pure (AuditRouteObservation endpointName mountChain routeTemplate locale)
  where
    bounded tooLong maximumLength value
      | Text.length value <= maximumLength = Right value
      | otherwise = Left tooLong

newtype ActivityId = ActivityId Word64

-- | Maps the identifier returned by the controlled PostgreSQL append
-- function into the application-owned opaque result type. The repository is
-- the only intended producer; callers can retain the result but cannot forge
-- an audit identifier.
activityIdFromDatabase :: Word64 -> ActivityId
activityIdFromDatabase = ActivityId

newtype ActivityAuditStore = ActivityAuditStore
  { appendAccountActivity :: AccountActivity -> IO (Either ActivityAuditStoreError ActivityId)
  }

data ActivityAuditStoreError
  = ActivityAuditUnavailable
  | ActivityAuditCapacityExceeded
  | ActivityAuditCorruptResult
