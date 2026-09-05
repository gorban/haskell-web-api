{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.EndpointMetadata (mkEndpointName, mkRouteTemplate)
import HarchWeb.Localization (locale)
import HarchWeb.SecurityEvent (RouteObservation (..), requiredModuleNameOrDie)
import WebApi.ActivityAudit

spec = describe "WebApi.ActivityAudit" $ do
  it "encodes every closed audit event with a stable code, version, and bounded detail" $ do
    expectPayload (PendingRegistrationDelivered RegistrationCreated) "pending-registration-delivered" (Just "created")
    expectPayload (PendingRegistrationDelivered RegistrationRetried) "pending-registration-delivered" (Just "retried")
    expectPayload VerificationResendDelivered "verification-resend-delivered" Nothing
    expectPayload EmailVerified "email-verified" Nothing
    expectPayload (AuthenticationRejected PasswordAuthenticationStage) "authentication-rejected" (Just "password")
    expectPayload (AuthenticationRejected SecondFactorAuthenticationStage) "authentication-rejected" (Just "second-factor")
    expectPayload (AuthenticationThrottled PasswordAuthenticationStage) "authentication-throttled" (Just "password")
    expectPayload (AuthenticationThrottled SecondFactorAuthenticationStage) "authentication-throttled" (Just "second-factor")
    expectPayload MfaEnrolled "mfa-enrolled" Nothing
    expectPayload (AccountSessionIssued PasswordAuthenticationMethod) "account-session-issued" (Just "password")
    expectPayload (AccountSessionIssued TotpAuthenticationMethod) "account-session-issued" (Just "totp")
    expectPayload (AccountSessionIssued RecoveryCodeAuthenticationMethod) "account-session-issued" (Just "recovery-code")
    expectPayload (AccountSessionEnded ExplicitLogout) "account-session-ended" (Just "explicit-logout")
    expectPayload (AccountSessionEnded SessionRevoked) "account-session-ended" (Just "revoked")

  it "has no event field through which a secret or request value can enter the payload" $ do
    let renderedPayloads = Text.unlines (map payloadText allAccountAuditEvents)
    Text.isInfixOf "person@example.test" renderedPayloads `shouldBe` False
    Text.isInfixOf "correct horse battery staple" renderedPayloads `shouldBe` False
    Text.isInfixOf "Bearer secret-token" renderedPayloads `shouldBe` False
    Text.isInfixOf "?return=/private" renderedPayloads `shouldBe` False

  it "projects only trusted declared route facts into bounded audit columns" $ do
    case auditRouteObservationFromTrusted (requiredTrustedRouteObservation "account.login" ("root" :| ["account"]) "/account/login" "en") of
      Left AuditRouteMountChainTooLong -> expectationFailure "expected a bounded mount chain"
      Left AuditRouteLocaleTooLong -> expectationFailure "expected a bounded locale"
      Right auditRoute -> do
        auditRouteEndpointName auditRoute `shouldBe` "account.login"
        auditRouteMountChain auditRoute `shouldBe` "root/account"
        auditRouteTemplate auditRoute `shouldBe` "/account/login"
        auditRouteLocale auditRoute `shouldBe` "en"

  it "rejects trusted declaration composition that would exceed an audit-column bound" $ do
    let longMountRoute = requiredTrustedRouteObservation "account.login" (Text.replicate 128 "a" :| [Text.replicate 128 "b", Text.replicate 128 "c", Text.replicate 128 "d", "e"]) "/account/login" "en"
        longLocaleRoute = requiredTrustedRouteObservation "account.login" ("root" :| []) "/account/login" (Text.replicate 17 "e")
    expectMountChainTooLong longMountRoute
    expectLocaleTooLong longLocaleRoute

  it "keeps unavailable audit storage in an explicit result rail" $ do
    appendAccountActivity unavailableStore (error "activity must not be evaluated for unavailable storage") >>= expectUnavailable

  it "materializes a successfully appended audit identifier without exposing it" $ do
    appendAccountActivity successfulStore (error "activity belongs to the storage adapter") >>= expectActivityId
  where
    allAccountAuditEvents =
      [ PendingRegistrationDelivered RegistrationCreated,
        PendingRegistrationDelivered RegistrationRetried,
        VerificationResendDelivered,
        EmailVerified,
        AuthenticationRejected PasswordAuthenticationStage,
        AuthenticationRejected SecondFactorAuthenticationStage,
        AuthenticationThrottled PasswordAuthenticationStage,
        AuthenticationThrottled SecondFactorAuthenticationStage,
        MfaEnrolled,
        AccountSessionIssued PasswordAuthenticationMethod,
        AccountSessionIssued TotpAuthenticationMethod,
        AccountSessionIssued RecoveryCodeAuthenticationMethod,
        AccountSessionEnded ExplicitLogout,
        AccountSessionEnded SessionRevoked
      ]
    unavailableStore = ActivityAuditStore (const (pure (Left ActivityAuditUnavailable)))
    successfulStore = ActivityAuditStore (const (pure (Right (activityIdFromDatabase 42))))

expectPayload :: AccountAuditEvent -> Text.Text -> Maybe Text.Text -> Expectation
expectPayload auditEvent expectedCode expectedDetail = do
  let payload = accountAuditEventPayload auditEvent
  accountAuditEventCode payload `shouldBe` expectedCode
  accountAuditPayloadVersion payload `shouldBe` 1
  accountAuditPayloadDetail payload `shouldBe` expectedDetail

payloadText :: AccountAuditEvent -> Text.Text
payloadText auditEvent =
  let payload = accountAuditEventPayload auditEvent
   in Text.intercalate "\n" (accountAuditEventCode payload : maybe [] pure (accountAuditPayloadDetail payload))

expectMountChainTooLong :: RouteObservation -> Expectation
expectMountChainTooLong routeObservation =
  case auditRouteObservationFromTrusted routeObservation of
    Left AuditRouteMountChainTooLong -> pure ()
    Left AuditRouteLocaleTooLong -> expectationFailure "expected the mount-chain error"
    Right _ -> expectationFailure "expected a rejected mount chain"

expectLocaleTooLong :: RouteObservation -> Expectation
expectLocaleTooLong routeObservation =
  case auditRouteObservationFromTrusted routeObservation of
    Left AuditRouteMountChainTooLong -> expectationFailure "expected the locale error"
    Left AuditRouteLocaleTooLong -> pure ()
    Right _ -> expectationFailure "expected a rejected locale"

expectUnavailable :: Either ActivityAuditStoreError ActivityId -> Expectation
expectUnavailable appendResult =
  case appendResult of
    Left ActivityAuditUnavailable -> pure ()
    Left ActivityAuditCapacityExceeded -> expectationFailure "expected unavailable storage"
    Left ActivityAuditCorruptResult -> expectationFailure "expected unavailable storage"
    Right _ -> expectationFailure "expected unavailable storage"

expectActivityId :: Either ActivityAuditStoreError ActivityId -> Expectation
expectActivityId appendResult =
  case appendResult of
    Left ActivityAuditUnavailable -> expectationFailure "expected an activity identifier"
    Left ActivityAuditCapacityExceeded -> expectationFailure "expected an activity identifier"
    Left ActivityAuditCorruptResult -> expectationFailure "expected an activity identifier"
    Right actualActivityId -> void (evaluate actualActivityId)

requiredTrustedRouteObservation :: Text.Text -> NonEmpty Text.Text -> Text.Text -> Text.Text -> RouteObservation
requiredTrustedRouteObservation endpointName mountChain routeTemplate localeName =
  RouteObservation
    { observedEndpointName = required "endpoint name" (mkEndpointName endpointName),
      observedMountChain = fmap requiredModuleNameOrDie mountChain,
      observedRouteTemplate = required "route template" (mkRouteTemplate routeTemplate),
      observedLocale = locale localeName
    }

required :: String -> Either error value -> value
required label = fromRight (error ("expected valid " <> label))
