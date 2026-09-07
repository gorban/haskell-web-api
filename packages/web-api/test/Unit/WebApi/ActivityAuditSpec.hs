{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (evaluate)
import Control.Monad (void)
import Data.Either (fromRight)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isInfixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text qualified as Text
import HarchWeb.Account (mkAccountId)
import HarchWeb.EndpointMetadata (mkEndpointName, mkRouteTemplate)
import HarchWeb.Localization (locale)
import HarchWeb.RequestId (mkRequestId)
import HarchWeb.SecurityEvent (RouteObservation (..), requiredModuleNameOrDie)
import System.Exit (ExitCode (ExitFailure, ExitSuccess))
import Unit.WebApi.TestSupport (migrationPostgresTestConfig, postgresTestConfig)
import WebApi.ActivityAudit
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Testing (PostgresCommand (..), PostgresCommandResult (..), accountAuditAppendResultFixStatements, accountAuditControlledAppendPolicyStatements, accountAuditInitialMaintenanceStatements, accountAuditInsertPolicyFixStatements, accountAuditMaintenanceJobName, accountAuditMaintenanceSchedule, accountAuditMigrationStatements, accountAuditRuntimeReconciliationStatements, bootstrapAccountAuditSchedulerWithRunner, cronRunDetailsRetentionJobName, cronRunDetailsRetentionSchedule, installAccountAuditSchedulerStatements)

spec = describe "WebApi.ActivityAudit" $ do
  it "encodes every closed audit event with a stable code, version, and bounded detail" $ do
    expectPayload (PendingRegistrationDelivered RegistrationCreated) "pending-registration-delivered" (Just "created")
    expectPayload (PendingRegistrationDelivered RegistrationRetried) "pending-registration-delivered" (Just "retried")
    expectPayload VerificationResendDelivered "verification-resend-delivered" Nothing
    expectPayload EmailVerified "email-verified" Nothing
    expectPayload (AuthenticationRejected PasswordAuthenticationStage) "authentication-rejected" (Just "password")
    expectPayload (AuthenticationRejected SecondFactorAuthenticationStage) "authentication-rejected" (Just "second-factor")
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

  it "keeps framework request correlation separate from activity identity and payload" $ do
    let activity = sampleActivity
    activityRequestId activity `shouldBe` requiredMaybe "request id" (mkRequestId "550e8400-e29b-41d4-a716-446655440000")
    payloadText (activityEvent activity) `shouldBe` "account-session-issued\npassword"

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

  it "keeps the immutable audit schema and deployment reconciliation on controlled function boundaries" $ do
    let schemaSql = Text.unlines accountAuditMigrationStatements
        policySql = Text.unlines (accountAuditInsertPolicyFixStatements <> accountAuditControlledAppendPolicyStatements)
        appendResultSql = Text.unlines accountAuditAppendResultFixStatements
        setupSql = Text.unlines accountAuditInitialMaintenanceStatements
        reconciliationSql = Text.unlines (accountAuditRuntimeReconciliationStatements "web_api_dev" "runtime\"role")
    expectAll
      ( auditMigrationExpectation "pg_cron extension" ("CREATE EXTENSION IF NOT EXISTS pg_cron" `Text.isInfixOf` schemaSql)
          :| [ auditMigrationExpectation "range partition parent" ("PARTITION BY RANGE (occurred_at)" `Text.isInfixOf` schemaSql),
               auditMigrationExpectation "forced RLS" ("FORCE ROW LEVEL SECURITY" `Text.isInfixOf` schemaSql),
               auditMigrationExpectation "runtime append grant" ("GRANT EXECUTE ON FUNCTION account_audit.append_activity" `Text.isInfixOf` reconciliationSql),
               auditMigrationExpectation "scheduler maintenance grant" ("GRANT EXECUTE ON FUNCTION account_audit.maintain_activity_partitions() TO web_api_audit_scheduler" `Text.isInfixOf` schemaSql),
               auditMigrationExpectation "controlled append policy" ("FOR INSERT TO PUBLIC WITH CHECK (true)" `Text.isInfixOf` policySql),
               auditMigrationExpectation "append identity result" ("currval(pg_get_serial_sequence('account_audit.activity', 'activity_id'))" `Text.isInfixOf` appendResultSql),
               auditMigrationExpectation "initial partition maintenance" ("PERFORM account_audit.maintain_activity_partitions()" `Text.isInfixOf` setupSql),
               auditMigrationExpectation "reader and scheduler connection grants" ("GRANT CONNECT ON DATABASE \"web_api_dev\" TO web_api_audit_reader, web_api_audit_scheduler" `Text.isInfixOf` reconciliationSql),
               auditMigrationExpectation "quoted runtime role identifier" ("\"runtime\"\"role\"" `Text.isInfixOf` reconciliationSql),
               auditMigrationExpectation "runtime scope literal" ("VALUES ('runtime\"role', 'default')" `Text.isInfixOf` reconciliationSql)
             ]
      )

  it "keeps the example scheduler contract on fixed safe maintenance commands" $ do
    let schedulerSql = Text.unlines installAccountAuditSchedulerStatements
    expectAll
      ( (accountAuditMaintenanceJobName `shouldBe` "account-audit-maintenance")
          :| [ accountAuditMaintenanceSchedule `shouldBe` "0 3 * * *",
               cronRunDetailsRetentionJobName `shouldBe` "web-api-cron-run-details-retention",
               cronRunDetailsRetentionSchedule `shouldBe` "41 3 * * *",
               ("SELECT account_audit.maintain_activity_partitions();" `Text.isInfixOf` schedulerSql) `shouldBe` True,
               ("username = current_user" `Text.isInfixOf` schedulerSql) `shouldBe` True,
               ("interval '30 days'" `Text.isInfixOf` schedulerSql) `shouldBe` True
             ]
      )

  it "reconciles the scheduler login as owner before registering jobs through its direct connection" $ do
    recordedCommandsReference <- newIORef ([] :: [PostgresCommand])
    let schedulerDatabaseConfig =
          postgresTestConfig
            { databaseUser = "web_api_audit_scheduler",
              databasePassword = "scheduler's-secret"
            }
        successfulRunner postgresCommand = do
          modifyIORef' recordedCommandsReference (<> [postgresCommand])
          pure (PostgresCommandResult ExitSuccess Text.empty Text.empty)
    bootstrapAccountAuditSchedulerWithRunner successfulRunner migrationPostgresTestConfig schedulerDatabaseConfig
      `shouldReturn` Right ()
    recordedCommands <- readIORef recordedCommandsReference
    fmap postgresEnvironment recordedCommands
      `shouldBe` [ [("PGPASSWORD", "owner-secret")],
                   [("PGPASSWORD", "scheduler's-secret")],
                   [("PGPASSWORD", "scheduler's-secret")]
                 ]
    fmap postgresArguments recordedCommands
      `shouldSatisfy` \case
        [ownerArguments, schedulerArguments, cleanupArguments] ->
          any ("ALTER ROLE web_api_audit_scheduler WITH LOGIN PASSWORD 'scheduler''s-secret'" `isInfixOf`) ownerArguments
            && "web_api_audit_scheduler" `elem` schedulerArguments
            && "web_api_audit_scheduler" `elem` cleanupArguments
        _ -> False

  it "does not open the scheduler connection when owner-side login reconciliation fails" $ do
    recordedCommandsReference <- newIORef ([] :: [PostgresCommand])
    let schedulerDatabaseConfig =
          postgresTestConfig
            { databaseUser = "web_api_audit_scheduler",
              databasePassword = "scheduler-secret"
            }
        failedResult = PostgresCommandResult (ExitFailure 1) Text.empty "owner connection failed"
        failingOwnerRunner postgresCommand = do
          modifyIORef' recordedCommandsReference (<> [postgresCommand])
          pure failedResult
    result <- bootstrapAccountAuditSchedulerWithRunner failingOwnerRunner migrationPostgresTestConfig schedulerDatabaseConfig
    result
      `shouldSatisfy` \case
        Left _ -> True
        Right () -> False
    recordedCommands <- readIORef recordedCommandsReference
    fmap postgresEnvironment recordedCommands `shouldBe` [[("PGPASSWORD", "owner-secret")]]
  where
    allAccountAuditEvents =
      [ PendingRegistrationDelivered RegistrationCreated,
        PendingRegistrationDelivered RegistrationRetried,
        VerificationResendDelivered,
        EmailVerified,
        AuthenticationRejected PasswordAuthenticationStage,
        AuthenticationRejected SecondFactorAuthenticationStage,
        MfaEnrolled,
        AccountSessionIssued PasswordAuthenticationMethod,
        AccountSessionIssued TotpAuthenticationMethod,
        AccountSessionIssued RecoveryCodeAuthenticationMethod,
        AccountSessionEnded ExplicitLogout,
        AccountSessionEnded SessionRevoked
      ]
    unavailableStore = ActivityAuditStore (const (pure (Left ActivityAuditUnavailable)))
    successfulStore = ActivityAuditStore (const (pure (Right (activityIdFromDatabase 42))))

sampleActivity :: AccountActivity
sampleActivity =
  AccountActivity
    { activitySubject = requiredMaybe "account id" (mkAccountId "account_01"),
      activityRequestId = requiredMaybe "request id" (mkRequestId "550e8400-e29b-41d4-a716-446655440000"),
      activityEvent = AccountSessionIssued PasswordAuthenticationMethod,
      activityRoute = Nothing
    }

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

requiredMaybe :: String -> Maybe value -> value
requiredMaybe label = fromMaybe (error ("expected valid " <> label))

auditMigrationExpectation :: String -> Bool -> Expectation
auditMigrationExpectation label condition =
  if condition then pure () else expectationFailure ("expected audit migration to include " <> label)
