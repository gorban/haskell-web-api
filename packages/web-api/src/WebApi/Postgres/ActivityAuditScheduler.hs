-- | The repository-owned pg_cron command contract for the example audit
-- deployment.  These statements are deliberately independent of setup I/O:
-- another application can invoke the same maintenance wrapper from a managed
-- scheduler without importing this module or adopting pg_cron.
module WebApi.Postgres.ActivityAuditScheduler
  ( accountAuditMaintenanceJobName,
    accountAuditMaintenanceSchedule,
    cronRunDetailsRetentionJobName,
    cronRunDetailsRetentionSchedule,
    accountAuditSchedulerRoleName,
    bootstrapAccountAuditScheduler,
    bootstrapAccountAuditSchedulerWithRunner,
    reconcileAccountAuditSchedulerLoginStatements,
    installAccountAuditScheduler,
    installAccountAuditSchedulerWithRunner,
    installAccountAuditSchedulerStatements,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import WebApi.Config (DatabaseConfig (..))
import WebApi.Postgres.Runtime (PostgresCommand, PostgresCommandResult, PostgresRunnerError, runPostgresCommand, runStatements)

accountAuditSchedulerRoleName :: Text
accountAuditSchedulerRoleName = "web_api_audit_scheduler"

accountAuditMaintenanceJobName :: Text
accountAuditMaintenanceJobName = "account-audit-maintenance"

accountAuditMaintenanceSchedule :: Text
accountAuditMaintenanceSchedule = "0 3 * * *"

cronRunDetailsRetentionJobName :: Text
cronRunDetailsRetentionJobName = "web-api-cron-run-details-retention"

cronRunDetailsRetentionSchedule :: Text
cronRunDetailsRetentionSchedule = "41 3 * * *"

-- | pg_cron keeps its metadata in the configured cron database, which may
-- differ from the application database where these commands must run.
-- 'cron.schedule_in_database' makes that target explicit and updates the
-- database for an existing same-name job. Its optional username argument is
-- omitted so pg_cron keeps the connected, least-privileged scheduler identity;
-- passing a username explicitly would require superuser authority.
installAccountAuditSchedulerStatements :: Text -> [Text]
installAccountAuditSchedulerStatements targetDatabase =
  let targetDatabaseLiteral = quotedLiteral targetDatabase
   in [ "SELECT cron.schedule_in_database('account-audit-maintenance', '0 3 * * *', $$SELECT account_audit.maintain_activity_partitions();$$, " <> targetDatabaseLiteral <> ");",
        "SELECT cron.schedule_in_database('web-api-cron-run-details-retention', '41 3 * * *', $$DELETE FROM cron.job_run_details WHERE username = current_user AND end_time IS NOT NULL AND end_time < statement_timestamp() - interval '30 days';$$, " <> targetDatabaseLiteral <> ");"
      ]

-- | The migration owner reconciles only this fixed login before a separate
-- scheduler connection registers jobs.  The password comes from deployment
-- configuration and is not embedded in a durable database-change record.
reconcileAccountAuditSchedulerLoginStatements :: Text -> [Text]
reconcileAccountAuditSchedulerLoginStatements password =
  [ "ALTER ROLE web_api_audit_scheduler WITH LOGIN PASSWORD " <> quotedLiteral password <> " NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT;"
  ]

-- | Execute the example schedule installation as the configured scheduler
-- login.  It deliberately does not call pg_cron's clock or inspect run
-- history: the repository owns command construction and invocation, while
-- pg_cron owns dispatch timing.
installAccountAuditScheduler :: DatabaseConfig -> Text -> IO (Either PostgresRunnerError ())
installAccountAuditScheduler =
  installAccountAuditSchedulerWithRunner runPostgresCommand

installAccountAuditSchedulerWithRunner ::
  (PostgresCommand -> IO PostgresCommandResult) ->
  DatabaseConfig ->
  Text ->
  IO (Either PostgresRunnerError ())
installAccountAuditSchedulerWithRunner runCommand databaseConfig targetDatabase =
  runStatements runCommand databaseConfig (installAccountAuditSchedulerStatements targetDatabase)

-- | Reconcile the fixed scheduler login with the migration owner and then
-- register the jobs over a separate direct scheduler connection.  This is the
-- example deployment adapter, rather than a framework scheduling capability:
-- managed schedulers can invoke the same no-argument database wrapper without
-- using this bootstrap.  The order is fail-fast so a failed owner operation
-- never leaves a scheduler connection attempting to register jobs.
bootstrapAccountAuditScheduler :: DatabaseConfig -> DatabaseConfig -> IO (Either PostgresRunnerError ())
bootstrapAccountAuditScheduler =
  bootstrapAccountAuditSchedulerWithRunner runPostgresCommand

bootstrapAccountAuditSchedulerWithRunner ::
  (PostgresCommand -> IO PostgresCommandResult) ->
  DatabaseConfig ->
  DatabaseConfig ->
  IO (Either PostgresRunnerError ())
bootstrapAccountAuditSchedulerWithRunner runCommand migrationDatabaseConfig schedulerDatabaseConfig = do
  ownerResult <-
    runStatements
      runCommand
      migrationDatabaseConfig
      (reconcileAccountAuditSchedulerLoginStatements (databasePassword schedulerDatabaseConfig))
  case ownerResult of
    Left runnerError -> pure (Left runnerError)
    Right () ->
      installAccountAuditSchedulerWithRunner
        runCommand
        schedulerDatabaseConfig
        (databaseName migrationDatabaseConfig)

quotedLiteral :: Text -> Text
quotedLiteral value = "'" <> Text.replace "'" "''" value <> "'"
