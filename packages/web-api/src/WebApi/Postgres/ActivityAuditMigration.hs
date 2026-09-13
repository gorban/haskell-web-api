{-# LANGUAGE OverloadedStrings #-}

-- | Immutable PostgreSQL change statements for the application-owned account
-- audit boundary.
--
-- Decision record (AHI-5, 2026-09-07): extend the existing
-- 'Postgres.DatabaseChange' ownership boundary rather than introducing an
-- audit-specific migration runner or putting PostgreSQL policy in Harch.  The
-- controlled append and maintenance functions are the only database-side
-- mutation boundaries.  They derive audit scope from @session_user@, use
-- owner-managed policy/registry tables, and keep retention whole-partition so
-- an application can replace its scheduler without replacing the security
-- model.
module WebApi.Postgres.ActivityAuditMigration
  ( accountAuditMigrationStatements,
    accountAuditInsertPolicyFixStatements,
    accountAuditControlledAppendPolicyStatements,
    accountAuditAppendResultFixStatements,
    accountAuditInitialMaintenanceStatements,
    accountAuditSessionIssueStatements,
    accountAuditSessionIssueConflictFixStatements,
    accountAuditSessionIssueInsertPrivilegeFixStatements,
    accountAuditRegistrationDeliveryStatements,
    accountAuditVerificationResendDeliveryStatements,
    accountAuditRuntimeReconciliationStatements,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

-- | The versioned audit schema.  Every value which can become an SQL
-- identifier is fixed here; application input reaches only function
-- parameters, where the controlled append function validates it.
accountAuditMigrationStatements :: [Text]
accountAuditMigrationStatements =
  [ "CREATE EXTENSION IF NOT EXISTS pg_cron;",
    createRole "account_audit_owner",
    createRole "web_api_audit_reader",
    createRole "web_api_audit_scheduler",
    "CREATE SCHEMA IF NOT EXISTS account_audit;",
    "ALTER SCHEMA account_audit OWNER TO account_audit_owner;",
    "REVOKE ALL ON SCHEMA account_audit FROM PUBLIC;",
    "REVOKE ALL ON SCHEMA cron FROM PUBLIC;",
    "CREATE TABLE IF NOT EXISTS account_audit.audit_policy (policy_key BOOLEAN PRIMARY KEY DEFAULT true CHECK (policy_key), retained_complete_months SMALLINT NOT NULL CHECK (retained_complete_months = 12), partition_row_limit BIGINT NOT NULL CHECK (partition_row_limit > 0), utilization_high_water_percent SMALLINT NOT NULL CHECK (utilization_high_water_percent BETWEEN 1 AND 100));",
    "ALTER TABLE account_audit.audit_policy OWNER TO account_audit_owner;",
    "INSERT INTO account_audit.audit_policy (policy_key, retained_complete_months, partition_row_limit, utilization_high_water_percent) VALUES (true, 12, 100000, 80) ON CONFLICT (policy_key) DO NOTHING;",
    "CREATE TABLE IF NOT EXISTS account_audit.runtime_scope (runtime_role_name NAME PRIMARY KEY, audit_scope_id TEXT NOT NULL CHECK (octet_length(audit_scope_id) BETWEEN 1 AND 128 AND audit_scope_id ~ '^[a-z0-9][a-z0-9_-]*$'));",
    "ALTER TABLE account_audit.runtime_scope OWNER TO account_audit_owner;",
    "CREATE TABLE IF NOT EXISTS account_audit.reader_scope_grant (reader_role_name NAME NOT NULL, audit_scope_id TEXT NOT NULL CHECK (octet_length(audit_scope_id) BETWEEN 1 AND 128 AND audit_scope_id ~ '^[a-z0-9][a-z0-9_-]*$'), PRIMARY KEY (reader_role_name, audit_scope_id));",
    "ALTER TABLE account_audit.reader_scope_grant OWNER TO account_audit_owner;",
    "CREATE TABLE IF NOT EXISTS account_audit.partition_registry (partition_name NAME PRIMARY KEY, lower_bound TIMESTAMPTZ NOT NULL, upper_bound TIMESTAMPTZ NOT NULL, accepted_row_count BIGINT NOT NULL DEFAULT 0 CHECK (accepted_row_count >= 0), UNIQUE (lower_bound, upper_bound), CHECK (lower_bound < upper_bound), CHECK (lower_bound = date_trunc('month', lower_bound AT TIME ZONE 'UTC') AT TIME ZONE 'UTC'), CHECK (upper_bound = lower_bound + interval '1 month'));",
    "ALTER TABLE account_audit.partition_registry OWNER TO account_audit_owner;",
    "CREATE TABLE IF NOT EXISTS account_audit.activity (occurred_at TIMESTAMPTZ NOT NULL, activity_id BIGINT GENERATED ALWAYS AS IDENTITY, audit_scope_id TEXT NOT NULL CHECK (octet_length(audit_scope_id) BETWEEN 1 AND 128 AND audit_scope_id ~ '^[a-z0-9][a-z0-9_-]*$'), account_id TEXT NOT NULL CHECK (octet_length(account_id) BETWEEN 1 AND 128), request_id TEXT NOT NULL CHECK (request_id ~ '^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'), event_code TEXT NOT NULL CHECK (event_code IN ('pending-registration-delivered', 'verification-resend-delivered', 'email-verified', 'authentication-rejected', 'mfa-enrolled', 'account-session-issued', 'account-session-ended')), payload_version SMALLINT NOT NULL CHECK (payload_version = 1), payload_detail TEXT, route_endpoint_name TEXT, route_mount_chain TEXT, route_template TEXT, route_locale TEXT, PRIMARY KEY (occurred_at, activity_id), CHECK ((event_code = 'pending-registration-delivered' AND payload_detail IN ('created', 'retried')) OR (event_code = 'verification-resend-delivered' AND payload_detail IS NULL) OR (event_code = 'email-verified' AND payload_detail IS NULL) OR (event_code = 'authentication-rejected' AND payload_detail IN ('password', 'second-factor')) OR (event_code = 'mfa-enrolled' AND payload_detail IS NULL) OR (event_code = 'account-session-issued' AND payload_detail IN ('password', 'totp', 'recovery-code')) OR (event_code = 'account-session-ended' AND payload_detail IN ('explicit-logout', 'revoked'))), CHECK ((route_endpoint_name IS NULL AND route_mount_chain IS NULL AND route_template IS NULL AND route_locale IS NULL) OR (route_endpoint_name IS NOT NULL AND route_mount_chain IS NOT NULL AND route_template IS NOT NULL AND route_locale IS NOT NULL)), CHECK (route_endpoint_name IS NULL OR octet_length(route_endpoint_name) BETWEEN 1 AND 128), CHECK (route_mount_chain IS NULL OR octet_length(route_mount_chain) BETWEEN 1 AND 512), CHECK (route_template IS NULL OR octet_length(route_template) BETWEEN 1 AND 512), CHECK (route_locale IS NULL OR octet_length(route_locale) BETWEEN 1 AND 16)) PARTITION BY RANGE (occurred_at);",
    "ALTER TABLE account_audit.activity OWNER TO account_audit_owner;",
    "CREATE INDEX IF NOT EXISTS account_audit_activity_subject_time ON account_audit.activity (account_id, occurred_at DESC);",
    "CREATE INDEX IF NOT EXISTS account_audit_activity_scope_request_time ON account_audit.activity (audit_scope_id, request_id, occurred_at DESC);",
    "ALTER TABLE account_audit.activity ENABLE ROW LEVEL SECURITY;",
    "ALTER TABLE account_audit.activity FORCE ROW LEVEL SECURITY;",
    readerHasScopeFunction,
    appendActivityFunction,
    maintainActivityPartitionsAtFunction,
    maintainActivityPartitionsFunction,
    "ALTER FUNCTION account_audit.reader_has_scope(TEXT) OWNER TO account_audit_owner;",
    "ALTER FUNCTION account_audit.append_activity(TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) OWNER TO account_audit_owner;",
    "ALTER FUNCTION account_audit.maintain_activity_partitions_at(TIMESTAMPTZ, INTERVAL) OWNER TO account_audit_owner;",
    "ALTER FUNCTION account_audit.maintain_activity_partitions() OWNER TO account_audit_owner;",
    "DROP POLICY IF EXISTS account_activity_owner_append ON account_audit.activity;",
    "CREATE POLICY account_activity_owner_append ON account_audit.activity FOR INSERT TO account_audit_owner WITH CHECK (true);",
    "DROP POLICY IF EXISTS account_activity_authorized_read ON account_audit.activity;",
    "CREATE POLICY account_activity_authorized_read ON account_audit.activity FOR SELECT TO web_api_audit_reader USING (account_audit.reader_has_scope(audit_scope_id));",
    "REVOKE ALL ON ALL TABLES IN SCHEMA account_audit FROM PUBLIC;",
    "REVOKE ALL ON ALL SEQUENCES IN SCHEMA account_audit FROM PUBLIC;",
    "REVOKE ALL ON ALL FUNCTIONS IN SCHEMA account_audit FROM PUBLIC;",
    "REVOKE ALL ON TABLE account_audit.activity FROM web_api_audit_reader, web_api_audit_scheduler;",
    "REVOKE ALL ON TABLE account_audit.partition_registry FROM web_api_audit_reader, web_api_audit_scheduler;",
    "REVOKE ALL ON TABLE account_audit.audit_policy FROM web_api_audit_reader, web_api_audit_scheduler;",
    "REVOKE ALL ON TABLE account_audit.runtime_scope FROM web_api_audit_reader, web_api_audit_scheduler;",
    "REVOKE ALL ON TABLE account_audit.reader_scope_grant FROM web_api_audit_reader, web_api_audit_scheduler;",
    "GRANT USAGE ON SCHEMA account_audit TO web_api_audit_reader, web_api_audit_scheduler;",
    "GRANT SELECT ON TABLE account_audit.activity TO web_api_audit_reader;",
    "GRANT EXECUTE ON FUNCTION account_audit.reader_has_scope(TEXT) TO web_api_audit_reader;",
    "GRANT EXECUTE ON FUNCTION account_audit.maintain_activity_partitions() TO web_api_audit_scheduler;",
    "GRANT USAGE ON SCHEMA cron TO web_api_audit_scheduler;",
    "GRANT EXECUTE ON FUNCTION cron.schedule(TEXT, TEXT, TEXT) TO web_api_audit_scheduler;",
    "GRANT DELETE ON TABLE cron.job_run_details TO web_api_audit_scheduler;"
  ]

-- | Deployment reconciliation for the selected application runtime role.  Its
-- login/password are deployment configuration rather than immutable domain
-- data, but the role-to-scope mapping is owner-managed and never comes from a
-- request, custom GUC, or audit append parameter.
accountAuditRuntimeReconciliationStatements :: Text -> Text -> [Text]
accountAuditRuntimeReconciliationStatements databaseName runtimeRoleName =
  [ "GRANT CONNECT ON DATABASE " <> quotedIdentifier databaseName <> " TO web_api_audit_reader, web_api_audit_scheduler;",
    "REVOKE ALL ON SCHEMA account_audit FROM " <> quotedIdentifier runtimeRoleName <> ";",
    "REVOKE ALL ON ALL TABLES IN SCHEMA account_audit FROM " <> quotedIdentifier runtimeRoleName <> ";",
    "REVOKE ALL ON ALL SEQUENCES IN SCHEMA account_audit FROM " <> quotedIdentifier runtimeRoleName <> ";",
    "REVOKE ALL ON ALL FUNCTIONS IN SCHEMA account_audit FROM " <> quotedIdentifier runtimeRoleName <> ";",
    "GRANT USAGE ON SCHEMA account_audit TO " <> quotedIdentifier runtimeRoleName <> ";",
    "GRANT EXECUTE ON FUNCTION account_audit.append_activity(TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) TO " <> quotedIdentifier runtimeRoleName <> ";",
    "GRANT EXECUTE ON FUNCTION account_audit.issue_account_session_with_activity(TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) TO " <> quotedIdentifier runtimeRoleName <> ";",
    "GRANT EXECUTE ON FUNCTION account_audit.complete_pending_registration_delivery_with_activity(TEXT, TEXT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) TO " <> quotedIdentifier runtimeRoleName <> ";",
    "GRANT EXECUTE ON FUNCTION account_audit.complete_verification_resend_with_activity(TEXT, TEXT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) TO " <> quotedIdentifier runtimeRoleName <> ";",
    "INSERT INTO account_audit.runtime_scope (runtime_role_name, audit_scope_id) VALUES (" <> quotedLiteral runtimeRoleName <> ", 'default') ON CONFLICT (runtime_role_name) DO NOTHING;"
  ]

-- | A forward correction to the initial owner-role policy. PostgreSQL selects
-- RLS role lists using the invoker even while a security-definer function has
-- switched @current_user@ to its owner. The predicate therefore checks the
-- function's effective owner, while direct runtime INSERT remains impossible
-- because that role has no table privilege at all.
accountAuditInsertPolicyFixStatements :: [Text]
accountAuditInsertPolicyFixStatements =
  [ "DROP POLICY account_activity_owner_append ON account_audit.activity;",
    "CREATE POLICY account_activity_controlled_append ON account_audit.activity FOR INSERT TO PUBLIC WITH CHECK (current_user = 'account_audit_owner');"
  ]

-- | The effective-user predicate is not visible to PostgreSQL's partitioned
-- table RLS check from this security-definer call: policy identity remains the
-- invoker at that boundary.  Function-only table privileges are the actual
-- access control boundary, so the final controlled-append policy is an
-- unconditional check and cannot enlarge runtime authority on its own.
accountAuditControlledAppendPolicyStatements :: [Text]
accountAuditControlledAppendPolicyStatements =
  [ "DROP POLICY account_activity_controlled_append ON account_audit.activity;",
    "CREATE POLICY account_activity_controlled_append ON account_audit.activity FOR INSERT TO PUBLIC WITH CHECK (true);"
  ]

-- | A controlled append may not use @RETURNING@ because that also evaluates
-- the runtime caller's intentionally absent SELECT RLS policy.  The generated
-- identity sequence is session-local, so its 'currval' returns the inserted
-- activity identity without granting a reader surface.
accountAuditAppendResultFixStatements :: [Text]
accountAuditAppendResultFixStatements =
  [ Text.replace
      "  RETURNING activity.activity_id INTO activity_id;"
      "  ;\n  SELECT currval(pg_get_serial_sequence('account_audit.activity', 'activity_id')) INTO activity_id;"
      appendActivityFunction
  ]

-- | New deployments prepare the current and next UTC partitions before the
-- runtime can issue an audited mutation. The command consumes the same safe
-- no-argument wrapper that a scheduler will call; it does not duplicate date
-- arithmetic in setup code.
accountAuditInitialMaintenanceStatements :: [Text]
accountAuditInitialMaintenanceStatements =
  [ "DO $$ BEGIN PERFORM account_audit.maintain_activity_partitions(); END $$;"
  ]

-- | The application-owned first @AuditRequired@ operation.  It is one SQL
-- statement under the runtime connection's transaction: a collision returns
-- no row, while a successful session insert calls the existing controlled
-- audit append before returning the session identity.
accountAuditSessionIssueStatements :: [Text]
accountAuditSessionIssueStatements =
  [ "GRANT USAGE ON SCHEMA web_api TO account_audit_owner;",
    "GRANT INSERT ON TABLE web_api.account_sessions TO account_audit_owner;",
    issueAccountSessionWithActivityFunction,
    "ALTER FUNCTION account_audit.issue_account_session_with_activity(TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) OWNER TO account_audit_owner;",
    "REVOKE ALL ON FUNCTION account_audit.issue_account_session_with_activity(TEXT, TEXT, BIGINT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) FROM PUBLIC;"
  ]

-- | Forward correction for the original session/audit operation. A
-- @RETURNS TABLE(session_id TEXT)@ declaration creates a PL/pgSQL output
-- variable named @session_id@, so PostgreSQL cannot resolve an unqualified
-- @ON CONFLICT (session_id)@ target. Its recorded SQL names the primary-key
-- constraint explicitly. It must remain byte-for-byte stable for deployed
-- database ledgers; the later insert-privilege correction handles the least-
-- privilege consequence of that target.
accountAuditSessionIssueConflictFixStatements :: [Text]
accountAuditSessionIssueConflictFixStatements =
  [ Text.replace
      "ON CONFLICT (session_id) DO NOTHING;"
      "ON CONFLICT ON CONSTRAINT account_sessions_pkey DO NOTHING;"
      issueAccountSessionWithActivityFunction
  ]

-- | The explicit conflict target parses but requires SELECT privilege on the
-- referenced table.  Preserve the audit owner's narrow INSERT-only grant by
-- catching the expected duplicate-key outcome around the plain insert rather
-- than broadening it to table reads. This is a new migration because the
-- prior corrective statement may already be present in a deployed ledger.
accountAuditSessionIssueInsertPrivilegeFixStatements :: [Text]
accountAuditSessionIssueInsertPrivilegeFixStatements =
  [ Text.replace
      "  INSERT INTO web_api.account_sessions (session_id, account_id, issued_at_nanoseconds, expires_at_nanoseconds)\n  VALUES (p_session_id, p_account_id, p_issued_at_nanoseconds, p_expires_at_nanoseconds)\n  ON CONFLICT (session_id) DO NOTHING;\n  GET DIAGNOSTICS v_inserted = ROW_COUNT;\n  IF v_inserted = 0 THEN RETURN; END IF;"
      "  BEGIN\n    INSERT INTO web_api.account_sessions (session_id, account_id, issued_at_nanoseconds, expires_at_nanoseconds)\n    VALUES (p_session_id, p_account_id, p_issued_at_nanoseconds, p_expires_at_nanoseconds);\n  EXCEPTION WHEN unique_violation THEN\n    RETURN;\n  END;"
      issueAccountSessionWithActivityFunction
  ]

-- | The registration email has already crossed the SMTP boundary before this
-- operation runs.  Settling its durable delivery claim and recording the
-- corresponding operator event therefore share this one transaction: an
-- audit-capacity or append failure leaves the claim retryable rather than
-- asserting delivery without evidence.
accountAuditRegistrationDeliveryStatements :: [Text]
accountAuditRegistrationDeliveryStatements =
  [ "GRANT USAGE ON SCHEMA web_api TO account_audit_owner;",
    "GRANT SELECT (account_id, token_digest, delivery_state), UPDATE (delivery_state, delivery_claimed_at_nanoseconds) ON TABLE web_api.email_verifications TO account_audit_owner;",
    completePendingRegistrationDeliveryWithActivityFunction,
    "ALTER FUNCTION account_audit.complete_pending_registration_delivery_with_activity(TEXT, TEXT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) OWNER TO account_audit_owner;",
    "REVOKE ALL ON FUNCTION account_audit.complete_pending_registration_delivery_with_activity(TEXT, TEXT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) FROM PUBLIC;"
  ]

-- | The generic resend lifecycle remains authoritative for candidate-token
-- promotion and delivery-window accounting. This controlled operation runs
-- that operation under the audit owner, then appends the required closed
-- event before returning so any append failure rolls the promotion back.
accountAuditVerificationResendDeliveryStatements :: [Text]
accountAuditVerificationResendDeliveryStatements =
  [ "GRANT USAGE ON SCHEMA web_api TO account_audit_owner;",
    "GRANT SELECT (account_id, email_verified_at_nanoseconds), UPDATE (email_verified_at_nanoseconds) ON TABLE web_api.accounts TO account_audit_owner;",
    "GRANT SELECT, DELETE ON TABLE web_api.verification_resend_claims TO account_audit_owner;",
    "GRANT DELETE, INSERT ON TABLE web_api.email_verifications TO account_audit_owner;",
    "GRANT INSERT ON TABLE web_api.verification_resend_deliveries TO account_audit_owner;",
    "GRANT USAGE ON SEQUENCE web_api.verification_resend_deliveries_delivery_id_seq TO account_audit_owner;",
    "GRANT EXECUTE ON FUNCTION web_api.complete_verification_resend(TEXT, TEXT, BIGINT) TO account_audit_owner;",
    completeVerificationResendWithActivityFunction,
    "ALTER FUNCTION account_audit.complete_verification_resend_with_activity(TEXT, TEXT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) OWNER TO account_audit_owner;",
    "REVOKE ALL ON FUNCTION account_audit.complete_verification_resend_with_activity(TEXT, TEXT, BIGINT, TEXT, TEXT, TEXT, SMALLINT, TEXT, TEXT, TEXT, TEXT, TEXT) FROM PUBLIC;"
  ]

completePendingRegistrationDeliveryWithActivityFunction :: Text
completePendingRegistrationDeliveryWithActivityFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.complete_pending_registration_delivery_with_activity(",
      "  p_account_id TEXT, p_token_digest TEXT, p_audit_account_id TEXT, p_request_id TEXT, p_event_code TEXT, p_payload_version SMALLINT, p_payload_detail TEXT,",
      "  p_route_endpoint_name TEXT, p_route_mount_chain TEXT, p_route_template TEXT, p_route_locale TEXT",
      ") RETURNS TABLE(account_id TEXT)",
      "LANGUAGE plpgsql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit, web_api",
      "AS $$",
      "BEGIN",
      "  IF p_account_id <> p_audit_account_id THEN RAISE EXCEPTION USING ERRCODE = '22023', MESSAGE = 'registration audit subject does not match delivery account'; END IF;",
      "  UPDATE web_api.email_verifications AS verification SET delivery_state = 'delivered', delivery_claimed_at_nanoseconds = NULL",
      "  WHERE verification.account_id = p_account_id AND verification.token_digest = p_token_digest AND verification.delivery_state = 'claimed';",
      "  IF NOT FOUND THEN RETURN; END IF;",
      "  PERFORM activity_id FROM account_audit.append_activity(p_audit_account_id, p_request_id, p_event_code, p_payload_version, p_payload_detail, p_route_endpoint_name, p_route_mount_chain, p_route_template, p_route_locale);",
      "  account_id := p_account_id; RETURN NEXT;",
      "END;",
      "$$;"
    ]

completeVerificationResendWithActivityFunction :: Text
completeVerificationResendWithActivityFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.complete_verification_resend_with_activity(",
      "  p_account_id TEXT, p_token_digest TEXT, p_now BIGINT, p_audit_account_id TEXT, p_request_id TEXT, p_event_code TEXT, p_payload_version SMALLINT, p_payload_detail TEXT,",
      "  p_route_endpoint_name TEXT, p_route_mount_chain TEXT, p_route_template TEXT, p_route_locale TEXT",
      ") RETURNS TABLE(outcome TEXT, value TEXT)",
      "LANGUAGE plpgsql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit, web_api",
      "AS $$",
      "DECLARE v_outcome TEXT; v_value TEXT;",
      "BEGIN",
      "  IF p_account_id <> p_audit_account_id THEN RAISE EXCEPTION USING ERRCODE = '22023', MESSAGE = 'verification resend audit subject does not match delivery account'; END IF;",
      "  SELECT completion.outcome, completion.value INTO v_outcome, v_value FROM web_api.complete_verification_resend(p_account_id, p_token_digest, p_now) AS completion;",
      "  IF NOT FOUND THEN RAISE EXCEPTION USING ERRCODE = 'XX000', MESSAGE = 'verification resend completion returned no result'; END IF;",
      "  IF v_outcome = 'lost' THEN outcome := 'lost'; value := ''; RETURN NEXT; RETURN; END IF;",
      "  IF v_outcome <> 'settled' OR v_value <> p_account_id THEN RAISE EXCEPTION USING ERRCODE = 'XX000', MESSAGE = 'verification resend completion returned an invalid result'; END IF;",
      "  PERFORM activity_id FROM account_audit.append_activity(p_audit_account_id, p_request_id, p_event_code, p_payload_version, p_payload_detail, p_route_endpoint_name, p_route_mount_chain, p_route_template, p_route_locale);",
      "  outcome := 'settled'; value := p_account_id; RETURN NEXT;",
      "END;",
      "$$;"
    ]

issueAccountSessionWithActivityFunction :: Text
issueAccountSessionWithActivityFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.issue_account_session_with_activity(",
      "  p_session_id TEXT, p_account_id TEXT, p_issued_at_nanoseconds BIGINT, p_expires_at_nanoseconds BIGINT,",
      "  p_audit_account_id TEXT, p_request_id TEXT, p_event_code TEXT, p_payload_version SMALLINT, p_payload_detail TEXT,",
      "  p_route_endpoint_name TEXT, p_route_mount_chain TEXT, p_route_template TEXT, p_route_locale TEXT",
      ") RETURNS TABLE(session_id TEXT)",
      "LANGUAGE plpgsql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit, web_api",
      "AS $$",
      "DECLARE v_inserted BIGINT;",
      "BEGIN",
      "  IF p_account_id <> p_audit_account_id THEN",
      "    RAISE EXCEPTION USING ERRCODE = '22023', MESSAGE = 'account session audit subject does not match session principal';",
      "  END IF;",
      "  INSERT INTO web_api.account_sessions (session_id, account_id, issued_at_nanoseconds, expires_at_nanoseconds)",
      "  VALUES (p_session_id, p_account_id, p_issued_at_nanoseconds, p_expires_at_nanoseconds)",
      "  ON CONFLICT (session_id) DO NOTHING;",
      "  GET DIAGNOSTICS v_inserted = ROW_COUNT;",
      "  IF v_inserted = 0 THEN RETURN; END IF;",
      "  PERFORM activity_id FROM account_audit.append_activity(",
      "    p_audit_account_id, p_request_id, p_event_code, p_payload_version, p_payload_detail,",
      "    p_route_endpoint_name, p_route_mount_chain, p_route_template, p_route_locale",
      "  );",
      "  session_id := p_session_id;",
      "  RETURN NEXT;",
      "END;",
      "$$;"
    ]

createRole :: Text -> Text
createRole roleName =
  "DO $$ BEGIN IF NOT EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = "
    <> quotedLiteral roleName
    <> ") THEN CREATE ROLE "
    <> quotedIdentifier roleName
    <> " NOLOGIN NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT; END IF; END $$;"

readerHasScopeFunction :: Text
readerHasScopeFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.reader_has_scope(p_audit_scope_id TEXT)",
      "RETURNS BOOLEAN",
      "LANGUAGE sql",
      "STABLE",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit",
      "AS $$",
      "  SELECT EXISTS (",
      "    SELECT 1",
      "    FROM account_audit.reader_scope_grant",
      "    WHERE reader_role_name = session_user",
      "      AND audit_scope_id = p_audit_scope_id",
      "  );",
      "$$;"
    ]

appendActivityFunction :: Text
appendActivityFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.append_activity(",
      "  p_account_id TEXT, p_request_id TEXT, p_event_code TEXT, p_payload_version SMALLINT, p_payload_detail TEXT,",
      "  p_route_endpoint_name TEXT, p_route_mount_chain TEXT, p_route_template TEXT, p_route_locale TEXT",
      ") RETURNS TABLE(activity_id BIGINT, utilization_percent SMALLINT)",
      "LANGUAGE plpgsql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit",
      "AS $$",
      "DECLARE",
      "  v_scope_id TEXT;",
      "  v_occurred_at TIMESTAMPTZ := statement_timestamp();",
      "  v_partition_limit BIGINT;",
      "  v_accepted_row_count BIGINT;",
      "BEGIN",
      "  IF octet_length(p_account_id) NOT BETWEEN 1 AND 128",
      "     OR p_request_id !~ '^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$'",
      "     OR p_payload_version <> 1",
      "     OR (p_route_endpoint_name IS NULL AND (p_route_mount_chain IS NOT NULL OR p_route_template IS NOT NULL OR p_route_locale IS NOT NULL))",
      "     OR (p_route_endpoint_name IS NOT NULL AND (p_route_mount_chain IS NULL OR p_route_template IS NULL OR p_route_locale IS NULL))",
      "     OR (p_route_endpoint_name IS NOT NULL AND octet_length(p_route_endpoint_name) NOT BETWEEN 1 AND 128)",
      "     OR (p_route_mount_chain IS NOT NULL AND octet_length(p_route_mount_chain) NOT BETWEEN 1 AND 512)",
      "     OR (p_route_template IS NOT NULL AND octet_length(p_route_template) NOT BETWEEN 1 AND 512)",
      "     OR (p_route_locale IS NOT NULL AND octet_length(p_route_locale) NOT BETWEEN 1 AND 16)",
      "     OR NOT ((p_event_code = 'pending-registration-delivered' AND p_payload_detail IN ('created', 'retried'))",
      "          OR (p_event_code = 'verification-resend-delivered' AND p_payload_detail IS NULL)",
      "          OR (p_event_code = 'email-verified' AND p_payload_detail IS NULL)",
      "          OR (p_event_code = 'authentication-rejected' AND p_payload_detail IN ('password', 'second-factor'))",
      "          OR (p_event_code = 'mfa-enrolled' AND p_payload_detail IS NULL)",
      "          OR (p_event_code = 'account-session-issued' AND p_payload_detail IN ('password', 'totp', 'recovery-code'))",
      "          OR (p_event_code = 'account-session-ended' AND p_payload_detail IN ('explicit-logout', 'revoked'))) THEN",
      "    RAISE EXCEPTION USING ERRCODE = '22023', MESSAGE = 'account audit append received invalid typed fields';",
      "  END IF;",
      "",
      "  SELECT audit_scope_id INTO v_scope_id",
      "  FROM account_audit.runtime_scope",
      "  WHERE runtime_role_name = session_user;",
      "  IF NOT FOUND THEN",
      "    RAISE EXCEPTION USING ERRCODE = '42501', MESSAGE = 'account audit runtime scope is not assigned';",
      "  END IF;",
      "",
      "  SELECT partition_row_limit INTO v_partition_limit",
      "  FROM account_audit.audit_policy",
      "  WHERE policy_key;",
      "  IF NOT FOUND THEN",
      "    RAISE EXCEPTION USING ERRCODE = '55000', MESSAGE = 'account audit policy is unavailable';",
      "  END IF;",
      "",
      "  UPDATE account_audit.partition_registry",
      "  SET accepted_row_count = accepted_row_count + 1",
      "  WHERE lower_bound <= v_occurred_at",
      "    AND upper_bound > v_occurred_at",
      "    AND accepted_row_count < v_partition_limit",
      "  RETURNING accepted_row_count INTO v_accepted_row_count;",
      "",
      "  IF NOT FOUND THEN",
      "    IF EXISTS (SELECT 1 FROM account_audit.partition_registry WHERE lower_bound <= v_occurred_at AND upper_bound > v_occurred_at) THEN",
      "      RAISE EXCEPTION USING ERRCODE = '53100', MESSAGE = 'account audit partition capacity is exhausted';",
      "    END IF;",
      "    RAISE EXCEPTION USING ERRCODE = '55000', MESSAGE = 'account audit writable partition is unavailable';",
      "  END IF;",
      "",
      "  INSERT INTO account_audit.activity (occurred_at, audit_scope_id, account_id, request_id, event_code, payload_version, payload_detail, route_endpoint_name, route_mount_chain, route_template, route_locale)",
      "  VALUES (v_occurred_at, v_scope_id, p_account_id, p_request_id, p_event_code, p_payload_version, p_payload_detail, p_route_endpoint_name, p_route_mount_chain, p_route_template, p_route_locale)",
      "  RETURNING activity.activity_id INTO activity_id;",
      "",
      "  utilization_percent := LEAST(100, ((v_accepted_row_count * 100) / v_partition_limit)::SMALLINT);",
      "  RETURN NEXT;",
      "END;",
      "$$;"
    ]

maintainActivityPartitionsAtFunction :: Text
maintainActivityPartitionsAtFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.maintain_activity_partitions_at(p_reference_time TIMESTAMPTZ, p_retention INTERVAL)",
      "RETURNS TABLE(created_partition_count INTEGER, dropped_partition_count INTEGER, oldest_retained_bound TIMESTAMPTZ, newest_prepared_bound TIMESTAMPTZ)",
      "LANGUAGE plpgsql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit",
      "AS $$",
      "DECLARE",
      "  v_current_month TIMESTAMPTZ;",
      "  v_next_month TIMESTAMPTZ;",
      "  v_retention_floor TIMESTAMPTZ;",
      "  v_partition_start TIMESTAMPTZ;",
      "  v_partition_end TIMESTAMPTZ;",
      "  v_partition_name NAME;",
      "  v_catalog_count BIGINT;",
      "  v_registry_count BIGINT;",
      "  v_registry_row RECORD;",
      "BEGIN",
      "  IF p_reference_time IS NULL OR p_retention <= interval '0' THEN",
      "    RAISE EXCEPTION USING ERRCODE = '22023', MESSAGE = 'account audit maintenance requires a positive reference policy';",
      "  END IF;",
      "  PERFORM set_config('TimeZone', 'UTC', true);",
      "  v_current_month := date_trunc('month', p_reference_time AT TIME ZONE 'UTC') AT TIME ZONE 'UTC';",
      "  v_next_month := v_current_month + interval '1 month';",
      "  v_retention_floor := date_trunc('month', (p_reference_time - p_retention) AT TIME ZONE 'UTC') AT TIME ZONE 'UTC';",
      "  created_partition_count := 0;",
      "  dropped_partition_count := 0;",
      "",
      "  SELECT count(*) INTO v_catalog_count",
      "  FROM pg_catalog.pg_inherits inheritance",
      "  WHERE inheritance.inhparent = 'account_audit.activity'::regclass;",
      "  SELECT count(*) INTO v_registry_count FROM account_audit.partition_registry;",
      "  IF v_catalog_count <> v_registry_count",
      "     OR EXISTS (",
      "       SELECT 1 FROM account_audit.partition_registry registry",
      "       WHERE NOT EXISTS (",
      "         SELECT 1 FROM pg_catalog.pg_inherits inheritance",
      "         JOIN pg_catalog.pg_class child ON child.oid = inheritance.inhrelid",
      "         JOIN pg_catalog.pg_namespace namespace ON namespace.oid = child.relnamespace",
      "         WHERE inheritance.inhparent = 'account_audit.activity'::regclass",
      "           AND namespace.nspname = 'account_audit'",
      "           AND child.relname = registry.partition_name",
      "           AND pg_catalog.pg_get_expr(child.relpartbound, child.oid) = format('FOR VALUES FROM (%L) TO (%L)', registry.lower_bound, registry.upper_bound)",
      "       )",
      "     ) THEN",
      "    RAISE EXCEPTION USING ERRCODE = '55000', MESSAGE = 'account audit partition registry disagrees with PostgreSQL catalog';",
      "  END IF;",
      "",
      "  FOREACH v_partition_start IN ARRAY ARRAY[v_current_month, v_next_month] LOOP",
      "    v_partition_end := v_partition_start + interval '1 month';",
      "    v_partition_name := ('activity_' || to_char(v_partition_start AT TIME ZONE 'UTC', 'YYYY_MM'))::NAME;",
      "    IF NOT EXISTS (SELECT 1 FROM account_audit.partition_registry WHERE partition_name = v_partition_name) THEN",
      "      EXECUTE format('CREATE TABLE account_audit.%I PARTITION OF account_audit.activity FOR VALUES FROM (%L) TO (%L)', v_partition_name, v_partition_start, v_partition_end);",
      "      EXECUTE format('ALTER TABLE account_audit.%I OWNER TO account_audit_owner', v_partition_name);",
      "      EXECUTE format('REVOKE ALL ON TABLE account_audit.%I FROM PUBLIC, web_api_audit_reader, web_api_audit_scheduler', v_partition_name);",
      "      INSERT INTO account_audit.partition_registry (partition_name, lower_bound, upper_bound) VALUES (v_partition_name, v_partition_start, v_partition_end);",
      "      created_partition_count := created_partition_count + 1;",
      "    END IF;",
      "  END LOOP;",
      "",
      "  FOR v_registry_row IN SELECT partition_name, lower_bound, upper_bound FROM account_audit.partition_registry WHERE upper_bound <= v_retention_floor ORDER BY upper_bound LOOP",
      "    IF NOT EXISTS (",
      "      SELECT 1 FROM pg_catalog.pg_inherits inheritance",
      "      JOIN pg_catalog.pg_class child ON child.oid = inheritance.inhrelid",
      "      JOIN pg_catalog.pg_namespace namespace ON namespace.oid = child.relnamespace",
      "      WHERE inheritance.inhparent = 'account_audit.activity'::regclass",
      "        AND namespace.nspname = 'account_audit'",
      "        AND child.relname = v_registry_row.partition_name",
      "        AND pg_catalog.pg_get_expr(child.relpartbound, child.oid) = format('FOR VALUES FROM (%L) TO (%L)', v_registry_row.lower_bound, v_registry_row.upper_bound)",
      "    ) THEN",
      "      RAISE EXCEPTION USING ERRCODE = '55000', MESSAGE = 'account audit partition registry disagrees with PostgreSQL catalog';",
      "    END IF;",
      "    EXECUTE format('DROP TABLE account_audit.%I', v_registry_row.partition_name);",
      "    DELETE FROM account_audit.partition_registry WHERE partition_name = v_registry_row.partition_name;",
      "    dropped_partition_count := dropped_partition_count + 1;",
      "  END LOOP;",
      "",
      "  SELECT min(lower_bound), max(upper_bound) INTO oldest_retained_bound, newest_prepared_bound FROM account_audit.partition_registry;",
      "  RETURN NEXT;",
      "END;",
      "$$;"
    ]

maintainActivityPartitionsFunction :: Text
maintainActivityPartitionsFunction =
  Text.unlines
    [ "CREATE OR REPLACE FUNCTION account_audit.maintain_activity_partitions()",
      "RETURNS TABLE(created_partition_count INTEGER, dropped_partition_count INTEGER, oldest_retained_bound TIMESTAMPTZ, newest_prepared_bound TIMESTAMPTZ)",
      "LANGUAGE sql",
      "SECURITY DEFINER",
      "SET search_path = pg_catalog, account_audit",
      "AS $$",
      "  SELECT *",
      "  FROM account_audit.maintain_activity_partitions_at(",
      "    statement_timestamp(),",
      "    make_interval(months => (SELECT retained_complete_months FROM account_audit.audit_policy WHERE policy_key))",
      "  );",
      "$$;"
    ]

quotedIdentifier :: Text -> Text
quotedIdentifier value = "\"" <> Text.replace "\"" "\"\"" value <> "\""

quotedLiteral :: Text -> Text
quotedLiteral value = "'" <> Text.replace "'" "''" value <> "'"
