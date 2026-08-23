{-# LANGUAGE OverloadedStrings #-}

module WebApi.Postgres.AccountRepository
  ( buildRuntimePostgresAccountStore,
    buildRuntimePostgresAccountStoreWithRunner,
    buildRuntimePostgresAccountProfileStore,
    buildRuntimePostgresAccountProfileStoreWithRunner,
    buildRuntimePostgresAccountCredentialStore,
    buildRuntimePostgresAccountCredentialStoreWithRunner,
  )
where

import Control.Monad.Except (ExceptT, liftEither, runExceptT)
import Core.Control.Error (liftEitherWith)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Account
  ( AccountId,
    EmailVerificationTokenDigest,
    StoredEmailVerification (..),
    accountIdText,
    emailVerificationTokenDigestText,
    mkAccountId,
    storedVerificationAccountId,
    storedVerificationEmail,
    storedVerificationExpiresAtNanoseconds,
    storedVerificationTokenDigest,
  )
import HarchWeb.Email (emailAddressText, mkEmailAddress)
import HarchWeb.Password (passwordHashText, readPasswordHash)
import HarchWeb.Time (unixTimeNanoseconds, unixTimeNanosecondsValue)
import HarchWeb.Username (mkUsername, usernameText)
import Text.Read (readMaybe)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStore (..),
    AccountStoreError (..),
    CreatePendingAccountOutcome (..),
    PendingAccount (..),
  )
import WebApi.Login
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (runPooledParameterizedRowsQuery)

buildRuntimePostgresAccountStore :: PostgresPool -> AccountStore
buildRuntimePostgresAccountStore !pool =
  buildRuntimePostgresAccountStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresAccountProfileStore :: PostgresPool -> AccountProfileStore
buildRuntimePostgresAccountProfileStore !pool =
  buildRuntimePostgresAccountProfileStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresAccountCredentialStore :: PostgresPool -> AccountCredentialStore
buildRuntimePostgresAccountCredentialStore !pool =
  buildRuntimePostgresAccountCredentialStoreWithRunner runPooledParameterizedRowsQuery pool

buildRuntimePostgresAccountCredentialStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountCredentialStore
buildRuntimePostgresAccountCredentialStoreWithRunner runQuery source =
  AccountCredentialStore findCredentialByEmail findCredentialByUsername
  where
    findCredentialByEmail emailAddress =
      findCredential findAccountCredentialByEmailQuery [emailAddressText emailAddress]
    findCredentialByUsername username =
      findCredential findAccountCredentialByUsernameQuery [usernameText username]
    findCredential query parameters =
      runExceptT $ do
        rows <-
          runStoreQuery AccountCredentialStoreUnavailable $
            runQuery source query parameters
        liftEither (decodeAccountCredentialRows rows)

buildRuntimePostgresAccountStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountStore
buildRuntimePostgresAccountStoreWithRunner runQuery source =
  AccountStore
    { createPendingAccount = createAccount,
      replaceEmailVerification = replaceVerification,
      findEmailVerification = findVerification,
      consumeEmailVerification = consumeVerification
    }
  where
    -- Two round trips, not one atomic statement: the accounts table has two
    -- independent unique constraints (email, case-insensitive username),
    -- and the insert below only targets the email one with
    -- 'ON CONFLICT (email_normalized) DO NOTHING', so a username collision
    -- must be found before attempting it. This leaves a narrow race for two
    -- concurrent registrations racing for the same available username: both
    -- can pass this check, and the loser's insert then violates the
    -- username unique index outright (an 'AccountStoreUnavailable' error,
    -- not a clean 'PendingAccountUsernameTaken' outcome) rather than a
    -- silent no-op. Accepted rather than a single CTE union query, matching
    -- BA's own suggested design.
    createAccount pendingAccount =
      runExceptT $ do
        usernameTaken <- case pendingAccountUsername pendingAccount of
          Nothing -> pure False
          Just username -> do
            availabilityRows <-
              unavailableAccountStoreQuery $
                runQuery source usernameAvailabilityQuery [usernameText username]
            pure (not (null availabilityRows))
        if usernameTaken
          then pure PendingAccountUsernameTaken
          else do
            rows <-
              unavailableAccountStoreQuery $
                runQuery
                  source
                  createPendingAccountQuery
                  [ accountIdText (pendingAccountId pendingAccount),
                    emailAddressText (pendingAccountEmail pendingAccount),
                    passwordHashText (pendingAccountPasswordHash pendingAccount),
                    emailVerificationTokenDigestText (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)),
                    Text.pack (show (unixTimeNanosecondsValue (storedVerificationExpiresAtNanoseconds (pendingAccountVerification pendingAccount)))),
                    Text.pack (show (unixTimeNanosecondsValue (pendingAccountCreatedAtNanoseconds pendingAccount))),
                    maybe Text.empty usernameText (pendingAccountUsername pendingAccount),
                    fromMaybe Text.empty (pendingAccountDisplayName pendingAccount)
                  ]
            liftEither (decodeCreatedAccount pendingAccount rows)

    replaceVerification verification =
      runExceptT $ do
        rows <-
          unavailableAccountStoreQuery $
            runQuery
              source
              replaceEmailVerificationQuery
              [ accountIdText (storedVerificationAccountId verification),
                emailVerificationTokenDigestText (storedVerificationTokenDigest verification),
                emailAddressText (storedVerificationEmail verification),
                Text.pack (show (unixTimeNanosecondsValue (storedVerificationExpiresAtNanoseconds verification)))
              ]
        liftEither (decodeReplacedVerification verification rows)

    findVerification tokenDigest =
      runExceptT $ do
        rows <-
          unavailableAccountStoreQuery $
            runQuery source findEmailVerificationQuery [emailVerificationTokenDigestText tokenDigest]
        liftEither (decodeStoredVerification tokenDigest rows)

    consumeVerification tokenDigest now =
      runExceptT $ do
        rows <-
          unavailableAccountStoreQuery $
            runQuery
              source
              consumeEmailVerificationQuery
              [emailVerificationTokenDigestText tokenDigest, Text.pack (show (unixTimeNanosecondsValue now))]
        liftEither (decodeConsumedVerification rows)

buildRuntimePostgresAccountProfileStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountProfileStore
buildRuntimePostgresAccountProfileStoreWithRunner runQuery source =
  AccountProfileStore $ \accountId ->
    runExceptT $ do
      rows <-
        unavailableAccountStoreQuery $
          runQuery source findAccountProfileQuery [accountIdText accountId]
      liftEither (decodeAccountProfileRows accountId rows)

runStoreQuery :: (Text -> storeError) -> IO (Either Text value) -> ExceptT storeError IO value
runStoreQuery = liftEitherWith

-- | 'AccountStoreUnavailable' partially applied to 'runStoreQuery', named
-- once instead of written at each call site above: an identical partial
-- application of the same constructor written separately at multiple call
-- sites gets merged by GHC into one shared CAF, crediting only the first
-- call site's own HPC tick even though every one genuinely runs in tests.
unavailableAccountStoreQuery :: IO (Either Text value) -> ExceptT AccountStoreError IO value
unavailableAccountStoreQuery = runStoreQuery AccountStoreUnavailable

decodeAccountCredentialRows :: [[Text]] -> Either AccountCredentialStoreError (Maybe AccountCredential)
decodeAccountCredentialRows rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue, passwordHashValue, verifiedAtValue]] -> do
      accountId <- maybe (Left (AccountCredentialStoreCorruptData "account credential lookup has an invalid account id")) Right (mkAccountId accountIdValue)
      passwordHash <- maybe (Left (AccountCredentialStoreCorruptData "account credential lookup has an invalid password hash")) Right (readPasswordHash passwordHashValue)
      Right (Just (AccountCredential accountId passwordHash (verifiedAtValue /= "")))
    _ -> Left (AccountCredentialStoreCorruptData ("unexpected account credential lookup result: " <> Text.pack (show rows)))

decodeAccountProfileRows :: AccountId -> [[Text]] -> Either AccountStoreError (Maybe AccountProfile)
decodeAccountProfileRows accountId rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue, emailAddressValue, usernameValue, displayNameValue, verifiedAtValue]] -> do
      returnedAccountId <- maybe (Left (AccountStoreCorruptData "account profile lookup has an invalid account id")) Right (mkAccountId accountIdValue)
      emailAddress <- maybe (Left (AccountStoreCorruptData "account profile lookup has an invalid email address")) Right (mkEmailAddress emailAddressValue)
      maybeUsername <-
        if Text.null usernameValue
          then Right Nothing
          else Just <$> maybe (Left (AccountStoreCorruptData "account profile lookup has an invalid username")) Right (mkUsername usernameValue)
      if returnedAccountId == accountId
        then Right (Just (AccountProfile returnedAccountId emailAddress maybeUsername (nonEmptyText displayNameValue) (verifiedAtValue /= "")))
        else Left (AccountStoreCorruptData "account profile lookup returned a different account id")
    _ -> Left (AccountStoreCorruptData ("unexpected account profile lookup result: " <> Text.pack (show rows)))

nonEmptyText :: Text -> Maybe Text
nonEmptyText "" = Nothing
nonEmptyText value = Just value

decodeCreatedAccount :: PendingAccount -> [[Text]] -> Either AccountStoreError CreatePendingAccountOutcome
decodeCreatedAccount pendingAccount rows =
  case rows of
    [] -> Right PendingAccountEmailTaken
    [[createdAccountId]]
      | createdAccountId == accountIdText (pendingAccountId pendingAccount) -> Right PendingAccountCreated
    _ -> Left (AccountStoreCorruptData ("unexpected pending-account result: " <> Text.pack (show rows)))

decodeReplacedVerification :: StoredEmailVerification -> [[Text]] -> Either AccountStoreError Bool
decodeReplacedVerification verification rows =
  case rows of
    [] -> Right False
    [[accountIdValue]]
      | accountIdValue == accountIdText (storedVerificationAccountId verification) -> Right True
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification replacement result: " <> Text.pack (show rows)))

decodeStoredVerification :: EmailVerificationTokenDigest -> [[Text]] -> Either AccountStoreError (Maybe StoredEmailVerification)
decodeStoredVerification tokenDigest rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue, emailAddressValue, expiresAtValue]] -> do
      accountId <- maybe (Left (AccountStoreCorruptData "email verification has an invalid account id")) Right (mkAccountId accountIdValue)
      emailAddress <- maybe (Left (AccountStoreCorruptData "email verification has an invalid email address")) Right (mkEmailAddress emailAddressValue)
      expiresAt <- unixTimeNanoseconds <$> maybe (Left (AccountStoreCorruptData "email verification has an invalid expiry")) Right (readMaybe (Text.unpack expiresAtValue))
      Right
        ( Just
            StoredEmailVerification
              { storedVerificationAccountId = accountId,
                storedVerificationEmail = emailAddress,
                storedVerificationTokenDigest = tokenDigest,
                storedVerificationExpiresAtNanoseconds = expiresAt
              }
        )
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification result: " <> Text.pack (show rows)))

decodeConsumedVerification :: [[Text]] -> Either AccountStoreError (Maybe AccountId)
decodeConsumedVerification rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue]] ->
      maybe
        (Left (AccountStoreCorruptData "email verification was consumed for an invalid account id"))
        (Right . Just)
        (mkAccountId accountIdValue)
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification consumption result: " <> Text.pack (show rows)))

usernameAvailabilityQuery, createPendingAccountQuery, replaceEmailVerificationQuery, findEmailVerificationQuery, consumeEmailVerificationQuery :: Text
usernameAvailabilityQuery = "SELECT 1 FROM web_api.accounts WHERE username IS NOT NULL AND lower(username) = lower($1) LIMIT 1;"
createPendingAccountQuery = "WITH inserted_account AS (INSERT INTO web_api.accounts (account_id, email_normalized, password_hash, created_at_nanoseconds, username, display_name) VALUES ($1, $2, $3, $6, NULLIF($7, ''), NULLIF($8, '')) ON CONFLICT (email_normalized) DO NOTHING RETURNING account_id) INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds) SELECT $4, account_id, $2, $5 FROM inserted_account RETURNING account_id;"
replaceEmailVerificationQuery = "WITH pending_account AS (SELECT account_id FROM web_api.accounts WHERE account_id = $1 AND email_verified_at_nanoseconds IS NULL FOR UPDATE), removed_verifications AS (DELETE FROM web_api.email_verifications WHERE account_id IN (SELECT account_id FROM pending_account)) INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds) SELECT $2, account_id, $3, $4 FROM pending_account RETURNING account_id;"
findEmailVerificationQuery = "SELECT account_id, email_normalized, expires_at_nanoseconds FROM web_api.email_verifications WHERE token_digest = $1;"
consumeEmailVerificationQuery = "WITH consumed_verification AS (DELETE FROM web_api.email_verifications WHERE token_digest = $1 AND expires_at_nanoseconds > $2 RETURNING account_id) UPDATE web_api.accounts SET email_verified_at_nanoseconds = $2 WHERE account_id IN (SELECT account_id FROM consumed_verification) RETURNING account_id;"

findAccountCredentialByEmailQuery, findAccountCredentialByUsernameQuery, findAccountProfileQuery :: Text
findAccountCredentialByEmailQuery = "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE email_normalized = $1;"
findAccountCredentialByUsernameQuery = "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE lower(username) = lower($1);"
findAccountProfileQuery = "SELECT account_id, email_normalized, COALESCE(username, ''), COALESCE(display_name, ''), COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE account_id = $1;"
