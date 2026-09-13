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
import Data.Word (Word64)
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
import HarchWeb.Time (UnixTimeNanoseconds, unixTimeNanoseconds, unixTimeNanosecondsValue)
import HarchWeb.Username (mkUsername, usernameText)
import Text.Read (readMaybe)
import WebApi.Account
  ( AccountProfile (..),
    AccountProfileStore (..),
    AccountStore (..),
    AccountStoreError (..),
    CreatePendingAccountOutcome (..),
    PendingAccount (..),
    PendingRegistrationClaim (..),
    PendingRegistrationDeliveryStage (..),
    PendingRegistrationStoragePolicy,
    VerificationResendAdmission (..),
    VerificationResendClaim (..),
    VerificationResendClaimSettlement (..),
    VerificationResendPolicy,
    VerificationResendSuppression (..),
    pendingRegistrationClaimLeaseNanoseconds,
    pendingRegistrationMaximumAccounts,
    verificationResendClaimLeaseNanoseconds,
    verificationResendMaximumDeliveries,
    verificationResendMaximumRecords,
    verificationResendWindowNanoseconds,
  )
import WebApi.Login
  ( AccountCredential (..),
    AccountCredentialStore (..),
    AccountCredentialStoreError (..),
  )
import WebApi.Postgres.Pool (PostgresPool)
import WebApi.Postgres.Runtime (renderUnexpectedResultShape, runPooledParameterizedRowsQuery)

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
  AccountCredentialStore findCredentialByEmail findCredentialByUsername replacePasswordHash
  where
    findCredentialByEmail emailAddress =
      findCredential findAccountCredentialByEmailQuery [emailAddressText emailAddress]
    findCredentialByUsername username =
      findCredential findAccountCredentialByUsernameQuery [usernameText username]
    findCredential query parameters =
      runExceptT $ do
        rows <- runCredentialQuery query parameters
        liftEither (decodeAccountCredentialRows rows)
    replacePasswordHash accountId previousHash replacementHash =
      runExceptT $ do
        rows <-
          runCredentialQuery
            replacePasswordHashIfCurrentQuery
            [accountIdText accountId, passwordHashText previousHash, passwordHashText replacementHash]
        liftEither (decodePasswordHashReplacement accountId rows)
    runCredentialQuery query parameters =
      runStoreQuery AccountCredentialStoreUnavailable (runQuery source query parameters)

buildRuntimePostgresAccountStoreWithRunner ::
  (source -> Text -> [Text] -> IO (Either Text [[Text]])) ->
  source ->
  AccountStore
buildRuntimePostgresAccountStoreWithRunner runQuery source =
  AccountStore
    { createPendingAccount = createAccount,
      completePendingRegistrationDelivery = completeRegistrationDelivery,
      releasePendingRegistrationDelivery = releaseRegistrationDelivery,
      reserveVerificationResend = reserveResend,
      completeVerificationResend = completeResend,
      releaseVerificationResend = releaseResend,
      replaceEmailVerification = replaceVerification,
      findEmailVerification = findVerification,
      consumeEmailVerification = consumeVerification
    }
  where
    createAccount storagePolicy pendingAccount =
      runExceptT $ do
        rows <-
          unavailableAccountStoreQuery $
            runQuery source stagePendingRegistrationQuery (stagePendingRegistrationParameters storagePolicy pendingAccount)
        liftEither (decodeStagedPendingAccount pendingAccount rows)

    completeRegistrationDelivery = updateRegistrationDeliveryClaim completePendingRegistrationDeliveryQuery

    releaseRegistrationDelivery = updateRegistrationDeliveryClaim releasePendingRegistrationDeliveryQuery

    reserveResend policy verification now =
      runExceptT $ do
        rows <- unavailableAccountStoreQuery $ runQuery source reserveVerificationResendQuery (reserveVerificationResendParameters policy verification now)
        liftEither (decodeVerificationResendAdmission verification rows)

    completeResend claim now =
      runExceptT $ do
        rows <- unavailableAccountStoreQuery $ runQuery source completeVerificationResendQuery [accountIdText (verificationResendClaimAccountId claim), emailVerificationTokenDigestText (verificationResendClaimTokenDigest claim), Text.pack (show (unixTimeNanosecondsValue now))]
        liftEither (decodeVerificationResendSettlement claim rows)

    releaseResend claim =
      runExceptT $ do
        rows <- unavailableAccountStoreQuery $ runQuery source releaseVerificationResendQuery [accountIdText (verificationResendClaimAccountId claim), emailVerificationTokenDigestText (verificationResendClaimTokenDigest claim)]
        liftEither (decodeVerificationResendSettlement claim rows)

    updateRegistrationDeliveryClaim query claim =
      runExceptT $ do
        rows <-
          unavailableAccountStoreQuery $
            runQuery
              source
              query
              [ accountIdText (pendingRegistrationClaimAccountId claim),
                emailVerificationTokenDigestText (pendingRegistrationClaimTokenDigest claim)
              ]
        liftEither (decodeRegistrationDeliveryClaimUpdate claim rows)

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
    _ -> Left (AccountCredentialStoreCorruptData ("unexpected account credential lookup result: " <> renderUnexpectedResultShape rows))

decodePasswordHashReplacement :: AccountId -> [[Text]] -> Either AccountCredentialStoreError Bool
decodePasswordHashReplacement accountId rows =
  case rows of
    [] -> Right False
    [[returnedAccountId]]
      | returnedAccountId == accountIdText accountId -> Right True
    _ -> Left (AccountCredentialStoreCorruptData ("unexpected password-hash replacement result: " <> renderUnexpectedResultShape rows))

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
    _ -> Left (AccountStoreCorruptData ("unexpected account profile lookup result: " <> renderUnexpectedResultShape rows))

nonEmptyText :: Text -> Maybe Text
nonEmptyText "" = Nothing
nonEmptyText value = Just value

decodeStagedPendingAccount :: PendingAccount -> [[Text]] -> Either AccountStoreError CreatePendingAccountOutcome
decodeStagedPendingAccount pendingAccount rows =
  case rows of
    [["created", accountIdValue]] -> do
      accountId <- maybe (Left (AccountStoreCorruptData "pending-registration staging returned an invalid account id")) Right (mkAccountId accountIdValue)
      Right (PendingAccountDeliveryClaimed (PendingRegistrationClaim accountId (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) PendingRegistrationCreated))
    [["retried", accountIdValue]] -> do
      accountId <- maybe (Left (AccountStoreCorruptData "pending-registration staging returned an invalid account id")) Right (mkAccountId accountIdValue)
      Right (PendingAccountDeliveryClaimed (PendingRegistrationClaim accountId (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)) PendingRegistrationRetried))
    [["email-taken", ""]] -> Right PendingAccountEmailTaken
    [["username-taken", ""]] -> Right PendingAccountUsernameTaken
    [["storage-exhausted", ""]] -> Right PendingAccountStorageExhausted
    _ -> Left (AccountStoreCorruptData ("unexpected pending-registration staging result: " <> renderUnexpectedResultShape rows))

decodeRegistrationDeliveryClaimUpdate :: PendingRegistrationClaim -> [[Text]] -> Either AccountStoreError Bool
decodeRegistrationDeliveryClaimUpdate claim rows =
  case rows of
    [] -> Right False
    [[accountIdValue]]
      | accountIdValue == accountIdText (pendingRegistrationClaimAccountId claim) -> Right True
    _ -> Left (AccountStoreCorruptData ("unexpected pending-registration delivery update result: " <> renderUnexpectedResultShape rows))

decodeVerificationResendAdmission :: StoredEmailVerification -> [[Text]] -> Either AccountStoreError VerificationResendAdmission
decodeVerificationResendAdmission verification rows =
  case rows of
    [["reserved", accountIdValue]] -> do
      accountId <- maybe (Left (AccountStoreCorruptData "verification resend reservation returned an invalid account id")) Right (mkAccountId accountIdValue)
      if accountId == storedVerificationAccountId verification
        then Right (VerificationResendReserved (VerificationResendClaim accountId (storedVerificationTokenDigest verification)))
        else Left (AccountStoreCorruptData "verification resend reservation returned a different account id")
    [["throttled", ""]] -> Right (VerificationResendAdmissionSuppressed VerificationResendThrottled)
    [["no-longer-pending", ""]] -> Right (VerificationResendAdmissionSuppressed VerificationResendNoLongerPending)
    [["storage-exhausted", ""]] -> Left (AccountStoreUnavailable "verification resend storage is exhausted")
    _ -> Left (AccountStoreCorruptData ("unexpected verification resend reservation result: " <> renderUnexpectedResultShape rows))

decodeVerificationResendSettlement :: VerificationResendClaim -> [[Text]] -> Either AccountStoreError VerificationResendClaimSettlement
decodeVerificationResendSettlement claim rows =
  case rows of
    [["settled", accountIdValue]]
      | accountIdValue == accountIdText (verificationResendClaimAccountId claim) -> Right VerificationResendClaimSettled
    [["lost", ""]] -> Right VerificationResendClaimLost
    _ -> Left (AccountStoreCorruptData ("unexpected verification resend claim settlement result: " <> renderUnexpectedResultShape rows))

decodeReplacedVerification :: StoredEmailVerification -> [[Text]] -> Either AccountStoreError Bool
decodeReplacedVerification verification rows =
  case rows of
    [] -> Right False
    [[accountIdValue]]
      | accountIdValue == accountIdText (storedVerificationAccountId verification) -> Right True
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification replacement result: " <> renderUnexpectedResultShape rows))

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
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification result: " <> renderUnexpectedResultShape rows))

decodeConsumedVerification :: [[Text]] -> Either AccountStoreError (Maybe AccountId)
decodeConsumedVerification rows =
  case rows of
    [] -> Right Nothing
    [[accountIdValue]] ->
      maybe
        (Left (AccountStoreCorruptData "email verification was consumed for an invalid account id"))
        (Right . Just)
        (mkAccountId accountIdValue)
    _ -> Left (AccountStoreCorruptData ("unexpected email-verification consumption result: " <> renderUnexpectedResultShape rows))

stagePendingRegistrationParameters :: PendingRegistrationStoragePolicy -> PendingAccount -> [Text]
stagePendingRegistrationParameters storagePolicy pendingAccount =
  [ accountIdText (pendingAccountId pendingAccount),
    emailAddressText (pendingAccountEmail pendingAccount),
    passwordHashText (pendingAccountPasswordHash pendingAccount),
    emailVerificationTokenDigestText (storedVerificationTokenDigest (pendingAccountVerification pendingAccount)),
    Text.pack (show (unixTimeNanosecondsValue (storedVerificationExpiresAtNanoseconds (pendingAccountVerification pendingAccount)))),
    Text.pack (show now),
    maybe Text.empty usernameText (pendingAccountUsername pendingAccount),
    fromMaybe Text.empty (pendingAccountDisplayName pendingAccount),
    Text.pack (show maximumAccounts),
    Text.pack (show claimRecoveryBefore)
  ]
  where
    now = unixTimeNanosecondsValue (pendingAccountCreatedAtNanoseconds pendingAccount)
    maximumAccounts = pendingRegistrationMaximumAccounts storagePolicy
    claimRecoveryBefore =
      if now > pendingRegistrationClaimLeaseNanoseconds storagePolicy
        then now - pendingRegistrationClaimLeaseNanoseconds storagePolicy
        else 0

reserveVerificationResendParameters :: VerificationResendPolicy -> StoredEmailVerification -> UnixTimeNanoseconds -> [Text]
reserveVerificationResendParameters policy verification now =
  [ accountIdText (storedVerificationAccountId verification),
    emailVerificationTokenDigestText (storedVerificationTokenDigest verification),
    emailAddressText (storedVerificationEmail verification),
    Text.pack (show (unixTimeNanosecondsValue (storedVerificationExpiresAtNanoseconds verification))),
    Text.pack (show current),
    Text.pack (show windowSince),
    Text.pack (show leaseRecoveryBefore),
    Text.pack (show (verificationResendMaximumDeliveries policy)),
    Text.pack (show (verificationResendMaximumRecords policy))
  ]
  where
    current = unixTimeNanosecondsValue now
    windowSince = subtractBounded current (verificationResendWindowNanoseconds policy)
    leaseRecoveryBefore = subtractBounded current (verificationResendClaimLeaseNanoseconds policy)

subtractBounded :: Word64 -> Word64 -> Word64
subtractBounded value amount =
  case compare value amount of
    GT -> value - amount
    EQ -> 0
    LT -> 0

stagePendingRegistrationQuery, completePendingRegistrationDeliveryQuery, releasePendingRegistrationDeliveryQuery, reserveVerificationResendQuery, completeVerificationResendQuery, releaseVerificationResendQuery, replaceEmailVerificationQuery, findEmailVerificationQuery, consumeEmailVerificationQuery, replacePasswordHashIfCurrentQuery :: Text
stagePendingRegistrationQuery = "SELECT outcome, value FROM web_api.stage_pending_registration($1, $2, $3, $4, $5, $6, $7, $8, $9, $10);"
completePendingRegistrationDeliveryQuery = "UPDATE web_api.email_verifications SET delivery_state = 'delivered', delivery_claimed_at_nanoseconds = NULL WHERE account_id = $1 AND token_digest = $2 AND delivery_state = 'claimed' RETURNING account_id;"
releasePendingRegistrationDeliveryQuery = "UPDATE web_api.email_verifications SET delivery_state = 'awaiting', delivery_claimed_at_nanoseconds = NULL WHERE account_id = $1 AND token_digest = $2 AND delivery_state = 'claimed' RETURNING account_id;"
reserveVerificationResendQuery = "SELECT outcome, value FROM web_api.reserve_verification_resend($1, $2, $3, $4, $5, $6, $7, $8, $9);"
completeVerificationResendQuery = "SELECT outcome, value FROM web_api.complete_verification_resend($1, $2, $3);"
releaseVerificationResendQuery = "SELECT outcome, value FROM web_api.release_verification_resend($1, $2);"
replaceEmailVerificationQuery = "WITH pending_account AS (SELECT account_id FROM web_api.accounts WHERE account_id = $1 AND email_verified_at_nanoseconds IS NULL FOR UPDATE), removed_verifications AS (DELETE FROM web_api.email_verifications WHERE account_id IN (SELECT account_id FROM pending_account)) INSERT INTO web_api.email_verifications (token_digest, account_id, email_normalized, expires_at_nanoseconds) SELECT $2, account_id, $3, $4 FROM pending_account RETURNING account_id;"
findEmailVerificationQuery = "SELECT account_id, email_normalized, expires_at_nanoseconds FROM web_api.email_verifications WHERE token_digest = $1;"
consumeEmailVerificationQuery = "WITH consumed_verification AS (DELETE FROM web_api.email_verifications WHERE token_digest = $1 AND expires_at_nanoseconds > $2 RETURNING account_id) UPDATE web_api.accounts SET email_verified_at_nanoseconds = $2 WHERE account_id IN (SELECT account_id FROM consumed_verification) RETURNING account_id;"
replacePasswordHashIfCurrentQuery = "UPDATE web_api.accounts SET password_hash = $3 WHERE account_id = $1 AND password_hash = $2 RETURNING account_id;"

findAccountCredentialByEmailQuery, findAccountCredentialByUsernameQuery, findAccountProfileQuery :: Text
findAccountCredentialByEmailQuery = "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE email_normalized = $1;"
findAccountCredentialByUsernameQuery = "SELECT account_id, password_hash, COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE lower(username) = lower($1);"
findAccountProfileQuery = "SELECT account_id, email_normalized, COALESCE(username, ''), COALESCE(display_name, ''), COALESCE(email_verified_at_nanoseconds::TEXT, '') FROM web_api.accounts WHERE account_id = $1;"
