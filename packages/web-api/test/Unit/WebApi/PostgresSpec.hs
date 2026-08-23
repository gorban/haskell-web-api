{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

{-# SPEC #-}

import Data.ByteString qualified as ByteString
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Account qualified as Account
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import HarchWeb.Password qualified as Password
import HarchWeb.Username qualified as Username
import Network.HTTP.Types qualified as Http
import System.Exit (ExitCode (..))
import TestSupport.RealPostgres (containerizedPsqlScriptContents, defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, ensureDefaultPostgresAvailableScript, withContainerizedPsqlOnPath)
import Unit.WebApi.TestSupport hiding (accountId, databaseConfig, emailAddress)
import WebApi.Account (AccountProfile (..), AccountProfileStore (..), AccountStore (..), CreatePendingAccountOutcome (..), PendingAccount (..))
import WebApi.App (buildAppWithDatabase)
import WebApi.Config (DatabaseConfig (..), defaultAppConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), HomePageData (..), SecondPageData (..))
import WebApi.Mfa (MfaStore (..), StoredTotpEnrollment (..))
import WebApi.Postgres (buildPostgresPageRepository)
import WebApi.Postgres.Testing (PostgresCommand (..), PostgresCommandResult (..), PostgresMigrationExecutor (..), PostgresMigrationResult (..), PostgresRunnerError (..), buildPostgresPageRepositoryWithRunner, buildRuntimePostgresAccountProfileStore, buildRuntimePostgresAccountProfileStoreWithRunner, buildRuntimePostgresAccountStore, buildRuntimePostgresAccountStoreWithRunner, buildRuntimePostgresMfaStore, buildRuntimePostgresPageRepositoryWithRunner, decodeRuntimeQueryValue, libpqConnectionValue, migrationStatementsFor, newPostgresPool, renderRuntimeConnectionErrorMessage, renderRuntimeResultErrorMessage, runPostgresMigrations, runPostgresMigrationsForRuntime, runPostgresMigrationsWithExecutor, runPostgresSeed, runPostgresSeedWithRunner, runRequiredScalarCommand, runRowsCommand, runRuntimeParameterizedRowsQuery, runRuntimeRowsQuery, runRuntimeScalarQuery, seedStatements)
import WebApi.Route (AppRoute (..), defaultRequestContext)
import WebApi.SetupPlan (TcpEndpoint (..))

spec = do
  describe "WebApi.Postgres" $ do
    it "uses bound parameters for pending account and verification persistence" $ do
      recordedQueriesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          username = fromMaybe (error "Expected username") (Username.mkUsername "person_01")
          token = requiredVerificationToken (Text.replicate 43 "a")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Just username,
                pendingAccountDisplayName = Just "Person Example",
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          runner config sql parameters =
            config `seq` do
              modifyIORef' recordedQueriesReference (<> [(sql, parameters)])
              pure $
                if "SELECT 1 FROM web_api.accounts" `Text.isInfixOf` sql
                  then Right []
                  else
                    if "INSERT INTO web_api.accounts" `Text.isInfixOf` sql
                      then Right [["account_01"]]
                      else
                        if "SELECT account_id, email_normalized" `Text.isInfixOf` sql
                          then Right [["account_01", "person@example.test", "500"]]
                          else
                            if "DELETE FROM web_api.email_verifications" `Text.isInfixOf` sql
                              then Right [["account_01"]]
                              else Left "unexpected query"
          accountStore = buildRuntimePostgresAccountStoreWithRunner runner postgresTestConfig
      assertAccountStoreSuccess (createPendingAccount accountStore pendingAccount) (\case PendingAccountCreated -> True; _ -> False)
      assertAccountStoreSuccess
        (findEmailVerification accountStore (Account.emailVerificationTokenDigest token))
        (\case Just storedVerification -> storedVerification == pendingAccountVerification pendingAccount; Nothing -> False)
      assertAccountStoreSuccess
        (replaceEmailVerification accountStore (pendingAccountVerification pendingAccount))
        id
      assertAccountStoreSuccess
        (consumeEmailVerification accountStore (Account.emailVerificationTokenDigest token) 499)
        (\case Just consumedAccountId -> consumedAccountId == accountId; Nothing -> False)
      recordedQueries <- readIORef recordedQueriesReference
      let queryText = Text.intercalate "\n" (map fst recordedQueries)
          parameterText = Text.intercalate "\n" (concatMap snd recordedQueries)
      Text.isInfixOf (Password.passwordHashText passwordHash) queryText `shouldBe` False
      Text.isInfixOf (Account.emailVerificationTokenDigestText (Account.emailVerificationTokenDigest token)) queryText `shouldBe` False
      Text.isInfixOf (Password.passwordHashText passwordHash) parameterText `shouldBe` True
      Text.isInfixOf (Account.emailVerificationTokenDigestText (Account.emailVerificationTokenDigest token)) parameterText `shouldBe` True
      Text.isInfixOf (Username.usernameText username) parameterText `shouldBe` True
      Text.isInfixOf "Person Example" parameterText `shouldBe` True

    it "checks username availability before inserting, and skips the check entirely when no username is given" $ do
      recordedQueriesReference <- newIORef []
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          username = fromMaybe (error "Expected username") (Username.mkUsername "person_01")
          token = requiredVerificationToken (Text.replicate 43 "a")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccountWith maybeUsername =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = maybeUsername,
                pendingAccountDisplayName = Nothing,
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          takenUsernameRunner _config sql parameters = do
            modifyIORef' recordedQueriesReference (<> [(sql, parameters)])
            pure $
              if "SELECT 1 FROM web_api.accounts" `Text.isInfixOf` sql
                then Right [["1"]]
                else Left "unexpected query: insert should not run after a taken username"
          takenUsernameStore = buildRuntimePostgresAccountStoreWithRunner takenUsernameRunner postgresTestConfig
      assertAccountStoreSuccess
        (createPendingAccount takenUsernameStore (pendingAccountWith (Just username)))
        (\case PendingAccountUsernameTaken -> True; _ -> False)
      takenUsernameQueries <- readIORef recordedQueriesReference
      length takenUsernameQueries `shouldBe` 1

      noUsernameQueriesReference <- newIORef []
      let noUsernameRunner _config sql parameters = do
            modifyIORef' noUsernameQueriesReference (<> [(sql, parameters)])
            pure (Right [["account_01"]])
          noUsernameStore = buildRuntimePostgresAccountStoreWithRunner noUsernameRunner postgresTestConfig
      assertAccountStoreSuccess
        (createPendingAccount noUsernameStore (pendingAccountWith Nothing))
        (\case PendingAccountCreated -> True; _ -> False)
      noUsernameQueries <- readIORef noUsernameQueriesReference
      length noUsernameQueries `shouldBe` 1
      any (\(sql, _) -> "SELECT 1 FROM web_api.accounts" `Text.isInfixOf` sql) noUsernameQueries `shouldBe` False

    it "maps malformed account-store query results to application-owned errors" $ do
      let accountId = requiredAccountId "account_01"
          emailAddress = requiredEmailAddress "person@example.test"
          token = requiredVerificationToken (Text.replicate 43 "a")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Nothing,
                pendingAccountDisplayName = Nothing,
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
          storeFor result = buildRuntimePostgresAccountStoreWithRunner (\_ _ _ -> pure result) postgresTestConfig
      assertAccountStoreError (createPendingAccount (storeFor (Left "connection failed")) pendingAccount) (isUnavailable "connection failed")
      assertAccountStoreSuccess (createPendingAccount (storeFor (Right [])) pendingAccount) (\case PendingAccountEmailTaken -> True; _ -> False)
      assertAccountStoreError (createPendingAccount (storeFor (Right [["other_account"]])) pendingAccount) (isCorrupt "unexpected pending-account result: [[\"other_account\"]]")
      assertAccountStoreError (replaceEmailVerification (storeFor (Left "connection failed")) (pendingAccountVerification pendingAccount)) (isUnavailable "connection failed")
      assertAccountStoreSuccess (replaceEmailVerification (storeFor (Right [])) (pendingAccountVerification pendingAccount)) not
      assertAccountStoreError (replaceEmailVerification (storeFor (Right [["other_account"]])) (pendingAccountVerification pendingAccount)) (isCorrupt "unexpected email-verification replacement result: [[\"other_account\"]]")
      assertAccountStoreError (findEmailVerification (storeFor (Left "connection failed")) (Account.emailVerificationTokenDigest token)) (isUnavailable "connection failed")
      assertAccountStoreSuccess (findEmailVerification (storeFor (Right [])) (Account.emailVerificationTokenDigest token)) (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (findEmailVerification (storeFor (Right [["invalid id", "person@example.test", "500"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid account id")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01", "invalid email", "500"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid email address")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01", "person@example.test", "invalid"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "email verification has an invalid expiry")
      assertAccountStoreError (findEmailVerification (storeFor (Right [["account_01"]])) (Account.emailVerificationTokenDigest token)) (isCorrupt "unexpected email-verification result: [[\"account_01\"]]")
      assertAccountStoreError (consumeEmailVerification (storeFor (Right [["invalid id"]])) (Account.emailVerificationTokenDigest token) 499) (isCorrupt "email verification was consumed for an invalid account id")
      assertAccountStoreError (consumeEmailVerification (storeFor (Left "connection failed")) (Account.emailVerificationTokenDigest token) 499) (isUnavailable "connection failed")
      assertAccountStoreSuccess (consumeEmailVerification (storeFor (Right [])) (Account.emailVerificationTokenDigest token) 499) (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (consumeEmailVerification (storeFor (Right [["account_01", "extra"]])) (Account.emailVerificationTokenDigest token) 499) (isCorrupt "unexpected email-verification consumption result: [[\"account_01\",\"extra\"]]")

    it "loads safe account profiles and rejects malformed profile rows" $ do
      let accountId = requiredAccountId "account_01"
          profileStoreFor result = buildRuntimePostgresAccountProfileStoreWithRunner (\_ _ _ -> pure result) postgresTestConfig
          username = fromMaybe (error "expected username") (Username.mkUsername "person_01")
          expectedProfile = AccountProfile accountId (requiredEmailAddress "person@example.test") (Just username) (Just "Person Example") True
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "person_01", "Person Example", "500"]])) accountId)
        ( \case
            Just profile ->
              accountProfileId profile == accountProfileId expectedProfile
                && accountProfileEmail profile == accountProfileEmail expectedProfile
                && accountProfileUsername profile == accountProfileUsername expectedProfile
                && accountProfileDisplayName profile == accountProfileDisplayName expectedProfile
                && accountProfileEmailVerified profile == accountProfileEmailVerified expectedProfile
            Nothing -> False
        )
      accountProfileId expectedProfile `shouldBe` accountId
      accountProfileEmail expectedProfile `shouldBe` requiredEmailAddress "person@example.test"
      accountProfileUsername expectedProfile `shouldBe` Just username
      accountProfileDisplayName expectedProfile `shouldBe` Just "Person Example"
      accountProfileEmailVerified expectedProfile `shouldBe` True
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "", "", ""]])) accountId)
        (\case Just profile -> not (accountProfileEmailVerified profile) && isNothing (accountProfileUsername profile) && isNothing (accountProfileDisplayName profile); Nothing -> False)
      assertAccountStoreSuccess
        (findAccountProfile (profileStoreFor (Right [])) accountId)
        (\case Nothing -> True; Just _ -> False)
      assertAccountStoreError (findAccountProfile (profileStoreFor (Left "connection failed")) accountId) (isUnavailable "connection failed")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["invalid id", "person@example.test", "", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid account id")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01", "invalid email", "", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid email address")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01", "person@example.test", "invalid username", "", ""]])) accountId) (isCorrupt "account profile lookup has an invalid username")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_02", "person@example.test", "", "", ""]])) accountId) (isCorrupt "account profile lookup returned a different account id")
      assertAccountStoreError (findAccountProfile (profileStoreFor (Right [["account_01"]])) accountId) (isCorrupt "unexpected account profile lookup result: [[\"account_01\"]]")
      testPool <- newPostgresPool (databasePoolCapacity postgresTestConfig) postgresTestConfig
      buildRuntimePostgresAccountProfileStore testPool `seq` pure ()

    it "executes the native account-profile adapter against a migrated PostgreSQL database" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
      realPool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      assertAccountStoreSuccess
        ( findAccountProfile
            (buildRuntimePostgresAccountProfileStore realPool)
            (requiredAccountId "profile_lookup_missing_01")
        )
        (\case Nothing -> True; Just _ -> False)

    it "translates database config into psql commands for page queries" $ do
      recordedCommandsReference <- newIORef []
      let runner command = do
            modifyIORef' recordedCommandsReference (<> [command])
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      successfulPostgresResult $
                        if Text.isInfixOf "locale = 'es'" sql
                          then "SSR rápido\nDatos compartidos"
                          else "Fast SSR\nShared route data"
                  | otherwise ->
                      failingPostgresResult "unexpected query"
          postgresEffect = buildPostgresPageRepositoryWithRunner runner postgresTestConfig
      timedHomeResult <- loadHomePageForRequest postgresEffect defaultRequestContext
      timedHomeResult
        `shouldBe` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      case databaseResultOperations timedHomeResult of
        [timedOperation] ->
          case ( databaseOperationStartedAtNanoseconds timedOperation,
                 databaseOperationEndedAtNanoseconds timedOperation
               ) of
            (Just startedAt, Just endedAt) -> endedAt `shouldSatisfy` (>= startedAt)
            _ -> expectationFailure "expected completed PostgreSQL operation timestamps"
        _ -> expectationFailure "expected one PostgreSQL home-page operation"
      loadHomePageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageValueForRequest postgresEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageValueForRequest postgresEffect spanishRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Charge depuis PostgreSQL.",
              secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
            }
      loadSecondPageForRequest postgresEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      recordedCommands <- readIORef recordedCommandsReference
      let expectedQueryCommand sql =
            PostgresCommand
              { postgresExecutable = "psql",
                postgresArguments =
                  [ "--host",
                    "db.internal",
                    "--port",
                    "6543",
                    "--dbname",
                    "web_api_prod",
                    "--username",
                    "web_api_app",
                    "--no-password",
                    "--set",
                    "ON_ERROR_STOP=1",
                    "--tuples-only",
                    "--no-align",
                    "--quiet",
                    "--command",
                    Text.unpack sql
                  ],
                postgresEnvironment = [("PGPASSWORD", "super-secret")]
              }
      recordedCommands
        `shouldBe` map
          expectedQueryCommand
          [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'es';",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;",
            "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';",
            "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;"
          ]

    it "maps missing rows and command failures into database errors" $ do
      let missingRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "route_slug = 'home'" sql ->
                      successfulPostgresResult Text.empty
                  | otherwise ->
                      failingPostgresResult "relation does not exist"
          postgresEffect = buildPostgresPageRepositoryWithRunner missingRunner postgresTestConfig
      loadHomePageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: ")
      loadSecondPageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "relation does not exist")
      loadSecondPageForRequest postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "relation does not exist"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "maps scalar query failures, malformed rows, and highlight query failures into explicit errors" $ do
      let homeFailureRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 2,
                      postgresStdout = Text.empty,
                      postgresStderr = Text.empty
                    }
                else successfulPostgresResult Text.empty
          malformedScalarRunner command =
            pure $
              if Text.isInfixOf "route_slug = 'home'" (commandSql command)
                then successfulPostgresResult "first\nsecond"
                else successfulPostgresResult Text.empty
          highlightFailureRunner command =
            pure $
              case commandSql command of
                sql
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" sql ->
                      successfulPostgresResult "Loaded from PostgreSQL."
                  | Text.isInfixOf "SELECT highlight FROM web_api.page_highlights" sql ->
                      failingPostgresResult "highlights unavailable"
                  | otherwise ->
                      successfulPostgresResult Text.empty
      loadHomePageValueForRequest (buildPostgresPageRepositoryWithRunner homeFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "psql command failed")
      loadHomePageValueForRequest (buildPostgresPageRepositoryWithRunner malformedScalarRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (HomePageDataError "expected exactly one row: first, second")
      loadSecondPageValueForRequest (buildPostgresPageRepositoryWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageForRequest (buildPostgresPageRepositoryWithRunner highlightFailureRunner postgresTestConfig) defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "translates database config into runtime SQL queries for page queries" $ do
      recordedScalarQueriesReference <- newIORef []
      recordedRowsQueriesReference <- newIORef []
      let scalarRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedScalarQueriesReference (<> [sql])
            pure $
              case sql of
                queryText
                  | Text.isInfixOf "route_slug = 'home'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'es'" queryText
                          then "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                          else "Server-rendered home page with stubbed content."
                  | Text.isInfixOf "SELECT summary FROM web_api.page_content WHERE route_slug = 'second'" queryText ->
                      Right $
                        if Text.isInfixOf "locale = 'es'" queryText
                          then "Charge depuis PostgreSQL."
                          else "Loaded from PostgreSQL."
                  | otherwise ->
                      Left "unexpected query"
          rowsRunner databaseConfig sql = do
            databaseConfig `shouldBe` postgresTestConfig
            modifyIORef' recordedRowsQueriesReference (<> [sql])
            pure $
              if Text.isInfixOf "locale = 'es'" sql
                then Right ["SSR rápido", "Datos compartidos"]
                else Right ["Fast SSR", "Shared route data"]
          postgresEffect =
            buildRuntimePostgresPageRepositoryWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageForRequest postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-home-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      loadSecondPageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Loaded from PostgreSQL.",
              secondPageDataHighlights = ["Fast SSR", "Shared route data"]
            }
      loadHomePageValueForRequest postgresEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageForRequest postgresEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Charge depuis PostgreSQL.",
                    secondPageDataHighlights = ["SSR rápido", "Datos compartidos"]
                  },
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }
      readIORef recordedScalarQueriesReference
        `shouldReturn` [ "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'es';",
                         "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'es';"
                       ]
      readIORef recordedRowsQueriesReference
        `shouldReturn` [ "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;",
                         "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'es' ORDER BY position ASC;"
                       ]

    it "maps runtime query failures into explicit database errors" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'home'" sql
                then Left "connection refused"
                else Right "Loaded from PostgreSQL."
          rowsRunner _ _ =
            pure (Left "highlights unavailable")
          postgresEffect =
            buildRuntimePostgresPageRepositoryWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadHomePageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "connection refused")
      loadSecondPageValueForRequest postgresEffect defaultRequestContext
        `shouldReturn` Left (SecondPageDataError "highlights unavailable")
      loadSecondPageForRequest postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "highlights unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  },
                DatabaseOperation
                  { databaseOperationName = "load-second-page-highlights",
                    databaseQueryTemplate = "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "maps runtime second-page summary failures without attempting highlight queries" $ do
      let scalarRunner _ sql =
            pure $
              if Text.isInfixOf "route_slug = 'second'" sql
                then Left "summary unavailable"
                else Right "Server-rendered home page with stubbed content."
          rowsRunner _ _ =
            error "expected runtime highlight query to be skipped when the second-page summary fails"
          postgresEffect =
            buildRuntimePostgresPageRepositoryWithRunner
              scalarRunner
              rowsRunner
              postgresTestConfig
      loadSecondPageForRequest postgresEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "summary unavailable"),
            databaseResultOperations =
              [ DatabaseOperation
                  { databaseOperationName = "load-second-page-summary",
                    databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                    databaseOperationStartedAtNanoseconds = Nothing,
                    databaseOperationEndedAtNanoseconds = Nothing
                  }
              ]
          }

    it "covers runtime libpq helper decoding branches" $ do
      decodeRuntimeQueryValue Nothing
        `shouldBe` Left "unexpected NULL column value"
      decodeRuntimeQueryValue (Just (ByteString.pack [115, 115, 114, 255]))
        `shouldBe` Right (Text.pack ['s', 's', 'r', '\xfffd'])
      renderRuntimeConnectionErrorMessage Nothing
        `shouldBe` "libpq connection failed"
      renderRuntimeConnectionErrorMessage (Just (ByteString.pack [32, 114, 117, 110, 255, 10]))
        `shouldBe` Text.pack ['r', 'u', 'n', '\xfffd']
      renderRuntimeResultErrorMessage Nothing
        `shouldBe` "libpq query failed"
      renderRuntimeResultErrorMessage (Just (ByteString.pack [32, 113, 117, 101, 114, 121, 255, 10]))
        `shouldBe` Text.pack ['q', 'u', 'e', 'r', 'y', '\xfffd']

    it "quotes libpq connection-string values so a quote or backslash cannot terminate them early" $
      expectAll
        ( (unescapeLibpqConnectionValue (libpqConnectionValue "o'brien") `shouldBe` Just "o'brien")
            :| [ unescapeLibpqConnectionValue (libpqConnectionValue "back\\slash") `shouldBe` Just "back\\slash",
                 unescapeLibpqConnectionValue (libpqConnectionValue "quote'then\\backslash") `shouldBe` Just "quote'then\\backslash",
                 unescapeLibpqConnectionValue (libpqConnectionValue "' sslmode=disable host=attacker") `shouldBe` Just "' sslmode=disable host=attacker",
                 unescapeLibpqConnectionValue (libpqConnectionValue "plain-password") `shouldBe` Just "plain-password",
                 unescapeLibpqConnectionValue (libpqConnectionValue "") `shouldBe` Just ""
               ]
        )

    it "runs direct runtime libpq queries and surfaces malformed-row, syntax, and connection failures explicitly" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig
        `shouldReturn` Right ()

      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT 'Loaded from PostgreSQL.'::text;"
        `shouldReturn` Right "Loaded from PostgreSQL."
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('Fast SSR'::text), ('Shared route data'::text)) AS runtime_rows(value);"
        `shouldReturn` Right ["Fast SSR", "Shared route data"]
      runRuntimeScalarQuery defaultRealPostgresConfig "SELECT value FROM (VALUES ('first'::text), ('second'::text)) AS runtime_rows(value);"
        `shouldReturn` Left "expected exactly one row: first, second"
      runRuntimeRowsQuery defaultRealPostgresConfig "SELECT NULL::text;"
        `shouldReturn` Left "unexpected NULL column value"
      runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT $1::text, $2::text;" ["first value", "second value"]
        `shouldReturn` Right [["first value", "second value"]]
      runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT NULL::text;" []
        `shouldReturn` Left "unexpected NULL column value"

      accountId <- Account.generateAccountId
      token <- Account.generateEmailVerificationToken
      let emailAddress = requiredEmailAddress (Account.accountIdText accountId <> "@example.test")
          passwordHash = fromMaybe (error "Expected password hash") (Password.hashPasswordWithSalt testPasswordHashingPolicy "0123456789abcdef" (Password.mkPassword "correct horse battery staple"))
          pendingAccount =
            PendingAccount
              { pendingAccountId = accountId,
                pendingAccountEmail = emailAddress,
                pendingAccountUsername = Nothing,
                pendingAccountDisplayName = Nothing,
                pendingAccountPasswordHash = passwordHash,
                pendingAccountVerification = Account.mkStoredEmailVerification accountId emailAddress 500 token,
                pendingAccountCreatedAtNanoseconds = 100
              }
      pool <- newPostgresPool (databasePoolCapacity defaultRealPostgresConfig) defaultRealPostgresConfig
      let accountStore = buildRuntimePostgresAccountStore pool
      assertAccountStoreSuccess (createPendingAccount accountStore pendingAccount) (\case PendingAccountCreated -> True; _ -> False)
      assertAccountStoreSuccess
        (findEmailVerification accountStore (Account.emailVerificationTokenDigest token))
        (\case Just storedVerification -> storedVerification == pendingAccountVerification pendingAccount; Nothing -> False)
      assertAccountStoreSuccess
        (consumeEmailVerification accountStore (Account.emailVerificationTokenDigest token) 499)
        (\case Just consumedAccountId -> consumedAccountId == accountId; Nothing -> False)

      let mfaStoreForAccount = buildRuntimePostgresMfaStore pool
          assertMfaBoolResult label action expected = do
            result <- action
            case result of
              Right actual | actual == expected -> pure ()
              _ -> expectationFailure label
      let recoveryCodeHash = Account.accountIdText accountId <> "-recovery-hash"
      assertMfaBoolResult "expected the first enrollment start to succeed" (saveUnconfirmedTotpEnrollment mfaStoreForAccount accountId "encrypted-envelope" 600) True
      assertMfaBoolResult "expected confirmation to succeed" (confirmTotpEnrollment mfaStoreForAccount accountId (recoveryCodeHash :| []) 700) True
      assertMfaBoolResult "expected a restart against a confirmed enrollment to be rejected" (saveUnconfirmedTotpEnrollment mfaStoreForAccount accountId "attacker-supplied-envelope" 800) False
      enrollmentAfterRejectedRestart <- loadTotpEnrollment mfaStoreForAccount accountId
      case enrollmentAfterRejectedRestart of
        Right (Just enrollment) -> do
          storedTotpEncryptedSecret enrollment `shouldBe` "encrypted-envelope"
          storedTotpConfirmedAtNanoseconds enrollment `shouldBe` Just 700
        _ -> expectationFailure "expected the confirmed TOTP enrollment to survive a rejected restart"

      syntaxResult <- runRuntimeRowsQuery defaultRealPostgresConfig "SELECT FROM"
      syntaxResult
        `shouldSatisfy` \case
          Left runtimeError ->
            Text.isInfixOf "syntax error" runtimeError
          Right rows ->
            error ("expected syntax failure, got rows: " <> show rows)

      parameterSyntaxResult <- runRuntimeParameterizedRowsQuery defaultRealPostgresConfig "SELECT FROM" []
      parameterSyntaxResult
        `shouldSatisfy` \case
          Left runtimeError -> Text.isInfixOf "syntax error" runtimeError
          Right rows -> error ("expected parameterized syntax failure, got rows: " <> show rows)

      withUnusedTcpEndpoint $ \unusedEndpoint -> do
        refusedResult <-
          runRuntimeScalarQuery
            defaultRealPostgresConfig
              { databasePort = tcpEndpointPort unusedEndpoint
              }
            "SELECT 1::text;"
        refusedResult
          `shouldSatisfy` \case
            Left runtimeError ->
              not (Text.null runtimeError)
                && not (Text.isInfixOf "posix_spawnp" runtimeError)
            Right value ->
              error ("expected connection failure, got value: " <> show value)
        parameterRefusedResult <-
          runRuntimeParameterizedRowsQuery
            defaultRealPostgresConfig
              { databasePort = tcpEndpointPort unusedEndpoint
              }
            "SELECT $1::text;"
            ["value"]
        parameterRefusedResult
          `shouldSatisfy` \case
            Left runtimeError -> not (Text.null runtimeError)
            Right rows -> error ("expected parameterized connection failure, got rows: " <> show rows)

    it "records same-identity schema versions and safely handles libpq migration failures" $ do
      ensureDefaultPostgresAvailable
      runPostgresMigrations defaultMigrationPostgresConfig `shouldReturn` Right ()
      runRuntimeRowsQuery defaultMigrationPostgresConfig "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
        `shouldReturn` Right ["initial-schema"]
      withUnusedTcpEndpoint $ \unusedEndpoint -> do
        runPostgresMigrations
          defaultMigrationPostgresConfig
            { databasePort = tcpEndpointPort unusedEndpoint
            }
          `shouldReturn` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      runPostgresMigrationsForRuntime
        defaultMigrationPostgresConfig
        defaultRealPostgresConfig {databaseName = "missing_ax_reconciliation_database"}
        `shouldReturn` Left (PostgresMigrationFailed "PostgreSQL migration command failed")

    it "runs every pending migration and its version record in one locked transaction" $ do
      recordedSqlReference <- newIORef []
      let executor =
            PostgresMigrationExecutor $ \sql -> do
              modifyIORef' recordedSqlReference (<> [sql])
              pure $
                if sql == "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;" || sql == "SELECT pg_advisory_xact_lock(782476311);"
                  then Just (PostgresMigrationRows [])
                  else Just PostgresMigrationCommandSucceeded
      runPostgresMigrationsWithExecutor executor migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
      recordedSql <- readIORef recordedSqlReference
      let versionedTransactionSql =
            [ "BEGIN;",
              "SELECT pg_advisory_xact_lock(782476311);",
              "CREATE SCHEMA IF NOT EXISTS web_api;",
              "CREATE TABLE IF NOT EXISTS web_api.schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);",
              "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
            ]
              <> migrationStatementsFor
              <> ["INSERT INTO web_api.schema_migrations (version) VALUES ('initial-schema');"]
      take (length versionedTransactionSql) recordedSql `shouldBe` versionedTransactionSql
      all
        (`elem` recordedSql)
        [ "ALTER SCHEMA web_api OWNER TO \"web_api_owner\";",
          "GRANT SELECT, INSERT, UPDATE, DELETE ON TABLE web_api.accounts TO \"web_api_app\";"
        ]
        `shouldBe` True
      last recordedSql `shouldBe` "COMMIT;"

    it "skips recorded migrations, rejects an unknown recorded version, and rolls back failed transactions" $ do
      recordedSqlReference <- newIORef []
      let executor appliedVersions failingSql =
            PostgresMigrationExecutor $ \sql -> do
              modifyIORef' recordedSqlReference (<> [sql])
              pure $
                if sql == "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;" || sql == "SELECT pg_advisory_xact_lock(782476311);"
                  then Just (PostgresMigrationRows appliedVersions)
                  else
                    if sql == failingSql
                      then Nothing
                      else Just PostgresMigrationCommandSucceeded
          setupSql =
            [ "BEGIN;",
              "SELECT pg_advisory_xact_lock(782476311);",
              "CREATE SCHEMA IF NOT EXISTS web_api;",
              "CREATE TABLE IF NOT EXISTS web_api.schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);",
              "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
            ]
      runPostgresMigrationsWithExecutor (executor ["initial-schema"] "never") migrationPostgresTestConfig postgresTestConfig `shouldReturn` Right ()
      skippedRecordedSql <- readIORef recordedSqlReference
      take (length setupSql) skippedRecordedSql `shouldBe` setupSql
      skippedRecordedSql `shouldContain` ["ALTER SCHEMA web_api OWNER TO \"web_api_owner\";"]
      skippedRecordedSql `shouldNotContain` ["INSERT INTO web_api.schema_migrations (version) VALUES ('initial-schema');"]
      last skippedRecordedSql `shouldBe` "COMMIT;"
      writeIORef recordedSqlReference []
      runPostgresMigrationsWithExecutor (executor ["removed-version"] "never") migrationPostgresTestConfig postgresTestConfig
        `shouldReturn` Left (PostgresMigrationFailed "Unknown PostgreSQL schema migration version: removed-version")
      readIORef recordedSqlReference `shouldReturn` (setupSql <> ["ROLLBACK;"])
      writeIORef recordedSqlReference []
      case filter (/= "CREATE SCHEMA IF NOT EXISTS web_api;") migrationStatementsFor of
        failingSql : _ -> do
          runPostgresMigrationsWithExecutor (executor [] failingSql) migrationPostgresTestConfig postgresTestConfig
            `shouldReturn` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
          readIORef recordedSqlReference
            `shouldReturn` (setupSql <> takeWhile (/= failingSql) migrationStatementsFor <> [failingSql, "ROLLBACK;"])
        [] -> expectationFailure "expected initial schema migration statements"

    it "rejects malformed migration protocol results and rolls back every post-BEGIN failure" $ do
      let setupSql =
            [ "BEGIN;",
              "SELECT pg_advisory_xact_lock(782476311);",
              "CREATE SCHEMA IF NOT EXISTS web_api;",
              "CREATE TABLE IF NOT EXISTS web_api.schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);",
              "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;"
            ]
          runWith migrationConfig runtimeConfig resultFor = do
            recordedSqlReference <- newIORef []
            let executor =
                  PostgresMigrationExecutor $ \sql -> do
                    modifyIORef' recordedSqlReference (<> [sql])
                    pure (resultFor sql)
            result <- runPostgresMigrationsWithExecutor executor migrationConfig runtimeConfig
            recordedSql <- readIORef recordedSqlReference
            pure (result, recordedSql)
          migrationFailure = Nothing
          commandSuccess sql
            | sql == "SELECT pg_advisory_xact_lock(782476311);" = Just (PostgresMigrationRows [])
            | otherwise = Just PostgresMigrationCommandSucceeded
          versionRows versions sql
            | sql == "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;" || sql == "SELECT pg_advisory_xact_lock(782476311);" = Just (PostgresMigrationRows versions)
            | otherwise = commandSuccess sql
      (beginResult, beginSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (const migrationFailure)
      beginResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      beginSql `shouldBe` ["BEGIN;"]

      (lockResult, lockSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "SELECT pg_advisory_xact_lock(782476311);" then migrationFailure else commandSuccess sql)
      lockResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      lockSql `shouldBe` ["BEGIN;", "SELECT pg_advisory_xact_lock(782476311);", "ROLLBACK;"]

      (bootstrapResult, bootstrapSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "CREATE TABLE IF NOT EXISTS web_api.schema_migrations (version TEXT PRIMARY KEY, applied_at TIMESTAMPTZ NOT NULL DEFAULT CURRENT_TIMESTAMP);" then migrationFailure else commandSuccess sql)
      bootstrapResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      bootstrapSql `shouldBe` take 4 setupSql <> ["ROLLBACK;"]

      (versionResult, versionSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "SELECT version FROM web_api.schema_migrations ORDER BY version ASC;" then migrationFailure else commandSuccess sql)
      versionResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      versionSql `shouldBe` setupSql <> ["ROLLBACK;"]

      (rowlessVersionResult, rowlessVersionSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig commandSuccess
      rowlessVersionResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration version query returned no rows")
      rowlessVersionSql `shouldBe` setupSql <> ["ROLLBACK;"]

      (rowCommandResult, rowCommandSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "BEGIN;" then Just (PostgresMigrationRows []) else versionRows [] sql)
      rowCommandResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command returned rows")
      rowCommandSql `shouldBe` ["BEGIN;"]

      (recordResult, recordSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "INSERT INTO web_api.schema_migrations (version) VALUES ('initial-schema');" then migrationFailure else versionRows [] sql)
      recordResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      recordSql `shouldBe` setupSql <> migrationStatementsFor <> ["INSERT INTO web_api.schema_migrations (version) VALUES ('initial-schema');", "ROLLBACK;"]

      (reconciliationResult, reconciliationSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "ALTER SCHEMA web_api OWNER TO \"web_api_owner\";" then migrationFailure else versionRows ["initial-schema"] sql)
      reconciliationResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      reconciliationSql `shouldBe` setupSql <> ["ALTER SCHEMA web_api OWNER TO \"web_api_owner\";", "ROLLBACK;"]

      (commitResult, commitSql) <-
        runWith migrationPostgresTestConfig postgresTestConfig (\sql -> if sql == "COMMIT;" then migrationFailure else versionRows ["initial-schema"] sql)
      commitResult `shouldBe` Left (PostgresMigrationFailed "PostgreSQL migration command failed")
      take (length setupSql) commitSql `shouldBe` setupSql
      drop (length commitSql - 2) commitSql `shouldBe` ["COMMIT;", "ROLLBACK;"]

      (sameIdentityResult, sameIdentitySql) <-
        runWith migrationPostgresTestConfig migrationPostgresTestConfig (versionRows ["initial-schema"])
      sameIdentityResult `shouldBe` Right ()
      sameIdentitySql `shouldNotContain` ["DO $$ BEGIN IF EXISTS (SELECT 1 FROM pg_catalog.pg_roles WHERE rolname = 'web_api_owner') THEN EXECUTE 'ALTER ROLE \"web_api_owner\" WITH LOGIN PASSWORD ''owner-secret'' NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT'; ELSE EXECUTE 'CREATE ROLE \"web_api_owner\" WITH LOGIN PASSWORD ''owner-secret'' NOSUPERUSER NOCREATEDB NOCREATEROLE NOREPLICATION INHERIT'; END IF; END $$;"]

    it "creates account verification, MFA, and opaque-session storage without persisting raw bearer secrets" $ do
      migrationStatementsFor
        `shouldSatisfy` \statements ->
          all
            (`elem` statements)
            [ "CREATE TABLE IF NOT EXISTS web_api.accounts (account_id TEXT PRIMARY KEY, email_normalized TEXT NOT NULL UNIQUE, username TEXT, display_name TEXT, password_hash TEXT NOT NULL, email_verified_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL);",
              "ALTER TABLE web_api.accounts ADD COLUMN IF NOT EXISTS username TEXT;",
              "ALTER TABLE web_api.accounts ADD COLUMN IF NOT EXISTS display_name TEXT;",
              "CREATE UNIQUE INDEX IF NOT EXISTS accounts_username_lower_unique ON web_api.accounts (lower(username)) WHERE username IS NOT NULL;",
              "CREATE TABLE IF NOT EXISTS web_api.email_verifications (token_digest TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, email_normalized TEXT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL);",
              "CREATE TABLE IF NOT EXISTS web_api.account_totp (account_id TEXT PRIMARY KEY REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, encrypted_secret BYTEA NOT NULL, confirmed_at_nanoseconds BIGINT, created_at_nanoseconds BIGINT NOT NULL, last_used_totp_counter BIGINT);",
              "ALTER TABLE web_api.account_totp ADD COLUMN IF NOT EXISTS last_used_totp_counter BIGINT;",
              "CREATE TABLE IF NOT EXISTS web_api.account_recovery_codes (account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, code_hash TEXT NOT NULL UNIQUE, created_at_nanoseconds BIGINT NOT NULL, used_at_nanoseconds BIGINT, PRIMARY KEY (account_id, code_hash));",
              "CREATE TABLE IF NOT EXISTS web_api.account_sessions (session_id TEXT PRIMARY KEY, account_id TEXT NOT NULL REFERENCES web_api.accounts (account_id) ON DELETE CASCADE, csrf_token TEXT NOT NULL, issued_at_nanoseconds BIGINT NOT NULL, expires_at_nanoseconds BIGINT NOT NULL, invalidated_at_nanoseconds BIGINT);"
            ]

    it "stops database setup when a migration or seed command fails" $ do
      case seedStatements of
        failingSeedStatement : _ -> do
          let runner command =
                pure $
                  if commandSql command == failingSeedStatement
                    then failingPostgresResult "seed failed"
                    else successfulPostgresResult Text.empty
          runPostgresSeedWithRunner runner postgresTestConfig
            `shouldReturn` Left
              ( PostgresCommandFailed
                  PostgresCommand
                    { postgresExecutable = "psql",
                      postgresArguments =
                        [ "--host",
                          "db.internal",
                          "--port",
                          "6543",
                          "--dbname",
                          "web_api_prod",
                          "--username",
                          "web_api_app",
                          "--no-password",
                          "--set",
                          "ON_ERROR_STOP=1",
                          "--command",
                          "DELETE FROM web_api.page_highlights;"
                        ],
                      postgresEnvironment = [("PGPASSWORD", "super-secret")]
                    }
                  PostgresCommandResult
                    { postgresExitCode = ExitFailure 1,
                      postgresStdout = Text.empty,
                      postgresStderr = "seed failed"
                    }
              )
        [] -> expectationFailure "expected at least one seed statement"

    it "keeps postgres command, result, and error values serializable and stable" $ do
      let command =
            PostgresCommand
              { postgresExecutable = "psql",
                postgresArguments = ["--command", "SELECT 1;"],
                postgresEnvironment = [("PGPASSWORD", "secret")]
              }
          commandResult =
            PostgresCommandResult
              { postgresExitCode = ExitSuccess,
                postgresStdout = "1",
                postgresStderr = Text.empty
              }
          failedCommandResult =
            PostgresCommandResult
              { postgresExitCode = ExitFailure 3,
                postgresStdout = Text.empty,
                postgresStderr = "boom"
              }
          runnerError = PostgresCommandFailed command commandResult
          unexpectedRowsError = UnexpectedQueryRows "expected exactly one row" ["first", "second"]
      command `shouldBe` command
      command `shouldNotBe` command {postgresArguments = ["--command", "SELECT 2;"]}
      commandResult `shouldBe` commandResult
      commandResult `shouldNotBe` commandResult {postgresStdout = "2"}
      runnerError `shouldBe` runnerError
      runnerError `shouldNotBe` PostgresCommandFailed command failedCommandResult
      unexpectedRowsError `shouldBe` unexpectedRowsError
      unexpectedRowsError `shouldNotBe` UnexpectedQueryRows "expected exactly one row" ["first"]
      show command
        `shouldBe` "PostgresCommand {postgresExecutable = \"psql\", postgresArguments = <redacted>, postgresEnvironment = <redacted>}"
      show command `shouldNotContain` "secret"
      show command `shouldNotContain` "SELECT 1"
      show commandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}"
      show failedCommandResult
        `shouldBe` "PostgresCommandResult {postgresExitCode = ExitFailure 3, postgresStdout = \"\", postgresStderr = \"boom\"}"
      show runnerError
        `shouldBe` "PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = <redacted>, postgresEnvironment = <redacted>}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})"
      show runnerError `shouldNotContain` "secret"
      show unexpectedRowsError
        `shouldBe` "UnexpectedQueryRows \"expected exactly one row\" [\"first\",\"second\"]"
      show [command]
        `shouldBe` "[PostgresCommand {postgresExecutable = \"psql\", postgresArguments = <redacted>, postgresEnvironment = <redacted>}]"
      show [commandResult]
        `shouldBe` "[PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"}]"
      show [runnerError]
        `shouldBe` "[PostgresCommandFailed (PostgresCommand {postgresExecutable = \"psql\", postgresArguments = <redacted>, postgresEnvironment = <redacted>}) (PostgresCommandResult {postgresExitCode = ExitSuccess, postgresStdout = \"1\", postgresStderr = \"\"})]"

    it "embeds the failing query text in a structured runner error for both row and scalar queries" $ do
      let failingResult =
            PostgresCommandResult
              { postgresExitCode = ExitFailure 1,
                postgresStdout = Text.empty,
                postgresStderr = "syntax error"
              }
          runner _ = pure failingResult
      rowsResult <- runRowsCommand runner postgresTestConfig "SELECT bogus"
      case rowsResult of
        Left (PostgresCommandFailed failedCommand failedCommandResult) -> do
          postgresExecutable failedCommand `shouldBe` "psql"
          postgresArguments failedCommand `shouldContain` ["SELECT bogus"]
          postgresEnvironment failedCommand `shouldBe` []
          failedCommandResult `shouldBe` failingResult
        _ -> expectationFailure "expected a structured PostgresCommandFailed error for the rows query"
      scalarResult <- runRequiredScalarCommand runner postgresTestConfig "SELECT bogus"
      case scalarResult of
        Left (PostgresCommandFailed failedCommand failedCommandResult) -> do
          postgresExecutable failedCommand `shouldBe` "psql"
          postgresArguments failedCommand `shouldContain` ["SELECT bogus"]
          postgresEnvironment failedCommand `shouldBe` []
          failedCommandResult `shouldBe` failingResult
        _ -> expectationFailure "expected a structured PostgresCommandFailed error for the scalar query"

    it "uses the default psql runner for effect loading and seed setup when psql is on PATH"
      $ withFakePsqlScript
        ( [ ("SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';", "Server-rendered home page with stubbed content."),
            ("SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';", "Second page content with stubbed data ready for future loaders."),
            ("SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;", Text.empty)
          ]
            <> fmap (,Text.empty) seedStatements
        )
      $ \argsLogPath -> do
        let application = buildAppWithDatabase defaultAppConfig (buildPostgresPageRepository postgresTestConfig)
        fmap stripVolatileDatabaseTimingResponse (HarchWeb.renderResponse application secondRequest)
          `shouldReturn` HarchWeb.PageResponseWithMetadata
            HarchWeb.ResponseBody
              { HarchWeb.responseStatus = Http.status200,
                HarchWeb.responseContentType = "text/html; charset=utf-8",
                HarchWeb.responseBody = "",
                HarchWeb.responseObservabilityAttributes = [],
                HarchWeb.responseLogEntries = [],
                HarchWeb.responseDatabaseOperations = expectedSecondDatabaseOperations
              }
            ( HarchWeb.Page
                { HarchWeb.pageTitle = "web-api: Second",
                  HarchWeb.pageRoute = SecondRoute,
                  HarchWeb.pageContext = defaultRequestContext,
                  HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
                  HarchWeb.pageBootstrapHooks = ["second-page"]
                }
            )
        runPostgresSeed postgresTestConfig `shouldReturn` Right ()
        let renderQueryLogEntry sql =
              "--host db.internal --port 6543 --dbname web_api_prod --username web_api_app --no-password --set ON_ERROR_STOP=1 --tuples-only --no-align --quiet --command "
                <> Text.unpack sql
            renderMutationLogEntry databaseConfig sql =
              "--host "
                <> Text.unpack (databaseHost databaseConfig)
                <> " --port "
                <> show (databasePort databaseConfig)
                <> " --dbname "
                <> Text.unpack (databaseName databaseConfig)
                <> " --username "
                <> Text.unpack (databaseUser databaseConfig)
                <> " --no-password --set ON_ERROR_STOP=1 --command "
                <> Text.unpack sql
        readFile argsLogPath
          `shouldReturn` unlines
            ( [ renderQueryLogEntry "SELECT summary FROM web_api.page_content WHERE route_slug = 'second' AND locale = 'en';",
                renderQueryLogEntry "SELECT highlight FROM web_api.page_highlights WHERE route_slug = 'second' AND locale = 'en' ORDER BY position ASC;"
              ]
                <> fmap (renderMutationLogEntry postgresTestConfig) seedStatements
            )

    it "uses stderr from the default psql runner when a command fails"
      $ withFakePsqlScriptResults
        [ ( "SELECT summary FROM web_api.page_content WHERE route_slug = 'home' AND locale = 'en';",
            PostgresCommandResult
              { postgresExitCode = ExitFailure 4,
                postgresStdout = Text.empty,
                postgresStderr = "default runner failed"
              }
          )
        ]
      $ \_ ->
        loadHomePageValueForRequest (buildPostgresPageRepository postgresTestConfig) defaultRequestContext
          `shouldReturn` Left (HomePageDataError "default runner failed")

    it "prefers a runtime that is already running the named postgres container in the containerized psql wrapper" $ do
      containerizedPsqlScriptContents `shouldContain'` "database_endpoint_is_reachable()"
      containerizedPsqlScriptContents `shouldContain'` "host_psql_path=\"${WEB_API_REAL_PSQL_PATH:-}\""
      containerizedPsqlScriptContents `shouldContain'` "if [ -n \"$host_psql_path\" ] && [ -x \"$host_psql_path\" ] && database_endpoint_is_reachable; then"
      containerizedPsqlScriptContents `shouldContain'` "runtime_with_running_container()"
      containerizedPsqlScriptContents `shouldContain'` "for candidate in docker podman; do"
      containerizedPsqlScriptContents `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      containerizedPsqlScriptContents `shouldContain'` "exec \"$runtime\" exec -e PGPASSWORD=\"${PGPASSWORD:-}\" web-api-postgres psql \"$@\""

    it "prefers a runtime that is already running the named postgres container before trying to start or create one" $ do
      ensureDefaultPostgresAvailableScript `shouldContain'` "database_endpoint_is_reachable()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "host_psql_is_available()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "owner_is_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "ensure_owner_superuser_via_host_psql()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "if database_endpoint_is_reachable && host_psql_is_available; then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "runtime_with_running_container()"
      ensureDefaultPostgresAvailableScript `shouldContain'` "for candidate in docker podman; do"
      ensureDefaultPostgresAvailableScript `shouldContain'` "elif runtime=$(runtime_with_existing_container); then"
      ensureDefaultPostgresAvailableScript `shouldContain'` "\"$runtime\" start web-api-postgres >/dev/null 2>&1 && return 0"

    it "loads seeded page data through the concrete postgres adapter against real PostgreSQL" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        let postgresEffect = buildPostgresPageRepository defaultRealPostgresConfig
        loadHomePageValueForRequest postgresEffect defaultRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Server-rendered home page with stubbed content."
              }
        loadSecondPageValueForRequest postgresEffect defaultRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                secondPageDataHighlights = []
              }
        loadHomePageValueForRequest postgresEffect spanishRequestContext
          `shouldReturn` Right
            HomePageData
              { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
              }
        loadSecondPageValueForRequest postgresEffect spanishRequestContext
          `shouldReturn` Right
            SecondPageData
              { secondPageDataSummary = "Contenido de la segunda pagina con datos de ejemplo listos para futuros cargadores.",
                secondPageDataHighlights = []
              }
