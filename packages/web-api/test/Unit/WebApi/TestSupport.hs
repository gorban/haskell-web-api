{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fixtures and assertions genuinely shared across the @Unit.WebApi@ specs.
--
-- The CN helper-usage audit (2026-08-25) keeps this as one test-only boundary:
-- configuration, account, route, page, PostgreSQL, and runtime specs share its
-- values directly, while the App runtime's socket/OTLP helpers retain their
-- private implementation details here.  Helpers with no external consumer are
-- deliberately not exported, so a future spec can only depend on a fixture
-- after the fixture's actual cross-spec use has been established.
module Unit.WebApi.TestSupport
  ( shouldReturnEqual,
    required,
    requiredDatabasePoolCapacity,
    databaseConfig,
    accountId,
    emailAddress,
    testSessionId,
    sessionIdValue,
    csrfTokenValue,
    opaqueSession,
    pureApplication,
    typedAccountActionRequest,
    productionTotpEncryptionKey,
    loadSecondPageForRequest,
    loadSecondPageValueForRequest,
    testTrustedForwardedProxy,
    requiredTestCidrBlock,
    trustedForwardedApplication,
    navigationAppConfig,
    homeRequest,
    secondRequest,
    spacesRequest,
    profileRequest,
    spanishRequestContext,
    testPathPrefix,
    assertSameProfilePageModel,
    explicitEnglishRequestContext,
    spanishHomeRequest,
    spanishSecondRequest,
    spanishSpacesRequest,
    prefixedHomeRequest,
    prefixedSecondRequest,
    prefixedSpanishSecondRequest,
    prefixedApiStatusRequest,
    spanishApiStatusRequest,
    notFoundRequest,
    spanishNotFoundRequest,
    apiStatusRequest,
    apiSecondRequest,
    apiNotFoundRequest,
    expectedApiJsonProtocolResponse,
    pureRouteMatcher,
    renderedShell,
    renderedShellForRequest,
    CapturedOtlpRequest (..),
    postgresTestConfig,
    unescapeLibpqConnectionValue,
    testPasswordHashingPolicy,
    testPasswordWorkGate,
    permissiveLoginAttemptStore,
    permissiveLoginThrottleContext,
    registrationEnvironmentAt,
    registrationRequestOf,
    requiredAccountId,
    requiredEmailAddress,
    requiredVerificationToken,
    requiredSecretNonce,
    requiredSecretEnvelope,
    enrollmentSessionIdValue,
    enrollmentCsrfTokenValue,
    enrollmentSessionStoreFor,
    isUnavailable,
    isCorrupt,
    assertAccountStoreError,
    assertAccountStoreSuccess,
    assertEmailVerificationResult,
    assertRegistrationResult,
    actionHasStatusAndFocus,
    actionResponseHasValidClientActionTransport,
    awaitDevSmtpEmail,
    metadataFields,
    migrationPostgresTestConfig,
    setupMigrationPostgresTestConfig,
    successfulPostgresResult,
    failingPostgresResult,
    commandSql,
    withTemporaryEnvironment,
    withCurrentDirectory,
    withUnreadableFile,
    withClearedAppEnvironment,
    withClearedRuntimeEnvironment,
    withClearedSetupEnvironment,
    withFakePsqlScriptResults,
    withFakePsqlScript,
    withListeningTcpEndpoint,
    withUnusedTcpEndpoint,
    withDefaultRuntimePortUnavailable,
    withOtlpCaptureServer,
    withSlowOtlpCaptureServer,
    readLoopbackHttpResponse,
    readLoopbackHttpResponseBytes,
    waitForRuntimeServerResponse,
    waitForRuntimeServerExit,
    buildHttpRequest,
    readAllSocketChunks,
    extractHttpBody,
    decodeChunkedBody,
    protocolResponseStrictBody,
    stripVolatileDatabaseTimingResponse,
    expectedSecondDatabaseOperations,
    expectedDatabaseOperation,
    lookupTextObservabilityAttribute,
  )
where

import Control.Concurrent (MVar, forkIO, killThread, newEmptyMVar, putMVar, threadDelay)
import Control.Exception (SomeException, displayException, finally, try)
import Control.Monad (unless)
import Crypto.Error (CryptoFailable, maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteStringChar8
import Data.Char (toLower)
import Data.IORef (IORef, readIORef)
import Data.List (find, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb qualified
import HarchWeb.Account (AccountId, mkAccountId)
import HarchWeb.Account qualified as Account
import HarchWeb.Action qualified as Action
import HarchWeb.Database qualified as HarchDatabase
import HarchWeb.DevSmtp qualified as DevSmtp
import HarchWeb.Email (EmailAddress, mkEmailAddress)
import HarchWeb.Email qualified as Email
import HarchWeb.LoginProtection qualified as LoginProtection
import HarchWeb.Observability qualified as Observability
import HarchWeb.Password qualified as Password
import HarchWeb.Secret qualified as Secret
import HarchWeb.Session (OpaqueSession (..), SessionId, mkCsrfToken, mkSessionId)
import HarchWeb.Session qualified as Session
import HarchWeb.Time (UnixTimeNanoseconds)
import Network.HTTP.Types qualified as Http
import Network.Socket (Family (AF_INET), SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, getSocketName, listen, socket, tupleToHostAddress)
import Network.Socket qualified as NetworkSocket
import Network.Socket.ByteString qualified as SocketByteString
import Numeric (readHex)
import System.Directory (createDirectory, getCurrentDirectory, removePathForcibly, setCurrentDirectory)
import System.Environment (getEnv, getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Unsafe (unsafePerformIO)
import System.Process (callProcess)
import Test.Hspec (Expectation, expectationFailure)
import Text.Read (readMaybe)
import WebApi (buildApp)
import WebApi.Account (AccountStore (..), AccountStoreError (..), EmailVerificationEnvironment (..), RegistrationEnvironment (..), RegistrationError (..), RegistrationRequest (..), RegistrationResult (..), VerificationDeliveryEnvironment (..), defaultPendingRegistrationStoragePolicy, defaultRegistrationDeliveryTimeout)
import WebApi.AccountPages (AccountAction, accountActions)
import WebApi.Config (AppConfig (..), DatabaseConfig (..), DatabasePoolCapacity, DatabaseTransportSecurity (..), ForwardedHeaderTrust (..), RequestPolicyConfig (..), StaticAssetRoot (..), StaticAssetsConfig (..), defaultAppConfig, defaultStaticAssetContentTypes, mkDatabasePoolCapacity)
import WebApi.Database (DatabaseError (..), DatabaseResult (..), PageRepository (..), SecondPageData (..))
import WebApi.Login (LoginAttemptAdmission (..), LoginAttemptReservation (..), LoginAttemptStore (..), LoginThrottleContext (..))
import WebApi.Page (AppPageModel (..), ProfilePageModel (..), renderPage)
import WebApi.Postgres.Testing (PostgresCommand (..), PostgresCommandResult (..))
import WebApi.Route (AppLocale (..), AppRequestContext (..), AppRoute (..), RouteMetadata (..), defaultRequestContext)
import WebApi.Route qualified
import WebApi.Session (MfaEnrollmentSessionStore (..))
import WebApi.SetupPlan (TcpEndpoint (..))

-- | Fail-fast equality check with a fixed message, for postgres-runner tests
-- whose actual/expected values have no useful 'Show' rendering to include.
shouldReturnEqual :: (Eq value) => IO value -> value -> Expectation
shouldReturnEqual action expected = do
  actual <- action
  unless (actual == expected) (expectationFailure "unexpected result")

-- | Unwraps a smart constructor's 'Just', for fixture values whose validity
-- the fixture itself already guarantees.
required :: String -> Maybe value -> value
required label = fromMaybe (error ("expected " <> label))

requiredDatabasePoolCapacity :: Int -> DatabasePoolCapacity
requiredDatabasePoolCapacity capacity = required "positive database pool capacity" (mkDatabasePoolCapacity capacity)

databaseConfig :: DatabaseConfig
databaseConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_test",
      databaseUser = "web_api_runtime",
      databasePassword = "password",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = requiredDatabasePoolCapacity 10,
      databaseTransportSecurity = DatabaseTransportLibpqDefault
    }

accountId :: AccountId
accountId =
  case mkAccountId "account_01" of
    Just value -> value
    Nothing -> error "expected a valid account id"

emailAddress :: EmailAddress
emailAddress = required "email address" (mkEmailAddress "person@example.test")

testSessionId :: SessionId
testSessionId =
  case mkSessionId sessionIdValue of
    Just value -> value
    Nothing -> error "expected a valid session id"

csrfTokenValue :: Text
csrfTokenValue = "abcdefghijklmnopqrstuvwxyz0123456789-_"

sessionIdValue :: Text
sessionIdValue = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_"

opaqueSession :: OpaqueSession AccountId
opaqueSession =
  case mkCsrfToken csrfTokenValue of
    Just csrfToken ->
      OpaqueSession
        { sessionId = testSessionId,
          sessionPrincipal = accountId,
          sessionCsrfToken = csrfToken,
          sessionIssuedAtNanoseconds = 100,
          sessionExpiresAtNanoseconds = 200
        }
    Nothing -> error "expected a valid csrf token"

pureApplication :: HarchWeb.Application AppRoute AccountAction AppRequestContext
pureApplication = buildApp defaultAppConfig

type AccountActionRequest = HarchWeb.ClientActionRequest AccountAction AppRequestContext

typedAccountActionRequest ::
  Text ->
  Text ->
  [(Text, Text)] ->
  AppRequestContext ->
  AccountActionRequest
typedAccountActionRequest method path fields requestContext =
  fromMaybe
    (error "expected a recognized account action test fixture")
    ( do
        action <-
          case Action.decodeAction
            accountActions
            HarchWeb.ClientActionPayload
              { HarchWeb.clientActionMethod = method,
                HarchWeb.clientActionPath = path,
                HarchWeb.clientActionFields = fields,
                HarchWeb.clientActionCsrfToken = Nothing,
                HarchWeb.clientActionIdempotencyKey = Nothing,
                HarchWeb.clientActionPayloadContext = requestContext
              } of
            HarchWeb.DecodedClientAction decodedAction -> Just decodedAction
            _ -> Nothing
        pure
          HarchWeb.ClientActionRequest
            { HarchWeb.clientAction = action,
              HarchWeb.clientActionRequestIdempotencyKey = Nothing,
              HarchWeb.clientActionContext = requestContext
            }
    )

productionTotpEncryptionKey :: Secret.SecretEncryptionKey
productionTotpEncryptionKey =
  fromMaybe
    (error "expected a valid production TOTP encryption key fixture")
    (Secret.mkSecretEncryptionKey "QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI")
{-# NOINLINE productionTotpEncryptionKey #-}

loadSecondPageForRequest :: PageRepository -> AppRequestContext -> IO (DatabaseResult SecondPageData)
loadSecondPageForRequest pageRepository requestContext =
  loadSecondPage pageRepository (requestLocale requestContext)

loadSecondPageValueForRequest :: PageRepository -> AppRequestContext -> IO (Either DatabaseError SecondPageData)
loadSecondPageValueForRequest pageRepository requestContext =
  databaseResultValue <$> loadSecondPageForRequest pageRepository requestContext

-- | Covers both 'Wai.defaultRequest''s built-in peer (@0.0.0.0@) and the
-- explicit loopback peer this file's fixtures use, so every existing
-- "trust forwarded headers" test keeps its request unchanged.
testTrustedForwardedProxy :: ForwardedHeaderTrust
testTrustedForwardedProxy = TrustForwardedFrom (requiredTestCidrBlock "0.0.0.0/1" :| [])

requiredTestCidrBlock :: Text.Text -> HarchWeb.CidrBlock
requiredTestCidrBlock cidrText =
  case HarchWeb.parseCidrBlock cidrText of
    Just cidrBlock -> cidrBlock
    Nothing -> error ("invalid test CIDR block: " <> Text.unpack cidrText)

trustedForwardedApplication :: HarchWeb.Application AppRoute AccountAction AppRequestContext
trustedForwardedApplication =
  buildApp
    defaultAppConfig
      { requestPolicy =
          (requestPolicy defaultAppConfig)
            { forwardedHeaderTrust = testTrustedForwardedProxy
            }
      }

navigationAppConfig :: AppConfig
navigationAppConfig =
  defaultAppConfig
    { staticAssets =
        StaticAssetsConfig
          { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
            staticAssetContentTypes = defaultStaticAssetContentTypes,
            staticCacheControlSeconds = Nothing
          }
    }

homeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
homeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = defaultRequestContext}

secondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
secondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = defaultRequestContext}

spacesRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spacesRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SpacesRoute, HarchWeb.requestContext = defaultRequestContext}

profileRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
profileRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = ProfileRoute, HarchWeb.requestContext = defaultRequestContext}

spanishRequestContext :: AppRequestContext
spanishRequestContext = defaultRequestContext {requestLocale = Spanish, requestLocaleIsExplicit = True}

assertSameProfilePageModel :: ProfilePageModel -> ProfilePageModel -> Expectation
assertSameProfilePageModel actual expected =
  if ProfilePage actual == ProfilePage expected
    then pure ()
    else expectationFailure "expected equal profile page models"

explicitEnglishRequestContext :: AppRequestContext
explicitEnglishRequestContext = defaultRequestContext {requestLocaleIsExplicit = True}

prefixedRequestContext :: AppRequestContext
prefixedRequestContext = defaultRequestContext {requestPathPrefix = testPathPrefix "/app"}

prefixedSpanishRequestContext :: AppRequestContext
prefixedSpanishRequestContext = spanishRequestContext {requestPathPrefix = testPathPrefix "/app"}

testPathPrefix :: Text -> HarchWeb.PathPrefix
testPathPrefix value =
  case HarchWeb.parseRequestPathPrefix value of
    Left parseError -> error ("invalid test path prefix: " <> show parseError)
    Right pathPrefix -> pathPrefix

spanishHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = spanishRequestContext}

spanishSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = spanishRequestContext}

spanishSpacesRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishSpacesRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SpacesRoute, HarchWeb.requestContext = spanishRequestContext}

prefixedHomeRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedHomeRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = HomeRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedRequestContext}

prefixedSpanishSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedSpanishSecondRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = SecondRoute, HarchWeb.requestContext = prefixedSpanishRequestContext}

prefixedApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
prefixedApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = prefixedRequestContext
    }

spanishApiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishApiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = spanishRequestContext
    }

notFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
notFoundRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = defaultRequestContext}

spanishNotFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
spanishNotFoundRequest = HarchWeb.RouteRequest {HarchWeb.requestRoute = NotFoundRoute, HarchWeb.requestContext = spanishRequestContext}

apiStatusRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiStatusRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = StatusApiRoute,
      HarchWeb.requestContext = defaultRequestContext
    }

apiSecondRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiSecondRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = SecondApiRoute,
      HarchWeb.requestContext = defaultRequestContext
    }

apiNotFoundRequest :: HarchWeb.RouteRequest AppRoute AppRequestContext
apiNotFoundRequest =
  HarchWeb.RouteRequest
    { HarchWeb.requestRoute = ApiNotFoundRoute,
      HarchWeb.requestContext = defaultRequestContext
    }

-- | @\/api\/status@ and @\/api\/second@ dispatch through
-- "HarchWeb.Api.Endpoint"'s typed boundary now, so a successful response is
-- a 'HarchWeb.ProtocolResponseResult' wrapping this shape rather than the
-- 'HarchWeb.BodyResponse' the shared page\/API selector still renders for
-- every other route.
expectedApiJsonProtocolResponse :: ByteString.ByteString -> HarchWeb.ProtocolResponse
expectedApiJsonProtocolResponse jsonBody =
  HarchWeb.ProtocolResponse
    { HarchWeb.protocolResponseStatus = Http.status200,
      -- Vary: Accept is unconditional on every typed API endpoint response,
      -- even a single-representation one (see the BF task record): the
      -- response genuinely depends on the request's Accept header, whether
      -- or not an alternative representation happens to be declared.
      HarchWeb.protocolResponseHeaders = [(Http.hContentType, "application/json"), (Http.hVary, "Accept")],
      HarchWeb.protocolResponseBody = HarchWeb.ProtocolResponseBytes jsonBody,
      HarchWeb.protocolResponseObservabilityAttributes = [],
      HarchWeb.protocolResponseLogEntries = [],
      HarchWeb.protocolResponseDatabaseOperations = []
    }

pureRouteMatcher :: Text -> HarchWeb.RouteRequest AppRoute AppRequestContext
pureRouteMatcher = WebApi.Route.matchRoute WebApi.Route.defaultRequestContext

renderedShell :: AppConfig -> AppRoute -> IO Text
renderedShell config route = do
  renderedShellForRequest
    config
    HarchWeb.RouteRequest
      { HarchWeb.requestRoute = route,
        HarchWeb.requestContext = defaultRequestContext
      }

renderedShellForRequest :: AppConfig -> HarchWeb.RouteRequest AppRoute AppRequestContext -> IO Text
renderedShellForRequest config routeRequest = do
  let application = buildApp config
  page <- renderPage config routeRequest
  pure (HarchWeb.renderDocumentForTests (HarchWeb.pageShell application page))

data CapturedOtlpRequest = CapturedOtlpRequest
  { capturedOtlpMethod :: ByteString.ByteString,
    capturedOtlpPath :: ByteString.ByteString,
    capturedOtlpHeaders :: [(ByteString.ByteString, ByteString.ByteString)],
    capturedOtlpBody :: ByteString.ByteString
  }

postgresTestConfig :: DatabaseConfig
postgresTestConfig =
  DatabaseConfig
    { databaseHost = "db.internal",
      databasePort = 6543,
      databaseName = "web_api_prod",
      databaseUser = "web_api_app",
      databasePassword = "super-secret",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = requiredDatabasePoolCapacity 10,
      databaseTransportSecurity = DatabaseTransportLibpqDefault
    }

-- | An independently written decoder for libpq's single-quoted conninfo
-- value syntax (a value is wrapped in @'...'@, and @\\\\@\/@\\'@ represent a
-- literal backslash\/quote inside it), so the round-trip tests below prove
-- 'libpqConnectionValue' against libpq's own escaping rule rather than
-- against a restatement of its own implementation.
unescapeLibpqConnectionValue :: Text -> Maybe Text
unescapeLibpqConnectionValue quoted =
  case Text.uncons quoted of
    Just ('\'', rest) -> unescapeLibpqConnectionValueBody rest
    _ -> Nothing

unescapeLibpqConnectionValueBody :: Text -> Maybe Text
unescapeLibpqConnectionValueBody remaining =
  case Text.uncons remaining of
    Nothing -> Nothing
    Just ('\'', trailing) | Text.null trailing -> Just Text.empty
    Just ('\'', _) -> Nothing
    Just ('\\', afterBackslash) ->
      case Text.uncons afterBackslash of
        Just (escapedCharacter, rest) | escapedCharacter `elem` ['\\', '\''] -> Text.cons escapedCharacter <$> unescapeLibpqConnectionValueBody rest
        _ -> Nothing
    Just (character, rest) -> Text.cons character <$> unescapeLibpqConnectionValueBody rest

testPasswordHashingPolicy :: Password.PasswordHashingPolicy
testPasswordHashingPolicy =
  fromMaybe
    (error "Expected valid test password hashing policy")
    (Password.mkPasswordHashingPolicy (Password.argon2Iterations 1) (Password.argon2MemoryKib 8) (Password.argon2Parallelism 1))

-- | A capacity that admits the production-strength hashes used by login
-- specs while leaving focused admission tests free to construct a smaller
-- gate. It is process-wide only for test convenience.
testPasswordWorkGate :: Password.PasswordWorkGate
testPasswordWorkGate =
  unsafePerformIO $
    Password.newPasswordWorkGate
      (fromMaybe (error "Expected valid test password-work budget") (Password.mkPasswordWorkBudget 524288))
{-# NOINLINE testPasswordWorkGate #-}

-- | A 'LoginAttemptStore' that always admits and successfully settles an
-- attempt. For tests exercising login/logout behavior unrelated to throttling.
permissiveLoginAttemptStore :: LoginAttemptStore
permissiveLoginAttemptStore =
  LoginAttemptStore
    { reserveLoginAttempt = \_ _ -> pure (Right (LoginAttemptReserved (LoginAttemptReservation "test-reservation"))),
      settleLoginAttempt = \_ _ -> pure (Right ()),
      cancelLoginAttempt = \_ -> pure (Right ())
    }

permissiveLoginThrottleContext :: UnixTimeNanoseconds -> LoginThrottleContext
permissiveLoginThrottleContext now =
  LoginThrottleContext
    { loginThrottleStore = permissiveLoginAttemptStore,
      loginThrottlePolicy = LoginProtection.defaultLoginProtectionPolicy,
      loginThrottleClientAddress = HarchWeb.defaultClientAddress,
      loginThrottleNow = now
    }

registrationEnvironmentAt ::
  (Password.PasswordHashingPolicy -> Password.Password -> IO (Maybe Password.PasswordHash)) ->
  AccountStore ->
  Email.EmailDelivery ->
  UnixTimeNanoseconds ->
  Word64 ->
  RegistrationEnvironment
registrationEnvironmentAt passwordHasher accountStore emailDelivery now verificationLifetime =
  RegistrationEnvironment
    { registrationPasswordHasher = passwordHasher,
      registrationHashingPolicy = testPasswordHashingPolicy,
      registrationPasswordWorkGate = testPasswordWorkGate,
      registrationStoragePolicy = defaultPendingRegistrationStoragePolicy,
      registrationVerificationEnvironment =
        EmailVerificationEnvironment
          { verificationStore = accountStore,
            verificationDeliveryEnvironment =
              VerificationDeliveryEnvironment
                { verificationDeliveryTimeout = defaultRegistrationDeliveryTimeout,
                  verificationDelivery = emailDelivery,
                  verificationLocale = Email.EmailEnglish,
                  verificationUrl = const "https://account.example.test/verify"
                },
            verificationNow = now,
            verificationLifetime = verificationLifetime
          }
    }

registrationRequestOf :: Email.EmailAddress -> RegistrationRequest
registrationRequestOf requestEmailAddress =
  RegistrationRequest
    { registrationEmail = requestEmailAddress,
      registrationPassword = Password.mkPassword "correct horse battery staple",
      registrationUsername = Nothing,
      registrationDisplayName = Nothing
    }

requiredAccountId :: Text -> Account.AccountId
requiredAccountId value =
  fromMaybe (error "Expected a valid account id") (Account.mkAccountId value)

requiredEmailAddress :: Text -> Email.EmailAddress
requiredEmailAddress value =
  fromMaybe (error "Expected a valid email address") (Email.mkEmailAddress value)

requiredVerificationToken :: Text -> Account.EmailVerificationToken
requiredVerificationToken value =
  fromMaybe (error "Expected a valid verification token") (Account.mkEmailVerificationToken value)

requiredSecretNonce :: ByteString.ByteString -> Secret.EncryptionNonce
requiredSecretNonce value =
  fromMaybe (error "Expected a valid secret encryption nonce") (Secret.mkEncryptionNonce value)

requiredSecretEnvelope :: CryptoFailable Text -> Text
requiredSecretEnvelope = fromMaybe (error "Expected secret encryption to succeed") . maybeCryptoError

enrollmentSessionIdValue :: Session.SessionId
enrollmentSessionIdValue =
  fromMaybe (error "Expected a valid MFA enrollment session id") (Session.mkSessionId "MFAENROLL0123456789ABCDEF0123456789ABCDEF01")

enrollmentCsrfTokenValue :: Session.CsrfToken
enrollmentCsrfTokenValue =
  fromMaybe (error "Expected a valid MFA enrollment CSRF token") (Session.mkCsrfToken "MFACSRF0123456789ABCDEF0123456789ABCDEF01234")

-- | A store whose only valid session binds 'enrollmentSessionIdValue' to the
-- given account, matching how 'issueMfaEnrollmentSession' would have issued
-- it. Any other session id is treated as absent.
enrollmentSessionStoreFor :: Account.AccountId -> MfaEnrollmentSessionStore
enrollmentSessionStoreFor enrollmentAccountId =
  MfaEnrollmentSessionStore
    { saveMfaEnrollmentSession = \_ -> pure (error "unexpected MFA enrollment session save"),
      loadMfaEnrollmentSession = \requestedSessionId ->
        pure $
          Right $
            if requestedSessionId == enrollmentSessionIdValue
              then
                Just
                  Session.OpaqueSession
                    { Session.sessionId = enrollmentSessionIdValue,
                      Session.sessionPrincipal = enrollmentAccountId,
                      Session.sessionCsrfToken = enrollmentCsrfTokenValue,
                      Session.sessionIssuedAtNanoseconds = 0,
                      Session.sessionExpiresAtNanoseconds = maxBound
                    }
              else Nothing,
      invalidateMfaEnrollmentSession = \_ _ -> pure (error "unexpected MFA enrollment session invalidation")
    }

isUnavailable :: Text -> AccountStoreError -> Bool
isUnavailable expectedError = \case
  AccountStoreUnavailable actualError -> actualError == expectedError
  AccountStoreCorruptData _ -> False

isCorrupt :: Text -> AccountStoreError -> Bool
isCorrupt expectedError = \case
  AccountStoreUnavailable _ -> False
  AccountStoreCorruptData actualError -> actualError == expectedError

assertAccountStoreError :: IO (Either AccountStoreError value) -> (AccountStoreError -> Bool) -> IO ()
assertAccountStoreError action matchesError = do
  result <- action
  case result of
    Left storeError | matchesError storeError -> pure ()
    _ -> expectationFailure "expected matching account-store error"

assertAccountStoreSuccess :: IO (Either AccountStoreError value) -> (value -> Bool) -> IO ()
assertAccountStoreSuccess action matchesValue = do
  result <- action
  case result of
    Right value | matchesValue value -> pure ()
    _ -> expectationFailure "expected matching account-store success"

assertEmailVerificationResult :: IO (Either AccountStoreError Account.EmailVerificationValidation) -> (Either AccountStoreError Account.EmailVerificationValidation -> Bool) -> IO ()
assertEmailVerificationResult action matchesResult = do
  result <- action
  if matchesResult result
    then pure ()
    else expectationFailure "unexpected email verification result"

assertRegistrationResult :: IO (Either RegistrationError RegistrationResult) -> (Either RegistrationError RegistrationResult -> Bool) -> IO ()
assertRegistrationResult action matchesResult = do
  result <- action
  if matchesResult result
    then pure ()
    else expectationFailure "unexpected registration result"

actionHasStatusAndFocus :: Int -> Maybe Text -> Text -> Maybe HarchWeb.ClientActionResponse -> Bool
actionHasStatusAndFocus expectedStatus expectedFocus expectedMessage = \case
  Just actionResponse ->
    actionResponseHasValidClientActionTransport actionResponse
      && Http.statusCode (HarchWeb.clientActionStatus actionResponse) == expectedStatus
      && HarchWeb.clientActionFocusId actionResponse == (HarchWeb.literalElementId <$> expectedFocus)
      && case HarchWeb.clientActionPatches actionResponse of
        [patch] ->
          HarchWeb.regionPatchId patch `elem` ["registration-region", "verification-region", "mfa-enrollment-region", "login-region", "logout-region"]
            && expectedMessage `Text.isInfixOf` HarchWeb.regionPatchHtml patch
        _ -> False
  Nothing -> False

-- | Check the observable HTTP action contract rather than forcing its derived
-- 'Show' instance.  An action response must render as the public JSON payload,
-- carry only well-formed headers, and expose well-formed structured diagnostics.
actionResponseHasValidClientActionTransport :: HarchWeb.ClientActionResponse -> Bool
actionResponseHasValidClientActionTransport actionResponse =
  HarchWeb.responseStatus transportResponse == HarchWeb.clientActionStatus actionResponse
    && HarchWeb.responseContentType transportResponse == "application/json; charset=utf-8"
    && "\"patches\":" `Text.isInfixOf` HarchWeb.responseBody transportResponse
    && "\"focusId\":" `Text.isInfixOf` HarchWeb.responseBody transportResponse
    && all wellFormedHeader (HarchWeb.clientActionHeaders actionResponse)
    && all wellFormedAttribute (HarchWeb.responseObservabilityAttributes transportResponse)
    && not (any Text.null (HarchWeb.responseLogEntries transportResponse))
  where
    transportResponse = HarchWeb.clientActionResponseBody actionResponse

    wellFormedHeader (_, value) = not (ByteString.null value)

    wellFormedAttribute attribute =
      not (Text.null (Observability.attributeName attribute))
        && case Observability.attributeValue attribute of
          Observability.TextAttribute value -> not (Text.null value)
          Observability.IntAttribute _ -> True

awaitDevSmtpEmail :: DevSmtp.DevSmtpServer -> Text -> IO (Maybe DevSmtp.DevSmtpEmail)
awaitDevSmtpEmail server recipient = go (100 :: Int)
  where
    go remaining = do
      received <- DevSmtp.takeLatestDevSmtpEmailTo server recipient
      case received of
        Just email -> pure (Just email)
        Nothing
          | remaining > 0 -> threadDelay 10000 >> go (remaining - 1)
          | otherwise -> pure Nothing

metadataFields :: RouteMetadata -> (Maybe Text, Text, Text, [Text])
metadataFields metadata =
  ( routePageSegment metadata,
    routePageSuffix metadata,
    routePageTitle metadata,
    routeEnhancementHooks metadata
  )

migrationPostgresTestConfig :: DatabaseConfig
migrationPostgresTestConfig =
  postgresTestConfig
    { databaseUser = "web_api_owner",
      databasePassword = "owner-secret"
    }

setupMigrationPostgresTestConfig :: DatabaseConfig
setupMigrationPostgresTestConfig =
  DatabaseConfig
    { databaseHost = "127.0.0.1",
      databasePort = 5432,
      databaseName = "web_api_dev",
      databaseUser = "web_api_owner",
      databasePassword = "owner-secret",
      databaseConnectTimeoutSeconds = 10,
      databasePoolCapacity = requiredDatabasePoolCapacity 10,
      databaseTransportSecurity = DatabaseTransportLibpqDefault
    }

successfulPostgresResult :: Text -> PostgresCommandResult
successfulPostgresResult stdoutText =
  PostgresCommandResult
    { postgresExitCode = ExitSuccess,
      postgresStdout = stdoutText,
      postgresStderr = Text.empty
    }

failingPostgresResult :: Text -> PostgresCommandResult
failingPostgresResult stderrText =
  PostgresCommandResult
    { postgresExitCode = ExitFailure 1,
      postgresStdout = Text.empty,
      postgresStderr = stderrText
    }

commandSql :: PostgresCommand -> Text
commandSql command =
  case reverse (postgresArguments command) of
    sqlArgument : _ -> Text.pack sqlArgument
    [] -> Text.empty

withTemporaryEnvironment :: String -> Maybe String -> IO a -> IO a
withTemporaryEnvironment key maybeValue action = do
  previousValue <- lookupEnv key
  case maybeValue of
    Just value -> setEnv key value
    Nothing -> unsetEnv key
  let restore =
        case previousValue of
          Just value -> setEnv key value
          Nothing -> unsetEnv key
  action `finally` restore

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory directory action = do
  previousDirectory <- getCurrentDirectory
  setCurrentDirectory directory
  action `finally` setCurrentDirectory previousDirectory

withUnreadableFile :: FilePath -> String -> IO a -> IO a
withUnreadableFile filePath _fileContents action = do
  createDirectory filePath
  action `finally` removePathForcibly filePath

withClearedAppEnvironment :: IO a -> IO a
withClearedAppEnvironment =
  withTemporaryEnvironment "APP_MODE" Nothing
    . withTemporaryEnvironment "DATABASE_HOST" Nothing
    . withTemporaryEnvironment "DATABASE_PORT" Nothing
    . withTemporaryEnvironment "DATABASE_NAME" Nothing
    . withTemporaryEnvironment "DATABASE_USER" Nothing
    . withTemporaryEnvironment "DATABASE_PASSWORD" Nothing

withClearedEnvironmentPrefixes :: [String] -> IO a -> IO a
withClearedEnvironmentPrefixes prefixes action = do
  environment <- getEnvironment
  let matchingKeys =
        [ key
        | (key, _) <- environment,
          any (`isPrefixOf` key) prefixes
        ]
  foldr (`withTemporaryEnvironment` Nothing) action matchingKeys

withClearedRuntimeEnvironment :: IO a -> IO a
withClearedRuntimeEnvironment =
  withClearedEnvironmentPrefixes
    [ "APP_TITLE_PREFIX",
      "LISTENER_",
      "STATIC_ASSET_CONTENT_TYPE_",
      "STATIC_ASSET_ROOT_",
      "STATIC_CACHE_CONTROL_SECONDS",
      "REDIRECT_HTTP_TO_HTTPS",
      "HTTPS_REDIRECT_PORT",
      "HSTS_",
      "CORS_",
      "CONTENT_SECURITY_POLICY",
      "X_CONTENT_TYPE_OPTIONS_NOSNIFF",
      "X_XSS_PROTECTION",
      "REFERRER_POLICY",
      "PERMISSIONS_POLICY",
      "X_FRAME_OPTIONS",
      "OTLP_TRACING_",
      "OTLP_METRICS_"
    ]

withClearedSetupEnvironment :: IO a -> IO a
withClearedSetupEnvironment =
  withClearedEnvironmentPrefixes
    [ "SETUP_AUTOSTART_",
      "WEB_API_MIGRATION_DATABASE_"
    ]

withFakePsqlScriptResults :: [(Text, PostgresCommandResult)] -> (FilePath -> IO a) -> IO a
withFakePsqlScriptResults commandResults action =
  withSystemTempDirectory "fake-psql" $ \tempDirectory -> do
    originalPath <- getEnv "PATH"
    let scriptPath = tempDirectory <> "/psql"
        argsLogPath = tempDirectory <> "/psql-args.log"
        scriptBody =
          unlines
            ( [ "#!/usr/bin/env bash",
                "set -euo pipefail",
                "printf '%s\\n' \"$*\" >> \"$PSQL_ARGS_LOG\"",
                "sql=''",
                "while [ \"$#\" -gt 0 ]; do",
                "  case \"$1\" in",
                "    --command)",
                "      sql=\"$2\"",
                "      shift 2",
                "      ;;",
                "    *)",
                "      shift",
                "      ;;",
                "  esac",
                "done",
                "case \"$sql\" in"
              ]
                ++ concatMap renderCase commandResults
                ++ [ "  *)",
                     "    exit 0",
                     "    ;;",
                     "esac"
                   ]
            )
    writeFile scriptPath scriptBody
    callProcess "chmod" ["+x", scriptPath]
    withTemporaryEnvironment "PSQL_ARGS_LOG" (Just argsLogPath) $
      withTemporaryEnvironment "PATH" (Just (tempDirectory <> ":" <> originalPath)) $
        action argsLogPath
  where
    renderCase (sqlText, commandResult) =
      [ "  " <> show (Text.unpack sqlText) <> ")"
      ]
        ++ renderStdoutLines (postgresStdout commandResult)
        ++ renderStderrLines (postgresStderr commandResult)
        ++ [ "    exit " <> renderExitCode (postgresExitCode commandResult),
             "    ;;"
           ]

    renderStdoutLines stdoutText =
      case Text.unpack stdoutText of
        "" -> []
        stdoutValue -> ["    printf %s\\\\n " <> show stdoutValue]

    renderStderrLines stderrText =
      case Text.unpack stderrText of
        "" -> []
        stderrValue -> ["    printf %s\\\\n " <> show stderrValue <> " >&2"]

    renderExitCode exitCode =
      case exitCode of
        ExitSuccess -> "0"
        ExitFailure code -> show code

withFakePsqlScript :: [(Text, Text)] -> (FilePath -> IO a) -> IO a
withFakePsqlScript commandOutputs =
  withFakePsqlScriptResults
    (map toSuccessfulCommandResult commandOutputs)
  where
    toSuccessfulCommandResult (sqlText, stdoutText) =
      (sqlText, successfulPostgresResult stdoutText)

withListeningTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withListeningTcpEndpoint action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ ->
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
        `finally` close listenerSocket
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback test socket"

withUnusedTcpEndpoint :: (TcpEndpoint -> IO a) -> IO a
withUnusedTcpEndpoint action = do
  reservedSocket <- socket AF_INET Stream defaultProtocol
  bind reservedSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  socketAddress <- getSocketName reservedSocket
  case socketAddress of
    SockAddrInet port _ -> do
      close reservedSocket
      action
        TcpEndpoint
          { tcpEndpointHost = "127.0.0.1",
            tcpEndpointPort = fromIntegral port
          }
    _ ->
      close reservedSocket
        >> error "expected IPv4 loopback reservation socket"

withDefaultRuntimePortUnavailable :: IO a -> IO a
withDefaultRuntimePortUnavailable action = do
  reservedSocketResult <- try (socket AF_INET Stream defaultProtocol >>= reserveDefaultRuntimePort) :: IO (Either IOError NetworkSocket.Socket)
  case reservedSocketResult of
    Left bindError
      | isAlreadyInUseError bindError -> action
      | otherwise -> ioError bindError
    Right reservedSocket ->
      action `finally` close reservedSocket
  where
    reserveDefaultRuntimePort reservedSocket = do
      bind reservedSocket (SockAddrInet 5001 (tupleToHostAddress (127, 0, 0, 1)))
      listen reservedSocket 1
      pure reservedSocket

withOtlpCaptureServer ::
  Http.Status ->
  ByteString.ByteString ->
  (Text -> MVar CapturedOtlpRequest -> IO a) ->
  IO a
withOtlpCaptureServer responseStatus responseBody action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ -> do
      capturedRequestReference <- newEmptyMVar
      let collectorUrl = Text.pack ("http://127.0.0.1:" <> show (fromIntegral port :: Int) <> "/v1/traces")
      serverThreadId <-
        forkIO $
          ( do
              (clientSocket, _) <- NetworkSocket.accept listenerSocket
              capturedRequest <- readCapturedHttpRequest clientSocket
              putMVar capturedRequestReference capturedRequest
              SocketByteString.sendAll clientSocket (buildHttpResponse responseStatus responseBody)
              close clientSocket
          )
            `finally` close listenerSocket
      action collectorUrl capturedRequestReference `finally` killThread serverThreadId
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback OTLP capture socket"

-- | Accepts exactly one connection like 'withOtlpCaptureServer', but waits
-- 'delayMicroseconds' after reading the request before responding — used
-- to hold the AU export worker busy on one item long enough to force the
-- bounded queue past capacity deterministically.
withSlowOtlpCaptureServer ::
  Int ->
  Http.Status ->
  ByteString.ByteString ->
  (Text -> IO a) ->
  IO a
withSlowOtlpCaptureServer delayMicroseconds responseStatus responseBody action = do
  listenerSocket <- socket AF_INET Stream defaultProtocol
  bind listenerSocket (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen listenerSocket 1
  socketAddress <- getSocketName listenerSocket
  case socketAddress of
    SockAddrInet port _ -> do
      let collectorUrl = Text.pack ("http://127.0.0.1:" <> show (fromIntegral port :: Int) <> "/v1/traces")
      serverThreadId <-
        forkIO $
          ( do
              (clientSocket, _) <- NetworkSocket.accept listenerSocket
              _ <- readCapturedHttpRequest clientSocket
              threadDelay delayMicroseconds
              SocketByteString.sendAll clientSocket (buildHttpResponse responseStatus responseBody)
              close clientSocket
          )
            `finally` close listenerSocket
      action collectorUrl `finally` killThread serverThreadId
    _ ->
      close listenerSocket
        >> error "expected IPv4 loopback slow OTLP capture socket"

readCapturedHttpRequest :: NetworkSocket.Socket -> IO CapturedOtlpRequest
readCapturedHttpRequest clientSocket = do
  requestBytes <- readHttpRequestBytes clientSocket
  let (headerBytes, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes
      requestBody = ByteString.drop 4 withSeparator
      headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)
      requestLine =
        case headerLines of
          line : _ -> line
          [] -> ByteString.empty
      (requestMethod, requestPath) =
        case ByteStringChar8.words requestLine of
          method : path : _ -> (method, path)
          _ -> (ByteString.empty, ByteString.empty)
  pure
    CapturedOtlpRequest
      { capturedOtlpMethod = requestMethod,
        capturedOtlpPath = requestPath,
        capturedOtlpHeaders = mapMaybe parseCapturedHeader (drop 1 headerLines),
        capturedOtlpBody = requestBody
      }

readHttpRequestBytes :: NetworkSocket.Socket -> IO ByteString.ByteString
readHttpRequestBytes clientSocket =
  readRequestChunks ByteString.empty Nothing
  where
    readRequestChunks accumulatedRequest knownContentLength = do
      chunk <- SocketByteString.recv clientSocket 4096
      let accumulatedRequest' = accumulatedRequest <> chunk
          contentLength =
            case knownContentLength of
              Just value -> Just value
              Nothing -> parseHttpContentLength accumulatedRequest'
      case contentLength of
        Just bodyLength
          | ByteString.length (extractHttpBody accumulatedRequest') >= bodyLength ->
              pure accumulatedRequest'
        _ ->
          if ByteString.null chunk
            then pure accumulatedRequest'
            else readRequestChunks accumulatedRequest' contentLength

parseHttpContentLength :: ByteString.ByteString -> Maybe Int
parseHttpContentLength requestBytes =
  case ByteStringChar8.breakSubstring "\r\n\r\n" requestBytes of
    (_, withSeparator)
      | ByteString.null withSeparator -> Nothing
    (headerBytes, _) ->
      lookup "content-length" (mapMaybe parseCapturedHeader (drop 1 headerLines)) >>= readMaybe . ByteStringChar8.unpack
      where
        headerLines = map stripHeaderLineEnd (ByteStringChar8.split '\n' headerBytes)

parseCapturedHeader :: ByteString.ByteString -> Maybe (ByteString.ByteString, ByteString.ByteString)
parseCapturedHeader headerLine =
  case ByteStringChar8.break (== ':') headerLine of
    (headerName, withSeparator)
      | ByteString.null withSeparator -> Nothing
      | otherwise ->
          Just
            ( ByteStringChar8.map toLower headerName,
              ByteStringChar8.dropWhile (== ' ') (stripHeaderLineEnd (ByteString.drop 1 withSeparator))
            )

stripHeaderLineEnd :: ByteString.ByteString -> ByteString.ByteString
stripHeaderLineEnd =
  ByteStringChar8.filter (/= '\r')

buildHttpResponse :: Http.Status -> ByteString.ByteString -> ByteString.ByteString
buildHttpResponse responseStatus responseBody =
  ByteStringChar8.pack $
    "HTTP/1.1 "
      <> show (Http.statusCode responseStatus)
      <> " "
      <> ByteStringChar8.unpack (Http.statusMessage responseStatus)
      <> "\r\nContent-Type: application/json\r\nContent-Length: "
      <> show (ByteString.length responseBody)
      <> "\r\nConnection: close\r\n\r\n"
      <> ByteStringChar8.unpack responseBody

readLoopbackHttpResponse :: Int -> Text -> IO Text
readLoopbackHttpResponse port path = do
  responseBytes <- readLoopbackHttpResponseBytes port path
  pure (TextEncoding.decodeUtf8 responseBytes)

readLoopbackHttpResponseBytes :: Int -> Text -> IO ByteString.ByteString
readLoopbackHttpResponseBytes port path = do
  clientSocket <- socket AF_INET Stream defaultProtocol
  connect clientSocket
  SocketByteString.sendAll clientSocket (buildHttpRequest path)
  responseBytes <- readAllSocketChunks clientSocket
  close clientSocket
  pure (extractHttpBody responseBytes)
  where
    connect clientSocket =
      NetworkSocket.connect clientSocket (SockAddrInet (fromIntegral port) (tupleToHostAddress (127, 0, 0, 1)))

waitForRuntimeServerResponse :: IORef (Maybe (Either SomeException ())) -> Int -> Text -> IO Text
waitForRuntimeServerResponse completionReference port path =
  waitForResponseAttempts (500 :: Int)
  where
    waitForResponseAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just (Left exception) ->
          expectationFailure ("expected runtime server to remain running, but it failed early: " <> displayException exception)
            >> pure Text.empty
        Just (Right ()) ->
          expectationFailure "expected runtime server to remain running, but it exited early"
            >> pure Text.empty
        Nothing -> do
          responseResult <- try (readLoopbackHttpResponse port path) :: IO (Either IOError Text)
          case responseResult of
            Right responseText -> pure responseText
            Left _
              | remainingAttempts > 0 -> do
                  threadDelay 10000
                  waitForResponseAttempts (remainingAttempts - 1)
              | otherwise ->
                  expectationFailure "expected runtime server to accept loopback HTTP requests"
                    >> pure Text.empty

waitForRuntimeServerExit :: IORef (Maybe (Either SomeException ())) -> IO ()
waitForRuntimeServerExit completionReference =
  waitForExitAttempts (500 :: Int)
  where
    waitForExitAttempts remainingAttempts = do
      completionResult <- readIORef completionReference
      case completionResult of
        Just _ -> pure ()
        Nothing
          | remainingAttempts > 0 -> do
              threadDelay 10000
              waitForExitAttempts (remainingAttempts - 1)
          | otherwise ->
              expectationFailure "expected runtime server to stop after being signalled"

buildHttpRequest :: Text -> ByteString.ByteString
buildHttpRequest path =
  ByteStringChar8.pack $
    "GET "
      <> Text.unpack path
      <> " HTTP/1.1\r\nHost: 127.0.0.1\r\nConnection: close\r\n\r\n"

readAllSocketChunks :: NetworkSocket.Socket -> IO ByteString.ByteString
readAllSocketChunks clientSocket = do
  chunk <- SocketByteString.recv clientSocket 4096
  if ByteString.null chunk
    then pure ByteString.empty
    else fmap (chunk <>) (readAllSocketChunks clientSocket)

extractHttpBody :: ByteString.ByteString -> ByteString.ByteString
extractHttpBody responseBytes =
  let (headers, withSeparator) = ByteStringChar8.breakSubstring "\r\n\r\n" responseBytes
      responseBody = ByteString.drop 4 withSeparator
   in if ByteStringChar8.isInfixOf "Transfer-Encoding: chunked" headers
        then decodeChunkedBody responseBody
        else responseBody

decodeChunkedBody :: ByteString.ByteString -> ByteString.ByteString
decodeChunkedBody chunkedBytes =
  case ByteStringChar8.breakSubstring "\r\n" chunkedBytes of
    (chunkSizeHex, withSizeSeparator)
      | ByteString.null withSizeSeparator ->
          chunkedBytes
      | otherwise ->
          case readHex (ByteStringChar8.unpack chunkSizeHex) of
            [(chunkSize, "")]
              | chunkSize == (0 :: Int) ->
                  ByteString.empty
              | otherwise ->
                  let chunkPayload = ByteString.drop 2 withSizeSeparator
                      (chunk, withChunkSuffix) = ByteString.splitAt chunkSize chunkPayload
                   in chunk <> decodeChunkedBody (ByteString.drop 2 withChunkSuffix)
            _ ->
              chunkedBytes

-- | 'HarchWeb.ProtocolResponseBody' has no 'Eq' instance of its own (its
-- streaming variant cannot support one), so extract the strict bytes a
-- typed endpoint's non-streaming response always carries instead of
-- comparing the body value directly.
protocolResponseStrictBody :: HarchWeb.ProtocolResponse -> ByteString.ByteString
protocolResponseStrictBody protocolResponse =
  case HarchWeb.protocolResponseBody protocolResponse of
    HarchWeb.ProtocolResponseBytes bodyBytes -> bodyBytes
    HarchWeb.ProtocolResponseStream _ -> error "expected a strict protocol response body"

stripVolatileDatabaseTimingResponse :: HarchWeb.Response route context -> HarchWeb.Response route context
stripVolatileDatabaseTimingResponse response =
  case response of
    HarchWeb.PageResponse page -> HarchWeb.PageResponse page
    HarchWeb.PageResponseWithMetadata responseBody page ->
      HarchWeb.PageResponseWithMetadata (stripVolatileDatabaseTimingResponseBody responseBody) page
    HarchWeb.BodyResponse responseBody ->
      HarchWeb.BodyResponse (stripVolatileDatabaseTimingResponseBody responseBody)
    HarchWeb.RedirectResponse responseBody location ->
      HarchWeb.RedirectResponse (stripVolatileDatabaseTimingResponseBody responseBody) location
    HarchWeb.ClientActionBodyResponse actionResponse ->
      HarchWeb.ClientActionBodyResponse actionResponse
    HarchWeb.EventStreamResponse responseBody eventSource ->
      HarchWeb.EventStreamResponse (stripVolatileDatabaseTimingResponseBody responseBody) eventSource
    HarchWeb.ProtocolResponseResult protocolResponse ->
      HarchWeb.ProtocolResponseResult protocolResponse

stripVolatileDatabaseTimingResponseBody :: HarchWeb.ResponseBody -> HarchWeb.ResponseBody
stripVolatileDatabaseTimingResponseBody responseBody =
  responseBody
    { HarchWeb.responseDatabaseOperations =
        map stripVolatileDatabaseTiming (HarchWeb.responseDatabaseOperations responseBody)
    }

stripVolatileDatabaseTiming :: HarchDatabase.DatabaseOperation -> HarchDatabase.DatabaseOperation
stripVolatileDatabaseTiming databaseOperation =
  databaseOperation
    { HarchDatabase.databaseOperationStartedAtNanoseconds = Nothing,
      HarchDatabase.databaseOperationEndedAtNanoseconds = Nothing
    }

expectedSecondDatabaseOperations :: [HarchDatabase.DatabaseOperation]
expectedSecondDatabaseOperations =
  [ expectedDatabaseOperation
      "load-second-page-summary"
      "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
    expectedDatabaseOperation
      "load-second-page-highlights"
      "SELECT highlight FROM web_api.page_highlights WHERE route_slug = ? AND locale = ? ORDER BY position ASC;"
  ]

expectedDatabaseOperation :: Text -> Text -> HarchDatabase.DatabaseOperation
expectedDatabaseOperation operationName queryTemplate =
  HarchDatabase.DatabaseOperation
    { HarchDatabase.databaseOperationSystem = "postgresql",
      HarchDatabase.databaseOperationName = operationName,
      HarchDatabase.databaseQueryTemplate = queryTemplate,
      HarchDatabase.databaseOperationStartedAtNanoseconds = Nothing,
      HarchDatabase.databaseOperationEndedAtNanoseconds = Nothing
    }

lookupTextObservabilityAttribute :: Text -> [Observability.ObservabilityAttribute] -> Maybe Text
lookupTextObservabilityAttribute attributeName attributes =
  find ((== attributeName) . Observability.attributeName) attributes >>= attributeTextValue
  where
    attributeTextValue attribute =
      case Observability.attributeValue attribute of
        Observability.TextAttribute value -> Just value
        Observability.IntAttribute _ -> Nothing
