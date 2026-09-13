{-# LANGUAGE OverloadedStrings #-}

-- | Durable admission-session issuance, validation, and CSRF grant binding.
--
-- The session is an application capability, not an Harch authentication
-- service.  This module keeps cookie policy, PostgreSQL adapter results, and
-- the admission-only binding together so a successful credential proof cannot
-- mint a bearer before durable persistence succeeds.
module App.Composed.Admission.Session
  ( AdmissionConfig (..),
    AdmissionConfigError (..),
    AdmissionGuardFailure (..),
    AdmissionSessionClockError (..),
    AdmissionSessionIssueError (..),
    AdmissionSessionStore (..),
    AdmissionSessionStoreError (..),
    defaultAdmissionSessionCookiePolicy,
    establishAdmissionPrincipal,
    issueAdmissionSession,
    mkAdmissionConfig,
    resolveAdmissionCsrfBinding,
  )
where

import App.Composed.Admission.Types
import App.Composed.Model
import Control.Monad.Except (ExceptT (..), runExceptT, throwError, withExceptT)
import Control.Monad.IO.Class (liftIO)
import Core.Control.Error (liftEitherWith)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb.Csrf (CsrfBindingResolution (..), csrfBindingFromCanonicalBytes)
import HarchWeb.EndpointSecurity (EndpointRequest (..))
import HarchWeb.RequestContext (RequestContext (..))
import HarchWeb.Session
  ( OpaqueSession (..),
    SessionCookieExtraction (..),
    SessionCookiePolicy (..),
    SessionValidation (..),
    extractSessionCookieId,
    generateSessionId,
    mkSessionCookieName,
    sessionCookieName,
    sessionCookieNameText,
    sessionId,
    sessionIdText,
    validateSession,
  )
import HarchWeb.Time (UnixTimeNanoseconds, addUnixTimeNanoseconds)
import HarchWeb.Time qualified as Time
import Network.Wai qualified as Wai

data AdmissionSessionStore = AdmissionSessionStore
  { saveAdmissionSession :: OpaqueSession AdmissionPrincipalId -> IO (Either AdmissionSessionStoreError Bool),
    loadAdmissionSession :: AdmissionSessionId -> IO (Either AdmissionSessionStoreError (Maybe (OpaqueSession AdmissionPrincipalId))),
    invalidateAdmissionSession :: AdmissionSessionId -> UnixTimeNanoseconds -> IO (Either AdmissionSessionStoreError Bool)
  }

data AdmissionSessionStoreError
  = AdmissionSessionStoreUnavailable
  | AdmissionSessionStoreCorrupt
  deriving (Eq, Show)

data AdmissionSessionClockError
  = AdmissionSessionClockUnavailable
  | AdmissionSessionClockCorrupt
  deriving (Eq, Show)

data AdmissionSessionIssueError
  = AdmissionSessionStoreIssue AdmissionSessionStoreError
  | AdmissionSessionClockIssue AdmissionSessionClockError
  deriving (Eq, Show)

data AdmissionConfig = AdmissionConfig
  { admissionConfigCookiePolicy :: SessionCookiePolicy,
    admissionConfigSessionStore :: AdmissionSessionStore,
    admissionConfigReadClock :: IO (Either AdmissionSessionClockError UnixTimeNanoseconds)
  }

instance Show AdmissionConfig where
  show _ = "AdmissionConfig <redacted>"

data AdmissionConfigError
  = AdmissionCookieMustUseHostPrefix
  | AdmissionCookieLifetimeMustBeOneDay
  deriving (Eq, Show)

mkAdmissionConfig :: SessionCookiePolicy -> AdmissionSessionStore -> IO (Either AdmissionSessionClockError UnixTimeNanoseconds) -> Either AdmissionConfigError AdmissionConfig
mkAdmissionConfig cookiePolicy sessionStore readClock
  | not ("__Host-" `Text.isPrefixOf` sessionCookieNameText (sessionCookieName cookiePolicy)) = Left AdmissionCookieMustUseHostPrefix
  | sessionCookieMaxAgeSeconds cookiePolicy /= admissionSessionLifetimeSeconds = Left AdmissionCookieLifetimeMustBeOneDay
  | otherwise = Right (AdmissionConfig cookiePolicy sessionStore readClock)

defaultAdmissionSessionCookiePolicy :: SessionCookiePolicy
defaultAdmissionSessionCookiePolicy =
  SessionCookiePolicy
    { sessionCookieName =
        case mkSessionCookieName "__Host-composed-admission" of
          Just cookieName -> cookieName
          Nothing -> error "static admission session cookie name is invalid",
      sessionCookieMaxAgeSeconds = admissionSessionLifetimeSeconds
    }

admissionSessionLifetimeSeconds :: Word64
admissionSessionLifetimeSeconds = 24 * 60 * 60

issueAdmissionSession :: AdmissionConfig -> AdmissionPrincipalId -> IO (Either AdmissionSessionIssueError (OpaqueSession AdmissionPrincipalId))
issueAdmissionSession config principalId =
  runExceptT $ do
    issuedAt <- withExceptT AdmissionSessionClockIssue (readAdmissionSessionClock config)
    expiresAt <-
      case addUnixTimeNanoseconds issuedAt (admissionSessionLifetimeSeconds * nanosecondsPerSecond) of
        Nothing -> throwError (AdmissionSessionStoreIssue AdmissionSessionStoreCorrupt)
        Just value -> pure value
    sessionToken <- liftIO generateSessionId
    let session = OpaqueSession sessionToken principalId issuedAt expiresAt
    saved <- liftAdmissionSessionStore (saveAdmissionSession (admissionConfigSessionStore config) session)
    case saved of
      True -> pure session
      False -> throwError (AdmissionSessionStoreIssue AdmissionSessionStoreCorrupt)

readAdmissionSessionClock :: AdmissionConfig -> ExceptT AdmissionSessionClockError IO UnixTimeNanoseconds
readAdmissionSessionClock = ExceptT . admissionConfigReadClock

liftAdmissionSessionStore :: IO (Either AdmissionSessionStoreError value) -> ExceptT AdmissionSessionIssueError IO value
liftAdmissionSessionStore = liftEitherWith AdmissionSessionStoreIssue

nanosecondsPerSecond :: Word64
nanosecondsPerSecond = 1000000000

resolveAdmissionCsrfBinding :: ComposedContext -> IO CsrfBindingResolution
resolveAdmissionCsrfBinding requestContext =
  pure $
    case requestLocal requestContext of
      RootLocal -> AnonymousCsrfBinding
      AdmissionEstablished principal ->
        BoundCsrfBinding
          (csrfBindingFromCanonicalBytes (renderAdmissionGrantBinding principal))
          (admissionPrincipalSessionExpiresAt principal)

renderAdmissionGrantBinding :: AdmissionPrincipal -> ByteString.ByteString
renderAdmissionGrantBinding principal =
  ByteString.intercalate
    "\NUL"
    [ "composed-domains-csrf-grants-v1",
      "admission-session",
      TextEncoding.encodeUtf8 (sessionIdText (unAdmissionSessionId (admissionPrincipalSessionId principal))),
      TextEncoding.encodeUtf8 (Text.pack (show (Time.unixTimeNanosecondsValue (admissionPrincipalSessionExpiresAt principal))))
    ]

data AdmissionGuardFailure
  = AdmissionNotEstablished
  | AdmissionUnavailable

establishAdmissionPrincipal :: AdmissionConfig -> EndpointRequest RootRoute ComposedContext RootAuthorization -> ExceptT AdmissionGuardFailure IO AdmissionPrincipal
establishAdmissionPrincipal config endpointRequest = do
  sessionToken <-
    case extractSessionCookieId (sessionCookieName (admissionConfigCookiePolicy config)) (Wai.requestHeaders (endpointWaiRequest endpointRequest)) of
      SessionCookieFound value -> pure (mkAdmissionSessionId value)
      SessionCookieMissing -> throwError AdmissionNotEstablished
      SessionCookieMalformed -> throwError AdmissionNotEstablished
      SessionCookieAmbiguous -> throwError AdmissionNotEstablished
  loadedSession <- do
    loadResult <- liftIO (loadAdmissionSession (admissionConfigSessionStore config) sessionToken)
    case loadResult of
      Left _ -> throwError AdmissionUnavailable
      Right value -> pure value
  now <- withExceptT (const AdmissionUnavailable) (readAdmissionSessionClock config)
  case validateSession now loadedSession of
    ActiveSession session ->
      pure
        ( mkAdmissionPrincipal
            (sessionPrincipal session)
            (mkAdmissionSessionId (sessionId session))
            (sessionExpiresAtNanoseconds session)
        )
    MissingSession -> throwError AdmissionNotEstablished
    ExpiredSession -> throwError AdmissionNotEstablished
