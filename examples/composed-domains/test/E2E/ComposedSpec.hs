{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{-# E2E_SPEC #-}

import App.Composed
import Catalog.Domain
import Crypto.Error (maybeCryptoError)
import Data.ByteString qualified as ByteString
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import Data.Word (Word64)
import HarchWeb
import HarchWeb.Csrf qualified as Csrf
import HarchWeb.LoginProtection (defaultLoginProtectionPolicy)
import HarchWeb.RequestContext (RequestContext (..), RequestIdentity (..))
import HarchWeb.Secret (encryptSecretWithNonce, mkEncryptionNonce, mkSecretEncryptionKey, mkSecretPlaintext)
import HarchWeb.Session (OpaqueSession (..))
import HarchWeb.Site qualified as Site
import HarchWeb.Time (unixTimeNanoseconds, unixTimeSeconds)
import HarchWeb.Totp (mkTotpSecret, renderTotpSecret, totpCode, totpCodeText)
import Orders.Domain

spec =
  describe "composed-domains real-browser behavior" $ do
    it "keeps localized public and mounted-domain navigation SSR-complete and enhanced" $
      withBrowserAndServer $ \browser server -> do
        let loginUrl = localServerBaseUrl server <> "/es/public/login"
            catalogUrl = localServerBaseUrl server <> "/es/catalog"
            ordersUrl = localServerBaseUrl server <> "/es/orders"
        ( runBrowserScenario browser do
            visit loginUrl
            assertAll
              ((,) <$> textContent (byRole Link `named` "Catalog") <*> textContent (byRole Link `named` "Orders"))
              (\(catalogLabel, ordersLabel) -> (catalogLabel `shouldBe` "Catalog") :| [ordersLabel `shouldBe` "Orders"])
            click (byRole Link `named` "Catalog")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "es catalog") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` catalogUrl)
                    :| [ heading `shouldBe` "es catalog",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 0}|])
                       ]
              )
            reload
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "es catalog"))
              (\(url, heading) -> (url `shouldBe` catalogUrl) :| [heading `shouldBe` "es catalog"])
            click (byRole Link `named` "Orders")
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "es orders"))
              (\(url, heading) -> (url `shouldBe` ordersUrl) :| [heading `shouldBe` "es orders"])
          )
          `shouldReturn` Right ()

    it "keeps default-locale mounted navigation SSR-complete across reload and enhancement" $
      withBrowserAndServer $ \browser server -> do
        let catalogUrl = localServerBaseUrl server <> "/catalog"
            ordersUrl = localServerBaseUrl server <> "/en/orders"
        ( runBrowserScenario browser do
            visit catalogUrl
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "en catalog"))
              (\(url, heading) -> (url `shouldBe` catalogUrl) :| [heading `shouldBe` "en catalog"])
            reload
            assertAll
              (textContent (byRole Heading `named` "en catalog"))
              (\heading -> (heading `shouldBe` "en catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "en orders") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` ordersUrl)
                    :| [ heading `shouldBe` "en orders",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {enhancedNavigationFetchCount = 1, hardNavigationCount = 1}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "keeps public and mounted-domain navigation usable when scripts are disabled" $
      withBrowserAndServer $ \browser server -> do
        let loginUrl = localServerBaseUrl server <> "/public/login"
            spanishLoginUrl = localServerBaseUrl server <> "/es/public/login"
        ( runBrowserScenario browser do
            visitWithoutScripts loginUrl
            assertAll
              ((,) <$> textContent (byRole Heading `named` "Login") <*> textContent (byRole Link `named` "Catalog"))
              (\(heading, catalogLabel) -> (heading `shouldBe` "Login") :| [catalogLabel `shouldBe` "Catalog"])
            click (byRole Link `named` "Catalog")
            assertAll
              (textContent (byRole Heading `named` "en catalog"))
              (\heading -> (heading `shouldBe` "en catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              (textContent (byRole Heading `named` "en orders"))
              (\heading -> (heading `shouldBe` "en orders") :| [])
            visitWithoutScripts spanishLoginUrl
            click (byRole Link `named` "Catalog")
            assertAll
              (textContent (byRole Heading `named` "es catalog"))
              (\heading -> (heading `shouldBe` "es catalog") :| [])
            click (byRole Link `named` "Orders")
            assertAll
              (textContent (byRole Heading `named` "es orders"))
              (\heading -> (heading `shouldBe` "es orders") :| [])
          )
          `shouldReturn` Right ()

    it "submits admission through the enhanced action and replaces credential history" $
      withAdmissionBrowserAndServer $ \browser server -> do
        let admissionUrl = localServerBaseUrl server <> "/public/admission"
            loginUrl = localServerBaseUrl server <> "/en/public/login"
        ( runBrowserScenario browser do
            visit admissionUrl
            fill (byLabel "Admission name") "support_operator"
            fill (byLabel "One-time code") browserAdmissionCode
            submit (byRole Form `named` "Admission")
            assertAll
              ((,,) <$> currentUrl <*> textContent (byRole Heading `named` "Login") <*> browserMetrics)
              ( \(url, heading, metrics) ->
                  (url `shouldBe` loginUrl)
                    :| [ heading `shouldBe` "Login",
                         $([|metrics|] `shouldMatch` [p|BrowserMetrics {hardNavigationCount = 0, mutationRequestCount = 1}|])
                       ]
              )
          )
          `shouldReturn` Right ()

    it "submits the same admission workflow through its CSRF-protected native fallback" $
      withAdmissionBrowserAndServer $ \browser server -> do
        let admissionUrl = localServerBaseUrl server <> "/public/admission"
            loginUrl = localServerBaseUrl server <> "/en/public/login"
        ( runBrowserScenario browser do
            visitWithoutScripts admissionUrl
            fill (byLabel "Admission name") "support_operator"
            fill (byLabel "One-time code") browserAdmissionCode
            submit (byRole Form `named` "Admission")
            assertAll
              ((,) <$> currentUrl <*> textContent (byRole Heading `named` "Login"))
              (\(url, heading) -> (url `shouldBe` loginUrl) :| [heading `shouldBe` "Login"])
          )
          `shouldReturn` Right ()

withBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  withLocalTestServer composedBrowserApplication (action browser)

withAdmissionBrowserAndServer :: (BrowserConfig -> LocalTestServer -> IO a) -> IO a
withAdmissionBrowserAndServer action = do
  loadedConfig <- loadPlaywrightBrowserConfig
  browser <-
    case loadedConfig of
      Left loadError -> expectationFailure loadError >> fail "unreachable"
      Right config -> pure config
  admissionApplication <- admissionBrowserApplication
  withLocalTestServer admissionApplication (action browser)

composedBrowserApplication :: Application RootRoute RootAction ComposedContext RootAuthorization
composedBrowserApplication =
  Site.buildSiteApplication (buildComposedSiteWithSecurity defaultComposedStaticAssets defaultLocalePolicy browserCsrfProtection browserSecurity catalogQueries catalogCommands ordersQueries ordersCommands)

admissionBrowserApplication :: IO (Application RootRoute RootAction ComposedContext RootAuthorization)
admissionBrowserApplication = do
  sessions <- newIORef ([] :: [OpaqueSession AdmissionPrincipalId])
  usedCounters <- newIORef ([] :: [Word64])
  let loginName = requiredBrowser "admission login" (mkAdmissionLoginName "support_operator")
      principalId = requiredBrowser "admission principal" (mkAdmissionPrincipalId "browser-operator")
      encryptionKey = requiredBrowser "admission encryption key" (mkSecretEncryptionKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
      secret = requiredBrowser "admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")
      now = unixTimeNanoseconds 123456000000000
      encryptedSecret =
        requiredBrowser
          "encrypted admission TOTP secret"
          ( mkEncryptedAdmissionTotpSecret
              =<< maybeCryptoError
                ( encryptSecretWithNonce
                    encryptionKey
                    (requiredBrowser "admission encryption nonce" (mkEncryptionNonce (ByteString.replicate 12 6)))
                    (mkSecretPlaintext (TextEncoding.encodeUtf8 (renderTotpSecret secret)))
                )
          )
      credential = StoredAdmissionCredential principalId encryptedSecret Nothing
      sessionStore =
        AdmissionSessionStore
          { saveAdmissionSession = \session -> do
              atomicModifyIORef' sessions (\saved -> (session : filter ((/= sessionId session) . sessionId) saved, Right True)),
            loadAdmissionSession = \requestedSessionId -> do
              saved <- readIORef sessions
              pure (Right (find ((== requestedSessionId) . mkAdmissionSessionId . sessionId) saved)),
            invalidateAdmissionSession = \requestedSessionId _ -> do
              atomicModifyIORef' sessions (\saved -> (filter ((/= requestedSessionId) . mkAdmissionSessionId . sessionId) saved, Right True))
          }
      credentialStore =
        AdmissionCredentialStore
          { findAdmissionCredential = \receivedLogin -> pure (Right (if receivedLogin == loginName then Just credential else Nothing)),
            markAdmissionTotpCounterUsed = \_ counter ->
              atomicModifyIORef' usedCounters (\used -> if counter `elem` used then (used, Right False) else (counter : used, Right True))
          }
      attemptStore =
        AdmissionAttemptStore
          { reserveAdmissionAttempt = \_ _ -> pure (Right (AdmissionAttemptReserved (AdmissionAttemptReservation "browser-reservation"))),
            settleAdmissionAttempt = \_ _ -> pure (Right ()),
            cancelAdmissionAttempt = \_ -> pure (Right ())
          }
      proofConfig =
        AdmissionProofConfig
          { admissionProofCredentials = credentialStore,
            admissionProofAttempts = attemptStore,
            admissionProofPolicy = defaultLoginProtectionPolicy,
            admissionProofEncryptionKey = encryptionKey,
            admissionProofReadClock = pure (Right now)
          }
  sessionConfig <-
    case mkAdmissionConfig defaultAdmissionSessionCookiePolicy sessionStore (pure (Right now)) of
      Left _ -> expectationFailure "expected browser admission session configuration" >> fail "unreachable"
      Right config -> pure config
  case buildComposedSiteWithAdmissionSecurity defaultComposedStaticAssets defaultLocalePolicy admissionBrowserCsrfProtection (AdmissionEnabled sessionConfig proofConfig) browserSecurity catalogQueries catalogCommands ordersQueries ordersCommands of
    Left _ -> expectationFailure "expected admission-enabled browser site" >> fail "unreachable"
    Right site -> pure (Site.buildSiteApplication site)

browserAdmissionCode :: Text
browserAdmissionCode =
  totpCodeText
    (totpCode (unixTimeSeconds 123456) (requiredBrowser "browser admission TOTP secret" (mkTotpSecret "JBSWY3DPEHPK3PXPJBSWY3DPEHPK3PXP")))

browserCsrfProtection :: Csrf.CsrfProtection ComposedContext
browserCsrfProtection =
  Csrf.signedCsrfProtection
    keyring
    Csrf.defaultSignedCsrfPolicy
    (pure 1000000000)
    (const (pure Csrf.AnonymousCsrfBinding))
  where
    keyId = requiredCsrf "browser CSRF key id" (Csrf.mkCsrfKeyId "composed-browser-v1")
    signingKey = requiredCsrf "browser CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "browser CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

admissionBrowserCsrfProtection :: Csrf.CsrfProtection ComposedContext
admissionBrowserCsrfProtection =
  Csrf.signedCsrfProtection
    keyring
    Csrf.defaultSignedCsrfPolicy
    (pure 1000000000)
    resolveAdmissionCsrfBinding
  where
    keyId = requiredCsrf "admission browser CSRF key id" (Csrf.mkCsrfKeyId "composed-browser-v1")
    signingKey = requiredCsrf "admission browser CSRF signing key" (Csrf.mkCsrfSigningKey "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
    keyring = requiredCsrf "admission browser CSRF keyring" (Csrf.mkSignedCsrfKeyring keyId ((keyId, signingKey) :| []))

requiredCsrf :: String -> Maybe value -> value
requiredCsrf label = fromMaybe (error ("expected " <> label))

requiredBrowser :: String -> Maybe value -> value
requiredBrowser label = fromMaybe (error ("expected " <> label))

browserSecurity :: ApplicationSecurity RootRoute ComposedContext RootAuthorization
browserSecurity =
  AuthenticationEnabled [] (AuthenticationGuard (pure . ContinueEndpoint . authenticatedContext . endpointRouteRequest)) []

authenticatedContext :: RouteRequest RootRoute ComposedContext -> ComposedContext
authenticatedContext request =
  (requestContext request)
    { requestIdentity = AuthenticatedIdentity (RootPrincipal (Just (locale "es")) ["catalog.read", "orders.read"])
    }

catalogQueries :: CatalogQueries
catalogQueries = CatalogQueries (\domainContext -> pure (catalogLocaleCode domainContext <> " catalog"))

catalogCommands :: CatalogCommands
catalogCommands = CatalogCommands (const (pure "refreshed"))

ordersQueries :: OrdersQueries
ordersQueries = OrdersQueries (\domainContext -> pure (ordersLocaleCode domainContext <> " orders"))

ordersCommands :: OrdersCommands
ordersCommands = OrdersCommands (\domainContext -> pure (OrderId ("order-" <> ordersLocaleCode domainContext)))
