{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.AccountPages (AccountActionTarget (..))
import WebApi.Config (AppConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Page (AppPageModel (..), AuthenticatedProfilePageDetails (..), CallToAction (..), PendingProfilePageDetails (..), ProfilePageModel (..), SignedOutProfilePageDetails (..), UnavailableProfilePageDetails (..), renderPage, renderPageFromRouteData)
import WebApi.Route (AppRoute (..), defaultRequestContext)
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

existingSpec :: SpecWith ()
existingSpec =
  describe "ProfilePageModel and its detail records" $
    it "compares every rendered profile identity field and keeps models printable" $ do
      let signInAction = CallToAction "Sign in" LoginRoute "/login"
          registrationAction = CallToAction "Create account" RegistrationRoute "/register"
          signOutAction = CallToAction "Sign out" LogoutRoute "/logout"
          signedOutModel = SignedOutProfilePage (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
          pendingModel = PendingProfilePage (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
          pendingModelWithIdentity = PendingProfilePage (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" (Just "pending-person") (Just "Pending Person") UpdateProfileTarget "Resend verification email" signOutAction)
          authenticatedModel = AuthenticatedProfilePage (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
          authenticatedModelWithIdentity = AuthenticatedProfilePage (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" (Just "authenticated-person") (Just "Authenticated Person") signOutAction)
          unavailableModel = UnavailableProfilePage (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
          models =
            [ (signedOutModel, "SignedOutProfilePage"),
              (pendingModel, "PendingProfilePage"),
              (authenticatedModel, "AuthenticatedProfilePage"),
              (unavailableModel, "UnavailableProfilePage")
            ]
      mapM_ assertProfilePageModelShow models
      -- Each detail record's own 'deriving (Eq, Show)' is only reached
      -- indirectly above, through the outer 'ProfilePageModel' constructor's
      -- derived instances; HPC does not credit those four declarations from
      -- that alone, confirmed directly by the coverage gate rather than
      -- assumed, so each is exercised here too, directly and on its own.
      -- Same-value, different-construction (not 'x == x').
      SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
        `shouldBe` SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
      PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
        `shouldBe` PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
      AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
        `shouldBe` AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
      UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
        `shouldBe` UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
      -- 'deriving (Eq)' writes only '=='; the unoverridden '/=' default
      -- method HPC boxes separately (this codebase's own established
      -- derived-instance lesson), so a genuine inequality is exercised too.
      SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction
        `shouldNotBe` SignedOutProfilePageDetails "Other" "Sign in to view and manage your profile." signInAction registrationAction
      PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
        `shouldNotBe` PendingProfilePageDetails "Other" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction
      AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction
        `shouldNotBe` AuthenticatedProfilePageDetails "Other" "You are signed in." "person@example.test" Nothing Nothing signOutAction
      UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction
        `shouldNotBe` UnavailableProfilePageDetails "Other" "Your profile is temporarily unavailable." signInAction
      show (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
        `shouldContain` "signedOutProfileHeading = \"Profile\""
      show (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
        `shouldContain` "pendingProfileHeading = \"Profile\""
      show (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
        `shouldContain` "authenticatedProfileHeading = \"Profile\""
      show (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
        `shouldContain` "unavailableProfileHeading = \"Profile\""
      -- Derived 'Show' also writes distinct 'showsPrec'/'showList' methods.
      showsPrec 11 (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction) ""
        `shouldBe` "("
        <> show (SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction)
        <> ")"
      showsPrec 11 (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction) ""
        `shouldBe` "("
        <> show (PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction)
        <> ")"
      showsPrec 11 (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction) ""
        `shouldBe` "("
        <> show (AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction)
        <> ")"
      showsPrec 11 (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction) ""
        `shouldBe` "("
        <> show (UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction)
        <> ")"
      show [SignedOutProfilePageDetails "Profile" "Sign in to view and manage your profile." signInAction registrationAction]
        `shouldContain` "signedOutProfileHeading = \"Profile\""
      show [PendingProfilePageDetails "Profile" "Verify your email address before continuing." "person@example.test" Nothing Nothing UpdateProfileTarget "Resend verification email" signOutAction]
        `shouldContain` "pendingProfileHeading = \"Profile\""
      show [AuthenticatedProfilePageDetails "Profile" "You are signed in." "person@example.test" Nothing Nothing signOutAction]
        `shouldContain` "authenticatedProfileHeading = \"Profile\""
      show [UnavailableProfilePageDetails "Profile" "Your profile is temporarily unavailable." signInAction]
        `shouldContain` "unavailableProfileHeading = \"Profile\""
      expectAll
        ( (ProfilePage signedOutModel == ProfilePage pendingModel `shouldBe` False)
            :| [ (pendingModel /= pendingModelWithIdentity)
                   `shouldBe` True,
                 (authenticatedModel /= authenticatedModelWithIdentity)
                   `shouldBe` True,
                 (pendingModel /= authenticatedModel)
                   `shouldBe` True
               ]
        )

assertProfilePageModelShow :: (ProfilePageModel, Text) -> Expectation
assertProfilePageModelShow (profilePageModel, expectedPrefix) =
  Text.pack (show (ProfilePage profilePageModel)) `shouldSatisfy` Text.isPrefixOf ("ProfilePage (" <> expectedPrefix)

spec = do
  existingSpec
  describe "renderPage" $ do
    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "renders the app-home spaces page entirely on the server" $
      renderPage defaultAppConfig spacesRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Spaces",
            HarchWeb.pageRoute = SpacesRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Site under construction</h1><p>Follow this space.</p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"not-found\"><h1 data-page-title=\"true\">Not Found</h1><p>The requested page could not be found.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a Spanish not-found page model" $
      renderPage defaultAppConfig spanishNotFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = spanishRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"not-found\"><h1 data-page-title=\"true\">No encontrado</h1><p>No se pudo encontrar la pagina solicitada.</p><p><a href=\"/es\" data-page-link=\"true\">Volver al inicio</a></p></section>"),
            HarchWeb.pageBootstrapHooks = []
          }

    it "renders selected route data without reloading it" $
      renderPageFromRouteData
        defaultAppConfig
        secondRequest
        ( SecondRouteDataResult
            ( Right
                SecondRouteData
                  { secondRouteSummary = "Shared domain summary.",
                    secondRouteHighlights = ["Shared loader"]
                  }
            )
        )
        `shouldBe` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Shared domain summary.</p><ul><li>Shared loader</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "keeps shared layout data consistent across all routes" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      secondShell <- renderedShell config SecondRoute
      notFoundShell <- renderedShell config NotFoundRoute
      Text.isInfixOf "<title>test-app: Second</title>" secondShell `shouldBe` True
      Text.isInfixOf "data-bootstrap-hooks=\"second-page\"" secondShell `shouldBe` True
      Text.isInfixOf "<title>test-app: Not Found</title>" notFoundShell `shouldBe` True
      Text.isInfixOf "<script nonce=\"" secondShell `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/assets/navigation.js\" defer></script>" secondShell `shouldBe` True

    it "keeps config, routes, and pages serializable and deterministic for tests" $ do
      let config =
            AppConfig
              { appTitlePrefix = "test-app",
                listenerConfigs = listenerConfigs defaultAppConfig,
                staticAssets = staticAssets defaultAppConfig,
                requestPolicy = requestPolicy defaultAppConfig,
                observability = observability defaultAppConfig
              }
      show config
        `shouldContain` ("staticAssetContentTypes = " <> show defaultStaticAssetContentTypes)
      show defaultRequestContext `shouldBe` "AppRequestContext {requestLocale = English, requestLocaleIsExplicit = False, requestCorrelationId = Nothing, requestPathPrefix = PathPrefix \"\", requestQueryParameters = [], requestSessionId = Nothing, requestMfaEnrollmentSessionId = Nothing}"
      show (renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []}))))
        `shouldBe` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute, pageContext = AppRequestContext {requestLocale = English, requestLocaleIsExplicit = False, requestCorrelationId = Nothing, requestPathPrefix = PathPrefix \"\", requestQueryParameters = [], requestSessionId = Nothing, requestMfaEnrollmentSessionId = Nothing}, pageBody = \"<section data-page=\\\"second\\\"><h1 data-page-title=\\\"true\\\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\\\"true\\\">No highlights yet.</p><p><a href=\\\"/\\\" data-page-link=\\\"true\\\">Return home</a></p></section>\", pageBootstrapHooks = [\"second-page\"]}"
      renderPage config secondRequest `shouldReturn` renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))
