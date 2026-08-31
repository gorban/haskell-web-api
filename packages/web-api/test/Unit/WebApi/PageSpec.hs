{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.AccountPages (AccountActionTarget (..))
import WebApi.Components.AppControls (appControls, requiredAppAccessibleName)
import WebApi.Config (AppConfig (..), defaultAppConfig, defaultStaticAssetContentTypes)
import WebApi.Page (AppPageModel (..), AuthenticatedProfilePageDetails (..), CallToAction (..), HelpPageModel (..), LanguagePageModel (..), PendingProfilePageDetails (..), ProfilePageModel (..), SignedOutProfilePageDetails (..), UnavailableProfilePageDetails (..), renderPage, renderPageFromRouteData)
import WebApi.Route (AppRequestContext, AppRoute (..), defaultRequestContext)
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
  describe "AHI-6 application control models" $ do
    it "keeps language and Help models comparable and printable" $ do
      let languageModel = LanguagePageModel "Language" "Choose a language."
          otherLanguageModel = LanguagePageModel "Other" "Choose a language."
          helpModel = HelpPageModel "Help" "Summary" "Guidance" (CallToAction "Sign in" LoginRoute "/login") (CallToAction "Register" RegistrationRoute "/register")
          otherHelpModel = helpModel {helpHeading = "Other"}
      expectAll
        ( (languageModel `shouldBe` LanguagePageModel "Language" "Choose a language.")
            :| [ languageModel `shouldNotBe` otherLanguageModel,
                 show languageModel `shouldContain` "languageHeading = \"Language\"",
                 showList [languageModel] "" `shouldContain` "LanguagePageModel",
                 helpModel `shouldBe` HelpPageModel "Help" "Summary" "Guidance" (CallToAction "Sign in" LoginRoute "/login") (CallToAction "Register" RegistrationRoute "/register"),
                 helpModel `shouldNotBe` otherHelpModel,
                 show helpModel `shouldContain` "helpHeading = \"Help\"",
                 showList [helpModel] "" `shouldContain` "HelpPageModel"
               ]
        )

    it "rejects an empty application accessible-name catalog value" $
      evaluate (requiredAppAccessibleName "   " `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "empty accessible-name catalog entry" `Text.isInfixOf` Text.pack message

  describe "renderPage" $ do
    it "selects a distinct second page model" $
      renderPage defaultAppConfig secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = appPageBody defaultRequestContext SecondRoute "<section data-page=\"second\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Second</h1><p class=\"harch-page-frame-summary\">Second page content with stubbed data ready for future loaders.</p><div class=\"harch-page-frame-content\"><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>",
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }

    it "renders the app-home spaces page entirely on the server" $
      renderPage defaultAppConfig spacesRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Spaces",
            HarchWeb.pageRoute = SpacesRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = appPageBody defaultRequestContext SpacesRoute "<section data-page=\"spaces\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Site under construction</h1><p class=\"harch-page-frame-summary\">Follow this space.</p><div class=\"harch-page-frame-content\"></div></section>",
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a stable not-found page model" $
      renderPage defaultAppConfig notFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = appPageBody defaultRequestContext NotFoundRoute "<section data-page=\"not-found\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Not Found</h1><p class=\"harch-page-frame-summary\">The requested page could not be found.</p><div class=\"harch-page-frame-content\"><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>",
            HarchWeb.pageBootstrapHooks = []
          }

    it "selects a Spanish not-found page model" $
      renderPage defaultAppConfig spanishNotFoundRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Not Found",
            HarchWeb.pageRoute = NotFoundRoute,
            HarchWeb.pageContext = spanishRequestContext,
            HarchWeb.pageBody = appPageBody spanishRequestContext NotFoundRoute "<section data-page=\"not-found\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">No encontrado</h1><p class=\"harch-page-frame-summary\">No se pudo encontrar la pagina solicitada.</p><div class=\"harch-page-frame-content\"><p><a href=\"/es\" data-page-link=\"true\">Volver al inicio</a></p></div></section>",
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
            HarchWeb.pageBody = appPageBody defaultRequestContext SecondRoute "<section data-page=\"second\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Second</h1><p class=\"harch-page-frame-summary\">Shared domain summary.</p><div class=\"harch-page-frame-content\"><ul><li>Shared loader</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>",
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
      let renderedPage = renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))
      show renderedPage `shouldContain` "Page {pageTitle = \"test-app: Second\", pageRoute = SecondRoute"
      show renderedPage `shouldContain` "data-harch-dialog-control"
      renderPage config secondRequest `shouldReturn` renderPageFromRouteData config secondRequest (SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Second page content with stubbed data ready for future loaders.", secondRouteHighlights = []})))

appPageBody :: AppRequestContext -> AppRoute -> Text -> HarchWeb.Html
appPageBody requestContext route body =
  HarchWeb.fragment
    [ HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml body),
      appControls requestContext route
    ]
