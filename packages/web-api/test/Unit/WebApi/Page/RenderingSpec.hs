{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (newIORef, readIORef)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.AccountPages (AccountActionTarget (..), emptyRegistrationForm)
import WebApi.Config (defaultAppConfig)
import WebApi.Database (DatabaseError (..), DatabaseSeed (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), AuthenticatedProfilePageDetails (..), CallToAction (..), PendingProfilePageDetails (..), ProfilePageModel (..), SecondPageModel (..), SignedOutProfilePageDetails (..), UnavailableProfilePageDetails (..), buildPageModel, renderPageBody, renderPageWithDatabase)
import WebApi.Route (AppRoute (..), defaultRequestContext)

spec = do
  describe "renderPageBody" $ do
    it "renders account page bodies alongside ordinary page bodies" $ do
      renderPageBody (RegistrationPage RegisterAccountTarget emptyRegistrationForm)
        `shouldSatisfy` Text.isInfixOf "data-page=\"registration\""
      resendTargetReference <- newIORef UpdateProfileTarget
      resendTarget <- readIORef resendTargetReference
      let pendingProfile =
            renderPageBody
              ( ProfilePage
                  ( PendingProfilePage
                      ( PendingProfilePageDetails
                          "Profile"
                          "Verify your email address before continuing."
                          "person@example.test"
                          (Just "person_01")
                          (Just "Person Example")
                          resendTarget
                          "Resend verification email"
                          (CallToAction "Sign out" LogoutRoute "/logout")
                      )
                  )
              )
      pendingProfile
        `shouldSatisfy` \html -> Text.isInfixOf "data-profile-resend=\"true\"" html && not (Text.isInfixOf "data-error-state=\"true\"" html)
      let anonymousProfile =
            renderPageBody
              ( ProfilePage
                  ( AuthenticatedProfilePage
                      ( AuthenticatedProfilePageDetails
                          "Profile"
                          "Signed in."
                          "person@example.test"
                          Nothing
                          Nothing
                          (CallToAction "Sign out" LogoutRoute "/logout")
                      )
                  )
              )
      anonymousProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-username"
      anonymousProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-display-name"

    it "renders every profile component state with scoped classes, explicit absence, and escaping" $ do
      let signedOutProfile =
            renderPageBody
              ( ProfilePage
                  ( SignedOutProfilePage
                      ( SignedOutProfilePageDetails
                          "Profile <unsafe>"
                          "Sign in & continue."
                          (CallToAction "Sign in" LoginRoute "/login")
                          (CallToAction "Create account" RegistrationRoute "/register")
                      )
                  )
              )
          authenticatedProfile =
            renderPageBody
              ( ProfilePage
                  ( AuthenticatedProfilePage
                      ( AuthenticatedProfilePageDetails
                          "Profile"
                          "Signed in."
                          "person&admin@example.test"
                          Nothing
                          Nothing
                          (CallToAction "Sign out" LogoutRoute "/logout")
                      )
                  )
              )
          unavailableProfile =
            renderPageBody
              ( ProfilePage
                  ( UnavailableProfilePage
                      ( UnavailableProfilePageDetails
                          "Profile"
                          "Profile access is unavailable."
                          (CallToAction "Sign in" LoginRoute "/login")
                      )
                  )
              )
      expectAll
        ( (signedOutProfile `shouldSatisfy` Text.isInfixOf "<h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Profile &lt;unsafe&gt;</h1>")
            :| [ signedOutProfile `shouldSatisfy` Text.isInfixOf "Sign in &amp; continue.",
                 authenticatedProfile `shouldSatisfy` Text.isInfixOf "data-profile-email=\"true\" class=\"harch-profile-identity-value\">person&amp;admin@example.test",
                 authenticatedProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-username",
                 authenticatedProfile `shouldNotSatisfy` Text.isInfixOf "data-profile-display-name",
                 unavailableProfile `shouldSatisfy` Text.isInfixOf "data-page=\"profile\" class=\"harch-page-frame-root\""
               ]
        )

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      secondShell <- renderedShell defaultAppConfig SecondRoute
      secondPageModel <- buildPageModel secondRequest
      renderPageBody secondPageModel
        `shouldBe` "<section data-page=\"second\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Second</h1><p class=\"harch-page-frame-summary\">Second page content with stubbed data ready for future loaders.</p><div class=\"harch-page-frame-content\"><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>"
      expectAll
        ( (Text.isInfixOf "<nav data-navigation-region=\"primary\" class=\"harch-app-shell-navigation\">" secondShell `shouldBe` True)
            :| [ Text.isInfixOf "href=\"/\" data-page-link=\"true\">Home" secondShell `shouldBe` True,
                 Text.isInfixOf "href=\"/second\" data-page-link=\"true\" aria-current=\"page\">Second" secondShell `shouldBe` True,
                 Text.isInfixOf "href=\"/spaces\" data-page-link=\"true\">Spaces" secondShell `shouldBe` True,
                 Text.isInfixOf "<main id=\"app-main\" data-navigation-content=\"true\" class=\"harch-app-shell-main\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True
               ]
        )

    it "renders the app-home spaces surface without requiring client code" $ do
      spacesPageModel <- buildPageModel spacesRequest
      renderPageBody spacesPageModel
        `shouldBe` "<section data-page=\"spaces\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Site under construction</h1><p class=\"harch-page-frame-summary\">Follow this space.</p><div class=\"harch-page-frame-content\"></div></section>"
      spanishSpacesPageModel <- buildPageModel spanishSpacesRequest
      renderPageBody spanishSpacesPageModel
        `shouldBe` "<section data-page=\"spaces\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Sitio en construcción</h1><p class=\"harch-page-frame-summary\">Sigan este espacio.</p><div class=\"harch-page-frame-content\"></div></section>"

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      secondPageModel <- buildPageModel secondRequest
      let secondBody = renderPageBody secondPageModel
      Text.isInfixOf "<section data-page=\"second\" class=\"harch-page-frame-root\">" secondBody `shouldBe` True
      Text.isInfixOf "data-page-title=\"true\"" secondBody `shouldBe` True
      Text.isInfixOf "data-page-link=\"true\"" secondBody `shouldBe` True
      Text.isInfixOf "<body" secondBody `shouldBe` False

    it "covers empty and populated highlight rendering branches" $ do
      secondPageModel <- buildPageModel secondRequest
      Text.isInfixOf "<p data-empty-state=\"true\">No highlights yet.</p>" (renderPageBody secondPageModel) `shouldBe` True
      renderPageBody
        ( SecondPage
            SecondPageModel
              { secondHeading = "Second",
                secondSummary = "Second page content with stubbed data ready for future loaders.",
                secondHighlights = ["Fast SSR", "Stable routes"],
                secondErrorMessage = Nothing,
                secondPrimaryAction =
                  CallToAction
                    { callToActionLabel = "Return home",
                      callToActionRoute = HomeRoute,
                      callToActionHref = "/"
                    }
              }
        )
        `shouldBe` "<section data-page=\"second\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Second</h1><p class=\"harch-page-frame-summary\">Second page content with stubbed data ready for future loaders.</p><div class=\"harch-page-frame-content\"><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>"

    it "renders an explicit error state when the second-page load fails" $
      renderPageWithDatabase
        defaultAppConfig
        ( buildSeededPageRepository
            DatabaseSeed
              { englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` HarchWeb.Page
          { HarchWeb.pageTitle = "web-api: Second",
            HarchWeb.pageRoute = SecondRoute,
            HarchWeb.pageContext = defaultRequestContext,
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\" class=\"harch-page-frame-root\"><h1 data-page-title=\"true\" class=\"harch-page-frame-title\">Second</h1><p class=\"harch-page-frame-summary\">Second page content is temporarily unavailable.</p><div class=\"harch-page-frame-content\"><p data-error-state=\"true\">Could not load second page data.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></div></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }
