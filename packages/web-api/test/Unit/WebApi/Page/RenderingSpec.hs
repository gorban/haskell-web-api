{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.IORef (newIORef, readIORef)
import Data.Text qualified as Text
import HarchWeb qualified
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.AccountPages (AccountActionTarget (..), emptyRegistrationForm)
import WebApi.Config (defaultAppConfig)
import WebApi.Database (DatabaseError (..), DatabaseSeed (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), AuthenticatedProfilePageDetails (..), CallToAction (..), PendingProfilePageDetails (..), ProfilePageModel (..), SecondPageModel (..), buildPageModel, renderPageBody, renderPageWithDatabase)
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

    it "renders the second page with distinct content while the shared shell stays the same" $ do
      secondShell <- renderedShell defaultAppConfig SecondRoute
      secondPageModel <- buildPageModel secondRequest
      renderPageBody secondPageModel
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><p data-empty-state=\"true\">No highlights yet.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"
      Text.isInfixOf "<nav data-navigation-region=\"primary\"><a href=\"/\" data-page-link=\"true\">Home</a><a href=\"/second\" data-page-link=\"true\" aria-current=\"page\">Second</a><a href=\"/spaces\" data-page-link=\"true\">Spaces</a><a href=\"/register\" data-page-link=\"true\">Create account</a><a href=\"/login\" data-page-link=\"true\">Sign in</a><a href=\"/profile\" data-page-link=\"true\">Profile</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"second-page\">" secondShell `shouldBe` True

    it "renders the app-home spaces surface without requiring client code" $ do
      spacesPageModel <- buildPageModel spacesRequest
      renderPageBody spacesPageModel
        `shouldBe` "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Site under construction</h1><p>Follow this space.</p></section>"
      spanishSpacesPageModel <- buildPageModel spanishSpacesRequest
      renderPageBody spanishSpacesPageModel
        `shouldBe` "<section data-page=\"spaces\"><h1 data-page-title=\"true\">Sitio en construcción</h1><p>Sigan este espacio.</p></section>"

    it "preserves page-body HTML invariants needed for later navigation enhancement" $ do
      secondPageModel <- buildPageModel secondRequest
      let secondBody = renderPageBody secondPageModel
      Text.isInfixOf "<section data-page=\"second\">" secondBody `shouldBe` True
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
        `shouldBe` "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p>Second page content with stubbed data ready for future loaders.</p><ul><li>Fast SSR</li><li>Stable routes</li></ul><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"

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
            HarchWeb.pageBody = HarchWeb.trustedHtml (MarkupUnsafe.unsafeTrustHtml "<section data-page=\"second\"><h1 data-page-title=\"true\">Second</h1><p data-error-state=\"true\">Could not load second page data.</p><p>Second page content is temporarily unavailable.</p><p><a href=\"/\" data-page-link=\"true\">Return home</a></p></section>"),
            HarchWeb.pageBootstrapHooks = ["second-page"]
          }
