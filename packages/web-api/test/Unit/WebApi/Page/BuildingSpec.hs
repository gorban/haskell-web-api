{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List (isInfixOf)
import HarchWeb qualified
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseSeed (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), CallToAction (..), HelpPageModel (..), LanguagePageModel (..), SecondPageModel (..), SpacesPageModel (..), buildCallToActionHref, buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase)
import WebApi.Route (AppRoute (..), defaultRequestContext)
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

spec = do
  describe "buildCallToActionHref" $ do
    it "preserves the route renderer's unsafe-URL diagnostic" $
      evaluate (buildCallToActionHref "javascript:alert(1)" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "WebApi.Route rendered an unsafe URL: javascript:alert(1)" `isInfixOf` message

  describe "buildPageModel" $ do
    it "localizes Spanish second-page return actions" $
      buildPageModel spanishSecondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Segunda",
              secondSummary = "Second page content with stubbed data ready for future loaders.",
              secondHighlights = [],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Volver al inicio",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/es"
                  }
            }

    it "ports the spaces placeholder with its source-app English and Spanish copy" $ do
      buildPageModel spacesRequest
        `shouldReturn` SpacesPage
          SpacesPageModel
            { spacesHeading = "Site under construction",
              spacesSummary = "Follow this space."
            }

    it "builds localized language and Help reference pages from static route data" $ do
      let languageRequest = HarchWeb.RouteRequest LanguageRoute defaultRequestContext
          spanishLanguageRequest = HarchWeb.RouteRequest LanguageRoute spanishRequestContext
          helpRequest = HarchWeb.RouteRequest HelpRoute defaultRequestContext
      buildPageModel languageRequest
        `shouldReturn` LanguagePage (LanguagePageModel "Choose a language" "Choose the language used for this page.")
      buildPageModel spanishLanguageRequest
        `shouldReturn` LanguagePage (LanguagePageModel "Elige un idioma" "Elige el idioma utilizado para esta pagina.")
      buildPageModel helpRequest
        `shouldReturn` HelpPage
          HelpPageModel
            { helpHeading = "Help and support",
              helpSummary = "Get help with account access and verification.",
              helpAccountGuidance = "Sign in to review your account, or create an account if you do not have one.",
              helpSignInAction = CallToAction "Sign in" LoginRoute "/login",
              helpRegistrationAction = CallToAction "Create account" RegistrationRoute "/register"
            }
      buildPageModel spanishSpacesRequest
        `shouldReturn` SpacesPage
          SpacesPageModel
            { spacesHeading = "Sitio en construcción",
              spacesSummary = "Sigan este espacio."
            }

    it "renders selected route data into a page model" $ do
      let selectedRouteData =
            SecondRouteDataResult
              ( Right
                  SecondRouteData
                    { secondRouteSummary = "Shared domain summary.",
                      secondRouteHighlights = ["Shared loader", "Shared renderer"]
                    }
              )
      buildPageModelFromRouteData secondRequest selectedRouteData
        `shouldBe` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Shared domain summary.",
              secondHighlights = ["Shared loader", "Shared renderer"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }
    it "loads second-page content from the database effect when provided" $
      buildPageModelWithDatabase
        ( buildSeededPageRepository
            DatabaseSeed
              { englishSecondPageData =
                  Right
                    SecondPageData
                      { secondPageDataSummary = "Loaded from the seeded database effect.",
                        secondPageDataHighlights = ["Fast SSR", "Progressive enhancement"]
                      },
                spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
              }
        )
        secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Loaded from the seeded database effect.",
              secondHighlights = ["Fast SSR", "Progressive enhancement"],
              secondErrorMessage = Nothing,
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }

    it "builds an explicit error-state second page when the database effect fails" $ do
      let failingSecondRepository =
            buildSeededPageRepository
              DatabaseSeed
                { englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                  spanishSecondPageData = Left (SecondPageDataError "seed unavailable")
                }
      buildPageModelWithDatabase failingSecondRepository secondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Second",
              secondSummary = "Second page content is temporarily unavailable.",
              secondHighlights = [],
              secondErrorMessage = Just "Could not load second page data.",
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Return home",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/"
                  }
            }
      buildPageModelWithDatabase failingSecondRepository spanishSecondRequest
        `shouldReturn` SecondPage
          SecondPageModel
            { secondHeading = "Segunda",
              secondSummary = "El contenido de la segunda pagina no esta disponible temporalmente.",
              secondHighlights = [],
              secondErrorMessage = Just "No se pudieron cargar los datos de la segunda pagina.",
              secondPrimaryAction =
                CallToAction
                  { callToActionLabel = "Volver al inicio",
                    callToActionRoute = HomeRoute,
                    callToActionHref = "/es"
                  }
            }
