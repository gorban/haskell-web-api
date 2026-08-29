{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List (isInfixOf)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseSeed (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), CallToAction (..), SecondPageModel (..), SpacesPageModel (..), buildCallToActionHref, buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase)
import WebApi.Route (AppRoute (..))
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

spec = do
  describe "buildCallToActionHref" $ do
    it "raises the unsafe-URL diagnostic when a rendered path is not a safe URL" $
      evaluate (buildCallToActionHref "javascript:alert(1)" `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "buildCallToAction: rendered an unsafe URL: javascript:alert(1)" `isInfixOf` message

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
