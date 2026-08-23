{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Exception (ErrorCall (..), evaluate)
import Data.List (isInfixOf)
import HarchWeb qualified
import Network.HTTP.Types qualified as Http
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseSeed (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.Page (AppPageModel (..), CallToAction (..), HomePageModel (..), SecondPageModel (..), SpacesPageModel (..), buildCallToActionHref, buildPageModel, buildPageModelFromRouteData, buildPageModelWithDatabase)
import WebApi.Response (renderApiResponseFromRouteData)
import WebApi.Route (AppRoute (..))
import WebApi.RouteData (RouteDataResult (..), SecondRouteData (..))

spec = do
  describe "buildCallToActionHref" $ do
    it "raises the unsafe-URL diagnostic when a rendered path is not a safe URL" $
      evaluate (buildCallToActionHref "javascript:alert(1)" Nothing `seq` ())
        `shouldThrow` \case
          ErrorCall message -> "buildCallToAction: rendered an unsafe URL: javascript:alert(1)" `isInfixOf` message

  describe "buildPageModel" $ do
    it "builds stubbed home page data with a navigation affordance" $
      buildPageModel homeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Server-rendered home page with stubbed content.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }

    it "keeps locale-aware action paths in stubbed page data" $
      buildPageModel spanishHomeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Inicio",
              homeSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados.",
              homeErrorMessage = Nothing,
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Ver la segunda página",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/es/second"
                  }
            }

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

    it "builds explicit home-page error state when the database effect fails" $ do
      let failingHomeRepository =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  spanishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      buildPageModelWithDatabase failingHomeRepository homeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Home",
              homeSummary = "Home page content is temporarily unavailable.",
              homeErrorMessage = Just "Could not load home page data.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Browse the second page",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/second"
                  }
            }
      buildPageModelWithDatabase failingHomeRepository spanishHomeRequest
        `shouldReturn` HomePage
          HomePageModel
            { homeHeading = "Inicio",
              homeSummary = "El contenido de la pagina de inicio no esta disponible temporalmente.",
              homeErrorMessage = Just "No se pudieron cargar los datos de la pagina de inicio.",
              homePrimaryAction =
                CallToAction
                  { callToActionLabel = "Ver la segunda página",
                    callToActionRoute = SecondRoute,
                    callToActionHref = "/es/second"
                  }
            }

    it "renders selected route data into both page models and API responses" $ do
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
      renderApiResponseFromRouteData selectedRouteData
        `shouldBe` HarchWeb.ResponseBody
          { HarchWeb.responseStatus = Http.status200,
            HarchWeb.responseContentType = "application/json",
            HarchWeb.responseBody = "{\"summary\":\"Shared domain summary.\",\"highlights\":[\"Shared loader\",\"Shared renderer\"]}",
            HarchWeb.responseObservabilityAttributes = [],
            HarchWeb.responseLogEntries = [],
            HarchWeb.responseDatabaseOperations = []
          }

    it "escapes hostile database content in API JSON" $ do
      let hostileRouteData =
            SecondRouteDataResult
              ( Right
                  SecondRouteData
                    { secondRouteSummary = "quote\" slash\\ newline\n control\t unicode ☃",
                      secondRouteHighlights = ["</script><script>alert(1)</script>", "\b"]
                    }
              )
      HarchWeb.responseBody (renderApiResponseFromRouteData hostileRouteData)
        `shouldBe` "{\"summary\":\"quote\\\" slash\\\\ newline\\n control\\t unicode ☃\",\"highlights\":[\"</script><script>alert(1)</script>\",\"\\u0008\"]}"

    it "loads second-page content from the database effect when provided" $
      buildPageModelWithDatabase
        ( buildSeededPageRepository
            DatabaseSeed
              { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                englishSecondPageData =
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
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
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
