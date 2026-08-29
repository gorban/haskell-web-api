{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseOperation (..), DatabaseSeed (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed)
import WebApi.RouteData (RouteDataResult (..), RouteDataSelection (..), SecondRouteData (..), selectRouteData, selectRouteDataSelectionWithDatabase, selectRouteDataWithDatabase)

spec = do
  describe "selectRouteData" $ do
    it "selects second-route domain data for the rendered page" $ do
      let seededDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Shared domain summary.",
                          secondPageDataHighlights = ["Shared loader", "Shared renderer"]
                        },
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectedRouteData <- selectRouteDataWithDatabase seededDatabaseEffect secondRequest
      selectedRouteData
        `shouldBe` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Shared domain summary.",
                  secondRouteHighlights = ["Shared loader", "Shared renderer"]
                }
          )

    it "keeps route-data selections deterministic while exposing database operations separately" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          routeDataSelection =
            RouteDataSelection
              { routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = "Shared domain summary", secondRouteHighlights = []})),
                routeDataDatabaseOperations = [databaseOperation]
              }
      routeDataSelection
        `shouldNotBe` routeDataSelection
          { routeDataDatabaseOperations = []
          }
      show routeDataSelection
        `shouldBe` "RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show [routeDataSelection]
        `shouldBe` "[RouteDataSelection {routeDataResult = SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = []})), routeDataDatabaseOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      selectRouteDataSelectionWithDatabase (buildSeededPageRepository defaultDatabaseSeed) secondRequest
        `shouldReturn` RouteDataSelection
          { routeDataResult =
              SecondRouteDataResult
                ( Right
                    SecondRouteData
                      { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                        secondRouteHighlights = []
                      }
                ),
            routeDataDatabaseOperations = []
          }

    it "keeps route-data selectors and derived instances deterministic for tests" $ do
      let secondRouteData =
            SecondRouteData
              { secondRouteSummary = "Shared domain summary",
                secondRouteHighlights = ["Shared loader"]
              }
          routeDataResult = SecondRouteDataResult (Right secondRouteData)
      secondRouteSummary secondRouteData `shouldBe` "Shared domain summary"
      secondRouteHighlights secondRouteData `shouldBe` ["Shared loader"]
      secondRouteData `shouldNotBe` secondRouteData {secondRouteHighlights = []}
      routeDataResult `shouldNotBe` NotFoundRouteDataResult
      show secondRouteData
        `shouldBe` "SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}"
      show routeDataResult
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show (SecondRouteDataResult (Right secondRouteData))
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show SpacesRouteDataResult `shouldBe` "SpacesRouteDataResult"
      show [secondRouteData]
        `shouldBe` "[SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}]"
      show [SpacesRouteDataResult] `shouldBe` "[SpacesRouteDataResult]"
      show [NotFoundRouteDataResult] `shouldBe` "[NotFoundRouteDataResult]"

    it "selects default stubbed route data without extra wiring" $ do
      selectRouteData secondRequest
        `shouldReturn` SecondRouteDataResult
          ( Right
              SecondRouteData
                { secondRouteSummary = "Second page content with stubbed data ready for future loaders.",
                  secondRouteHighlights = []
                }
          )
      selectRouteData spacesRequest `shouldReturn` SpacesRouteDataResult
      selectRouteDataSelectionWithDatabase (buildSeededPageRepository defaultDatabaseSeed) spacesRequest
        `shouldReturn` RouteDataSelection SpacesRouteDataResult []
      selectRouteData apiNotFoundRequest `shouldReturn` NotFoundRouteDataResult
