{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), HomePageData (..), PageRepository (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Route (AppLocale (..))
import WebApi.RouteData (HomeRouteData (..), RouteDataResult (..), RouteDataSelection (..), SecondRouteData (..), StatusApiData (..), selectRouteData, selectRouteDataSelectionWithDatabase, selectRouteDataWithDatabase)

spec = do
  describe "selectRouteData" $ do
    it "selects the same second-route domain data for page and API route families" $ do
      let seededDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = englishHomePageData defaultDatabaseSeed,
                  spanishHomePageData = spanishHomePageData defaultDatabaseSeed,
                  englishSecondPageData =
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
      selectRouteDataWithDatabase seededDatabaseEffect apiSecondRequest `shouldReturn` selectedRouteData

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
      routeDataSelection `shouldBe` routeDataSelection
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

    it "loads home-route data from the database effect and preserves explicit failures" $ do
      let seededDatabaseEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Loaded from the seeded database effect."
                        },
                  spanishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  englishSecondPageData = englishSecondPageData defaultDatabaseSeed,
                  spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                }
      selectRouteDataWithDatabase seededDatabaseEffect homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Loaded from the seeded database effect."
                }
          )
      selectRouteDataWithDatabase seededDatabaseEffect spanishHomeRequest
        `shouldReturn` HomeRouteDataResult
          (Left (HomePageDataError "home seed unavailable"))

    it "preserves home-route database operations alongside selected data" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-home-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          observedHomeEffect =
            defaultPageRepository
              { loadHomePage =
                  \_ ->
                    pure
                      DatabaseResult
                        { databaseResultValue = Right (HomePageData "Observed home summary."),
                          databaseResultOperations = [databaseOperation]
                        }
              }
      selectRouteDataSelectionWithDatabase observedHomeEffect homeRequest
        `shouldReturn` RouteDataSelection
          { routeDataResult = HomeRouteDataResult (Right (HomeRouteData "Observed home summary.")),
            routeDataDatabaseOperations = [databaseOperation]
          }

    it "keeps route-data selectors and derived instances deterministic for tests" $ do
      let homeRouteData =
            HomeRouteData
              { homeRouteSummary = "Stubbed home summary"
              }
          otherHomeRouteData =
            HomeRouteData
              { homeRouteSummary = "Different home summary"
              }
          secondRouteData =
            SecondRouteData
              { secondRouteSummary = "Shared domain summary",
                secondRouteHighlights = ["Shared loader"]
              }
          statusApiData =
            StatusApiData
              { statusApiLocale = Spanish
              }
          routeDataResult = HomeRouteDataResult (Right homeRouteData)
      homeRouteSummary homeRouteData `shouldBe` "Stubbed home summary"
      secondRouteSummary secondRouteData `shouldBe` "Shared domain summary"
      secondRouteHighlights secondRouteData `shouldBe` ["Shared loader"]
      statusApiLocale statusApiData `shouldBe` Spanish
      homeRouteData `shouldBe` homeRouteData
      homeRouteData `shouldNotBe` otherHomeRouteData
      secondRouteData `shouldNotBe` secondRouteData {secondRouteHighlights = []}
      statusApiData `shouldBe` statusApiData
      statusApiData `shouldNotBe` StatusApiData {statusApiLocale = English}
      routeDataResult `shouldBe` routeDataResult
      routeDataResult `shouldNotBe` NotFoundRouteDataResult
      show homeRouteData `shouldBe` "HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}"
      show secondRouteData
        `shouldBe` "SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}"
      show statusApiData `shouldBe` "StatusApiData {statusApiLocale = Spanish}"
      show routeDataResult
        `shouldBe` "HomeRouteDataResult (Right (HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}))"
      show (SecondRouteDataResult (Right secondRouteData))
        `shouldBe` "SecondRouteDataResult (Right (SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}))"
      show (StatusApiDataResult statusApiData)
        `shouldBe` "StatusApiDataResult (StatusApiData {statusApiLocale = Spanish})"
      show SpacesRouteDataResult `shouldBe` "SpacesRouteDataResult"
      show [homeRouteData] `shouldBe` "[HomeRouteData {homeRouteSummary = \"Stubbed home summary\"}]"
      show [secondRouteData]
        `shouldBe` "[SecondRouteData {secondRouteSummary = \"Shared domain summary\", secondRouteHighlights = [\"Shared loader\"]}]"
      show [statusApiData] `shouldBe` "[StatusApiData {statusApiLocale = Spanish}]"
      show [SpacesRouteDataResult] `shouldBe` "[SpacesRouteDataResult]"
      show [NotFoundRouteDataResult] `shouldBe` "[NotFoundRouteDataResult]"

    it "selects default stubbed and status route data without extra wiring" $ do
      selectRouteData homeRequest
        `shouldReturn` HomeRouteDataResult
          ( Right
              HomeRouteData
                { homeRouteSummary = "Server-rendered home page with stubbed content."
                }
          )
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
      selectRouteData spanishApiStatusRequest
        `shouldReturn` StatusApiDataResult
          StatusApiData
            { statusApiLocale = Spanish
            }
      selectRouteData apiNotFoundRequest `shouldReturn` NotFoundRouteDataResult
