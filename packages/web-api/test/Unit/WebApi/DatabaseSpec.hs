{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), HomePageData (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Route (defaultRequestContext)

spec = do
  describe "defaultDatabaseSeed" $ do
    it "defines deterministic page-facing seeded results for both locales" $
      defaultDatabaseSeed
        `shouldBe` DatabaseSeed
          { englishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            spanishHomePageData =
              Right
                HomePageData
                  { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
                  },
            englishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            spanishSecondPageData =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  }
          }

    it "keeps seeded database data serializable and stable for tests" $ do
      let homePageData = HomePageData {homePageDataSummary = "Seeded home"}
          otherHomePageData = HomePageData {homePageDataSummary = "Different home"}
          secondPageData =
            SecondPageData
              { secondPageDataSummary = "Seeded second",
                secondPageDataHighlights = ["One"]
              }
          otherSecondPageData =
            SecondPageData
              { secondPageDataSummary = "Other second",
                secondPageDataHighlights = []
              }
          homeError = HomePageDataError "home unavailable"
          secondError = SecondPageDataError "second unavailable"
          databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          databaseResult =
            DatabaseResult
              { databaseResultValue = Right homePageData,
                databaseResultOperations = [databaseOperation]
              }
          seededDatabase =
            DatabaseSeed
              { englishHomePageData = Right homePageData,
                spanishHomePageData = Left homeError,
                englishSecondPageData = Right secondPageData,
                spanishSecondPageData = Left secondError
              }
      homePageData `shouldBe` homePageData
      homePageData `shouldNotBe` otherHomePageData
      secondPageData `shouldBe` secondPageData
      secondPageData `shouldNotBe` otherSecondPageData
      homeError `shouldBe` homeError
      homeError `shouldNotBe` secondError
      databaseOperation `shouldBe` databaseOperation
      databaseOperation `shouldNotBe` databaseOperation {databaseOperationName = "load-home-page-summary"}
      databaseResult `shouldBe` databaseResult
      databaseResult
        `shouldNotBe` databaseResult
          { databaseResultOperations = []
          }
      seededDatabase `shouldBe` seededDatabase
      seededDatabase
        `shouldNotBe` seededDatabase
          { spanishSecondPageData = Right otherSecondPageData
          }
      show (HomePageData {homePageDataSummary = "Seeded home"})
        `shouldBe` "HomePageData {homePageDataSummary = \"Seeded home\"}"
      show (SecondPageData {secondPageDataSummary = "Seeded second", secondPageDataHighlights = ["One"]})
        `shouldBe` "SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}"
      show (HomePageDataError "home unavailable")
        `shouldBe` "HomePageDataError \"home unavailable\""
      show (SecondPageDataError "second unavailable")
        `shouldBe` "SecondPageDataError \"second unavailable\""
      show databaseOperation
        `shouldBe` "DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}"
      show databaseResult
        `shouldBe` "DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show seededDatabase
        `shouldBe` "DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), spanishHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}"
      show [HomePageData {homePageDataSummary = "Seeded home"}]
        `shouldBe` "[HomePageData {homePageDataSummary = \"Seeded home\"}]"
      show [homeError, secondError]
        `shouldBe` "[HomePageDataError \"home unavailable\",SecondPageDataError \"second unavailable\"]"
      show [databaseOperation]
        `shouldBe` "[DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]"
      show [databaseResult]
        `shouldBe` "[DatabaseResult {databaseResultValue = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      show
        [ SecondPageData
            { secondPageDataSummary = "Seeded second",
              secondPageDataHighlights = ["One"]
            }
        ]
        `shouldBe` "[SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}]"
      show [seededDatabase]
        `shouldBe` "[DatabaseSeed {englishHomePageData = Right (HomePageData {homePageDataSummary = \"Seeded home\"}), spanishHomePageData = Left (HomePageDataError \"home unavailable\"), englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}]"

  describe "buildSeededPageRepository" $ do
    it "loads page-oriented seeded data for both English and Spanish requests" $ do
      let englishEffect = buildSeededPageRepository defaultDatabaseSeed
      loadHomePageForRequest englishEffect defaultRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                HomePageData
                  { homePageDataSummary = "Server-rendered home page with stubbed content."
                  },
            databaseResultOperations = []
          }
      loadHomePageValueForRequest englishEffect defaultRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            }
      loadSecondPageValueForRequest englishEffect defaultRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadHomePageValueForRequest englishEffect spanishRequestContext
        `shouldReturn` Right
          HomePageData
            { homePageDataSummary = "Inicio renderizado en el servidor con datos de desarrollo preconfigurados."
            }
      loadSecondPageValueForRequest englishEffect spanishRequestContext
        `shouldReturn` Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
      loadSecondPageForRequest englishEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue =
              Right
                SecondPageData
                  { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
                    secondPageDataHighlights = []
                  },
            databaseResultOperations = []
          }

    it "returns explicit seeded errors without collapsing page-specific failures" $ do
      let seededEffect =
            buildSeededPageRepository
              DatabaseSeed
                { englishHomePageData = Left (HomePageDataError "home seed unavailable"),
                  spanishHomePageData =
                    Right
                      HomePageData
                        { homePageDataSummary = "Inicio sembrado"
                        },
                  englishSecondPageData =
                    Right
                      SecondPageData
                        { secondPageDataSummary = "Second seed",
                          secondPageDataHighlights = ["Known branch"]
                        },
                  spanishSecondPageData = Left (SecondPageDataError "second seed unavailable")
                }
      loadHomePageValueForRequest seededEffect defaultRequestContext
        `shouldReturn` Left (HomePageDataError "home seed unavailable")
      loadSecondPageValueForRequest seededEffect spanishRequestContext
        `shouldReturn` Left (SecondPageDataError "second seed unavailable")
      loadSecondPageForRequest seededEffect spanishRequestContext
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left (SecondPageDataError "second seed unavailable"),
            databaseResultOperations = []
          }

    it "keeps the default seeded interpreter deterministic for repeated requests" $ do
      firstHome <- loadHomePageValueForRequest defaultPageRepository defaultRequestContext
      secondHome <- loadHomePageValueForRequest defaultPageRepository defaultRequestContext
      firstHome `shouldBe` secondHome
      firstSecond <- loadSecondPageValueForRequest defaultPageRepository spanishRequestContext
      secondSecond <- loadSecondPageValueForRequest defaultPageRepository spanishRequestContext
      firstSecond `shouldBe` secondSecond
