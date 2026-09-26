{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Route (defaultRequestContext)

spec = do
  describe "defaultDatabaseSeed" $ do
    it "defines deterministic second-page results for both locales" $
      defaultDatabaseSeed
        `shouldBe` DatabaseSeed
          { englishSecondPageData = Right (SecondPageData "Second page content with stubbed data ready for future loaders." []),
            spanishSecondPageData = Right (SecondPageData "Second page content with stubbed data ready for future loaders." [])
          }

    it "keeps seeded database data serializable and stable for tests" $ do
      let secondPageData = SecondPageData "Seeded second" ["One"]
          otherSecondPageData = SecondPageData "Other second" []
          secondError = SecondPageDataError "second unavailable"
          databaseOperation = DatabaseOperation "load-second-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;" Nothing Nothing
          databaseResult = DatabaseResult (Right secondPageData) [databaseOperation]
          seededDatabase = DatabaseSeed (Right secondPageData) (Left secondError)
      secondPageData `shouldNotBe` otherSecondPageData
      secondError `shouldNotBe` SecondPageDataError "other failure"
      databaseOperation `shouldNotBe` databaseOperation {databaseOperationName = "other-operation"}
      databaseResult `shouldNotBe` databaseResult {databaseResultOperations = []}
      seededDatabase `shouldNotBe` seededDatabase {spanishSecondPageData = Right otherSecondPageData}
      show secondPageData `shouldBe` "SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}"
      show secondError `shouldBe` "SecondPageDataError \"second unavailable\""
      show databaseOperation `shouldBe` "DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}"
      show databaseResult `shouldBe` "DatabaseResult {databaseResultValue = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}"
      show seededDatabase
        `shouldBe` ( "DatabaseSeed {englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}),"
                       ++ " spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}"
                   )
      show [secondPageData] `shouldBe` "[SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}]"
      show [secondError] `shouldBe` "[SecondPageDataError \"second unavailable\"]"
      show [databaseResult] `shouldBe` "[DatabaseResult {databaseResultValue = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-second-page-summary\", databaseQueryTemplate = \"SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;\"}]}]"
      show [seededDatabase] `shouldBe` "[DatabaseSeed {englishSecondPageData = Right (SecondPageData {secondPageDataSummary = \"Seeded second\", secondPageDataHighlights = [\"One\"]}), spanishSecondPageData = Left (SecondPageDataError \"second unavailable\")}]"
      showsPrec 11 secondPageData "" `shouldBe` "(" <> show secondPageData <> ")"
      showsPrec 11 secondError "" `shouldBe` "(" <> show secondError <> ")"
      showsPrec 11 databaseResult "" `shouldBe` "(" <> show databaseResult <> ")"
      showsPrec 11 seededDatabase "" `shouldBe` "(" <> show seededDatabase <> ")"

  describe "buildSeededPageRepository" $ do
    it "loads seeded second-page data for both English and Spanish requests" $ do
      let repository = buildSeededPageRepository defaultDatabaseSeed
          expected = Right (SecondPageData "Second page content with stubbed data ready for future loaders." [])
      loadSecondPageValueForRequest repository defaultRequestContext `shouldReturn` expected
      loadSecondPageForRequest repository spanishRequestContext
        `shouldReturn` DatabaseResult expected []

    it "preserves explicit locale-specific failures" $ do
      let repository =
            buildSeededPageRepository
              DatabaseSeed
                { englishSecondPageData = Right (SecondPageData "Second seed" ["Known branch"]),
                  spanishSecondPageData = Left (SecondPageDataError "second seed unavailable")
                }
      loadSecondPageValueForRequest repository spanishRequestContext
        `shouldReturn` Left (SecondPageDataError "second seed unavailable")

    it "keeps the default seeded interpreter deterministic for repeated requests" $ do
      firstResult <- loadSecondPageValueForRequest defaultPageRepository defaultRequestContext
      secondResult <- loadSecondPageValueForRequest defaultPageRepository defaultRequestContext
      firstResult `shouldBe` secondResult
