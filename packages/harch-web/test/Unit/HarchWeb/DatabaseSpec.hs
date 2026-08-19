{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Unit.HarchWeb.DatabaseSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import HarchWeb.Database
import Test.Hspec
import TestCore.CustomAssertions (expectAll)

data SampleDatabaseRequest result where
  LoadDisplayName :: SampleDatabaseRequest Text
  LoadLoginCount :: SampleDatabaseRequest Int

data SampleDatabaseError
  = DisplayNameUnavailable
  | LoginCountUnavailable
  deriving (Eq, Show)

sampleDatabaseEffect :: DatabaseEffect SampleDatabaseError SampleDatabaseRequest
sampleDatabaseEffect =
  DatabaseEffect $ \case
    LoadDisplayName ->
      pure $
        DatabaseResult
          { databaseResultValue = Right "Ada",
            databaseResultOperations = [displayNameOperation]
          }
    LoadLoginCount ->
      pure $
        DatabaseResult
          { databaseResultValue = Left LoginCountUnavailable,
            databaseResultOperations = [loginCountOperation]
          }

displayNameOperation :: DatabaseOperation
displayNameOperation =
  DatabaseOperation
    { databaseOperationSystem = "postgresql",
      databaseOperationName = "load-display-name",
      databaseQueryTemplate = "SELECT display_name FROM account WHERE id = ?;",
      databaseOperationStartedAtNanoseconds = Just 10,
      databaseOperationEndedAtNanoseconds = Just 20
    }

loginCountOperation :: DatabaseOperation
loginCountOperation =
  DatabaseOperation
    { databaseOperationSystem = "postgresql",
      databaseOperationName = "load-login-count",
      databaseQueryTemplate = "SELECT login_count FROM account WHERE id = ?;",
      databaseOperationStartedAtNanoseconds = Nothing,
      databaseOperationEndedAtNanoseconds = Nothing
    }

spec :: Spec
spec = do
  describe "DatabaseEffect" $ do
    it "preserves each operation's typed result while exposing performed operations" $ do
      runDatabaseEffect sampleDatabaseEffect LoadDisplayName
        `shouldReturn` DatabaseResult
          { databaseResultValue = Right "Ada",
            databaseResultOperations = [displayNameOperation]
          }
      runDatabaseEffect sampleDatabaseEffect LoadLoginCount
        `shouldReturn` DatabaseResult
          { databaseResultValue = Left LoginCountUnavailable,
            databaseResultOperations = [loginCountOperation]
          }

  describe "DatabaseResult" $ do
    it "builds operation-free success and failure results without prescribing domain errors" $ do
      let successfulResult :: DatabaseResult SampleDatabaseError Text
          successfulResult = databaseSuccess "Ada"
          failedResult :: DatabaseResult SampleDatabaseError Text
          failedResult = databaseFailure DisplayNameUnavailable
      expectAll
        ( ( successfulResult
              `shouldBe` DatabaseResult
                { databaseResultValue = Right "Ada",
                  databaseResultOperations = []
                }
          )
            :| [ failedResult
                   `shouldBe` DatabaseResult
                     { databaseResultValue = Left DisplayNameUnavailable,
                       databaseResultOperations = []
                     }
               ]
        )

    it "covers result selectors and derived instances" $ do
      let result :: DatabaseResult SampleDatabaseError Int
          result =
            DatabaseResult
              { databaseResultValue = Right (7 :: Int),
                databaseResultOperations = [loginCountOperation]
              }
          otherResult :: DatabaseResult SampleDatabaseError Int
          otherResult =
            DatabaseResult
              { databaseResultValue = Left LoginCountUnavailable,
                databaseResultOperations = []
              }
      expectAll
        ( (databaseResultValue result `shouldBe` Right 7)
            :| [ databaseResultOperations result `shouldBe` [loginCountOperation],
                 result == result `shouldBe` True,
                 result /= otherResult `shouldBe` True,
                 show result `shouldBe` "DatabaseResult {databaseResultValue = Right 7, databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-login-count\", databaseOperationSystem = \"postgresql\", databaseQueryTemplate = \"SELECT login_count FROM account WHERE id = ?;\"}]}",
                 show [result] `shouldBe` "[DatabaseResult {databaseResultValue = Right 7, databaseResultOperations = [DatabaseOperation {databaseOperationName = \"load-login-count\", databaseOperationSystem = \"postgresql\", databaseQueryTemplate = \"SELECT login_count FROM account WHERE id = ?;\"}]}]"
               ]
        )

  describe "DatabaseOperation" $ do
    it "keeps query metadata stable while ignoring volatile timing" $ do
      let sameQueryWithDifferentTiming =
            displayNameOperation
              { databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Just 999
              }
          differentQuery = loginCountOperation
      expectAll
        ( (databaseOperationSystem displayNameOperation `shouldBe` "postgresql")
            :| [ databaseOperationName displayNameOperation `shouldBe` "load-display-name",
                 databaseQueryTemplate displayNameOperation `shouldBe` "SELECT display_name FROM account WHERE id = ?;",
                 databaseOperationStartedAtNanoseconds displayNameOperation `shouldBe` Just 10,
                 databaseOperationEndedAtNanoseconds displayNameOperation `shouldBe` Just 20,
                 displayNameOperation == sameQueryWithDifferentTiming `shouldBe` True,
                 displayNameOperation == differentQuery `shouldBe` False,
                 displayNameOperation /= differentQuery `shouldBe` True,
                 show displayNameOperation `shouldBe` "DatabaseOperation {databaseOperationName = \"load-display-name\", databaseOperationSystem = \"postgresql\", databaseQueryTemplate = \"SELECT display_name FROM account WHERE id = ?;\"}",
                 show [displayNameOperation] `shouldBe` "[DatabaseOperation {databaseOperationName = \"load-display-name\", databaseOperationSystem = \"postgresql\", databaseQueryTemplate = \"SELECT display_name FROM account WHERE id = ?;\"}]"
               ]
        )
