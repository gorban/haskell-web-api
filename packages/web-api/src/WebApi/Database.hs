{-# LANGUAGE OverloadedStrings #-}

module WebApi.Database
  ( PageRepository (..),
    DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    DatabaseSeed (..),
    SecondPageData (..),
    buildSeededPageRepository,
    defaultPageRepository,
    defaultDatabaseSeed,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import WebApi.Route
  ( AppLocale (..),
  )

newtype DatabaseError = SecondPageDataError Text
  deriving (Eq, Show)

data SecondPageData = SecondPageData
  { secondPageDataSummary :: Text,
    secondPageDataHighlights :: [Text]
  }
  deriving (Eq, Show)

data DatabaseOperation = DatabaseOperation
  { databaseOperationName :: Text,
    databaseQueryTemplate :: Text,
    databaseOperationStartedAtNanoseconds :: Maybe Word64,
    databaseOperationEndedAtNanoseconds :: Maybe Word64
  }

instance Eq DatabaseOperation where
  left == right =
    databaseOperationName left == databaseOperationName right
      && databaseQueryTemplate left == databaseQueryTemplate right

instance Show DatabaseOperation where
  showsPrec precedence databaseOperation =
    showParen (precedence > 10) $
      showString "DatabaseOperation {databaseOperationName = "
        . shows (databaseOperationName databaseOperation)
        . showString ", databaseQueryTemplate = "
        . shows (databaseQueryTemplate databaseOperation)
        . showString "}"

data DatabaseResult a = DatabaseResult
  { databaseResultValue :: Either DatabaseError a,
    databaseResultOperations :: [DatabaseOperation]
  }
  deriving (Eq, Show)

data DatabaseSeed = DatabaseSeed
  { englishSecondPageData :: Either DatabaseError SecondPageData,
    spanishSecondPageData :: Either DatabaseError SecondPageData
  }
  deriving (Eq, Show)

newtype PageRepository = PageRepository
  { loadSecondPage :: AppLocale -> IO (DatabaseResult SecondPageData)
  }

defaultDatabaseSeed :: DatabaseSeed
defaultDatabaseSeed =
  DatabaseSeed
    { englishSecondPageData =
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

defaultPageRepository :: PageRepository
defaultPageRepository = buildSeededPageRepository defaultDatabaseSeed

buildSeededPageRepository :: DatabaseSeed -> PageRepository
buildSeededPageRepository seed =
  PageRepository
    { loadSecondPage = loadSeededSecondPage
    }
  where
    loadSeededSecondPage locale =
      pure $
        DatabaseResult
          { databaseResultValue =
              case locale of
                English -> englishSecondPageData seed
                Spanish -> spanishSecondPageData seed,
            databaseResultOperations = []
          }
