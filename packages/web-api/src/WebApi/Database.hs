{-# LANGUAGE OverloadedStrings #-}

module WebApi.Database
  ( PageRepository (..),
    DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    DatabaseSeed (..),
    HomePageData (..),
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

data DatabaseError
  = HomePageDataError Text
  | SecondPageDataError Text
  deriving (Eq)

instance Show DatabaseError where
  showsPrec precedence databaseError =
    showParen (precedence > 10) $
      case databaseError of
        HomePageDataError message -> showString "HomePageDataError " . shows message
        SecondPageDataError message -> showString "SecondPageDataError " . shows message

newtype HomePageData = HomePageData
  { homePageDataSummary :: Text
  }
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
  { englishHomePageData :: Either DatabaseError HomePageData,
    spanishHomePageData :: Either DatabaseError HomePageData,
    englishSecondPageData :: Either DatabaseError SecondPageData,
    spanishSecondPageData :: Either DatabaseError SecondPageData
  }
  deriving (Eq, Show)

data PageRepository = PageRepository
  { loadHomePage :: AppLocale -> IO (DatabaseResult HomePageData),
    loadSecondPage :: AppLocale -> IO (DatabaseResult SecondPageData)
  }

defaultDatabaseSeed :: DatabaseSeed
defaultDatabaseSeed =
  DatabaseSeed
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

defaultPageRepository :: PageRepository
defaultPageRepository = buildSeededPageRepository defaultDatabaseSeed

buildSeededPageRepository :: DatabaseSeed -> PageRepository
buildSeededPageRepository seed =
  PageRepository
    { loadHomePage = loadSeededHomePage,
      loadSecondPage = loadSeededSecondPage
    }
  where
    loadSeededHomePage locale =
      pure $
        DatabaseResult
          { databaseResultValue =
              case locale of
                English -> englishHomePageData seed
                Spanish -> spanishHomePageData seed,
            databaseResultOperations = []
          }
    loadSeededSecondPage locale =
      pure $
        DatabaseResult
          { databaseResultValue =
              case locale of
                English -> englishSecondPageData seed
                Spanish -> spanishSecondPageData seed,
            databaseResultOperations = []
          }
