{-# LANGUAGE OverloadedStrings #-}

module WebApi.Database
  ( DatabaseEffect (..),
    DatabaseError (..),
    DatabaseOperation (..),
    DatabaseResult (..),
    DatabaseSeed (..),
    HomePageData (..),
    SecondPageData (..),
    buildSeededDatabaseEffect,
    defaultDatabaseEffect,
    defaultDatabaseSeed,
  )
where

import Data.Text (Text)
import Data.Word (Word64)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
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
    frenchHomePageData :: Either DatabaseError HomePageData,
    englishSecondPageData :: Either DatabaseError SecondPageData,
    frenchSecondPageData :: Either DatabaseError SecondPageData
  }
  deriving (Eq, Show)

data DatabaseEffect = DatabaseEffect
  { loadHomePageData :: AppRequestContext -> IO (Either DatabaseError HomePageData),
    loadHomePageDataWithObservability :: AppRequestContext -> IO (DatabaseResult HomePageData),
    loadSecondPageData :: AppRequestContext -> IO (Either DatabaseError SecondPageData),
    loadSecondPageDataWithObservability :: AppRequestContext -> IO (DatabaseResult SecondPageData)
  }

defaultDatabaseSeed :: DatabaseSeed
defaultDatabaseSeed =
  DatabaseSeed
    { englishHomePageData =
        Right
          HomePageData
            { homePageDataSummary = "Server-rendered home page with stubbed content."
            },
      frenchHomePageData =
        Right
          HomePageData
            { homePageDataSummary = "Accueil cote serveur avec des donnees de developpement preconfigurees."
            },
      englishSecondPageData =
        Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            },
      frenchSecondPageData =
        Right
          SecondPageData
            { secondPageDataSummary = "Second page content with stubbed data ready for future loaders.",
              secondPageDataHighlights = []
            }
    }

defaultDatabaseEffect :: DatabaseEffect
defaultDatabaseEffect = buildSeededDatabaseEffect defaultDatabaseSeed

buildSeededDatabaseEffect :: DatabaseSeed -> DatabaseEffect
buildSeededDatabaseEffect seed =
  DatabaseEffect
    { loadHomePageData = fmap databaseResultValue . loadSeededHomePageData,
      loadHomePageDataWithObservability = loadSeededHomePageData,
      loadSecondPageData = fmap databaseResultValue . loadSeededSecondPageData,
      loadSecondPageDataWithObservability = loadSeededSecondPageData
    }
  where
    loadSeededHomePageData requestContext =
      pure $
        DatabaseResult
          { databaseResultValue =
              case requestLocale requestContext of
                English -> englishHomePageData seed
                French -> frenchHomePageData seed,
            databaseResultOperations = []
          }
    loadSeededSecondPageData requestContext =
      pure $
        DatabaseResult
          { databaseResultValue =
              case requestLocale requestContext of
                English -> englishSecondPageData seed
                French -> frenchSecondPageData seed,
            databaseResultOperations = []
          }
