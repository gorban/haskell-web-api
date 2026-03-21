{-# LANGUAGE OverloadedStrings #-}

module WebApi.Database
  ( DatabaseEffect (..),
    DatabaseError (..),
    DatabaseSeed (..),
    HomePageData (..),
    SecondPageData (..),
    buildSeededDatabaseEffect,
    defaultDatabaseEffect,
    defaultDatabaseSeed,
  )
where

import Data.Text (Text)
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

data DatabaseSeed = DatabaseSeed
  { englishHomePageData :: Either DatabaseError HomePageData,
    frenchHomePageData :: Either DatabaseError HomePageData,
    englishSecondPageData :: Either DatabaseError SecondPageData,
    frenchSecondPageData :: Either DatabaseError SecondPageData
  }
  deriving (Eq, Show)

data DatabaseEffect = DatabaseEffect
  { loadHomePageData :: AppRequestContext -> IO (Either DatabaseError HomePageData),
    loadSecondPageData :: AppRequestContext -> IO (Either DatabaseError SecondPageData)
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
    { loadHomePageData =
        \requestContext ->
          pure $
            case requestLocale requestContext of
              English -> englishHomePageData seed
              French -> frenchHomePageData seed,
      loadSecondPageData =
        \requestContext ->
          pure $
            case requestLocale requestContext of
              English -> englishSecondPageData seed
              French -> frenchSecondPageData seed
    }
