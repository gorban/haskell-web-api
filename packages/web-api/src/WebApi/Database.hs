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
import Data.Text qualified as Text
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
  { loadHomePageData :: AppRequestContext -> Either DatabaseError HomePageData,
    loadSecondPageData :: AppRequestContext -> Either DatabaseError SecondPageData
  }

defaultDatabaseSeed :: DatabaseSeed
defaultDatabaseSeed =
  DatabaseSeed
    { englishHomePageData =
        Right
          HomePageData
            { homePageDataSummary = Text.pack "Server-rendered home page with seeded development data."
            },
      frenchHomePageData =
        Right
          HomePageData
            { homePageDataSummary = Text.pack "Accueil cote serveur avec des donnees de developpement preconfigurees."
            },
      englishSecondPageData =
        Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Second page content loaded from the seeded database effect.",
              secondPageDataHighlights = [Text.pack "Fast SSR", Text.pack "Stable routes"]
            },
      frenchSecondPageData =
        Right
          SecondPageData
            { secondPageDataSummary = Text.pack "Contenu de la seconde page charge depuis l'effet de base de donnees seedee.",
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
          case requestLocale requestContext of
            English -> englishHomePageData seed
            French -> frenchHomePageData seed,
      loadSecondPageData =
        \requestContext ->
          case requestLocale requestContext of
            English -> englishSecondPageData seed
            French -> frenchSecondPageData seed
    }
