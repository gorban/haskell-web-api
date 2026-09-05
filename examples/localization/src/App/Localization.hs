{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | A small, complete catalog: page markup receives translated fields and an
-- in-memory adapter's typed failure is rendered at an API boundary. Icelandic
-- deliberately proves the ICU plural category path with @11 hlutir@.
module App.Localization
  ( ExampleLocale (..),
    ExampleMessage (..),
    FavoriteSaveError (..),
    FavoritesDatabase,
    exampleLocalizer,
    localizedApiError,
    newFavoritesDatabase,
    renderFavoritesPage,
    saveFavorite,
  )
where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Int (Int64)
import Data.Text (Text)
import HarchWeb qualified
import HarchWeb.Localization.Quasi (message)
import HarchWeb.Markup (harch)

data ExampleLocale = English | Spanish | Icelandic
  deriving (Eq, Show)

data ExampleMessage
  = FavoritesTitle
  | FavoriteCount
  | DuplicateFavorite

data FavoriteSaveError = FavoriteAlreadyExists
  deriving (Show)

newtype FavoritesDatabase = FavoritesDatabase (IORef [Text])

newFavoritesDatabase :: [Text] -> IO FavoritesDatabase
newFavoritesDatabase = fmap FavoritesDatabase . newIORef

-- | The translated heading and ICU count are inserted into ordinary SSR HTML,
-- while the document's @lang@ comes from the same locale value.
renderFavoritesPage :: ExampleLocale -> Int64 -> Either HarchWeb.MessageRenderError Text
renderFavoritesPage requestedLocale count = do
  title <- localized requestedLocale FavoritesTitle []
  favoriteCount <- localized requestedLocale FavoriteCount [("count", HarchWeb.messageNumber count)]
  pure
    ( HarchWeb.renderHtml
        [harch|
          <main lang={localeCode requestedLocale}>
            <h1>{title}</h1>
            <p data-favorite-count="true">{favoriteCount}</p>
          </main>
        |]
    )

saveFavorite :: FavoritesDatabase -> Text -> IO (Either FavoriteSaveError ())
saveFavorite (FavoritesDatabase favoritesReference) favorite =
  atomicModifyIORef' favoritesReference $ \favorites ->
    if favorite `elem` favorites
      then (favorites, Left FavoriteAlreadyExists)
      else (favorites <> [favorite], Right ())

-- | An API handler would return this value as its safe public body while
-- retaining the typed adapter error for its private diagnostics.
localizedApiError :: ExampleLocale -> FavoriteSaveError -> Either HarchWeb.MessageRenderError Text
localizedApiError requestedLocale FavoriteAlreadyExists = localized requestedLocale DuplicateFavorite []

localized :: ExampleLocale -> ExampleMessage -> [(Text, HarchWeb.MessageArgument)] -> Either HarchWeb.MessageRenderError Text
localized requestedLocale messageKey arguments = HarchWeb.renderLocalizedMessage exampleLocalizer messageKey (localeFor requestedLocale) (HarchWeb.messageArguments arguments)

exampleLocalizer :: HarchWeb.Localizer ExampleMessage
exampleLocalizer =
  HarchWeb.localizer $ \messageKey requestedLocale ->
    case HarchWeb.localeText requestedLocale of
      "en" -> Just (templateFor messageKey English)
      "es" -> Just (templateFor messageKey Spanish)
      "is" -> Just (templateFor messageKey Icelandic)
      _ -> Nothing

localeFor :: ExampleLocale -> HarchWeb.Locale
localeFor requestedLocale =
  HarchWeb.locale $
    case requestedLocale of
      English -> "en"
      Spanish -> "es"
      Icelandic -> "is"

localeCode :: ExampleLocale -> Text
localeCode = HarchWeb.localeText . localeFor

templateFor :: ExampleMessage -> ExampleLocale -> HarchWeb.MessageTemplate
templateFor messageKey requestedLocale =
  case (messageKey, requestedLocale) of
    (FavoritesTitle, English) -> [message|Favorite places|]
    (FavoritesTitle, Spanish) -> [message|Lugares favoritos|]
    (FavoritesTitle, Icelandic) -> [message|Uppahaldsstadir|]
    (FavoriteCount, English) -> [message|{count, plural, one {# favorite} other {# favorites}}|]
    (FavoriteCount, Spanish) -> [message|{count, plural, one {# favorito} other {# favoritos}}|]
    (FavoriteCount, Icelandic) -> [message|{count, plural, one {# hlutur} other {# hlutir}}|]
    (DuplicateFavorite, English) -> [message|That favorite already exists.|]
    (DuplicateFavorite, Spanish) -> [message|Ese favorito ya existe.|]
    (DuplicateFavorite, Icelandic) -> [message|Uppahaldsid er thegar til.|]
