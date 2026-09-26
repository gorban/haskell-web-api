{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import App.Localization
import HarchWeb qualified
import Test.Hspec

main :: IO ()
main = hspec $ describe "Unit.localization example" $ do
  it "renders Icelandic HTML through ICU plural categories" $
    renderFavoritesPage Icelandic 11
      `shouldBe` Right "<main lang=\"is\"><h1>Uppahaldsstadir</h1><p data-favorite-count=\"true\">11 hlutir</p></main>"

  it "renders the translated HTML fields in every catalog locale" $ do
    renderFavoritesPage English 1
      `shouldBe` Right "<main lang=\"en\"><h1>Favorite places</h1><p data-favorite-count=\"true\">1 favorite</p></main>"
    renderFavoritesPage Spanish 2
      `shouldBe` Right "<main lang=\"es\"><h1>Lugares favoritos</h1><p data-favorite-count=\"true\">2 favoritos</p></main>"

  it "turns its in-memory adapter failure into a localized API message" $ do
    database <- newFavoritesDatabase ["Haskell cafe"]
    firstSave <- saveFavorite database "Icelandic cafe"
    duplicateSave <- saveFavorite database "Icelandic cafe"
    existingSave <- saveFavorite database "Haskell cafe"
    case firstSave of
      Right () -> pure ()
      Left failure -> expectationFailure (show failure)
    case duplicateSave of
      Left FavoriteAlreadyExists -> pure ()
      Right () -> expectationFailure "expected the duplicate adapter error"
    case existingSave of
      Left FavoriteAlreadyExists -> pure ()
      Right () -> expectationFailure "expected the existing adapter error"
    localizedApiError English FavoriteAlreadyExists `shouldBe` Right "That favorite already exists."
    localizedApiError Spanish FavoriteAlreadyExists `shouldBe` Right "Ese favorito ya existe."
    localizedApiError Icelandic FavoriteAlreadyExists `shouldBe` Right "Uppahaldsid er thegar til."

  it "keeps an unknown catalog locale on the framework lookup-failure rail" $
    HarchWeb.renderLocalizedMessage exampleLocalizer FavoritesTitle (HarchWeb.locale "fr") (HarchWeb.messageArguments [])
      `shouldBe` Left HarchWeb.MessageNotFound

  it "exercises the example's closed values" $ do
    English `shouldNotBe` Spanish
    show Icelandic `shouldBe` "Icelandic"
    shows Icelandic "!" `shouldBe` "Icelandic!"
    showList [Icelandic] "" `shouldBe` "[Icelandic]"
    show FavoriteAlreadyExists `shouldBe` "FavoriteAlreadyExists"
    shows FavoriteAlreadyExists "!" `shouldBe` "FavoriteAlreadyExists!"
    showList [FavoriteAlreadyExists] "" `shouldBe` "[FavoriteAlreadyExists]"
