{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.ClientStorage

spec = describe "HarchWeb.ClientStorage" $ do
  it "accepts bounded distinct declarations while preserving storage class" $ do
    let localKey = validKey LocalStorage "draft"
        sessionKey = validKey SessionStorage "draft"
        cleanup = clientStorageCleanup [localKey, sessionKey]
    expectAll
      ( (browserStorageKeyClass localKey `shouldBe` LocalStorage)
          :| [ browserStorageKeyText sessionKey `shouldBe` "draft",
               cleanup `shouldBe` Right (cleanupFromEntries [localKey, sessionKey]),
               clientStorageCleanupEntries noClientStorageCleanup `shouldBe` []
             ]
      )

  it "rejects unsafe authored keys without retaining their text" $
    expectAll
      ( (browserStorageKey LocalStorage "" `shouldBe` Left EmptyBrowserStorageKey)
          :| [ browserStorageKey SessionStorage (replicateText 257) `shouldBe` Left BrowserStorageKeyTooLong
             ]
      )

  it "rejects duplicate declarations and an over-capacity response effect" $ do
    let key = validKey LocalStorage "draft"
        keys = [validKey LocalStorage ("key-" <> showText index) | index <- [1 .. 33 :: Int]]
    expectAll
      ( (clientStorageCleanup [key, key] `shouldBe` Left DuplicateBrowserStorageKey)
          :| [ clientStorageCleanup keys `shouldBe` Left TooManyBrowserStorageKeys
             ]
      )

  it "keeps the public declaration and error values inspectable" $ do
    let localKey = validKey LocalStorage "draft"
        sessionKey = validKey SessionStorage "draft"
        laterSessionKey = validKey SessionStorage "recent-draft"
        cleanup = cleanupFromEntries [localKey, sessionKey]
        laterCleanup = cleanupFromEntries [localKey, laterSessionKey]
    expectAll
      ( (hasDerivedContract [LocalStorage, SessionStorage] `shouldBe` True)
          :| [ hasTwoValueOrder LocalStorage SessionStorage `shouldBe` True,
               hasDerivedContract [localKey, sessionKey, laterSessionKey] `shouldBe` True,
               hasThreeValueOrder localKey sessionKey laterSessionKey `shouldBe` True,
               hasDerivedContract [EmptyBrowserStorageKey, BrowserStorageKeyTooLong] `shouldBe` True,
               hasDerivedContract [noClientStorageCleanup, cleanup, laterCleanup] `shouldBe` True,
               hasDerivedContract [TooManyBrowserStorageKeys, DuplicateBrowserStorageKey] `shouldBe` True
             ]
      )

hasDerivedContract :: (Eq value, Show value) => [value] -> Bool
hasDerivedContract values =
  sum [fromEnum (left == right) | left <- values, right <- values] == length values
    && sum [fromEnum (left /= right) | left <- values, right <- values]
      == length values * (length values - 1)
    && sum [length (show value) + length (showList [value] "") | value <- values] > 0

hasTwoValueOrder :: (Ord value) => value -> value -> Bool
hasTwoValueOrder first second =
  [compare first first, compare first second, compare second first, compare second second]
    == [EQ, LT, GT, EQ]
    && first <= first
    && first < second
    && second > first
    && second >= second
    && max first second == second
    && min first second == first

hasThreeValueOrder :: (Ord value) => value -> value -> value -> Bool
hasThreeValueOrder first second third =
  hasTwoValueOrder first second
    && hasTwoValueOrder second third
    && first < third
    && third > first

validKey :: BrowserStorageClass -> Text -> BrowserStorageKey
validKey storageClass keyText =
  case browserStorageKey storageClass keyText of
    Left errorValue -> error (show errorValue)
    Right key -> key

cleanupFromEntries :: [BrowserStorageKey] -> ClientStorageCleanup
cleanupFromEntries entries =
  case clientStorageCleanup entries of
    Left errorValue -> error (show errorValue)
    Right cleanup -> cleanup

replicateText :: Int -> Text
replicateText lengthValue = Text.replicate lengthValue "x"

showText :: Int -> Text
showText = Text.pack . show
