{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# SPEC #-}

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text qualified as Text
import HarchWeb.Localization
import HarchWeb.Localization.Internal (decodeRenderedIcuUtf8)
import HarchWeb.Localization.Quasi (message, validateMessageTemplate)

data TestMessage = ItemCount

spec =
  describe "ICU localization" $ do
    it "selects Icelandic plural categories through an application-provided lookup" $ do
      let catalog messageKey requestedLocale =
            case (messageKey, localeText requestedLocale) of
              (ItemCount, "is") -> Just (messageTemplate "{count, plural, one {# hlutur} other {# hlutir}}")
              _ -> Nothing
      let rendered = renderLocalizedMessage (localizer catalog) ItemCount (locale "is") (messageArguments [("count", messageNumber (11 :: Int64))])
      rendered `shouldBe` Right "11 hlutir"

    it "keeps an unknown application key on the explicit lookup-failure rail" $ do
      let rendered = renderLocalizedMessage (localizer (\_ _ -> Nothing)) ItemCount (locale "en") (messageArguments [])
      rendered `shouldBe` Left MessageNotFound

    it "provides an extendable empty default catalog while HarchWeb owns no end-user copy" $
      renderLocalizedMessage defaultLocalizer NoDefaultMessage (locale "en") (messageArguments [])
        `shouldBe` Left MessageNotFound

    it "formats named text and returns an explicit error for an invalid ICU template" $ do
      let catalog messageKey requestedLocale =
            case (messageKey, localeText requestedLocale) of
              (ItemCount, "en") -> Just (messageTemplate "Hello, {name}!")
              (ItemCount, "invalid") -> Just (messageTemplate "{count, plural")
              _ -> Nothing
      let rendered = renderLocalizedMessage (localizer catalog) ItemCount (locale "en") (messageArguments [("name", messageText "Ada")])
          malformed = renderLocalizedMessage (localizer catalog) ItemCount (locale "invalid") (messageArguments [])
      expectAll
        ( (rendered `shouldBe` Right "Hello, Ada!")
            :| [ malformed `shouldBe` Left MessageFormatRejected,
                 locale "en" `shouldNotBe` locale "is",
                 locale "en" < locale "is" `shouldBe` True,
                 locale "en" <= locale "is" `shouldBe` True,
                 locale "is" > locale "en" `shouldBe` True,
                 locale "is" >= locale "en" `shouldBe` True,
                 compare (locale "en") (locale "en") `shouldBe` EQ,
                 min (locale "en") (locale "is") `shouldBe` locale "en",
                 max (locale "en") (locale "is") `shouldBe` locale "is",
                 compare (locale "is") (locale "en") `shouldBe` GT,
                 show (locale "en") `shouldBe` "Locale \"en\"",
                 showsPrec 11 (locale "en") "" `shouldBe` "(Locale \"en\")",
                 showList [locale "en"] "" `shouldBe` "[Locale \"en\"]",
                 messageText "Ada" `shouldNotBe` messageText "Grace",
                 messageText "Ada" `shouldNotBe` messageNumber 2,
                 messageNumber (2 :: Int64) `shouldNotBe` messageText "Ada",
                 show (messageText "Ada") `shouldBe` "MessageText \"Ada\"",
                 show (messageNumber (2 :: Int64)) `shouldBe` "MessageNumber 2",
                 showsPrec 11 (messageNumber (2 :: Int64)) "" `shouldBe` "(MessageNumber 2)",
                 showList [messageText "Ada", messageNumber 2] "" `shouldBe` "[MessageText \"Ada\",MessageNumber 2]",
                 messageNumber (2 :: Int64) `shouldNotBe` messageNumber 3,
                 show (messageTemplate "Hello") `shouldBe` "MessageTemplate \"Hello\"",
                 showsPrec 11 (messageTemplate "Hello") "" `shouldBe` "(MessageTemplate \"Hello\")",
                 showList [messageTemplate "Hello"] "" `shouldBe` "[MessageTemplate \"Hello\"]",
                 messageTemplate (Text.pack ['H', 'e', 'l', 'l', 'o']) `shouldBe` messageTemplate ("Hel" <> "lo"),
                 messageTemplate "Hello" `shouldNotBe` messageTemplate "Goodbye",
                 MessageNotFound `shouldNotBe` MessageFormatRejected,
                 show MessageFormatRejected `shouldBe` "MessageFormatRejected",
                 showsPrec 11 MessageNotFound "" `shouldBe` "MessageNotFound",
                 showList [MessageNotFound, MessageFormatRejected] "" `shouldBe` "[MessageNotFound,MessageFormatRejected]"
               ]
        )

    it "preserves embedded NUL bytes across the UTF-8 ICU boundary" $ do
      let catalog messageKey requestedLocale =
            case (messageKey, localeText requestedLocale) of
              (ItemCount, "en") -> Just (messageTemplate "before {na\NULme} after")
              _ -> Nothing
      let rendered = renderLocalizedMessage (localizer catalog) ItemCount (locale "en") (messageArguments [("na\NULme", messageText "value\NULdetail")])
      rendered `shouldBe` Right "before value\NULdetail after"

    it "rejects an embedded NUL in a locale instead of accepting its truncated prefix" $
      renderLocalizedMessage (localizer (\_ _ -> Just (messageTemplate "Hello"))) ItemCount (locale "en\NULforged") (messageArguments [])
        `shouldBe` Left MessageFormatRejected

    it "rejects malformed bytes returned by the native UTF-8 boundary" $
      decodeRenderedIcuUtf8 "\xc3\x28" `shouldBe` Left MessageFormatRejected

    it "checks static ICU template brace structure before application compilation" $
      expectAll
        ( (validateMessageTemplate "{count, plural, one {# item} other {# items}}" `shouldBe` Right ())
            :| [ validateMessageTemplate "{count, plural, one {# item}" `shouldBe` Left "unterminated ICU argument",
                 validateMessageTemplate "{}" `shouldBe` Left "empty ICU argument",
                 show ([message|Hello, {name}!|] :: MessageTemplate) `shouldBe` "MessageTemplate \"Hello, {name}!\""
               ]
        )
