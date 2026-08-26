{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{-# SPEC #-}

import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Localization
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
                 equalValues (locale "en") (locale "en") `shouldBe` True,
                 notEqualValues (locale "en") (locale "is") `shouldBe` True,
                 locale "en" < locale "is" `shouldBe` True,
                 locale "en" <= locale "is" `shouldBe` True,
                 locale "is" > locale "en" `shouldBe` True,
                 locale "is" >= locale "en" `shouldBe` True,
                 compare (locale "en") (locale "en") `shouldBe` EQ,
                 min (locale "en") (locale "is") `shouldBe` locale "en",
                 max (locale "en") (locale "is") `shouldBe` locale "is",
                 compare (locale "is") (locale "en") `shouldBe` GT,
                 renderedValue (locale "en") `shouldBe` "Locale \"en\"",
                 renderedWithPrecedence 11 (locale "en") "" `shouldBe` "(Locale \"en\")",
                 renderedValueList [locale "en"] "" `shouldBe` "[Locale \"en\"]",
                 equalValues (messageText "Ada") (messageText "Ada") `shouldBe` True,
                 notEqualValues (messageText "Ada") (messageText "Grace") `shouldBe` True,
                 notEqualValues (messageText "Ada") (messageNumber 2) `shouldBe` True,
                 notEqualValues (messageNumber (2 :: Int64)) (messageText "Ada") `shouldBe` True,
                 renderedValue (messageText "Ada") `shouldBe` "MessageText \"Ada\"",
                 equalValues (messageNumber (2 :: Int64)) (messageNumber 2) `shouldBe` True,
                 renderedValue (messageNumber (2 :: Int64)) `shouldBe` "MessageNumber 2",
                 renderedWithPrecedence 11 (messageNumber (2 :: Int64)) "" `shouldBe` "(MessageNumber 2)",
                 renderedValueList [messageText "Ada", messageNumber 2] "" `shouldBe` "[MessageText \"Ada\",MessageNumber 2]",
                 notEqualValues (messageNumber (2 :: Int64)) (messageNumber 3) `shouldBe` True,
                 renderedValue (messageTemplate "Hello") `shouldBe` "MessageTemplate \"Hello\"",
                 renderedWithPrecedence 11 (messageTemplate "Hello") "" `shouldBe` "(MessageTemplate \"Hello\")",
                 renderedValueList [messageTemplate "Hello"] "" `shouldBe` "[MessageTemplate \"Hello\"]",
                 equalValues (messageTemplate "Hello") (messageTemplate "Hello") `shouldBe` True,
                 notEqualValues (messageTemplate "Hello") (messageTemplate "Goodbye") `shouldBe` True,
                 equalValues MessageFormatRejected MessageFormatRejected `shouldBe` True,
                 notEqualValues MessageNotFound MessageFormatRejected `shouldBe` True,
                 renderedValue MessageFormatRejected `shouldBe` "MessageFormatRejected",
                 renderedWithPrecedence 11 MessageNotFound "" `shouldBe` "MessageNotFound",
                 renderedValueList [MessageNotFound, MessageFormatRejected] "" `shouldBe` "[MessageNotFound,MessageFormatRejected]"
               ]
        )

    it "checks static ICU template brace structure before application compilation" $
      expectAll
        ( (validateMessageTemplate "{count, plural, one {# item} other {# items}}" `shouldBe` Right ())
            :| [ validateMessageTemplate "{count, plural, one {# item}" `shouldBe` Left "unterminated ICU argument",
                 validateMessageTemplate "{}" `shouldBe` Left "empty ICU argument",
                 renderedValue ([message|Hello, {name}!|] :: MessageTemplate) `shouldBe` "MessageTemplate \"Hello, {name}!\""
               ]
        )
