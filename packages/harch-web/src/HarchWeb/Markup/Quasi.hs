{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Compile-time XML-like syntax for the escaping-by-default markup AST and
-- the shared Template Haskell implementation behind localization templates.
module HarchWeb.Markup.Quasi
  ( harch,
    message,
    validateMessageTemplate,
  )
where

import Data.Text qualified as Text
import HarchWeb.Localization (messageTemplate)
import HarchWeb.Markup.Quasi.Lowering (lowerNodes)
import HarchWeb.Markup.Quasi.Parser (parseMarkup, renderParseError)
import Language.Haskell.TH (Exp, Q, location)
import Language.Haskell.TH.Quote (QuasiQuoter (..))

harch :: QuasiQuoter
harch =
  QuasiQuoter
    { quoteExp = quoteMarkup,
      quotePat = unsupportedContext "patterns",
      quoteType = unsupportedContext "types",
      quoteDec = unsupportedContext "declarations"
    }

quoteMarkup :: String -> Q Exp
quoteMarkup source = do
  sourceLocation <- location
  case parseMarkup source of
    Left parseError -> fail (renderParseError sourceLocation parseError)
    Right nodes -> lowerNodes nodes

unsupportedContext :: String -> String -> Q a
unsupportedContext context _ = fail ("harch quasiquoter only supports expressions, not " <> context)

-- | Rejects unbalanced or empty ICU argument braces. ICU remains the
-- authoritative formatter at runtime, while this authoring check catches the
-- structural mistakes that otherwise first appear when an application path is
-- rendered.
validateMessageTemplate :: String -> Either String ()
validateMessageTemplate = go False 0 []
  where
    go :: Bool -> Int -> [String] -> String -> Either String ()
    go _ depth _ []
      | depth == 0 = Right ()
      | otherwise = Left "unterminated ICU argument"
    go quoted depth contents ('\'' : '\'' : remaining) = go quoted depth contents remaining
    go quoted depth contents ('\'' : remaining) = go (not quoted) depth contents remaining
    go True depth contents (_ : remaining) = go True depth contents remaining
    go False depth contents ('{' : remaining) = go False (depth + 1) ([] : contents) remaining
    go False 0 _ ('}' : _) = Left "unexpected closing ICU argument brace"
    go False depth ([] : parentContents) ('}' : remaining)
      | depth == 1 = Left "empty ICU argument"
      | otherwise = go False (depth - 1) parentContents remaining
    go False depth (content : parentContents) ('}' : remaining)
      | depth == 1 && all (`elem` [' ', '\t', '\n']) content = Left "empty ICU argument"
      | otherwise = go False (depth - 1) parentContents remaining
    go False depth contents (character : remaining) =
      case contents of
        [] -> go False depth [] remaining
        content : parentContents -> go False depth ((character : content) : parentContents) remaining

message :: QuasiQuoter
message =
  QuasiQuoter
    { quoteExp = quoteMessage,
      quotePat = unsupportedMessageContext "pattern",
      quoteType = unsupportedMessageContext "type",
      quoteDec = unsupportedMessageContext "declaration"
    }

quoteMessage :: String -> Q Exp
quoteMessage source =
  case validateMessageTemplate source of
    Left reason -> fail ("invalid ICU message template: " <> reason)
    Right () -> [|messageTemplate (Text.pack source)|]

unsupportedMessageContext :: String -> String -> Q a
unsupportedMessageContext context _ = fail ("ICU message quasiquoter cannot be used in a " <> context)
