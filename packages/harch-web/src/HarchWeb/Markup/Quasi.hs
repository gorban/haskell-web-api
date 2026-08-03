-- | Compile-time XML-like syntax for the escaping-by-default markup AST.
module HarchWeb.Markup.Quasi
  ( harch,
  )
where

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
