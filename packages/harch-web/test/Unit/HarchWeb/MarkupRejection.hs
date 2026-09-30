{-# LANGUAGE TemplateHaskellQuotes #-}

module Unit.HarchWeb.MarkupRejection
  ( rejectedMarkup,
  )
where

import HarchWeb (harch)
import Language.Haskell.TH (Exp, Q, recover)
import Language.Haskell.TH.Quote (quoteExp)

rejectedMarkup :: String -> Q Exp
rejectedMarkup source =
  recover
    [|True|]
    (quoteExp harch source >> [|False|])
