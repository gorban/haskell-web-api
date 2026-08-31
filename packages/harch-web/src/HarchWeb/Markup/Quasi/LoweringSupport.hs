{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Private shared support for markup quasiquoter lowering.  It keeps the
-- dependency-specific expression parser failure and source-position rendering
-- identical for node and native-attribute lowering without making either
-- collaborator depend on the other's vocabulary.
module HarchWeb.Markup.Quasi.LoweringSupport
  ( failAt,
    fromStringLiteral,
    parseExpression,
    textLiteral,
  )
where

import Control.Exception (ErrorCall (..), evaluate, try)
import Data.String (fromString)
import Data.Text qualified as Text
import HarchWeb.Markup.Quasi.Parser (Position (..))
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH
  ( Exp (..),
    Lit (..),
    Q,
    runIO,
  )
import Language.Haskell.TH.Ppr (pprint)

-- | Decision (BV, 2026-08-21, per @docs/design-guidance.md@'s
-- missing-framework-capability protocol): see @docs/design-guidance.md@'s
-- "Follow-up decision — BV" for the full record of why this catches the
-- crash rather than implementing TH-quote support.
--
-- 'Meta.parseExp' does not return a 'Left' for every unsupported expression:
-- a bare Template Haskell name quote (@'Just@) parses, then throws an
-- uncaught 'ErrorCall' from deep inside @haskell-src-meta@'s own AST
-- translation the moment the result is forced (@toExp: not implemented:
-- VarQuote ...@) — confirmed directly against @haskell-src-meta-0.8.16@,
-- not assumed. Left uncaught, that crash would surface as a confusing
-- GHC-internal panic at the *calling* module's compile time, far from where
-- the actual mistake is. Fully consuming 'pprint' inside 'Q' (which runs in
-- 'IO') traverses the complete parsed 'Exp', including nested children,
-- without adding a second parser or treating TH name quotes as supported
-- syntax. That converts the dependency's crash into the same clean,
-- positioned parse failure an ordinary syntax error already gets.
parseExpression :: Position -> String -> Q Exp
parseExpression position expressionSource =
  case Meta.parseExp expressionSource of
    Left message -> failAt position ("invalid Haskell expression: " <> message)
    Right expression -> do
      forcedExpression <- runIO (try (evaluate (length (pprint expression))))
      case forcedExpression of
        Left (ErrorCall message) ->
          failAt position ("unsupported Haskell expression syntax: " <> message)
        Right _ -> pure expression

textLiteral :: String -> Exp
textLiteral literal = AppE (VarE 'Text.pack) (LitE (StringL literal))

-- | Like 'textLiteral', but for a validated newtype ('DataAttributeSuffix',
-- 'SafeUrl') reached through its 'IsString' instance instead of building a
-- plain 'Text' value: the generated code reads the same as a template author
-- writing the string literal directly with 'OverloadedStrings'.
fromStringLiteral :: String -> Exp
fromStringLiteral literal = AppE (VarE 'fromString) (LitE (StringL literal))

failAt :: Position -> String -> Q a
failAt (Position line column) message =
  fail ("harch:" <> show line <> ":" <> show column <> ": " <> message)
