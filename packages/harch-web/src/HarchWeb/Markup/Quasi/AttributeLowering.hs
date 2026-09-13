{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Private lowering for the closed native-attribute vocabulary.
--
-- Decision (AHI-6Q, 2026-08-31): keep the exhaustive literal and expression
-- folds here rather than a stringly data table.  Their per-attribute
-- validation and quoted 'Name' references preserve typed constructors and
-- splice-site hygiene; 'Lowering' remains only the node/component
-- orchestrator.
module HarchWeb.Markup.Quasi.AttributeLowering
  ( lowerAttribute,
  )
where

import Data.List.NonEmpty (NonEmpty (..))
import HarchWeb.Markup.Implementation qualified as Impl
import HarchWeb.Markup.Quasi.LoweringSupport
  ( failAt,
    fromStringLiteral,
    parseExpression,
    textLiteral,
  )
import HarchWeb.Markup.Quasi.Parser
  ( MarkupAttribute (..),
    Position,
  )
import Language.Haskell.TH
  ( Exp (..),
    Lit (..),
    Name,
    Q,
  )

lowerAttribute :: MarkupAttribute -> Q Exp
lowerAttribute markupAttribute =
  case markupAttribute of
    LiteralAttribute position attributeName literal ->
      lowerLiteralAttribute position attributeName literal
    ExpressionAttribute position attributeName expressionSource ->
      lowerExpressionAttribute position attributeName expressionSource
    FlagAttribute position attributeName -> lowerFlagAttribute position attributeName

lowerLiteralAttribute :: Position -> String -> String -> Q Exp
lowerLiteralAttribute position attributeName literal =
  case attributeName of
    "id" -> applyNamed 'Impl.elementId [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "for" -> applyNamed 'Impl.labelFor [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "aria-controls" -> applyNamed 'Impl.ariaControls [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "aria-current" -> lowerLiteralAriaCurrent position literal
    "aria-describedby" -> lowerLiteralIdReferences position literal
    "aria-errormessage" -> applyNamed 'Impl.ariaErrorMessage [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "aria-invalid" -> lowerLiteralAriaInvalid position literal
    "aria-expanded" -> lowerLiteralAriaExpanded position literal
    "aria-haspopup" -> lowerLiteralAriaHasPopup position literal
    "aria-hidden" -> lowerLiteralAriaHidden position literal
    "aria-labelledby" -> applyNamed 'Impl.ariaLabelledBy [applyNamedPure 'Impl.literalElementId [textLiteral literal]]
    "tabindex" -> lowerLiteralTabIndex position literal
    "class" -> failAt position "class requires an interpolated CssClass expression"
    -- 'href' takes a 'SafeUrl', not 'Text': a quoted literal in markup source
    -- is the template author's own text, validated once at compile time
    -- through 'SafeUrl''s 'IsString' instance.
    "href" -> lowerTextAttribute position attributeName (fromStringLiteral literal)
    _ -> lowerTextAttribute position attributeName (textLiteral literal)

lowerExpressionAttribute :: Position -> String -> String -> Q Exp
lowerExpressionAttribute position attributeName expressionSource = do
  expression <- parseExpression position expressionSource
  case attributeName of
    "id" -> applyNamed 'Impl.elementId [expression]
    "for" -> applyNamed 'Impl.labelFor [expression]
    "aria-controls" -> applyNamed 'Impl.ariaControls [expression]
    "aria-describedby" -> applyNamed 'Impl.ariaDescribedBy [expression]
    "aria-errormessage" -> applyNamed 'Impl.ariaErrorMessage [expression]
    "aria-invalid" -> applyNamed 'Impl.ariaInvalid [expression]
    "aria-expanded" -> applyNamed 'Impl.ariaExpanded [expression]
    "aria-hidden" -> applyNamed 'Impl.ariaHidden [expression]
    "aria-labelledby" -> applyNamed 'Impl.ariaLabelledBy [expression]
    "class" -> applyNamed 'Impl.className [expression]
    "tabindex" -> applyNamed 'Impl.tabIndex [expression]
    _ -> lowerTextAttribute position attributeName expression

lowerLiteralIdReferences :: Position -> String -> Q Exp
lowerLiteralIdReferences position literal =
  case words literal of
    [] -> failAt position "aria-describedby requires at least one element ID"
    firstIdentifier : remainingIdentifiers ->
      applyNamed
        'Impl.ariaDescribedBy
        [ AppE
            (AppE (ConE '(:|)) (elementIdentifier firstIdentifier))
            (ListE (map elementIdentifier remainingIdentifiers))
        ]
  where
    elementIdentifier = applyNamedPure 'Impl.literalElementId . pure . textLiteral

lowerLiteralAriaInvalid :: Position -> String -> Q Exp
lowerLiteralAriaInvalid position literal =
  case literal of
    "true" -> applyNamed 'Impl.ariaInvalid [ConE 'True]
    "false" -> applyNamed 'Impl.ariaInvalid [ConE 'False]
    _ -> failAt position "aria-invalid must be true or false"

lowerLiteralAriaCurrent :: Position -> String -> Q Exp
lowerLiteralAriaCurrent position literal =
  case literal of
    "page" -> pure (VarE 'Impl.ariaCurrentPage)
    _ -> failAt position "aria-current must be page"

lowerLiteralAriaExpanded :: Position -> String -> Q Exp
lowerLiteralAriaExpanded position = lowerLiteralBoolean position "aria-expanded" 'Impl.ariaExpanded

lowerLiteralAriaHidden :: Position -> String -> Q Exp
lowerLiteralAriaHidden position = lowerLiteralBoolean position "aria-hidden" 'Impl.ariaHidden

lowerLiteralBoolean :: Position -> String -> Name -> String -> Q Exp
lowerLiteralBoolean position attributeName constructorName literal =
  case literal of
    "true" -> applyNamed constructorName [ConE 'True]
    "false" -> applyNamed constructorName [ConE 'False]
    _ -> failAt position (attributeName <> " must be true or false")

lowerLiteralAriaHasPopup :: Position -> String -> Q Exp
lowerLiteralAriaHasPopup position literal =
  case literal of
    "dialog" -> pure (VarE 'Impl.ariaHasPopupDialog)
    _ -> failAt position "aria-haspopup must be dialog"

lowerLiteralTabIndex :: Position -> String -> Q Exp
lowerLiteralTabIndex position literal =
  case reads literal of
    [(tabOrder, "")] -> applyNamed 'Impl.tabIndex [LitE (IntegerL tabOrder)]
    _ -> failAt position "tabindex must be an integer"

lowerFlagAttribute :: Position -> String -> Q Exp
lowerFlagAttribute position attributeName =
  case attributeName of
    "required" -> pure (VarE 'Impl.required)
    "selected" -> pure (VarE 'Impl.selected)
    "open" -> pure (VarE 'Impl.dialogOpen)
    _
      | Just suffix <- dataAttributeSuffix attributeName -> applyNamed 'Impl.dataFlag [fromStringLiteral suffix]
      | otherwise -> failAt position ("unsupported boolean attribute " <> attributeName)

lowerTextAttribute :: Position -> String -> Exp -> Q Exp
lowerTextAttribute position attributeName valueExpression =
  case lookup attributeName textAttributeConstructors of
    Just constructorName -> applyNamed constructorName [valueExpression]
    Nothing
      | Just suffix <- dataAttributeSuffix attributeName ->
          applyNamed 'Impl.dataAttribute [fromStringLiteral suffix, valueExpression]
      | otherwise -> failAt position ("unsupported attribute " <> attributeName)

-- | Every framework identifier below is resolved through a quoted 'Name'
-- ('Impl.foo'), not 'mkName', so a splice site's own local bindings can never
-- shadow it. Component names remain deliberately open at the splice site.
textAttributeConstructors :: [(String, Name)]
textAttributeConstructors =
  [ ("action", 'Impl.formAction),
    ("aria-label", 'Impl.ariaLabel),
    ("aria-live", 'Impl.ariaLive),
    ("autocomplete", 'Impl.autocomplete),
    ("href", 'Impl.href),
    ("inputmode", 'Impl.inputMode),
    ("lang", 'Impl.lang),
    ("maxlength", 'Impl.maxLength),
    ("method", 'Impl.method),
    ("minlength", 'Impl.minLength),
    ("name", 'Impl.name),
    ("role", 'Impl.role),
    ("type", 'Impl.inputType),
    ("value", 'Impl.value)
  ]

dataAttributeSuffix :: String -> Maybe String
dataAttributeSuffix attributeName =
  case splitAt 5 attributeName of
    ("data-", suffix)
      | not (null suffix) -> Just suffix
    _ -> Nothing

applyNamed :: Name -> [Exp] -> Q Exp
applyNamed name arguments = pure (applyNamedPure name arguments)

applyNamedPure :: Name -> [Exp] -> Exp
applyNamedPure name = foldl AppE (VarE name)
