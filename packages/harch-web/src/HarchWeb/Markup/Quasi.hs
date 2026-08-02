{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Compile-time XML-like syntax for the escaping-by-default markup AST.
module HarchWeb.Markup.Quasi
  ( harch,
  )
where

import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.Functor ((<&>))
import Data.List (intercalate)
import Data.Text qualified as Text
import Language.Haskell.Meta.Parse qualified as Meta
import Language.Haskell.TH (Exp (..), Lit (..), Loc, Q, loc_filename, loc_start, location, mkName)
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

data Position = Position
  { positionLine :: Int,
    positionColumn :: Int
  }
  deriving (Eq, Show)

data ParseError = ParseError Position String
  deriving (Eq, Show)

data ParseState = ParseState
  { parseRemaining :: String,
    parsePosition :: Position
  }

data MarkupNode
  = NativeNode Position String [MarkupAttribute] [MarkupNode]
  | ComponentNode Position String String [MarkupNode]
  | RegionNode Position String
  | InterpolationNode Position String
  | LiteralNode String

data MarkupAttribute
  = LiteralAttribute Position String String
  | ExpressionAttribute Position String String
  | FlagAttribute Position String

parseMarkup :: String -> Either ParseError [MarkupNode]
parseMarkup source = do
  (nodes, finalState) <- parseChildren Nothing (ParseState source (Position 1 1))
  case parseRemaining finalState of
    "" -> Right nodes
    _ -> parseFailure finalState "unexpected markup after the root node"

parseChildren :: Maybe String -> ParseState -> Either ParseError ([MarkupNode], ParseState)
parseChildren expectedClosingTag = go []
  where
    go parsedNodes state =
      case parseRemaining state of
        "" ->
          case expectedClosingTag of
            Nothing -> Right (reverse parsedNodes, state)
            Just tagName -> parseFailure state ("missing closing tag </" <> tagName <> ">")
        '<' : '/' : _ -> do
          (closingTag, nextState) <- parseClosingTag state
          case expectedClosingTag of
            Just tagName
              | closingTag == tagName -> Right (reverse parsedNodes, nextState)
              | otherwise -> parseFailure state ("expected closing tag </" <> tagName <> "> but found </" <> closingTag <> ">")
            Nothing -> parseFailure state ("unexpected closing tag </" <> closingTag <> ">")
        '<' : _ -> do
          (node, nextState) <- parseNode state
          go (node : parsedNodes) nextState
        '{' : _ -> do
          (expressionPosition, expressionSource, nextState) <- parseBracedExpression state
          go (InterpolationNode expressionPosition expressionSource : parsedNodes) nextState
        _ -> do
          let (literal, nextState) = takeLiteral state
          case decodeXmlEntities (normaliseLiteral literal) of
            "" -> go parsedNodes nextState
            normalised -> go (LiteralNode normalised : parsedNodes) nextState

parseNode :: ParseState -> Either ParseError (MarkupNode, ParseState)
parseNode initialState = do
  (_, afterOpen) <- consumeCharacter '<' initialState
  (nodePosition, tagName, afterTagName) <- parseTagName afterOpen
  (attributes, selfClosing, afterAttributes) <- parseAttributes afterTagName
  case tagKind tagName of
    NativeTag tagConstructor isVoid ->
      case (isVoid, selfClosing) of
        (True, False) -> parseFailure initialState ("void element <" <> tagName <> "> must be self-closing")
        (True, True) -> Right (NativeNode nodePosition tagConstructor attributes [], afterAttributes)
        (False, True) -> parseFailure initialState ("native element <" <> tagName <> "> cannot be self-closing")
        (False, False) -> do
          (children, afterChildren) <- parseChildren (Just tagName) afterAttributes
          Right (NativeNode nodePosition tagConstructor attributes children, afterChildren)
    ComponentTag componentName
      | tagName == "Region" -> parseRegionNode initialState attributes selfClosing afterAttributes
      | selfClosing -> do
          properties <- componentProperties initialState attributes
          Right (ComponentNode nodePosition componentName properties [], afterAttributes)
      | otherwise -> do
          properties <- componentProperties initialState attributes
          (children, afterChildren) <- parseChildren (Just tagName) afterAttributes
          Right (ComponentNode nodePosition componentName properties children, afterChildren)

parseRegionNode :: ParseState -> [MarkupAttribute] -> Bool -> ParseState -> Either ParseError (MarkupNode, ParseState)
parseRegionNode initialState attributes selfClosing nextState
  | not selfClosing = parseFailure initialState "<Region> must be self-closing"
  | otherwise =
      case attributes of
        [ExpressionAttribute _ "value" expressionSource] -> Right (RegionNode (parsePosition initialState) expressionSource, nextState)
        _ -> parseFailure initialState "<Region> requires exactly one value={...} attribute"

componentProperties :: ParseState -> [MarkupAttribute] -> Either ParseError String
componentProperties state attributes =
  case attributes of
    [ExpressionAttribute _ "props" expressionSource] -> Right expressionSource
    _ -> parseFailure state "components require exactly one props={...} attribute"

data TagKind
  = NativeTag String Bool
  | ComponentTag String

tagKind :: String -> TagKind
tagKind tagName =
  case tagName of
    firstCharacter : _
      | isLower firstCharacter -> NativeTag (nativeTagConstructor tagName) (tagName == "input")
    _ -> ComponentTag (componentFunctionName tagName)

nativeTagConstructor :: String -> String
nativeTagConstructor tagName =
  case tagName of
    "a" -> "anchorTag"
    "button" -> "buttonTag"
    "code" -> "codeTag"
    "div" -> "divTag"
    "form" -> "formTag"
    "h1" -> "headingOneTag"
    "h2" -> "headingTwoTag"
    "input" -> "inputTag"
    "label" -> "labelTag"
    "li" -> "listItemTag"
    "option" -> "optionTag"
    "p" -> "paragraphTag"
    "section" -> "sectionTag"
    "select" -> "selectTag"
    "ul" -> "listTag"
    _ -> ""

componentFunctionName :: String -> String
componentFunctionName componentName =
  case reverse (splitOn '.' componentName) of
    [] -> componentName
    lastPart : reversedPrefix -> intercalate "." (reverse reversedPrefix <> [lowerInitial lastPart])

splitOn :: Char -> String -> [String]
splitOn separator = foldr step [""]
  where
    step character pieces
      | character == separator = "" : pieces
      | otherwise =
          case pieces of
            [] -> [[character]]
            current : remaining -> (character : current) : remaining

lowerInitial :: String -> String
lowerInitial value =
  case value of
    [] -> []
    firstCharacter : remaining -> toLowerAscii firstCharacter : remaining

toLowerAscii :: Char -> Char
toLowerAscii character
  | isUpper character = toEnum (fromEnum character + 32)
  | otherwise = character

parseAttributes :: ParseState -> Either ParseError ([MarkupAttribute], Bool, ParseState)
parseAttributes initialState = go [] (skipWhitespace initialState)
  where
    go parsedAttributes state =
      case parseRemaining state of
        '/' : '>' : _ -> Right (reverse parsedAttributes, True, advanceString "/>" state)
        '>' : _ -> Right (reverse parsedAttributes, False, advanceCharacter '>' state)
        "" -> parseFailure state "unterminated opening tag"
        _ -> do
          (attribute, afterAttribute) <- parseAttribute state
          go (attribute : parsedAttributes) (skipWhitespace afterAttribute)

parseAttribute :: ParseState -> Either ParseError (MarkupAttribute, ParseState)
parseAttribute initialState = do
  (attributePosition, attributeName, afterName) <- parseAttributeName initialState
  case parseRemaining afterName of
    '=' : _ ->
      let afterEquals = advanceCharacter '=' afterName
       in case parseRemaining afterEquals of
            '"' : _ -> do
              (literalValue, afterValue) <- parseQuotedValue afterEquals
              Right (LiteralAttribute attributePosition attributeName (decodeXmlEntities literalValue), afterValue)
            '{' : _ -> do
              (_, expressionSource, afterExpression) <- parseBracedExpression afterEquals
              Right (ExpressionAttribute attributePosition attributeName expressionSource, afterExpression)
            _ -> parseFailure afterEquals ("attribute " <> attributeName <> " must use a quoted literal or {...} expression")
    _ -> Right (FlagAttribute attributePosition attributeName, afterName)

parseTagName :: ParseState -> Either ParseError (Position, String, ParseState)
parseTagName = parseNamedValue "tag"

parseAttributeName :: ParseState -> Either ParseError (Position, String, ParseState)
parseAttributeName = parseNamedValue "attribute"

parseNamedValue :: String -> ParseState -> Either ParseError (Position, String, ParseState)
parseNamedValue label state =
  let (name, nextState) = takeWhileState validNameCharacter state
   in case name of
        "" -> parseFailure state ("expected " <> label <> " name")
        firstCharacter : _
          | isAlphaNum firstCharacter -> Right (parsePosition state, name, nextState)
          | otherwise -> parseFailure state ("invalid " <> label <> " name")
  where
    validNameCharacter character = isAlphaNum character || character == '-' || character == '.'

parseQuotedValue :: ParseState -> Either ParseError (String, ParseState)
parseQuotedValue initialState = do
  (_, afterQuote) <- consumeCharacter '"' initialState
  go [] afterQuote
  where
    go accumulated state =
      case parseRemaining state of
        '"' : _ -> Right (reverse accumulated, advanceCharacter '"' state)
        "" -> parseFailure initialState "unterminated quoted attribute value"
        character : _ -> go (character : accumulated) (advanceCharacter character state)

parseBracedExpression :: ParseState -> Either ParseError (Position, String, ParseState)
parseBracedExpression initialState = do
  (_, afterOpen) <- consumeCharacter '{' initialState
  go 1 [] afterOpen
  where
    expressionPosition = parsePosition initialState

    go :: Int -> String -> ParseState -> Either ParseError (Position, String, ParseState)
    go depth accumulated state =
      case parseRemaining state of
        "" -> parseFailure initialState "unterminated {...} expression"
        '"' : _ -> do
          (quoted, afterQuoted) <- parseDelimited '"' state
          go depth (reverse quoted <> accumulated) afterQuoted
        '\'' : _ -> do
          (quoted, afterQuoted) <- parseDelimited '\'' state
          go depth (reverse quoted <> accumulated) afterQuoted
        '{' : _ -> go (depth + 1) ('{' : accumulated) (advanceCharacter '{' state)
        '}' : _
          | depth == 1 -> Right (expressionPosition, reverse accumulated, advanceCharacter '}' state)
          | otherwise -> go (depth - 1) ('}' : accumulated) (advanceCharacter '}' state)
        character : _ -> go depth (character : accumulated) (advanceCharacter character state)

parseDelimited :: Char -> ParseState -> Either ParseError (String, ParseState)
parseDelimited delimiter initialState = do
  (_, afterOpen) <- consumeCharacter delimiter initialState
  go [delimiter] afterOpen
  where
    go accumulated state =
      case parseRemaining state of
        "" -> parseFailure initialState "unterminated string or character literal in {...} expression"
        '\\' : escapedCharacter : _ -> go (escapedCharacter : '\\' : accumulated) (advanceString ['\\', escapedCharacter] state)
        character : _
          | character == delimiter -> Right (reverse (delimiter : accumulated), advanceCharacter delimiter state)
          | otherwise -> go (character : accumulated) (advanceCharacter character state)

parseClosingTag :: ParseState -> Either ParseError (String, ParseState)
parseClosingTag initialState = do
  (_, afterOpen) <- consumeCharacter '<' initialState
  (_, afterSlash) <- consumeCharacter '/' afterOpen
  (_, tagName, afterTagName) <- parseTagName afterSlash
  let afterWhitespace = skipWhitespace afterTagName
  (_, afterClose) <- consumeCharacter '>' afterWhitespace
  Right (tagName, afterClose)

takeLiteral :: ParseState -> (String, ParseState)
takeLiteral = takeWhileState (\character -> character /= '<' && character /= '{')

takeWhileState :: (Char -> Bool) -> ParseState -> (String, ParseState)
takeWhileState predicate = go []
  where
    go accumulated state =
      case parseRemaining state of
        character : _
          | predicate character -> go (character : accumulated) (advanceCharacter character state)
        _ -> (reverse accumulated, state)

skipWhitespace :: ParseState -> ParseState
skipWhitespace = snd . takeWhileState isSpace

consumeCharacter :: Char -> ParseState -> Either ParseError (Char, ParseState)
consumeCharacter expected state =
  case parseRemaining state of
    actual : _
      | actual == expected -> Right (actual, advanceCharacter actual state)
      | otherwise -> parseFailure state ("expected '" <> [expected] <> "'")
    "" -> parseFailure state ("expected '" <> [expected] <> "' but reached end of markup")

advanceString :: String -> ParseState -> ParseState
advanceString characters state = foldl (flip advanceCharacter) state characters

advanceCharacter :: Char -> ParseState -> ParseState
advanceCharacter character state =
  case parseRemaining state of
    _ : remaining ->
      let Position line column = parsePosition state
          nextPosition
            | character == '\n' = Position (line + 1) 1
            | otherwise = Position line (column + 1)
       in ParseState remaining nextPosition
    "" -> state

normaliseLiteral :: String -> String
normaliseLiteral literal
  | '\n' `elem` literal = unwords (filter (not . null) (map trim (lines literal)))
  | otherwise = literal
  where
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

decodeXmlEntities :: String -> String
decodeXmlEntities = go
  where
    go value =
      case value of
        '&' : 'a' : 'm' : 'p' : ';' : remaining -> '&' : go remaining
        '&' : 'a' : 'p' : 'o' : 's' : ';' : remaining -> '\'' : go remaining
        '&' : 'g' : 't' : ';' : remaining -> '>' : go remaining
        '&' : 'l' : 't' : ';' : remaining -> '<' : go remaining
        '&' : 'q' : 'u' : 'o' : 't' : ';' : remaining -> '\"' : go remaining
        character : remaining -> character : go remaining
        [] -> []

parseFailure :: ParseState -> String -> Either ParseError a
parseFailure state message = Left (ParseError (parsePosition state) message)

renderParseError :: Loc -> ParseError -> String
renderParseError sourceLocation (ParseError (Position relativeLine relativeColumn) message) =
  let (sourceLine, sourceColumn) = loc_start sourceLocation
      absoluteLine = sourceLine + relativeLine - 1
      absoluteColumn
        | relativeLine == 1 = sourceColumn + relativeColumn - 1
        | otherwise = relativeColumn
   in loc_filename sourceLocation <> ":" <> show absoluteLine <> ":" <> show absoluteColumn <> ": harch: " <> message

lowerNodes :: [MarkupNode] -> Q Exp
lowerNodes nodes = traverse lowerNode nodes <&> (AppE (VarE (mkName "fragment")) . ListE)

lowerNode :: MarkupNode -> Q Exp
lowerNode node =
  case node of
    NativeNode position tagConstructor attributes children -> lowerNativeNode position tagConstructor attributes children
    ComponentNode position componentName properties children -> lowerComponentNode position componentName properties children
    RegionNode position expressionSource -> AppE (VarE (mkName "regionHtml")) <$> parseExpression position expressionSource
    InterpolationNode position expressionSource -> AppE (VarE (mkName "toHtml")) <$> parseExpression position expressionSource
    LiteralNode literal -> pure (AppE (VarE (mkName "text")) (textLiteral literal))

lowerNativeNode :: Position -> String -> [MarkupAttribute] -> [MarkupNode] -> Q Exp
lowerNativeNode position tagConstructor attributes children = do
  tagExpression <- namedValue position tagConstructor
  attributeExpressions <- traverse lowerAttribute attributes
  childExpressions <- traverse lowerNode children
  case tagConstructor of
    "inputTag" -> pure (AppE (AppE (VarE (mkName "voidElement")) tagExpression) (ListE attributeExpressions))
    _ -> pure (AppE (AppE (AppE (VarE (mkName "element")) tagExpression) (ListE attributeExpressions)) (ListE childExpressions))

lowerComponentNode :: Position -> String -> String -> [MarkupNode] -> Q Exp
lowerComponentNode position componentName properties children = do
  componentExpression <- parseExpression position componentName
  propertiesExpression <- parseExpression position properties
  childExpressions <- traverse lowerNode children
  pure (AppE (AppE componentExpression propertiesExpression) (ListE childExpressions))

lowerAttribute :: MarkupAttribute -> Q Exp
lowerAttribute markupAttribute =
  case markupAttribute of
    LiteralAttribute position attributeName literal -> lowerLiteralAttribute position attributeName literal
    ExpressionAttribute position attributeName expressionSource -> lowerExpressionAttribute position attributeName expressionSource
    FlagAttribute position attributeName -> lowerFlagAttribute position attributeName

lowerLiteralAttribute :: Position -> String -> String -> Q Exp
lowerLiteralAttribute position attributeName literal =
  case attributeName of
    "id" -> applyNamed "elementId" [applyNamedPure "literalElementId" [textLiteral literal]]
    "for" -> applyNamed "labelFor" [applyNamedPure "literalElementId" [textLiteral literal]]
    "class" -> failAt position "class requires an interpolated CssClass expression"
    _ -> lowerTextAttribute position attributeName (textLiteral literal)

lowerExpressionAttribute :: Position -> String -> String -> Q Exp
lowerExpressionAttribute position attributeName expressionSource = do
  expression <- parseExpression position expressionSource
  case attributeName of
    "id" -> applyNamed "elementId" [expression]
    "for" -> applyNamed "labelFor" [expression]
    "class" -> applyNamed "className" [expression]
    _ -> lowerTextAttribute position attributeName expression

lowerFlagAttribute :: Position -> String -> Q Exp
lowerFlagAttribute position attributeName =
  case attributeName of
    "required" -> namedValue position "required"
    _
      | Just suffix <- dataAttributeSuffix attributeName -> applyNamed "dataFlag" [textLiteral suffix]
      | otherwise -> failAt position ("unsupported boolean attribute " <> attributeName)

lowerTextAttribute :: Position -> String -> Exp -> Q Exp
lowerTextAttribute position attributeName valueExpression =
  case attributeName of
    "action" -> applyNamed "formAction" [valueExpression]
    "aria-label" -> applyNamed "ariaLabel" [valueExpression]
    "aria-live" -> applyNamed "ariaLive" [valueExpression]
    "autocomplete" -> applyNamed "autocomplete" [valueExpression]
    "href" -> applyNamed "href" [valueExpression]
    "inputmode" -> applyNamed "inputMode" [valueExpression]
    "maxlength" -> applyNamed "maxLength" [valueExpression]
    "method" -> applyNamed "method" [valueExpression]
    "minlength" -> applyNamed "minLength" [valueExpression]
    "name" -> applyNamed "name" [valueExpression]
    "role" -> applyNamed "role" [valueExpression]
    "type" -> applyNamed "inputType" [valueExpression]
    "value" -> applyNamed "value" [valueExpression]
    _
      | Just suffix <- dataAttributeSuffix attributeName -> applyNamed "dataAttribute" [textLiteral suffix, valueExpression]
      | otherwise -> failAt position ("unsupported attribute " <> attributeName)

dataAttributeSuffix :: String -> Maybe String
dataAttributeSuffix attributeName =
  case splitAt 5 attributeName of
    ("data-", suffix)
      | not (null suffix) -> Just suffix
    _ -> Nothing

namedValue :: Position -> String -> Q Exp
namedValue position valueName
  | null valueName = failAt position "unsupported native element"
  | otherwise = pure (VarE (mkName valueName))

applyNamed :: String -> [Exp] -> Q Exp
applyNamed valueName arguments = pure (applyNamedPure valueName arguments)

applyNamedPure :: String -> [Exp] -> Exp
applyNamedPure valueName = foldl AppE (VarE (mkName valueName))

parseExpression :: Position -> String -> Q Exp
parseExpression position expressionSource =
  case Meta.parseExp expressionSource of
    Left message -> failAt position ("invalid Haskell expression: " <> message)
    Right expression -> pure expression

textLiteral :: String -> Exp
textLiteral literal = AppE (VarE 'Text.pack) (LitE (StringL literal))

failAt :: Position -> String -> Q a
failAt (Position line column) message = fail ("harch:" <> show line <> ":" <> show column <> ": " <> message)
