module HarchWeb.Markup.Quasi.Parser
  ( MarkupAttribute (..),
    MarkupNode (..),
    Position (..),
    parseMarkup,
    renderParseError,
  )
where

import Data.Char (isAlphaNum, isLower, isSpace, toLower)
import Data.List (intercalate)
import Language.Haskell.TH (Loc, loc_filename, loc_start)

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
  = NativeNode Position String Bool [MarkupAttribute] [MarkupNode]
  | ComponentNode Position String [MarkupAttribute] [MarkupNode]
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
              | otherwise ->
                  parseFailure
                    state
                    ("expected closing tag </" <> tagName <> "> but found </" <> closingTag <> ">")
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
    NativeTag Nothing _ ->
      parseFailure initialState ("unsupported native element <" <> tagName <> ">")
    NativeTag (Just tagConstructor) isVoid ->
      case (isVoid, selfClosing) of
        (True, False) -> parseFailure initialState ("void element <" <> tagName <> "> must be self-closing")
        (True, True) -> Right (NativeNode nodePosition tagConstructor True attributes [], afterAttributes)
        (False, True) -> parseFailure initialState ("native element <" <> tagName <> "> cannot be self-closing")
        (False, False) -> do
          (children, afterChildren) <- parseChildren (Just tagName) afterAttributes
          Right (NativeNode nodePosition tagConstructor False attributes children, afterChildren)
    ComponentTag componentName
      | tagName == "Region" -> parseRegionNode initialState attributes selfClosing afterAttributes
      | selfClosing -> Right (ComponentNode nodePosition componentName attributes [], afterAttributes)
      | otherwise -> do
          (children, afterChildren) <- parseChildren (Just tagName) afterAttributes
          Right (ComponentNode nodePosition componentName attributes children, afterChildren)

parseRegionNode :: ParseState -> [MarkupAttribute] -> Bool -> ParseState -> Either ParseError (MarkupNode, ParseState)
parseRegionNode initialState attributes selfClosing nextState
  | not selfClosing = parseFailure initialState "<Region> must be self-closing"
  | otherwise =
      case attributes of
        [ExpressionAttribute _ "value" expressionSource] ->
          Right (RegionNode (parsePosition initialState) expressionSource, nextState)
        _ -> parseFailure initialState "<Region> requires exactly one value={...} attribute"

data TagKind
  = NativeTag (Maybe String) Bool
  | ComponentTag String

tagKind :: String -> TagKind
tagKind tagName =
  case tagName of
    firstCharacter : _
      | isLower firstCharacter ->
          let maybeTagConstructor = nativeTagConstructor tagName
           in NativeTag maybeTagConstructor (maybe False (`elem` voidTagConstructors) maybeTagConstructor)
    _ -> ComponentTag (componentFunctionName tagName)

nativeTagConstructor :: String -> Maybe String
nativeTagConstructor tagName = lookup tagName nativeTagConstructors

nativeTagConstructors :: [(String, String)]
nativeTagConstructors =
  [ ("a", "anchorTag"),
    ("button", "buttonTag"),
    ("br", "breakTag"),
    ("code", "codeTag"),
    ("div", "divTag"),
    ("form", "formTag"),
    ("h1", "headingOneTag"),
    ("h2", "headingTwoTag"),
    ("hr", "horizontalRuleTag"),
    ("img", "imageTag"),
    ("input", "inputTag"),
    ("label", "labelTag"),
    ("li", "listItemTag"),
    ("meta", "metaTag"),
    ("option", "optionTag"),
    ("p", "paragraphTag"),
    ("section", "sectionTag"),
    ("select", "selectTag"),
    ("ul", "listTag")
  ]

voidTagConstructors :: [String]
voidTagConstructors = ["breakTag", "horizontalRuleTag", "imageTag", "inputTag", "metaTag"]

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
    firstCharacter : remaining -> toLower firstCharacter : remaining

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
            _ ->
              parseFailure
                afterEquals
                ("attribute " <> attributeName <> " must use a quoted literal or {...} expression")
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
        '\'' : _
          | startsCharacterLiteral accumulated state -> do
              (quoted, afterQuoted) <- parseDelimited '\'' state
              go depth (reverse quoted <> accumulated) afterQuoted
          | otherwise -> go depth ('\'' : accumulated) (advanceCharacter '\'' state)
        '{' : _ -> go (depth + 1) ('{' : accumulated) (advanceCharacter '{' state)
        '}' : _
          | depth == 1 -> Right (expressionPosition, reverse accumulated, advanceCharacter '}' state)
          | otherwise -> go (depth - 1) ('}' : accumulated) (advanceCharacter '}' state)
        character : _ -> go depth (character : accumulated) (advanceCharacter character state)

startsCharacterLiteral :: String -> ParseState -> Bool
startsCharacterLiteral accumulated state =
  case accumulated of
    previousCharacter : _
      | identifierCanEndWith previousCharacter -> False
    _ -> hasCharacterLiteralTerminator (drop 1 (parseRemaining state))

identifierCanEndWith :: Char -> Bool
identifierCanEndWith character = isAlphaNum character || character == '_' || character == '\''

hasCharacterLiteralTerminator :: String -> Bool
hasCharacterLiteralTerminator = go
  where
    go remaining =
      case remaining of
        [] -> False
        '\\' : _ : afterEscape -> go afterEscape
        ['\\'] -> False
        '\'' : _ -> True
        '}' : '\'' : _ -> True
        '}' : _ -> False
        _ : afterCharacter -> go afterCharacter

parseDelimited :: Char -> ParseState -> Either ParseError (String, ParseState)
parseDelimited delimiter initialState = do
  (_, afterOpen) <- consumeCharacter delimiter initialState
  go [delimiter] afterOpen
  where
    go accumulated state =
      case parseRemaining state of
        "" -> parseFailure initialState "unterminated string or character literal in {...} expression"
        '\\' : escapedCharacter : _ ->
          go (escapedCharacter : '\\' : accumulated) (advanceString ['\\', escapedCharacter] state)
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
        '&' : 'q' : 'u' : 'o' : 't' : ';' : remaining -> '"' : go remaining
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
   in loc_filename sourceLocation
        <> ":"
        <> show absoluteLine
        <> ":"
        <> show absoluteColumn
        <> ": harch: "
        <> message
