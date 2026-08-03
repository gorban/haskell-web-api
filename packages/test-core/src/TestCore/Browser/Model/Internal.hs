{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module TestCore.Browser.Model.Internal
  ( AriaRole (..),
    BrowserObservation,
    CompiledObservation (..),
    Locator,
    attributeValue,
    browserMetrics,
    byAltText,
    byLabel,
    byPlaceholder,
    byRole,
    byTestId,
    byText,
    byTitle,
    compileObservation,
    containingText,
    css,
    currentUrl,
    fromJsonResult,
    inputValue,
    isFocused,
    isVisible,
    named,
    textContent,
    within,
  )
where

import Data.Aeson (FromJSON, Result (..), ToJSON (toJSON), Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encoding qualified as AesonEncoding
import Data.Text (Text)
import Data.Text qualified as Text
import TestCore.Browser.Types (BrowserMetrics)

data AriaRole
  = Button
  | Checkbox
  | Form
  | Heading
  | Link
  | List
  | ListItem
  | Navigation
  | Radio
  | Status
  | Textbox
  deriving (Show)

data Locator
  = RoleLocator AriaRole (Maybe Text)
  | LabelLocator Text
  | TextLocator Text
  | PlaceholderLocator Text
  | AltTextLocator Text
  | TitleLocator Text
  | TestIdLocator Text
  | CssLocator Text
  | WithinLocator Locator Locator
  | ContainingTextLocator Locator Text

instance ToJSON Locator where
  toJSON locator =
    case locator of
      RoleLocator role accessibleName ->
        object
          [ "kind" .= ("role" :: Text),
            "role" .= renderAriaRole role,
            "name" .= accessibleName
          ]
      LabelLocator labelText -> textLocatorJson "label" labelText
      TextLocator visibleText -> textLocatorJson "text" visibleText
      PlaceholderLocator placeholderText -> textLocatorJson "placeholder" placeholderText
      AltTextLocator alternativeText -> textLocatorJson "altText" alternativeText
      TitleLocator titleText -> textLocatorJson "title" titleText
      TestIdLocator testId -> textLocatorJson "testId" testId
      CssLocator selector -> textLocatorJson "css" selector
      WithinLocator parent child ->
        object
          [ "kind" .= ("within" :: Text),
            "parent" .= parent,
            "child" .= child
          ]
      ContainingTextLocator target containedText ->
        object
          [ "kind" .= ("containingText" :: Text),
            "locator" .= target,
            "text" .= containedText
          ]
    where
      textLocatorJson kind value = object ["kind" .= (kind :: Text), "text" .= value]

  toEncoding = AesonEncoding.value . toJSON
  toEncodingList = AesonEncoding.value . Aeson.toJSONList

renderAriaRole :: AriaRole -> Text
renderAriaRole = Text.toLower . Text.pack . show

byRole :: AriaRole -> Locator
byRole role = RoleLocator role Nothing

named :: Locator -> Text -> Locator
named locator accessibleName =
  case locator of
    RoleLocator role _ -> RoleLocator role (Just accessibleName)
    _ -> ContainingTextLocator locator accessibleName

byLabel :: Text -> Locator
byLabel = LabelLocator

byText :: Text -> Locator
byText = TextLocator

byPlaceholder :: Text -> Locator
byPlaceholder = PlaceholderLocator

byAltText :: Text -> Locator
byAltText = AltTextLocator

byTitle :: Text -> Locator
byTitle = TitleLocator

byTestId :: Text -> Locator
byTestId = TestIdLocator

css :: Text -> Locator
css = CssLocator

within :: Locator -> Locator -> Locator
within = WithinLocator

containingText :: Locator -> Text -> Locator
containingText = ContainingTextLocator

data ObservationLeaf a where
  TextContentObservation :: Locator -> ObservationLeaf Text
  InputValueObservation :: Locator -> ObservationLeaf Text
  AttributeValueObservation :: Locator -> Text -> ObservationLeaf (Maybe Text)
  FocusedObservation :: Locator -> ObservationLeaf Bool
  VisibleObservation :: Locator -> ObservationLeaf Bool
  CurrentUrlObservation :: ObservationLeaf Text
  BrowserMetricsObservation :: ObservationLeaf BrowserMetrics

data BrowserObservation a where
  PureObservation :: a -> BrowserObservation a
  MapObservation :: (a -> b) -> BrowserObservation a -> BrowserObservation b
  ApplyObservation :: BrowserObservation (a -> b) -> BrowserObservation a -> BrowserObservation b
  LeafObservation :: (FromJSON a) => ObservationLeaf a -> BrowserObservation a

instance Functor BrowserObservation where
  fmap = MapObservation

instance Applicative BrowserObservation where
  pure = PureObservation
  (<*>) = ApplyObservation

textContent :: Locator -> BrowserObservation Text
textContent = LeafObservation . TextContentObservation

inputValue :: Locator -> BrowserObservation Text
inputValue = LeafObservation . InputValueObservation

attributeValue :: Locator -> Text -> BrowserObservation (Maybe Text)
attributeValue locator attributeName = LeafObservation (AttributeValueObservation locator attributeName)

isFocused :: Locator -> BrowserObservation Bool
isFocused = LeafObservation . FocusedObservation

isVisible :: Locator -> BrowserObservation Bool
isVisible = LeafObservation . VisibleObservation

currentUrl :: BrowserObservation Text
currentUrl = LeafObservation CurrentUrlObservation

browserMetrics :: BrowserObservation BrowserMetrics
browserMetrics = LeafObservation BrowserMetricsObservation

data CompiledObservation a = CompiledObservation
  { compiledRequests :: [Value],
    decodeCompiledValues :: [Value] -> Either String (a, [Value])
  }

compileObservation :: BrowserObservation a -> CompiledObservation a
compileObservation observation =
  case observation of
    PureObservation value -> CompiledObservation [] (\remaining -> Right (value, remaining))
    MapObservation transform child ->
      let compiledChild = compileObservation child
       in CompiledObservation
            (compiledRequests compiledChild)
            ( \values -> do
                (childValue, remaining) <- decodeCompiledValues compiledChild values
                pure (transform childValue, remaining)
            )
    ApplyObservation functionObservation valueObservation ->
      let compiledFunction = compileObservation functionObservation
          compiledValue = compileObservation valueObservation
       in CompiledObservation
            (compiledRequests compiledFunction <> compiledRequests compiledValue)
            ( \values -> do
                (functionValue, afterFunction) <- decodeCompiledValues compiledFunction values
                (argumentValue, remaining) <- decodeCompiledValues compiledValue afterFunction
                pure (functionValue argumentValue, remaining)
            )
    LeafObservation leaf ->
      CompiledObservation
        [observationLeafJson leaf]
        ( \case
            [] -> Left "browser runner omitted an observation value"
            value : remaining -> do
              decodedValue <- fromJsonResult value
              Right (decodedValue, remaining)
        )

observationLeafJson :: ObservationLeaf a -> Value
observationLeafJson leaf =
  case leaf of
    TextContentObservation locator -> locatedObservation "textContent" locator []
    InputValueObservation locator -> locatedObservation "inputValue" locator []
    AttributeValueObservation locator attributeName -> locatedObservation "attributeValue" locator ["attribute" .= attributeName]
    FocusedObservation locator -> locatedObservation "focused" locator []
    VisibleObservation locator -> locatedObservation "visible" locator []
    CurrentUrlObservation -> object ["kind" .= ("currentUrl" :: Text)]
    BrowserMetricsObservation -> object ["kind" .= ("browserMetrics" :: Text)]
  where
    locatedObservation kind locator fields =
      object (["kind" .= (kind :: Text), "locator" .= locator] <> fields)

fromJsonResult :: (FromJSON a) => Value -> Either String a
fromJsonResult value =
  case Aeson.fromJSON value of
    Error message -> Left message
    Success decodedValue -> Right decodedValue
