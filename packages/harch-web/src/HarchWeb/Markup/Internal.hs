{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Markup.Internal
  ( Attribute (..),
    AttributeName (..),
    ElementId (..),
    Html (..),
    Node (..),
    Region (..),
    RegionId (..),
    RegionPatch (..),
    Tag (..),
    TrustedHtml (..),
    attribute,
    booleanAttribute,
    element,
    fragment,
    renderHtml,
    text,
    voidElement,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LazyText
import Data.Text.Lazy.Builder qualified as Builder

newtype Html = Html
  { htmlNodes :: [Node]
  }

instance Eq Html where
  left == right = renderHtml left == renderHtml right

instance Show Html where
  show = show . renderHtml

data Node
  = ElementNode Tag [Attribute] [Node]
  | VoidElementNode Tag [Attribute]
  | TextNode Text
  | TrustedNode TrustedHtml

newtype Tag = Tag
  { tagText :: Text
  }
  deriving (Eq, Show)

data Attribute = Attribute AttributeName (Maybe Text)

instance Eq Attribute where
  Attribute (AttributeName leftName) leftValue == Attribute (AttributeName rightName) rightValue =
    leftName == rightName && leftValue == rightValue

instance Show Attribute where
  show (Attribute (AttributeName name) value) = "Attribute " <> show name <> " " <> show value

newtype AttributeName = AttributeName
  { attributeNameText :: Text
  }

newtype ElementId = ElementId
  { elementIdText :: Text
  }
  deriving (Eq, Show)

newtype RegionId = RegionId
  { regionIdElementId :: ElementId
  }
  deriving (Eq, Show)

newtype TrustedHtml = TrustedHtml
  { trustedHtmlText :: Text
  }
  deriving (Eq, Show)

data Region = Region
  { regionIdentifier :: RegionId,
    regionRootTag :: Tag,
    regionAttributes :: [Attribute],
    regionChildren :: [Html]
  }
  deriving (Eq, Show)

newtype RegionPatch = ReplaceRegion
  { replacementRegion :: Region
  }
  deriving (Eq, Show)

text :: Text -> Html
text value = Html [TextNode value]

fragment :: [Html] -> Html
fragment children = Html (concatMap htmlNodes children)

element :: Tag -> [Attribute] -> [Html] -> Html
element tag attributes children =
  Html [ElementNode tag attributes (concatMap htmlNodes children)]

voidElement :: Tag -> [Attribute] -> Html
voidElement tag attributes = Html [VoidElementNode tag attributes]

attribute :: AttributeName -> Text -> Attribute
attribute name value = Attribute name (Just value)

booleanAttribute :: AttributeName -> Attribute
booleanAttribute name = Attribute name Nothing

renderHtml :: Html -> Text
renderHtml (Html nodes) = LazyText.toStrict (Builder.toLazyText (foldMap renderNode nodes))

renderNode :: Node -> Builder.Builder
renderNode node =
  case node of
    ElementNode tag attributes children ->
      "<"
        <> Builder.fromText (tagText tag)
        <> foldMap renderAttribute attributes
        <> ">"
        <> foldMap renderNode children
        <> "</"
        <> Builder.fromText (tagText tag)
        <> ">"
    VoidElementNode tag attributes ->
      "<" <> Builder.fromText (tagText tag) <> foldMap renderAttribute attributes <> ">"
    TextNode value -> escapeHtmlText value
    TrustedNode trustedHtml -> Builder.fromText (trustedHtmlText trustedHtml)

renderAttribute :: Attribute -> Builder.Builder
renderAttribute (Attribute name maybeValue) =
  case maybeValue of
    Just value ->
      " " <> Builder.fromText (attributeNameText name) <> "=\"" <> escapeHtmlAttribute value <> "\""
    Nothing -> " " <> Builder.fromText (attributeNameText name)

escapeHtmlText :: Text -> Builder.Builder
escapeHtmlText = escapeHtml

escapeHtmlAttribute :: Text -> Builder.Builder
escapeHtmlAttribute = escapeHtml

escapeHtml :: Text -> Builder.Builder
escapeHtml = Text.foldr (\character rendered -> escapeCharacter character <> rendered) mempty

escapeCharacter :: Char -> Builder.Builder
escapeCharacter character =
  case character of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '\"' -> "&quot;"
    -- Numeric references were supported by legacy HTML parsers that did not support `&apos;`.
    '\'' -> "&#39;"
    _ -> Builder.singleton character
