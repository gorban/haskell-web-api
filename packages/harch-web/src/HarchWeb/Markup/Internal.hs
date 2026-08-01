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
    element,
    fragment,
    renderHtml,
    text,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

newtype Html = Html
  { htmlNodes :: [Node]
  }
  deriving (Eq, Show)

data Node
  = ElementNode Tag [Attribute] [Node]
  | TextNode Text
  | TrustedNode TrustedHtml
  deriving (Eq, Show)

newtype Tag = Tag
  { tagText :: Text
  }
  deriving (Eq, Show)

data Attribute = Attribute AttributeName Text
  deriving (Eq, Show)

newtype AttributeName = AttributeName
  { attributeNameText :: Text
  }
  deriving (Eq, Show)

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

attribute :: AttributeName -> Text -> Attribute
attribute = Attribute

renderHtml :: Html -> Text
renderHtml (Html nodes) = Text.concat (map renderNode nodes)

renderNode :: Node -> Text
renderNode node =
  case node of
    ElementNode tag attributes children ->
      "<"
        <> tagText tag
        <> Text.concat (map renderAttribute attributes)
        <> ">"
        <> Text.concat (map renderNode children)
        <> "</"
        <> tagText tag
        <> ">"
    TextNode value -> escapeHtmlText value
    TrustedNode trustedHtml -> trustedHtmlText trustedHtml

renderAttribute :: Attribute -> Text
renderAttribute (Attribute name value) =
  " " <> attributeNameText name <> "=\"" <> escapeHtmlAttribute value <> "\""

escapeHtmlText :: Text -> Text
escapeHtmlText = Text.concatMap escapeCharacter

escapeHtmlAttribute :: Text -> Text
escapeHtmlAttribute = Text.concatMap escapeCharacter

escapeCharacter :: Char -> Text
escapeCharacter character =
  case character of
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '\"' -> "&quot;"
    '\'' -> "&#39;"
    _ -> Text.singleton character
