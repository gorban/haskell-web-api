{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Markup
  ( Attribute,
    ElementId,
    Html,
    Region,
    RegionId,
    RegionPatch,
    Tag,
    TrustedHtml,
    ariaLabel,
    className,
    divTag,
    element,
    elementId,
    fragment,
    headingOneTag,
    href,
    mkElementId,
    mkRegionId,
    paragraphTag,
    region,
    regionHtml,
    replaceRegion,
    role,
    sectionTag,
    text,
    trustedHtml,
    renderHtml,
  )
where

import Data.Text (Text)
import HarchWeb.Markup.Internal
  ( Attribute,
    AttributeName (..),
    ElementId,
    Html,
    Region (..),
    RegionId,
    RegionPatch,
    Tag (..),
    TrustedHtml,
    attribute,
    element,
    fragment,
    renderHtml,
    text,
  )
import HarchWeb.Markup.Internal qualified as Internal
import HarchWeb.StaticAssets (CssClass, cssClassText)

className :: CssClass -> Attribute
className = attribute (AttributeName "class") . cssClassText

ariaLabel :: Text -> Attribute
ariaLabel = attribute (AttributeName "aria-label")

role :: Text -> Attribute
role = attribute (AttributeName "role")

href :: Text -> Attribute
href = attribute (AttributeName "href")

elementId :: ElementId -> Attribute
elementId elementIdentifier =
  attribute (AttributeName "id") (Internal.elementIdText elementIdentifier)

mkElementId :: Text -> Maybe ElementId
mkElementId identifier
  | identifier == "" = Nothing
  | otherwise = Just (Internal.ElementId identifier)

mkRegionId :: ElementId -> RegionId
mkRegionId = Internal.RegionId

divTag :: Tag
divTag = Tag "div"

headingOneTag :: Tag
headingOneTag = Tag "h1"

paragraphTag :: Tag
paragraphTag = Tag "p"

sectionTag :: Tag
sectionTag = Tag "section"

trustedHtml :: TrustedHtml -> Html
trustedHtml value = Internal.Html [Internal.TrustedNode value]

region :: RegionId -> Tag -> [Attribute] -> [Html] -> Region
region identifier rootTag attributes children =
  Region
    { regionIdentifier = identifier,
      regionRootTag = rootTag,
      regionAttributes = filter (not . isReservedRegionAttribute) attributes,
      regionChildren = children
    }

regionHtml :: Region -> Html
regionHtml renderedRegion =
  element
    (regionRootTag renderedRegion)
    (regionFrameworkAttributes (regionIdentifier renderedRegion) <> regionAttributes renderedRegion)
    (regionChildren renderedRegion)

replaceRegion :: Region -> RegionPatch
replaceRegion = Internal.ReplaceRegion

regionFrameworkAttributes :: RegionId -> [Attribute]
regionFrameworkAttributes (Internal.RegionId identifier) =
  [ elementId identifier,
    attribute (AttributeName "data-harch-region") "true"
  ]

isReservedRegionAttribute :: Attribute -> Bool
isReservedRegionAttribute (Internal.Attribute name _) =
  name == AttributeName "id" || name == AttributeName "data-harch-region"
