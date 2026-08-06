{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Markup.Implementation
  ( Attribute,
    ElementId,
    Html,
    MarkupContent (toHtml),
    Region,
    RegionId,
    RegionPatch,
    Tag,
    TrustedHtml,
    anchorTag,
    ariaLabel,
    ariaLive,
    autocomplete,
    buttonTag,
    className,
    codeTag,
    dataAttribute,
    dataFlag,
    divTag,
    element,
    elementId,
    enctype,
    fragment,
    headingOneTag,
    headingTwoTag,
    formTag,
    formAction,
    href,
    hidden,
    inputTag,
    inputType,
    inputMode,
    harch,
    labelFor,
    labelTag,
    literalElementId,
    listItemTag,
    listTag,
    maxLength,
    method,
    minLength,
    mkElementId,
    mkRegionId,
    name,
    paragraphTag,
    optionTag,
    region,
    regionHtml,
    regionPatchHtml,
    regionPatchId,
    replaceRegion,
    required,
    role,
    sectionTag,
    selectTag,
    text,
    trustedHtml,
    value,
    voidElement,
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
    booleanAttribute,
    element,
    fragment,
    renderHtml,
    text,
    voidElement,
  )
import HarchWeb.Markup.Internal qualified as Internal
import HarchWeb.Markup.Quasi (harch)
import HarchWeb.StaticAssets (CssClass, cssClassText)

-- | Values that may be embedded between markup tags. Text is escaped by the
-- existing 'text' constructor; 'Html' remains safe because it is already AST.
class MarkupContent value where
  toHtml :: value -> Html

instance MarkupContent Html where
  toHtml = id

instance MarkupContent Text where
  toHtml = text

instance MarkupContent [Html] where
  toHtml = fragment

className :: CssClass -> Attribute
className = attribute (AttributeName "class") . cssClassText

dataAttribute :: Text -> Text -> Attribute
dataAttribute attributeSuffix = attribute (AttributeName ("data-" <> attributeSuffix))

dataFlag :: Text -> Attribute
dataFlag attributeSuffix = booleanAttribute (AttributeName ("data-" <> attributeSuffix))

formAction :: Text -> Attribute
formAction = attribute (AttributeName "action")

ariaLabel :: Text -> Attribute
ariaLabel = attribute (AttributeName "aria-label")

ariaLive :: Text -> Attribute
ariaLive = attribute (AttributeName "aria-live")

autocomplete :: Text -> Attribute
autocomplete = attribute (AttributeName "autocomplete")

role :: Text -> Attribute
role = attribute (AttributeName "role")

href :: Text -> Attribute
href = attribute (AttributeName "href")

hidden :: Attribute
hidden = booleanAttribute (AttributeName "hidden")

inputType :: Text -> Attribute
inputType = attribute (AttributeName "type")

inputMode :: Text -> Attribute
inputMode = attribute (AttributeName "inputmode")

labelFor :: ElementId -> Attribute
labelFor elementIdentifier =
  attribute (AttributeName "for") (Internal.elementIdText elementIdentifier)

method :: Text -> Attribute
method = attribute (AttributeName "method")

-- | A form's @enctype@, e.g. @multipart\/form-data@ for a native file
-- upload. Only meaningful on a @method="post"@ form.
enctype :: Text -> Attribute
enctype = attribute (AttributeName "enctype")

minLength :: Text -> Attribute
minLength = attribute (AttributeName "minlength")

maxLength :: Text -> Attribute
maxLength = attribute (AttributeName "maxlength")

name :: Text -> Attribute
name = attribute (AttributeName "name")

required :: Attribute
required = booleanAttribute (AttributeName "required")

value :: Text -> Attribute
value = attribute (AttributeName "value")

elementId :: ElementId -> Attribute
elementId elementIdentifier =
  attribute (AttributeName "id") (Internal.elementIdText elementIdentifier)

mkElementId :: Text -> Maybe ElementId
mkElementId identifier
  | identifier == "" = Nothing
  | otherwise = Just (Internal.ElementId identifier)

-- | An opaque ID for a framework-owned literal. Dynamic or user-provided IDs
-- must use 'mkElementId' so validation remains explicit at the boundary.
literalElementId :: Text -> ElementId
literalElementId = Internal.ElementId

mkRegionId :: ElementId -> RegionId
mkRegionId = Internal.RegionId

divTag :: Tag
divTag = Tag "div"

anchorTag :: Tag
anchorTag = Tag "a"

buttonTag :: Tag
buttonTag = Tag "button"

formTag :: Tag
formTag = Tag "form"

headingOneTag :: Tag
headingOneTag = Tag "h1"

headingTwoTag :: Tag
headingTwoTag = Tag "h2"

inputTag :: Tag
inputTag = Tag "input"

labelTag :: Tag
labelTag = Tag "label"

listItemTag :: Tag
listItemTag = Tag "li"

listTag :: Tag
listTag = Tag "ul"

codeTag :: Tag
codeTag = Tag "code"

selectTag :: Tag
selectTag = Tag "select"

optionTag :: Tag
optionTag = Tag "option"

paragraphTag :: Tag
paragraphTag = Tag "p"

sectionTag :: Tag
sectionTag = Tag "section"

trustedHtml :: TrustedHtml -> Html
trustedHtml trustedValue = Internal.Html [Internal.TrustedNode trustedValue]

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

regionPatchId :: RegionPatch -> Text
regionPatchId (Internal.ReplaceRegion renderedRegion) =
  case regionIdentifier renderedRegion of
    Internal.RegionId (Internal.ElementId identifier) -> identifier

regionPatchHtml :: RegionPatch -> Text
regionPatchHtml (Internal.ReplaceRegion renderedRegion) = renderHtml (regionHtml renderedRegion)

regionFrameworkAttributes :: RegionId -> [Attribute]
regionFrameworkAttributes (Internal.RegionId identifier) =
  [ elementId identifier,
    attribute (AttributeName "data-harch-region") "true"
  ]

isReservedRegionAttribute :: Attribute -> Bool
isReservedRegionAttribute (Internal.Attribute attributeName _) =
  attributeName == AttributeName "id" || attributeName == AttributeName "data-harch-region"
