{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Markup.Implementation
  ( Attribute,
    DataAttributeSuffix,
    ElementId,
    Html,
    MarkupContent (toHtml),
    Region,
    RegionId,
    RegionPatch,
    NormalTag,
    SafeUrl,
    TrustedHtml,
    VoidTag,
    anchorTag,
    ariaLabel,
    ariaLive,
    autocomplete,
    buttonTag,
    breakTag,
    className,
    codeTag,
    dataAttribute,
    dataAttributeSuffixText,
    dataFlag,
    divTag,
    element,
    elementId,
    enctype,
    fragment,
    headingOneTag,
    headingTwoTag,
    horizontalRuleTag,
    imageTag,
    formTag,
    formAction,
    href,
    hidden,
    inputTag,
    inputType,
    inputMode,
    lang,
    labelFor,
    labelTag,
    literalElementId,
    listItemTag,
    listTag,
    mainTag,
    metaTag,
    maxLength,
    method,
    minLength,
    mkDataAttributeSuffix,
    mkElementId,
    mkRegionId,
    mkSafeUrl,
    name,
    paragraphTag,
    optionTag,
    region,
    regionHtml,
    regionPatchHtml,
    regionPatchId,
    replaceRegion,
    required,
    requiredSafeUrlOrDie,
    role,
    safeUrlText,
    sectionTag,
    selectTag,
    text,
    trustedHtml,
    value,
    voidElement,
    renderHtml,
  )
where

import Data.Char qualified as Char
import Data.Maybe (fromMaybe)
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Markup.Internal
  ( Attribute,
    AttributeName (..),
    ElementId,
    Html,
    NormalTag (..),
    Region (..),
    RegionId,
    RegionPatch,
    TrustedHtml,
    VoidTag (..),
    attribute,
    booleanAttribute,
    element,
    fragment,
    renderHtml,
    text,
    voidElement,
  )
import HarchWeb.Markup.Internal qualified as Internal
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

-- | The suffix half of a @data-*@ attribute name (the part after
-- @data-@). Attribute values are HTML-escaped before rendering, but an
-- attribute *name* is written directly into markup with no escaping at
-- all — an unvalidated suffix could close the attribute early and inject
-- an event-handler attribute (e.g. @dataAttribute "x\" onmouseover=\"evil()"
-- "v"@). Restricted to the same character set HTML custom-data-attribute
-- names actually need: lowercase ASCII letters, digits, and hyphens.
--
-- Decision (BR, 2026-08-21, per @docs/design-guidance.md@'s
-- extend-vs-new-abstraction rule): this and 'SafeUrl' extend
-- 'HarchWeb.Markup.Internal.AttributeName''s existing (but previously
-- unvalidated) newtype boundary rather than adding a parallel checking
-- mechanism, and each carries an 'IsString' instance so every existing
-- @OverloadedStrings@ call site keeps compiling unchanged while a runtime
-- @Text@ value must go through the explicit smart constructor. See
-- @docs/design-guidance.md@'s \"Follow-up decision — BR\" for the full
-- record, including the allowlist-vs-blocklist call for 'SafeUrl' and the
-- quasiquoter capability gap this design surfaced.
newtype DataAttributeSuffix = DataAttributeSuffix Text
  deriving (Eq, Show)

dataAttributeSuffixText :: DataAttributeSuffix -> Text
dataAttributeSuffixText (DataAttributeSuffix suffix) = suffix

mkDataAttributeSuffix :: Text -> Maybe DataAttributeSuffix
mkDataAttributeSuffix suffix =
  if not (Text.null suffix) && Text.all isDataAttributeSuffixCharacter suffix
    then Just (DataAttributeSuffix suffix)
    else Nothing
  where
    isDataAttributeSuffixCharacter character = Char.isAsciiLower character || Char.isDigit character || character == '-'

-- | Lets an application author write @dataAttribute "harch-action" ...@ as
-- a plain string literal (every call site in this codebase already does):
-- 'OverloadedStrings' resolves the literal through this instance at
-- compile time against a value the author wrote themselves, not against
-- caller-supplied or untrusted text — 'mkDataAttributeSuffix' remains the
-- only way to validate a suffix built from a runtime 'Text' value. A
-- malformed literal is a programming mistake caught the first time the
-- page it appears on is rendered, the same failure mode 'OverloadedStrings'
-- literals of other validated types already accept throughout Haskell.
instance IsString DataAttributeSuffix where
  fromString suffix = fromMaybe (error ("invalid data-attribute suffix literal: " <> show suffix)) (mkDataAttributeSuffix (Text.pack suffix))

dataAttribute :: DataAttributeSuffix -> Text -> Attribute
dataAttribute attributeSuffix = attribute (AttributeName ("data-" <> dataAttributeSuffixText attributeSuffix))

dataFlag :: DataAttributeSuffix -> Attribute
dataFlag attributeSuffix = booleanAttribute (AttributeName ("data-" <> dataAttributeSuffixText attributeSuffix))

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

-- | A URL that cannot execute script when a browser navigates to it.
-- 'escapeHtmlAttribute' already protects the surrounding markup, but no
-- amount of HTML escaping stops a browser from running @javascript:@ (or
-- @data:text/html,…@, @vbscript:@, …) the moment a link is followed —
-- that requires validating the URL's own scheme, not just quoting it
-- safely into an attribute. Allowlisted, not blocklisted: only a relative
-- reference (no scheme at all) or an explicit @http@\/@https@ URL is
-- accepted, rather than naming every dangerous scheme and hoping the list
-- is complete.
newtype SafeUrl = SafeUrl Text
  deriving (Eq, Show)

safeUrlText :: SafeUrl -> Text
safeUrlText (SafeUrl url) = url

mkSafeUrl :: Text -> Maybe SafeUrl
mkSafeUrl url =
  if isSafeUrlScheme url then Just (SafeUrl url) else Nothing

-- | Browsers strip embedded tabs\/newlines\/carriage returns and leading
-- whitespace before determining a URL's effective scheme, so
-- @"java\\tscript:alert(1)"@ and @" javascript:alert(1)"@ both still run
-- as script despite not literally starting with @"javascript:"@. Stripping
-- the same characters here before reading the scheme closes that gap
-- rather than only catching the literal, unobfuscated case.
isSafeUrlScheme :: Text -> Bool
isSafeUrlScheme url =
  case Text.uncons remainderAfterScheme of
    Just (':', _) -> Text.toLower candidateScheme `elem` ["http", "https"]
    _ -> True
  where
    strippedUrl = Text.filter (`notElem` (" \t\n\r" :: String)) url
    candidateScheme = Text.takeWhile isSchemeCharacter strippedUrl
    remainderAfterScheme = Text.drop (Text.length candidateScheme) strippedUrl
    isSchemeCharacter character = Char.isAsciiUpper character || Char.isAsciiLower character || Char.isDigit character || character == '+' || character == '-' || character == '.'

-- | Mirrors 'DataAttributeSuffix'\'s 'IsString' instance: a plain string
-- literal is the application author's own text, validated once at compile
-- time against a value they wrote themselves. A caller building a URL from
-- a runtime 'Text' value (a redirect target, a user-supplied link, …) must
-- go through 'mkSafeUrl' and handle a rejected scheme explicitly.
instance IsString SafeUrl where
  fromString url = fromMaybe (error ("invalid or unsafe URL literal: " <> show url)) (mkSafeUrl (Text.pack url))

-- | For code that renders a URL from a fixed, typed route table rather than
-- from a string literal (so 'IsString' does not apply) and can show, by
-- construction, that the rendered text will always satisfy 'mkSafeUrl' — a
-- typed route renderer covering a closed set of constructors, for example.
-- A rejection here means the route table itself was defined to render an
-- unsafe URL, a programming mistake in that renderer, not a runtime
-- condition its callers need to handle. Follows the same shape as
-- @WebApi.Login@'s @requiredPasswordHashOrDie@: extracted into its own
-- named, exported helper so a dedicated test can force the failure path
-- directly with a deliberately unsafe 'Maybe' value, leaving every real
-- call site's own coverage untouched by the branch it can never take.
requiredSafeUrlOrDie :: Text -> Maybe SafeUrl -> SafeUrl
requiredSafeUrlOrDie context = fromMaybe (error ("HarchWeb.Markup: " <> Text.unpack context))

href :: SafeUrl -> Attribute
href = attribute (AttributeName "href") . safeUrlText

hidden :: Attribute
hidden = booleanAttribute (AttributeName "hidden")

inputType :: Text -> Attribute
inputType = attribute (AttributeName "type")

inputMode :: Text -> Attribute
inputMode = attribute (AttributeName "inputmode")

-- | The document language of a native HTML element.  This is a closed,
-- escaping attribute primitive rather than an arbitrary-attribute escape
-- hatch: dynamic language values remain escaped by the shared renderer.
--
-- Decision (MX, 2026-08-27): @main@ and @lang@ are standard, general HTML
-- vocabulary squarely owned by this existing markup AST and quasiquoter.
-- Extend that closed vocabulary rather than adding an application-only raw
-- HTML path or a parallel arbitrary-attribute abstraction.
lang :: Text -> Attribute
lang = attribute (AttributeName "lang")

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

divTag :: NormalTag
divTag = NormalTag "div"

anchorTag :: NormalTag
anchorTag = NormalTag "a"

buttonTag :: NormalTag
buttonTag = NormalTag "button"

formTag :: NormalTag
formTag = NormalTag "form"

headingOneTag :: NormalTag
headingOneTag = NormalTag "h1"

headingTwoTag :: NormalTag
headingTwoTag = NormalTag "h2"

inputTag :: VoidTag
inputTag = VoidTag "input"

labelTag :: NormalTag
labelTag = NormalTag "label"

listItemTag :: NormalTag
listItemTag = NormalTag "li"

listTag :: NormalTag
listTag = NormalTag "ul"

mainTag :: NormalTag
mainTag = NormalTag "main"

codeTag :: NormalTag
codeTag = NormalTag "code"

selectTag :: NormalTag
selectTag = NormalTag "select"

optionTag :: NormalTag
optionTag = NormalTag "option"

paragraphTag :: NormalTag
paragraphTag = NormalTag "p"

sectionTag :: NormalTag
sectionTag = NormalTag "section"

breakTag :: VoidTag
breakTag = VoidTag "br"

horizontalRuleTag :: VoidTag
horizontalRuleTag = VoidTag "hr"

imageTag :: VoidTag
imageTag = VoidTag "img"

metaTag :: VoidTag
metaTag = VoidTag "meta"

trustedHtml :: TrustedHtml -> Html
trustedHtml trustedValue = Internal.Html [Internal.TrustedNode trustedValue]

region :: RegionId -> NormalTag -> [Attribute] -> [Html] -> Region
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
isReservedRegionAttribute (Internal.Attribute (AttributeName attributeName) _) =
  attributeName == "id" || attributeName == "data-harch-region"
