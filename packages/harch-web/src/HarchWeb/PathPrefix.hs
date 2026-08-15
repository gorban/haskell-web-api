{-# LANGUAGE OverloadedStrings #-}

-- | Private normalization for browser-visible path prefixes.
module HarchWeb.PathPrefix
  ( PathPrefix,
    UrlPath,
    applyPathPrefix,
    mkPathPrefix,
    mkUrlPath,
    normalizePathPrefix,
    pathPrefixText,
    stripPathPrefix,
    urlPathText,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

-- | A configured mount-point prefix (for example, an @X-Forwarded-Prefix@
-- value). Opaque so it cannot be transposed with 'UrlPath' at an
-- 'applyPathPrefix'/'stripPathPrefix' call site.
newtype PathPrefix = PathPrefix Text

mkPathPrefix :: Text -> PathPrefix
mkPathPrefix = PathPrefix

pathPrefixText :: PathPrefix -> Text
pathPrefixText (PathPrefix value) = value

-- | A browser-visible URL path being prefixed or stripped.
newtype UrlPath = UrlPath Text

mkUrlPath :: Text -> UrlPath
mkUrlPath = UrlPath

urlPathText :: UrlPath -> Text
urlPathText (UrlPath value) = value

normalizePathPrefix :: Text -> Text
normalizePathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> Text.empty
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
   in Text.dropWhileEnd (== '/') slashPrefixedPrefix

applyPathPrefix :: PathPrefix -> UrlPath -> UrlPath
applyPathPrefix (PathPrefix pathPrefix) (UrlPath path) =
  let normalizedPrefix = normalizePathPrefix pathPrefix
   in UrlPath
        (if Text.null normalizedPrefix then path else if path == "/" then normalizedPrefix else normalizedPrefix <> path)

stripPathPrefix :: PathPrefix -> UrlPath -> UrlPath
stripPathPrefix (PathPrefix pathPrefix) (UrlPath path) =
  let normalizedPrefix = normalizePathPrefix pathPrefix
   in UrlPath
        ( if Text.null normalizedPrefix
            then path
            else if path == normalizedPrefix then "/" else maybe path ("/" <>) (Text.stripPrefix (normalizedPrefix <> "/") path)
        )
