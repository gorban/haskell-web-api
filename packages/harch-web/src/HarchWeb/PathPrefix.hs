{-# LANGUAGE OverloadedStrings #-}

-- | Private normalization for browser-visible path prefixes.
module HarchWeb.PathPrefix
  ( applyPathPrefix,
    normalizePathPrefix,
    stripPathPrefix,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

normalizePathPrefix :: Text -> Text
normalizePathPrefix pathPrefix =
  let trimmedPrefix = Text.strip pathPrefix
      slashPrefixedPrefix =
        case (Text.null trimmedPrefix || trimmedPrefix == "/", Text.isPrefixOf "/" trimmedPrefix) of
          (True, _) -> Text.empty
          (False, True) -> trimmedPrefix
          (False, False) -> "/" <> trimmedPrefix
   in Text.dropWhileEnd (== '/') slashPrefixedPrefix

applyPathPrefix :: Text -> Text -> Text
applyPathPrefix pathPrefix path =
  let normalizedPrefix = normalizePathPrefix pathPrefix
   in if Text.null normalizedPrefix then path else if path == "/" then normalizedPrefix else normalizedPrefix <> path

stripPathPrefix :: Text -> Text -> Text
stripPathPrefix pathPrefix path =
  let normalizedPrefix = normalizePathPrefix pathPrefix
   in if Text.null normalizedPrefix
        then path
        else if path == normalizedPrefix then "/" else maybe path ("/" <>) (Text.stripPrefix (normalizedPrefix <> "/") path)
