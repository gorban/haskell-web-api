{-# LANGUAGE OverloadedStrings #-}

-- | Private normalization for browser-visible path prefixes.
module HarchWeb.PathPrefix
  ( PathPrefix,
    PathPrefixError (..),
    UrlPath,
    applyPathPrefix,
    emptyPathPrefix,
    mkUrlPath,
    parsePathPrefix,
    pathPrefixText,
    stripPathPrefix,
    urlPathText,
  )
where

import Data.Char (isAlphaNum, isAscii, isControl)
import Data.Text (Text)
import Data.Text qualified as Text

-- | A validated browser-visible mount-point prefix.  It is opaque so callers
-- cannot transpose it with 'UrlPath' at an 'applyPathPrefix'/'stripPathPrefix'
-- call site or manufacture an unvalidated forwarded value.
newtype PathPrefix = PathPrefix Text
  deriving (Eq, Show)

-- | A low-cardinality reason a forwarded mount prefix was rejected.  The raw
-- header is deliberately absent: it can originate with a client even when a
-- proxy is trusted to forward it.
data PathPrefixError
  = PathPrefixMultipleSlashes
  | PathPrefixUnsafeCharacter
  | PathPrefixAmbiguousSegment
  deriving (Eq, Show)

emptyPathPrefix :: PathPrefix
emptyPathPrefix = PathPrefix Text.empty

pathPrefixText :: PathPrefix -> Text
pathPrefixText (PathPrefix value) = value

-- | A browser-visible URL path being prefixed or stripped.
newtype UrlPath = UrlPath Text

mkUrlPath :: Text -> UrlPath
mkUrlPath = UrlPath

urlPathText :: UrlPath -> Text
urlPathText (UrlPath value) = value

-- | Parse an optional leading-slash mount prefix into a canonical absolute
-- path.  Prefix segments are deliberately limited to URI unreserved ASCII
-- characters; accepting percent escapes, backslashes, delimiters, controls,
-- or empty segments would make browser interpretation differ from route
-- matching.  A root value is represented by 'emptyPathPrefix'.
parsePathPrefix :: Text -> Either PathPrefixError PathPrefix
parsePathPrefix rawPrefix
  | Text.any isControl rawPrefix = Left PathPrefixUnsafeCharacter
  | Text.null trimmedPrefix || trimmedPrefix == "/" = Right emptyPathPrefix
  | Text.isPrefixOf "//" trimmedPrefix || Text.isInfixOf "//" trimmedPrefix = Left PathPrefixMultipleSlashes
  | otherwise =
      let normalizedPrefix =
            Text.dropWhileEnd
              (== '/')
              (if Text.isPrefixOf "/" trimmedPrefix then trimmedPrefix else "/" <> trimmedPrefix)
          segments = Text.splitOn "/" (Text.drop 1 normalizedPrefix)
       in if any (`elem` [".", ".."]) segments
            then Left PathPrefixAmbiguousSegment
            else
              if all (Text.all isSafePathSegmentCharacter) segments
                then Right (PathPrefix normalizedPrefix)
                else Left PathPrefixUnsafeCharacter
  where
    trimmedPrefix = Text.strip rawPrefix

isSafePathSegmentCharacter :: Char -> Bool
isSafePathSegmentCharacter character =
  isAscii character && (isAlphaNum character || character `elem` ['-', '.', '_', '~'])

applyPathPrefix :: PathPrefix -> UrlPath -> UrlPath
applyPathPrefix (PathPrefix pathPrefix) (UrlPath path) =
  UrlPath
    (if Text.null pathPrefix then path else if path == "/" then pathPrefix else pathPrefix <> path)

stripPathPrefix :: PathPrefix -> UrlPath -> UrlPath
stripPathPrefix (PathPrefix pathPrefix) (UrlPath path) =
  UrlPath
    ( if Text.null pathPrefix
        then path
        else if path == pathPrefix then "/" else maybe path ("/" <>) (Text.stripPrefix (pathPrefix <> "/") path)
    )
