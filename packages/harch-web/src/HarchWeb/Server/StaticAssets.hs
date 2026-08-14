{-# LANGUAGE OverloadedStrings #-}

-- | Private filesystem dispatch for configured static assets.
module HarchWeb.Server.StaticAssets
  ( serveStaticAssetResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf, maximumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.StaticAssets (StaticAssetRoot (..), StaticAssetsConfig (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (joinPath, pathSeparator, takeExtension, (</>))

serveStaticAssetResponse :: StaticAssetsConfig -> Text -> IO (Maybe (Text, Wai.Response))
serveStaticAssetResponse staticAssetsConfig requestPath =
  case matchStaticAssetRoot staticAssetsConfig requestPath of
    Nothing -> pure Nothing
    Just (matchedRoot, relativeAssetPath) ->
      case sanitizeStaticAssetPath relativeAssetPath of
        Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))
        Just safeAssetPath ->
          case staticAssetContentType staticAssetsConfig safeAssetPath of
            Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))
            Just assetContentType -> do
              let assetFilePath = staticDirectory matchedRoot </> safeAssetPath
              maybeAssetContents <- readStaticAssetWithinRoot (staticDirectory matchedRoot) assetFilePath
              case maybeAssetContents of
                Just assetContents ->
                  pure
                    ( Just
                        ( staticAssetRoutePath matchedRoot,
                          Wai.responseLBS
                            Http.status200
                            (staticAssetHeaders staticAssetsConfig assetContentType)
                            (LazyByteString.fromStrict assetContents)
                        )
                    )
                Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))

-- | Read an asset only when it both exists and canonicalizes to a path still
-- rooted under the configured static directory. 'sanitizeStaticAssetPath'
-- already rejects every segment that could make 'assetFilePath' escape the
-- root by construction; this closes the remaining escape a symlink planted
-- inside the root could open.
readStaticAssetWithinRoot :: FilePath -> FilePath -> IO (Maybe ByteString.ByteString)
readStaticAssetWithinRoot rootDirectory assetFilePath = do
  assetExists <- doesFileExist assetFilePath
  if not assetExists
    then pure Nothing
    else do
      canonicalRoot <- canonicalizePath rootDirectory
      canonicalAsset <- canonicalizePath assetFilePath
      if (canonicalRoot <> [pathSeparator]) `isPrefixOf` canonicalAsset
        then Just <$> ByteString.readFile assetFilePath
        else pure Nothing

staticAssetRoutePath :: StaticAssetRoot -> Text
staticAssetRoutePath staticRoot =
  case staticUrlPrefix staticRoot of
    "/" -> "/*"
    staticPrefix -> staticPrefix <> "/*"

matchStaticAssetRoot :: StaticAssetsConfig -> Text -> Maybe (StaticAssetRoot, Text)
matchStaticAssetRoot staticAssetsConfig requestPath =
  case matchedRoots of
    [] -> Nothing
    _ -> Just (maximumBy compareStaticPrefixLength matchedRoots)
  where
    matchedRoots =
      [ (staticRoot, assetPath)
      | staticRoot <- staticAssetRoots staticAssetsConfig,
        Just assetPath <- [stripStaticPrefix (staticUrlPrefix staticRoot) requestPath]
      ]

    compareStaticPrefixLength (leftRoot, _) (rightRoot, _) =
      compare (Text.length (staticUrlPrefix leftRoot)) (Text.length (staticUrlPrefix rightRoot))

stripStaticPrefix :: Text -> Text -> Maybe Text
stripStaticPrefix configuredPrefix requestPath =
  let normalizedPrefix = normalizeStaticAssetRoutePrefix configuredPrefix
   in if Text.null normalizedPrefix
        then
          if requestPath == "/"
            then Just Text.empty
            else Text.stripPrefix "/" requestPath
        else
          if requestPath == normalizedPrefix
            then Just Text.empty
            else Text.stripPrefix (normalizedPrefix <> "/") requestPath

normalizeStaticAssetRoutePrefix :: Text -> Text
normalizeStaticAssetRoutePrefix prefix =
  fromMaybe prefix (Text.stripSuffix "/" prefix)

-- | Validate and translate a request-relative asset path into a genuinely
-- relative 'FilePath'. Every segment must be non-empty and drawn from a
-- small allowlist of ASCII filename characters, which rejects a leading
-- @\/@ segment (the absolute-path escape: 'System.FilePath.</>' discards its
-- left operand when the right one is absolute), @.@/@..@ segments, and
-- hidden (dotfile) segments in one predicate instead of three.
--
-- 'Text.splitOn' on a non-empty separator always returns a non-empty list
-- (the empty-path case is a single-element list containing an empty
-- segment), so there is no empty-list case to guard against here.
sanitizeStaticAssetPath :: Text -> Maybe FilePath
sanitizeStaticAssetPath relativeAssetPath =
  let segments = Text.splitOn "/" relativeAssetPath
   in if all isSafeStaticAssetSegment segments
        then Just (joinPath (map Text.unpack segments))
        else Nothing

isSafeStaticAssetSegment :: Text -> Bool
isSafeStaticAssetSegment segment =
  not (Text.null segment)
    && not ("." `Text.isPrefixOf` segment)
    && Text.all isSafeStaticAssetSegmentChar segment

isSafeStaticAssetSegmentChar :: Char -> Bool
isSafeStaticAssetSegmentChar character =
  isAsciiLower character
    || isAsciiUpper character
    || isDigit character
    || character `elem` ("._-" :: String)

staticAssetHeaders :: StaticAssetsConfig -> Text -> Http.ResponseHeaders
staticAssetHeaders staticAssetsConfig assetContentType =
  (Http.hContentType, TextEncoding.encodeUtf8 assetContentType)
    : maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)

staticCacheControlHeaderValue :: StaticAssetsConfig -> Maybe Text
staticCacheControlHeaderValue staticAssetsConfig =
  fmap (\seconds -> Text.pack ("public, max-age=" <> show seconds)) (staticCacheControlSeconds staticAssetsConfig)

staticAssetContentType :: StaticAssetsConfig -> FilePath -> Maybe Text
staticAssetContentType staticAssetsConfig assetFilePath =
  lookup (Text.pack (takeExtension assetFilePath)) (staticAssetContentTypes staticAssetsConfig)

missingStaticAssetResponse :: StaticAssetsConfig -> Wai.Response
missingStaticAssetResponse staticAssetsConfig =
  Wai.responseLBS
    Http.status404
    ( (Http.hContentType, TextEncoding.encodeUtf8 "text/plain; charset=utf-8")
        : maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)
    )
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 "Not Found"))
