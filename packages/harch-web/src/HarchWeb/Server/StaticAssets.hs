{-# LANGUAGE OverloadedStrings #-}

-- | Private filesystem dispatch for configured static assets.
module HarchWeb.Server.StaticAssets
  ( serveStaticAssetResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (maximumBy)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.StaticAssets (StaticAssetRoot (..), StaticAssetsConfig (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (doesFileExist)
import System.FilePath (splitDirectories, takeExtension, (</>))

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
              assetExists <- doesFileExist assetFilePath
              case assetExists of
                True -> do
                  assetContents <- ByteString.readFile assetFilePath
                  pure
                    ( Just
                        ( staticAssetRoutePath matchedRoot,
                          Wai.responseLBS
                            Http.status200
                            (staticAssetHeaders staticAssetsConfig assetContentType)
                            (LazyByteString.fromStrict assetContents)
                        )
                    )
                False -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse staticAssetsConfig))

staticAssetRoutePath :: StaticAssetRoot -> Text
staticAssetRoutePath staticRoot =
  case staticUrlPrefix staticRoot of
    "/" -> "/*"
    staticPrefix -> staticPrefix <> "/*"

matchStaticAssetRoot :: StaticAssetsConfig -> Text -> Maybe (StaticAssetRoot, FilePath)
matchStaticAssetRoot staticAssetsConfig requestPath =
  case matchedRoots of
    [] -> Nothing
    _ -> Just (maximumBy compareStaticPrefixLength matchedRoots)
  where
    matchedRoots =
      [ (staticRoot, Text.unpack assetPath)
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

sanitizeStaticAssetPath :: FilePath -> Maybe FilePath
sanitizeStaticAssetPath assetPath =
  case splitDirectories assetPath of
    [] -> Nothing
    segments ->
      if all isSafeSegment segments
        then Just assetPath
        else Nothing
  where
    isSafeSegment segment =
      not (null segment)
        && segment /= "."
        && segment /= ".."
        && not (isHiddenStaticAssetSegment segment)

isHiddenStaticAssetSegment :: FilePath -> Bool
isHiddenStaticAssetSegment segment =
  case segment of
    '.' : _ -> True
    _ -> False

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
