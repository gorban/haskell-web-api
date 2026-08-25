{-# LANGUAGE OverloadedStrings #-}

-- | Private filesystem dispatch for configured static assets.
--
-- A matched asset is authenticated against its canonical configured root before
-- its metadata is read. Successful GET responses remain file-backed
-- 'Wai.responseFile' values (and therefore let Warp use its sendfile path),
-- while a request validator or a single byte range selects the corresponding
-- RFC 9110 response without buffering the file in the framework.  The
-- filesystem is owned by the configured application root; request input only
-- selects a safe relative name and cannot choose a file handle or storage
-- backend.
module HarchWeb.Server.StaticAssets
  ( serveStaticAssetResponse,
  )
where

import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf, maximumBy)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Word (Word8)
import HarchWeb.StaticAssets (StaticAssetRoot (..), StaticAssetsConfig (..))
import Network.HTTP.Date (formatHTTPDate, httpDateToUTC, parseHTTPDate, utcToHTTPDate)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath (joinPath, pathSeparator, takeExtension, (</>))
import System.Posix.Files (fileSize, getFileStatus, modificationTimeHiRes)
import Text.Read (readMaybe)

serveStaticAssetResponse :: StaticAssetsConfig -> Wai.Request -> Text -> IO (Maybe (Text, Wai.Response))
serveStaticAssetResponse staticAssetsConfig request requestPath =
  case matchStaticAssetRoot staticAssetsConfig requestPath of
    Nothing -> pure Nothing
    Just (matchedRoot, relativeAssetPath) ->
      case sanitizeStaticAssetPath relativeAssetPath of
        Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse))
        Just safeAssetPath ->
          case staticAssetContentType staticAssetsConfig safeAssetPath of
            Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse))
            Just assetContentType -> do
              let assetFilePath = staticDirectory matchedRoot </> safeAssetPath
              maybeAssetFile <- staticAssetFileWithinRoot (staticDirectory matchedRoot) assetFilePath
              case maybeAssetFile of
                Just assetFile ->
                  pure
                    ( Just
                        ( staticAssetRoutePath matchedRoot,
                          staticAssetResponse staticAssetsConfig request assetContentType assetFile
                        )
                    )
                Nothing -> pure (Just (staticAssetRoutePath matchedRoot, missingStaticAssetResponse))

data StaticAssetFile = StaticAssetFile
  { staticAssetFilePath :: FilePath,
    staticAssetFileSize :: Integer,
    staticAssetFileModifiedAt :: POSIXTime
  }

-- | Read an asset's stable serving metadata only when it both exists and canonicalizes to a path still
-- rooted under the configured static directory. 'sanitizeStaticAssetPath'
-- already rejects every segment that could make 'assetFilePath' escape the
-- root by construction; this closes the remaining escape a symlink planted
-- inside the root could open.
staticAssetFileWithinRoot :: FilePath -> FilePath -> IO (Maybe StaticAssetFile)
staticAssetFileWithinRoot rootDirectory assetFilePath = do
  assetExists <- doesFileExist assetFilePath
  if not assetExists
    then pure Nothing
    else do
      canonicalRoot <- canonicalizePath rootDirectory
      canonicalAsset <- canonicalizePath assetFilePath
      if (canonicalRoot <> [pathSeparator]) `isPrefixOf` canonicalAsset
        then do
          status <- getFileStatus canonicalAsset
          pure
            ( Just
                StaticAssetFile
                  { staticAssetFilePath = canonicalAsset,
                    staticAssetFileSize = fromIntegral (fileSize status),
                    staticAssetFileModifiedAt = modificationTimeHiRes status
                  }
            )
        else pure Nothing

staticAssetResponse :: StaticAssetsConfig -> Wai.Request -> Text -> StaticAssetFile -> Wai.Response
staticAssetResponse staticAssetsConfig request assetContentType assetFile
  | staticAssetNotModified request assetFile =
      Wai.responseLBS Http.status304 (staticAssetHeaders staticAssetsConfig assetContentType assetFile) LazyByteString.empty
  | otherwise =
      case requestByteRange request (staticAssetFileSize assetFile) of
        UnsatisfiableStaticRange ->
          Wai.responseLBS
            Http.status416
            (staticAssetHeaders staticAssetsConfig assetContentType assetFile <> [(Http.hContentRange, contentRangeUnsatisfied (staticAssetFileSize assetFile)), (Http.hContentLength, "0")])
            LazyByteString.empty
        FullStaticRange ->
          staticAssetFileResponse request Http.status200 (staticAssetHeaders staticAssetsConfig assetContentType assetFile <> [(Http.hContentLength, decimalHeaderValue (staticAssetFileSize assetFile))]) assetFile Nothing
        SatisfiableStaticRange filePart ->
          staticAssetFileResponse
            request
            Http.status206
            ( staticAssetHeaders staticAssetsConfig assetContentType assetFile
                <> [ (Http.hContentRange, contentRangeForFilePart filePart),
                     (Http.hContentLength, decimalHeaderValue (Wai.filePartByteCount filePart))
                   ]
            )
            assetFile
            (Just filePart)

staticAssetFileResponse :: Wai.Request -> Http.Status -> Http.ResponseHeaders -> StaticAssetFile -> Maybe Wai.FilePart -> Wai.Response
staticAssetFileResponse request status headers assetFile maybeFilePart =
  if Wai.requestMethod request == Http.methodHead
    then Wai.responseLBS status headers LazyByteString.empty
    else Wai.responseFile status headers (staticAssetFilePath assetFile) maybeFilePart

data StaticRange
  = FullStaticRange
  | SatisfiableStaticRange Wai.FilePart
  | UnsatisfiableStaticRange

requestByteRange :: Wai.Request -> Integer -> StaticRange
requestByteRange request assetSize =
  case lookup Http.hRange (Wai.requestHeaders request) of
    Nothing -> FullStaticRange
    Just rangeHeader ->
      maybe
        UnsatisfiableStaticRange
        SatisfiableStaticRange
        (ByteString.stripPrefix "bytes=" rangeHeader >>= parseSingleByteRange assetSize)

parseSingleByteRange :: Integer -> ByteString.ByteString -> Maybe Wai.FilePart
parseSingleByteRange assetSize rangeSpec
  | ByteString.any (== 44) rangeSpec = Nothing
  | otherwise =
      case ByteString.break (== 45) rangeSpec of
        (startText, suffixText)
          | ByteString.null suffixText -> Nothing
          | ByteString.null startText -> do
              suffixLength <- parseNonnegativeInteger (ByteString.drop 1 suffixText)
              if suffixLength <= 0 || assetSize <= 0
                then Nothing
                else
                  let byteCount = min suffixLength assetSize
                   in Just (Wai.FilePart (assetSize - byteCount) byteCount assetSize)
          | otherwise -> do
              startOffset <- parseNonnegativeInteger startText
              if startOffset >= assetSize
                then Nothing
                else
                  let endText = ByteString.drop 1 suffixText
                   in if ByteString.null endText
                        then Just (Wai.FilePart startOffset (assetSize - startOffset) assetSize)
                        else do
                          requestedEnd <- parseNonnegativeInteger endText
                          if requestedEnd < startOffset
                            then Nothing
                            else
                              let endOffset = min requestedEnd (assetSize - 1)
                               in Just (Wai.FilePart startOffset (endOffset - startOffset + 1) assetSize)

parseNonnegativeInteger :: ByteString.ByteString -> Maybe Integer
parseNonnegativeInteger decimalText
  | ByteString.null decimalText = Nothing
  | ByteString.all isAsciiDigit decimalText = readMaybe (Text.unpack (TextEncoding.decodeUtf8 decimalText))
  | otherwise = Nothing

isAsciiDigit :: Word8 -> Bool
isAsciiDigit byte = byte >= 48 && byte <= 57

staticAssetNotModified :: Wai.Request -> StaticAssetFile -> Bool
staticAssetNotModified request assetFile =
  case lookup Http.hIfNoneMatch (Wai.requestHeaders request) of
    Just ifNoneMatch -> ifNoneMatchMatches (staticAssetETag assetFile) ifNoneMatch
    Nothing ->
      case lookup Http.hIfModifiedSince (Wai.requestHeaders request) >>= parseHTTPDate of
        Nothing -> False
        Just modifiedSince -> staticAssetModifiedAtWholeSeconds assetFile <= floor (utcTimeToPOSIXSeconds (httpDateToUTC modifiedSince))

ifNoneMatchMatches :: ByteString.ByteString -> ByteString.ByteString -> Bool
ifNoneMatchMatches currentETag ifNoneMatch =
  any (matches currentETag) (ByteString.split 44 ifNoneMatch)
  where
    matches etag candidate =
      let normalizedCandidate = trimOptionalWhitespace candidate
       in normalizedCandidate == "*" || weakETagValue normalizedCandidate == weakETagValue etag

weakETagValue :: ByteString.ByteString -> ByteString.ByteString
weakETagValue etag = fromMaybe etag (ByteString.stripPrefix "W/" etag)

trimOptionalWhitespace :: ByteString.ByteString -> ByteString.ByteString
trimOptionalWhitespace = ByteString.dropWhileEnd isOptionalWhitespace . ByteString.dropWhile isOptionalWhitespace

isOptionalWhitespace :: Word8 -> Bool
isOptionalWhitespace byte = byte == 32 || byte == 9

staticAssetETag :: StaticAssetFile -> ByteString.ByteString
staticAssetETag assetFile =
  "W/\"" <> decimalHeaderValue (staticAssetFileSize assetFile) <> "-" <> renderedModifiedAt <> "\""
  where
    renderedModifiedAt = TextEncoding.encodeUtf8 (Text.pack (show (staticAssetFileModifiedAt assetFile)))

staticAssetModifiedAtWholeSeconds :: StaticAssetFile -> Integer
staticAssetModifiedAtWholeSeconds = floor . staticAssetFileModifiedAt

contentRangeForFilePart :: Wai.FilePart -> ByteString.ByteString
contentRangeForFilePart filePart =
  "bytes "
    <> decimalHeaderValue (Wai.filePartOffset filePart)
    <> "-"
    <> decimalHeaderValue (Wai.filePartOffset filePart + Wai.filePartByteCount filePart - 1)
    <> "/"
    <> decimalHeaderValue (Wai.filePartFileSize filePart)

contentRangeUnsatisfied :: Integer -> ByteString.ByteString
contentRangeUnsatisfied assetSize = "bytes */" <> decimalHeaderValue assetSize

decimalHeaderValue :: Integer -> ByteString.ByteString
decimalHeaderValue = TextEncoding.encodeUtf8 . Text.pack . show

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
        Just assetPath <- [stripStaticPrefix (staticUrlPrefix staticRoot) requestPath],
        staticAssetRootCanOwnPath staticAssetsConfig staticRoot assetPath
      ]

    compareStaticPrefixLength (leftRoot, _) (rightRoot, _) =
      compare (Text.length (staticUrlPrefix leftRoot)) (Text.length (staticUrlPrefix rightRoot))

staticAssetRootCanOwnPath :: StaticAssetsConfig -> StaticAssetRoot -> Text -> Bool
staticAssetRootCanOwnPath staticAssetsConfig staticRoot assetPath =
  not (Text.null (normalizeStaticAssetRoutePrefix (staticUrlPrefix staticRoot)))
    || isJust (staticAssetContentType staticAssetsConfig (Text.unpack assetPath))

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

staticAssetHeaders :: StaticAssetsConfig -> Text -> StaticAssetFile -> Http.ResponseHeaders
staticAssetHeaders staticAssetsConfig assetContentType assetFile =
  (Http.hContentType, TextEncoding.encodeUtf8 assetContentType)
    : [ (Http.hETag, staticAssetETag assetFile),
        (Http.hLastModified, staticAssetLastModified assetFile),
        (Http.hAcceptRanges, "bytes")
      ]
      <> maybe [] (\cacheHeader -> [(Http.hCacheControl, TextEncoding.encodeUtf8 cacheHeader)]) (staticCacheControlHeaderValue staticAssetsConfig)

staticAssetLastModified :: StaticAssetFile -> ByteString.ByteString
staticAssetLastModified assetFile =
  formatHTTPDate
    ( utcToHTTPDate
        (posixSecondsToUTCTime (fromIntegral (staticAssetModifiedAtWholeSeconds assetFile)))
    )

staticCacheControlHeaderValue :: StaticAssetsConfig -> Maybe Text
staticCacheControlHeaderValue staticAssetsConfig =
  fmap (\seconds -> Text.pack ("public, max-age=" <> show seconds)) (staticCacheControlSeconds staticAssetsConfig)

staticAssetContentType :: StaticAssetsConfig -> FilePath -> Maybe Text
staticAssetContentType staticAssetsConfig assetFilePath =
  lookup (Text.pack (takeExtension assetFilePath)) (staticAssetContentTypes staticAssetsConfig)

missingStaticAssetResponse :: Wai.Response
missingStaticAssetResponse =
  Wai.responseLBS
    Http.status404
    [(Http.hContentType, TextEncoding.encodeUtf8 "text/plain; charset=utf-8")]
    (LazyByteString.fromStrict (TextEncoding.encodeUtf8 "Not Found"))
