{-# LANGUAGE OverloadedStrings #-}

-- | Declarative, method-aware HTTP API endpoints (see @TASKS.md@ item AB):
-- path/method matching and 'ApiMatchResult'/@Allow@ derivation, an
-- accumulating-error 'RequestCodec' for query and header fields, buffered
-- request-body decoding selected by @Content-Type@ ('ApiBodyDecoder'), a
-- fully-buffered 'ApiResponseBody', and RFC 9110 @Accept@ representation
-- negotiation. A streaming request-body decoder, such as multipart, is a
-- separate concern; see 'HarchWeb.Api.Multipart'.
--
-- This is a standalone library capability: it is not yet wired into an
-- application's default request dispatcher (see @docs/design-guidance.md@).
-- Every declared endpoint today matches one fixed, context-independent
-- path. Typed path captures are a documented future extension and are not
-- yet supported.
module HarchWeb.Api
  ( ApiMethod (..),
    ApiPath,
    ApiEndpoint,
    ApiMatchResult (..),
    apiMethodText,
    apiEndpoint,
    apiEndpointTarget,
    at,
    matchApiEndpoints,
    apiAllowHeaderValue,
    ApiHttpResponse (..),
    respondApiMatch,
    ApiRequestData (..),
    apiRequestDataFromWaiRequest,
    ApiRequestSource (..),
    ApiRequestParseError (..),
    ApiFieldValue,
    RequestField,
    RequestCodec,
    apiTextValue,
    parseApiField,
    queryField,
    headerField,
    requiredField,
    optionalField,
    fieldWithDefault,
    runRequestCodec,
    ApiBodyDecoder (..),
    MissingContentTypePolicy (..),
    ApiBodyOutcome (..),
    selectApiBodyDecoder,
    jsonBodyDecoder,
    textBodyDecoder,
    bytesBodyDecoder,
    ApiResponseBody (..),
    apiJsonResponse,
    apiTextResponse,
    apiBytesResponse,
    AcceptedRange (..),
    ApiNegotiationResult (..),
    parseAcceptHeader,
    selectRepresentation,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.CaseInsensitive qualified as CaseInsensitive
import Data.Functor.Compose (Compose (..), getCompose)
import Data.List (foldl1', nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import Data.Text.Encoding.Error qualified as TextEncodingError
import Data.Text.Read qualified as TextRead
import Network.Wai qualified as Wai

-- | Methods an 'ApiEndpoint' can declare. @HEAD@ is never declared directly:
-- a matched @GET@ endpoint answers a @HEAD@ request with the same target,
-- and a caller renders the response without a body. @OPTIONS@ is likewise
-- never declared; synthesize it from 'apiAllowHeaderValue' for the matched
-- path rather than maintaining a second method table.
data ApiMethod
  = ApiGet
  | ApiPost
  | ApiPut
  | ApiPatch
  | ApiDelete
  deriving (Eq, Show)

apiMethodText :: ApiMethod -> Text
apiMethodText methodValue =
  case methodValue of
    ApiGet -> "GET"
    ApiPost -> "POST"
    ApiPut -> "PUT"
    ApiPatch -> "PATCH"
    ApiDelete -> "DELETE"

-- | A static, context-independent request path.
newtype ApiPath = ApiPath Text
  deriving (Eq, Show)

-- | Declare the fixed path an endpoint matches, e.g. @at "\/api\/status"@.
at :: Text -> ApiPath
at = ApiPath

data ApiEndpoint target = ApiEndpoint
  { apiEndpointTarget :: target,
    apiEndpointMethod :: ApiMethod,
    apiEndpointPath :: ApiPath
  }

apiEndpoint :: target -> ApiMethod -> ApiPath -> ApiEndpoint target
apiEndpoint = ApiEndpoint

data ApiMatchResult target
  = -- | No declared endpoint matches this path: respond @404 Not Found@.
    NoApiRouteMatch
  | -- | The path matches, but no declared endpoint accepts this method:
    -- respond @405 Method Not Allowed@ with these methods in 'apiAllowHeaderValue'.
    ApiMethodNotAllowed (NonEmpty ApiMethod)
  | -- | A declared endpoint matches the request method exactly.
    ApiRouteMatched target
  | -- | The path matches a declared @GET@ endpoint and the request method is
    -- @HEAD@: render that target's response without a body.
    ApiRouteMatchedHead target
  deriving (Eq, Show)

-- | Match a request method and path against a declared endpoint table.
matchApiEndpoints :: Text -> Text -> [ApiEndpoint target] -> ApiMatchResult target
matchApiEndpoints requestMethod requestPath endpoints =
  case filter (endpointAtPath requestPath) endpoints of
    [] -> NoApiRouteMatch
    firstPathMatch : remainingPathMatches ->
      matchMethod requestMethod (firstPathMatch :| remainingPathMatches)

matchMethod :: Text -> NonEmpty (ApiEndpoint target) -> ApiMatchResult target
matchMethod requestMethod pathMatches =
  case NonEmpty.filter (endpointHasMethod requestMethod) pathMatches of
    matched : _ -> ApiRouteMatched (apiEndpointTarget matched)
    [] ->
      if requestMethod == "HEAD"
        then case NonEmpty.filter (endpointHasMethod "GET") pathMatches of
          matchedGet : _ -> ApiRouteMatchedHead (apiEndpointTarget matchedGet)
          [] -> ApiMethodNotAllowed (declaredMethods pathMatches)
        else ApiMethodNotAllowed (declaredMethods pathMatches)

endpointAtPath :: Text -> ApiEndpoint target -> Bool
endpointAtPath requestPath endpointValue =
  case apiEndpointPath endpointValue of
    ApiPath declaredPath -> declaredPath == requestPath

endpointHasMethod :: Text -> ApiEndpoint target -> Bool
endpointHasMethod requestMethod endpointValue =
  apiMethodText (apiEndpointMethod endpointValue) == requestMethod

declaredMethods :: NonEmpty (ApiEndpoint target) -> NonEmpty ApiMethod
declaredMethods (firstEndpoint :| remainingEndpoints) =
  firstMethod :| nub (filter (/= firstMethod) (map apiEndpointMethod remainingEndpoints))
  where
    firstMethod = apiEndpointMethod firstEndpoint

-- | Render the @Allow@ header value for a matched path's declared methods,
-- including the @HEAD@ and @OPTIONS@ methods synthesized from them.
apiAllowHeaderValue :: NonEmpty ApiMethod -> Text
apiAllowHeaderValue declaredMethodsValue =
  Text.intercalate
    ", "
    ( map apiMethodText (NonEmpty.toList declaredMethodsValue)
        <> ["HEAD" | ApiGet `elem` declaredMethodsValue]
        <> ["OPTIONS"]
    )

-- | The protocol-level shape of a rendered response: status, headers, and an
-- optional body (omitted for @HEAD@). Framework/transport-agnostic; adapt it
-- to a concrete server's response type at the integration boundary.
data ApiHttpResponse = ApiHttpResponse
  { apiHttpResponseStatus :: Int,
    apiHttpResponseHeaders :: [(Text, Text)],
    apiHttpResponseBody :: Maybe ApiResponseBody
  }
  deriving (Eq, Show)

-- | Render a match into its protocol-level response: @404@/@405@+@Allow@ for
-- the two non-matching outcomes, and the rendered target's status/headers
-- otherwise, with the body omitted for a @HEAD@ match. @renderTarget@ is run
-- at most once, only when the match owns the request.
respondApiMatch :: (target -> ApiResponseBody) -> ApiMatchResult target -> ApiHttpResponse
respondApiMatch renderTarget matchResult =
  case matchResult of
    NoApiRouteMatch -> ApiHttpResponse 404 [] Nothing
    ApiMethodNotAllowed declaredMethodsValue ->
      ApiHttpResponse 405 [("Allow", apiAllowHeaderValue declaredMethodsValue)] Nothing
    ApiRouteMatched target -> renderedApiResponse (renderTarget $! target)
    ApiRouteMatchedHead target -> (renderedApiResponse (renderTarget $! target)) {apiHttpResponseBody = Nothing}

renderedApiResponse :: ApiResponseBody -> ApiHttpResponse
renderedApiResponse body =
  ApiHttpResponse
    { apiHttpResponseStatus = 200,
      apiHttpResponseHeaders = [("Content-Type", apiResponseContentType body)],
      apiHttpResponseBody = Just body
    }

-- | The pre-parsed request data a 'RequestCodec' decodes from. Path capture
-- and body sources are documented future extensions; only query parameters
-- and headers are supported today.
data ApiRequestData = ApiRequestData
  { apiRequestQueryParameters :: [(Text, Text)],
    apiRequestHeaders :: [(Text, Text)]
  }
  deriving (Eq, Show)

-- | Extract a 'RequestCodec'-ready 'ApiRequestData' from a WAI request. A
-- query parameter present without a value (@?flag@) decodes as an empty
-- value rather than being dropped; header and query bytes are decoded
-- leniently rather than failing the whole request on invalid UTF-8.
apiRequestDataFromWaiRequest :: Wai.Request -> ApiRequestData
apiRequestDataFromWaiRequest request =
  ApiRequestData
    { apiRequestQueryParameters =
        [ (decodeUtf8Leniently name, maybe "" decodeUtf8Leniently value)
        | (name, value) <- Wai.queryString request
        ],
      apiRequestHeaders =
        [ (decodeUtf8Leniently (CaseInsensitive.foldedCase name), decodeUtf8Leniently value)
        | (name, value) <- Wai.requestHeaders request
        ]
    }

-- Kept eta-expanded (not point-free) so HPC ticks the decode call on every
-- invocation rather than treating it as a once-shared CAF reference.
{-# ANN decodeUtf8Leniently ("HLint: ignore Eta reduce" :: String) #-}
decodeUtf8Leniently :: ByteString -> Text
decodeUtf8Leniently bytes = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode bytes

-- | The declared source of an individual request field.
data ApiRequestSource
  = ApiQuerySource
  | ApiHeaderSource
  deriving (Eq, Show)

data ApiRequestParseError
  = MissingApiField ApiRequestSource Text
  | DuplicateApiField ApiRequestSource Text
  | InvalidApiField ApiRequestSource Text
  deriving (Eq, Show)

newtype ApiFieldValue value = ApiFieldValue
  { runApiFieldValue :: Text -> Maybe value
  }

-- | An unvalidated field value, kept as-is.
apiTextValue :: ApiFieldValue Text
apiTextValue = ApiFieldValue Just

parseApiField :: (Text -> Maybe value) -> ApiFieldValue value
parseApiField = ApiFieldValue

newtype RequestField value = RequestField (ApiRequestData -> ([ApiRequestParseError], Maybe value))

-- | An accumulating-validation applicative: independent field errors from
-- separate 'RequestCodec' combinators concatenate rather than short-circuit,
-- matching 'HarchWeb.Action.ActionDecoder'.
type RequestCodec value = Compose ((->) ApiRequestData) (Compose ((,) [ApiRequestParseError]) Maybe) value

requestCodec :: (ApiRequestData -> ([ApiRequestParseError], Maybe value)) -> RequestCodec value
requestCodec decode = Compose (Compose . decode)

runRequestCodec :: RequestCodec value -> ApiRequestData -> ([ApiRequestParseError], Maybe value)
runRequestCodec codec requestData = getCompose (getCompose codec requestData)

sourceFields :: ApiRequestSource -> ApiRequestData -> [(Text, Text)]
sourceFields source requestData =
  case source of
    ApiQuerySource -> apiRequestQueryParameters requestData
    ApiHeaderSource -> apiRequestHeaders requestData

requestField :: ApiRequestSource -> Text -> ApiFieldValue value -> RequestField value
requestField source fieldName valueDecoder =
  RequestField $ \requestData ->
    case [fieldValue | (name, fieldValue) <- sourceFields source requestData, name == fieldName] of
      [fieldValue] ->
        maybe
          ([InvalidApiField source fieldName], Nothing)
          (\value -> ([], Just value))
          (runApiFieldValue valueDecoder fieldValue)
      [] -> ([MissingApiField source fieldName], Nothing)
      _ -> ([DuplicateApiField source fieldName], Nothing)

-- | Declare a field sourced from the request's query parameters.
queryField :: Text -> ApiFieldValue value -> RequestField value
queryField = requestField ApiQuerySource

-- | Declare a field sourced from the request's headers.
headerField :: Text -> ApiFieldValue value -> RequestField value
headerField = requestField ApiHeaderSource

requiredField :: RequestField value -> RequestCodec value
requiredField (RequestField decode) = requestCodec decode

optionalField :: RequestField value -> RequestCodec (Maybe value)
optionalField (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just (Just value))
      ([MissingApiField _ _], Nothing) -> ([], Just Nothing)
      (parseErrors, _) -> (parseErrors, Nothing)

fieldWithDefault :: value -> RequestField value -> RequestCodec value
fieldWithDefault defaultValue (RequestField decode) =
  requestCodec $ \requestData ->
    case decode requestData of
      ([], Just value) -> ([], Just value)
      ([MissingApiField _ _], Nothing) -> ([], Just defaultValue)
      parseErrors -> parseErrors

-- | Decodes a fully-buffered request body declared for one @Content-Type@
-- media type (ignoring its parameters, e.g. @charset@). A streaming body
-- decoder, such as multipart, is a separate, non-buffered concern; see
-- 'HarchWeb.Api.Multipart'.
data ApiBodyDecoder request = ApiBodyDecoder
  { apiBodyDecoderMediaType :: Text,
    apiBodyDecoderParse :: ByteString -> Either Text request
  }

-- | What a missing @Content-Type@ header means for a declared endpoint.
data MissingContentTypePolicy
  = -- | Treat a missing header the same as an unsupported one.
    RejectMissingContentType
  | -- | Decode as if this media type were declared, e.g. an application or
    -- framework JSON default.
    AssumeMediaType Text
  deriving (Eq, Show)

data ApiBodyOutcome request
  = -- | No declared decoder accepts the request's media type (or none was
    -- given and the policy rejects that): respond @415 Unsupported Media Type@,
    -- advertising these declared media types.
    ApiUnsupportedMediaType [Text]
  | -- | The body exceeded the caller's declared byte limit: respond @413@.
    ApiBodyTooLarge
  | -- | The selected decoder rejected the body's syntax: respond @400@.
    ApiMalformedBody
  | -- | Successfully decoded; semantic validation (@422@) is a separate,
    -- application-owned concern from here on.
    ApiDecodedBody request
  deriving (Eq, Show)

-- | Select a declared decoder by the request's @Content-Type@ (ignoring its
-- parameters) and run it against an already-bounded body. Never reads more
-- of the body itself; the caller supplies @maxBodyBytes@ enforcement
-- against however it obtained @bodyBytes@.
selectApiBodyDecoder ::
  MissingContentTypePolicy ->
  Int ->
  [ApiBodyDecoder request] ->
  Maybe Text ->
  ByteString ->
  ApiBodyOutcome request
selectApiBodyDecoder missingPolicy maxBodyBytes decoders maybeContentType bodyBytes
  | ByteString.length bodyBytes > maxBodyBytes = ApiBodyTooLarge
  | otherwise =
      case resolvedMediaType of
        Nothing -> ApiUnsupportedMediaType declaredMediaTypes
        Just mediaType ->
          case [decoder | decoder <- decoders, Text.toLower (apiBodyDecoderMediaType decoder) == mediaType] of
            [] -> ApiUnsupportedMediaType declaredMediaTypes
            decoder : _ ->
              either (const ApiMalformedBody) ApiDecodedBody (apiBodyDecoderParse decoder bodyBytes)
  where
    declaredMediaTypes = map apiBodyDecoderMediaType decoders
    resolvedMediaType =
      case maybeContentType of
        Just contentTypeValue -> contentTypeMediaType contentTypeValue
        Nothing ->
          case missingPolicy of
            RejectMissingContentType -> Nothing
            AssumeMediaType mediaType -> Just (Text.toLower mediaType)

contentTypeMediaType :: Text -> Maybe Text
contentTypeMediaType contentTypeValue = do
  (typeText, subtypeText) <- parseMediaRange (Text.strip (fst (Text.breakOn ";" contentTypeValue)))
  pure (Text.toLower typeText <> "/" <> Text.toLower subtypeText)

jsonBodyDecoder :: (FromJSON request) => ApiBodyDecoder request
jsonBodyDecoder =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = "application/json",
      apiBodyDecoderParse = \bodyBytes ->
        case Aeson.eitherDecodeStrict' bodyBytes of
          Left errorMessage -> Left (Text.pack errorMessage)
          Right decodedValue -> Right decodedValue
    }

-- | Decodes a strict-UTF-8 @text/plain@ body; invalid UTF-8 is a malformed
-- body rather than a lenient best-effort decode.
textBodyDecoder :: ApiBodyDecoder Text
textBodyDecoder =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = "text/plain",
      apiBodyDecoderParse = \bodyBytes ->
        case TextEncoding.decodeUtf8' bodyBytes of
          Left _decodeError -> Left "invalid UTF-8 body"
          Right decodedText -> Right decodedText
    }

-- | Passes the body through unparsed for the given media type.
bytesBodyDecoder :: Text -> ApiBodyDecoder ByteString
bytesBodyDecoder mediaType =
  ApiBodyDecoder
    { apiBodyDecoderMediaType = mediaType,
      apiBodyDecoderParse = Right
    }

-- | A rendered API response body. Content negotiation and streaming bodies
-- are documented future extensions; every response today is fully buffered.
data ApiResponseBody = ApiResponseBody
  { apiResponseContentType :: Text,
    apiResponseBodyBytes :: ByteString
  }
  deriving (Eq, Show)

apiJsonResponse :: (ToJSON value) => value -> ApiResponseBody
apiJsonResponse value =
  ApiResponseBody
    { apiResponseContentType = "application/json; charset=utf-8",
      apiResponseBodyBytes = LazyByteString.toStrict (Aeson.encode value)
    }

apiTextResponse :: Text -> ApiResponseBody
apiTextResponse bodyText =
  ApiResponseBody
    { apiResponseContentType = "text/plain; charset=utf-8",
      apiResponseBodyBytes = TextEncoding.encodeUtf8 bodyText
    }

apiBytesResponse :: Text -> ByteString -> ApiResponseBody
apiBytesResponse contentType bodyBytes =
  ApiResponseBody
    { apiResponseContentType = contentType,
      apiResponseBodyBytes = bodyBytes
    }

-- | One parsed entry from an @Accept@ header: a media range plus its quality
-- weight. Media-type parameters beyond @q@ are retained but not currently
-- required to match a declared representation's own parameters, since every
-- declared representation handled here is a bare @type\/subtype@ value.
data AcceptedRange = AcceptedRange
  { acceptedRangeType :: Text,
    acceptedRangeSubtype :: Text,
    acceptedRangeParameters :: [(Text, Text)],
    acceptedRangeQuality :: Double
  }
  deriving (Eq, Show)

-- | Parse an @Accept@ header value into its declared media ranges. A
-- malformed entry is dropped rather than failing the whole header.
parseAcceptHeader :: Text -> [AcceptedRange]
parseAcceptHeader headerValue =
  Maybe.mapMaybe parseAcceptEntry (Text.splitOn "," headerValue)

parseAcceptEntry :: Text -> Maybe AcceptedRange
parseAcceptEntry entry =
  let (mediaRangeText, parameterSection) = Text.breakOn ";" (Text.strip entry)
      parameterTexts =
        if Text.null parameterSection
          then []
          else Text.splitOn ";" (Text.drop 1 parameterSection)
   in do
        (typeText, subtypeText) <- parseMediaRange (Text.strip mediaRangeText)
        let parameters = Maybe.mapMaybe parseAcceptParameter parameterTexts
        pure
          AcceptedRange
            { acceptedRangeType = Text.toLower typeText,
              acceptedRangeSubtype = Text.toLower subtypeText,
              acceptedRangeParameters = filter ((/= "q") . fst) parameters,
              acceptedRangeQuality = qualityFromParameters parameters
            }

parseMediaRange :: Text -> Maybe (Text, Text)
parseMediaRange mediaRangeText =
  case Text.splitOn "/" mediaRangeText of
    [typeText, subtypeText] | not (Text.null typeText), not (Text.null subtypeText) -> Just (typeText, subtypeText)
    _ -> Nothing

parseAcceptParameter :: Text -> Maybe (Text, Text)
parseAcceptParameter parameterText =
  case Text.breakOn "=" (Text.strip parameterText) of
    (name, value)
      | not (Text.null name),
        Text.isPrefixOf "=" value ->
          Just (Text.toLower name, Text.strip (Text.drop 1 value))
    _ -> Nothing

qualityFromParameters :: [(Text, Text)] -> Double
qualityFromParameters parameters =
  case lookup "q" parameters of
    Nothing -> 1.0
    Just qualityText ->
      case TextRead.double qualityText of
        Right (qualityValue, _) -> qualityValue
        Left _ -> 1.0

mediaRangeSpecificity :: AcceptedRange -> Int
mediaRangeSpecificity range
  | acceptedRangeType range == "*" = 0
  | acceptedRangeSubtype range == "*" = 1
  | otherwise = 2

mediaTypeParts :: Text -> (Text, Text)
mediaTypeParts mediaType =
  case Text.splitOn "/" mediaType of
    [typeText, subtypeText] -> (Text.toLower typeText, Text.toLower subtypeText)
    _ -> (Text.toLower mediaType, "")

rangeMatchesRepresentation :: (Text, Text) -> AcceptedRange -> Bool
rangeMatchesRepresentation (declaredType, declaredSubtype) range =
  (acceptedRangeType range == "*" || acceptedRangeType range == declaredType)
    && (acceptedRangeSubtype range == "*" || acceptedRangeSubtype range == declaredSubtype)

-- | The single most specific range that applies to a declared representation.
-- Per RFC 9110 section 12.5.1, when more than one range in the header
-- applies to a representation, the most specific one governs its quality
-- regardless of a less specific range's own quality.
bestMatchingRange :: Text -> [AcceptedRange] -> Maybe AcceptedRange
bestMatchingRange declaredMediaType ranges =
  case filter (rangeMatchesRepresentation (mediaTypeParts declaredMediaType)) ranges of
    [] -> Nothing
    matches -> Just (foldl1' preferMoreSpecific matches)
  where
    preferMoreSpecific left right =
      if mediaRangeSpecificity right > mediaRangeSpecificity left then right else left

data ApiNegotiationResult
  = -- | The @Accept@ header explicitly excludes every declared representation:
    -- respond @406 Not Acceptable@.
    NoAcceptableRepresentation
  | -- | The selected declared representation. A caller that declares more
    -- than one representation must add @Vary: Accept@ to the response.
    SelectedRepresentation Text
  deriving (Eq, Show)

-- | Negotiate a response representation from a declared,
-- server-preference-ordered list and an optional @Accept@ header. A missing
-- header selects the first declared representation. An explicit header that
-- excludes every declared representation is @406@; otherwise each
-- representation's most specific matching range determines its quality, the
-- highest quality is selected, and ties keep server declaration order.
selectRepresentation :: NonEmpty Text -> Maybe Text -> ApiNegotiationResult
selectRepresentation declaredRepresentations maybeAcceptHeader =
  case maybeAcceptHeader of
    Nothing -> SelectedRepresentation (NonEmpty.head declaredRepresentations)
    Just headerValue ->
      case acceptableCandidates (parseAcceptHeader headerValue) of
        [] -> NoAcceptableRepresentation
        candidates -> SelectedRepresentation (fst (foldl1' preferHigherQuality candidates))
  where
    acceptableCandidates ranges =
      [ (representation, acceptedRangeQuality bestRange)
      | representation <- NonEmpty.toList declaredRepresentations,
        Just bestRange <- [bestMatchingRange representation ranges],
        acceptedRangeQuality bestRange > 0
      ]
    preferHigherQuality left right =
      if snd right > snd left then right else left
