{-# LANGUAGE OverloadedStrings #-}

module HarchWeb.Observability
  ( HttpServerMetrics (..),
    ObservabilityAttribute (..),
    ObservabilityAttributeValue (..),
    RequestObservability (..),
    RequestSpan (..),
    ResponseKind (..),
    buildRequestObservability,
    forceRequestObservability,
    requestObservabilityAttributes,
    requestSpanName,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text

data ObservabilityAttributeValue
  = TextAttribute Text
  | IntAttribute Int
  deriving (Eq, Show)

data ObservabilityAttribute = ObservabilityAttribute
  { attributeName :: Text,
    attributeValue :: ObservabilityAttributeValue
  }
  deriving (Eq, Show)

data ResponseKind
  = PageResponseKind
  | BodyResponseKind
  deriving (Eq, Show)

data RequestSpan = RequestSpan
  { requestSpanDisplayName :: Text,
    requestSpanAttributes :: [ObservabilityAttribute]
  }
  deriving (Eq, Show)

data HttpServerMetrics = HttpServerMetrics
  { requestDurationMetricName :: Text,
    activeRequestsMetricName :: Text,
    httpServerMetricAttributes :: [ObservabilityAttribute]
  }
  deriving (Eq, Show)

data RequestObservability = RequestObservability
  { observabilityRequestSpan :: RequestSpan,
    observabilityHttpServerMetrics :: HttpServerMetrics
  }
  deriving (Eq, Show)

requestSpanName :: Text -> Text -> Text
requestSpanName method routePath =
  Text.concat [method, " ", routePath]

requestObservabilityAttributes ::
  Text ->
  Text ->
  Text ->
  Text ->
  Int ->
  ResponseKind ->
  [ObservabilityAttribute] ->
  [ObservabilityAttribute]
requestObservabilityAttributes method scheme requestPath routePath statusCode responseKind extraAttributes =
  commonAttributes ++ extraAttributes
  where
    commonAttributes =
      [ ObservabilityAttribute
          { attributeName = "http.request.method",
            attributeValue = TextAttribute method
          },
        ObservabilityAttribute
          { attributeName = "url.scheme",
            attributeValue = TextAttribute scheme
          },
        ObservabilityAttribute
          { attributeName = "url.path",
            attributeValue = TextAttribute requestPath
          },
        ObservabilityAttribute
          { attributeName = "http.route",
            attributeValue = TextAttribute routePath
          },
        ObservabilityAttribute
          { attributeName = "http.response.status_code",
            attributeValue = IntAttribute statusCode
          },
        ObservabilityAttribute
          { attributeName = "harch.response.kind",
            attributeValue = TextAttribute (responseKindText responseKind)
          }
      ]

buildRequestObservability ::
  Text ->
  Text ->
  Text ->
  Text ->
  Int ->
  ResponseKind ->
  [ObservabilityAttribute] ->
  RequestObservability
buildRequestObservability method scheme requestPath routePath statusCode responseKind extraAttributes =
  let attributes = requestObservabilityAttributes method scheme requestPath routePath statusCode responseKind extraAttributes
      displayPath = requestSpanPath requestPath routePath statusCode
   in RequestObservability
        { observabilityRequestSpan =
            RequestSpan
              { requestSpanDisplayName = requestSpanName method displayPath,
                requestSpanAttributes = attributes
              },
          observabilityHttpServerMetrics =
            HttpServerMetrics
              { requestDurationMetricName = "http.server.request.duration",
                activeRequestsMetricName = "http.server.active_requests",
                httpServerMetricAttributes = attributes
              }
        }

requestSpanPath :: Text -> Text -> Int -> Text
requestSpanPath requestPath routePath statusCode =
  if statusCode == 404 && requestPath /= routePath && Text.isSuffixOf "/404" routePath
    then requestPath
    else routePath

forceRequestObservability :: RequestObservability -> ()
forceRequestObservability requestObservability =
  forceRequestSpan (observabilityRequestSpan requestObservability) `seq`
    forceHttpServerMetrics (observabilityHttpServerMetrics requestObservability)

forceRequestSpan :: RequestSpan -> ()
forceRequestSpan requestSpan =
  Text.length (requestSpanDisplayName requestSpan) `seq`
    forceAttributes (requestSpanAttributes requestSpan)

forceHttpServerMetrics :: HttpServerMetrics -> ()
forceHttpServerMetrics httpServerMetrics =
  Text.length (requestDurationMetricName httpServerMetrics) `seq`
    Text.length (activeRequestsMetricName httpServerMetrics) `seq`
      forceAttributes (httpServerMetricAttributes httpServerMetrics)

forceAttributes :: [ObservabilityAttribute] -> ()
forceAttributes attributes =
  case attributes of
    [] -> ()
    attribute : remainingAttributes ->
      forceAttribute attribute `seq` forceAttributes remainingAttributes

forceAttribute :: ObservabilityAttribute -> ()
forceAttribute attribute =
  Text.length (attributeName attribute) `seq`
    forceAttributeValue (attributeValue attribute)

forceAttributeValue :: ObservabilityAttributeValue -> ()
forceAttributeValue attributeValue =
  case attributeValue of
    TextAttribute textValue -> Text.length textValue `seq` ()
    IntAttribute intValue -> intValue `seq` ()

responseKindText :: ResponseKind -> Text
responseKindText responseKind =
  case responseKind of
    PageResponseKind -> "page"
    BodyResponseKind -> "body"
