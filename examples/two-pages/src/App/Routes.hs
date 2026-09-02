{-# LANGUAGE OverloadedStrings #-}

module App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    PreviewSlug,
    TwoPageAction (..),
    TwoPageActionTarget,
    TwoPageNavigationTarget (..),
    TwoPageRoute (..),
    mkPreviewSlug,
    previewSlugText,
    routeCodec,
    routeHref,
    twoPageActions,
    twoPageActionEndpointMetadata,
    twoPageNavigationPath,
    twoPageNavigationHref,
    twoPageActionPath,
    twoPageEndpointMetadata,
    twoPagePreviewEndpointMetadata,
    requiredEndpointName,
    requiredRouteTemplate,
  )
where

import App.Pages.Route.Generated
  ( PageRoute (..),
    pageRoutePath,
    parsePageRoute,
  )
import Data.Char (isAsciiLower, isDigit)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb
  ( AccessRequirement (AllowUnauthenticated),
    EndpointMetadata,
    EndpointName,
    EndpointProtocol (ActionEndpoint, HtmlEndpoint),
    PathSegment,
    RouteCodec (..),
    RouteLocation (..),
    RouteMethod (..),
    RouteParseResult (..),
    RouteRequest (..),
    RouteTemplate,
    SafeUrl,
    encodeRouteLocation,
    mkEndpointMetadata,
    mkEndpointName,
    mkRouteTemplate,
    mkSafeUrl,
    requiredPathSegment,
    requiredSafeUrlOrDie,
    routeMethodPolicy,
    safeUrlText,
  )
import HarchWeb.Action
  ( ActionCodec,
    formField,
    post,
    singleActionCodecWithMetadata,
    singleOrDefault,
    staticActionEndpointMetadata,
    staticActionPath,
    textValue,
  )

data ApiRoute
  = LiveDataEvents
  deriving (Show)

type TwoPageActionTarget = ()

data TwoPageNavigationTarget
  = NavigationPage PageRoute
  | NavigationPreview PreviewSlug

newtype TwoPageAction = SubscribeAction Text

newtype PreviewSlug = PreviewSlug Text
  deriving (Show)

data CustomRoute
  = PreviewPage PreviewSlug
  | NativeSubscriptionFallback
  deriving (Show)

data TwoPageRoute
  = Page PageRoute
  | Api ApiRoute
  | Custom CustomRoute
  deriving (Show)

twoPageEndpointMetadata :: EndpointProtocol -> TwoPageRoute -> EndpointMetadata ()
twoPageEndpointMetadata endpointProtocolValue route =
  mkEndpointMetadata
    (requiredEndpointName (endpointNameForRoute route))
    (requiredRouteTemplate (routeTemplateForRoute route))
    endpointProtocolValue
    AllowUnauthenticated

-- | The preview declaration has one dynamic slug but one static endpoint
-- identity. Keep metadata independent of a rendered slug so the route table
-- cannot accidentally depend on request-derived text.
twoPagePreviewEndpointMetadata :: EndpointMetadata ()
twoPagePreviewEndpointMetadata =
  mkEndpointMetadata
    (requiredEndpointName "two-pages.preview")
    (requiredRouteTemplate "/preview/{slug}")
    HtmlEndpoint
    AllowUnauthenticated

endpointNameForRoute :: TwoPageRoute -> Text
endpointNameForRoute route =
  case route of
    Page HomePage -> "two-pages.home"
    Page SecondPage -> "two-pages.second"
    Page LiveDataPage -> "two-pages.live-data"
    Page PageNotFound -> "two-pages.not-found"
    Api LiveDataEvents -> "two-pages.live-data-events"
    Custom (PreviewPage _) -> "two-pages.preview"
    Custom NativeSubscriptionFallback -> "two-pages.native-subscription"

routeTemplateForRoute :: TwoPageRoute -> Text
routeTemplateForRoute route =
  case route of
    Page HomePage -> "/"
    Page SecondPage -> "/second"
    Page LiveDataPage -> "/live-data"
    Page PageNotFound -> "/404"
    Api LiveDataEvents -> "/live-data/events"
    Custom (PreviewPage _) -> "/preview/{slug}"
    Custom NativeSubscriptionFallback -> "/native-subscribe"

requiredEndpointName :: Text -> EndpointName
requiredEndpointName endpointNameValue =
  case mkEndpointName endpointNameValue of
    Right endpointName -> endpointName
    Left metadataError -> error ("invalid two-pages endpoint name: " <> show metadataError)

requiredRouteTemplate :: Text -> RouteTemplate
requiredRouteTemplate routeTemplateValue =
  case mkRouteTemplate routeTemplateValue of
    Right routeTemplate -> routeTemplate
    Left metadataError -> error ("invalid two-pages route template: " <> show metadataError)

-- | Harch's route codec needs one route-identity operation. In this example a
-- route's rendered internal path is unique, so equality deliberately stays at
-- that public composition boundary rather than exposing equality for each
-- route fragment.
instance Eq TwoPageRoute where
  leftRoute == rightRoute = routeHref leftRoute == routeHref rightRoute

routeCodec :: RouteCodec TwoPageRoute ()
routeCodec =
  RouteCodec
    { parseRoute = \requestContext location ->
        let normalizedPath = safeUrlText (encodeRouteLocation (location {routeQueryFields = []}))
         in case normalizedPath of
              "/live-data/events" ->
                RouteParsed
                  RouteRequest
                    { requestRoute = Api LiveDataEvents,
                      requestContext = requestContext
                    }
              "/native-subscribe" ->
                RouteParsed
                  RouteRequest
                    { requestRoute = Custom NativeSubscriptionFallback,
                      requestContext = requestContext
                    }
              _ ->
                case parsePreviewPath normalizedPath of
                  Just previewSlug ->
                    RouteParsed
                      RouteRequest
                        { requestRoute = Custom (PreviewPage previewSlug),
                          requestContext = requestContext
                        }
                  Nothing ->
                    maybe RouteNotMatched (RouteParsed . (\page -> RouteRequest {requestRoute = Page page, requestContext = requestContext})) (parsePageRoute normalizedPath),
      renderRoute = routeLocation . requestRoute,
      notFoundRequest = \requestContext ->
        RouteRequest {requestRoute = Page PageNotFound, requestContext = requestContext},
      routeMethods = routeMethodPolicy . twoPageRouteMethods
    }

twoPageRouteMethods :: TwoPageRoute -> [RouteMethod]
twoPageRouteMethods route =
  case route of
    Page PageNotFound -> []
    Page _ -> [RouteGet]
    Api LiveDataEvents -> [RouteGet]
    Custom (PreviewPage _) -> [RouteGet]
    Custom NativeSubscriptionFallback -> [RouteGet, RoutePost]

routeHref :: TwoPageRoute -> Text
routeHref route =
  case route of
    Page page -> pageRoutePath page
    Api LiveDataEvents -> "/live-data/events"
    Custom (PreviewPage previewSlug) -> "/preview/" <> previewSlugText previewSlug
    Custom NativeSubscriptionFallback -> "/native-subscribe"

routeLocation :: TwoPageRoute -> RouteLocation
routeLocation route =
  RouteLocation
    { routePathSegments = routeSegmentsFor route,
      routeQueryFields = []
    }

-- | Rendering routes straight into structured segments avoids reparsing the
-- application's own trusted route strings.  Each segment is either static or
-- a 'PreviewSlug', whose smart constructor rules out path separators.
routeSegmentsFor :: TwoPageRoute -> [PathSegment]
routeSegmentsFor route =
  case route of
    Page HomePage -> []
    Page SecondPage -> [requiredPathSegment "second"]
    Page LiveDataPage -> [requiredPathSegment "live-data"]
    Page PageNotFound -> [requiredPathSegment "404"]
    Api LiveDataEvents -> [requiredPathSegment "live-data", requiredPathSegment "events"]
    Custom (PreviewPage previewSlug) -> [requiredPathSegment "preview", requiredPathSegment (previewSlugText previewSlug)]
    Custom NativeSubscriptionFallback -> [requiredPathSegment "native-subscribe"]

twoPageActionPath :: TwoPageActionTarget -> Maybe Text
twoPageActionPath = staticActionPath twoPageActions

twoPageActions :: ActionCodec TwoPageActionTarget () () TwoPageAction
twoPageActions =
  singleActionCodecWithMetadata
    ()
    (post "/actions/subscribe")
    subscribeActionMetadata
    (SubscribeAction <$> singleOrDefault "" (formField "email" textValue))

twoPageActionEndpointMetadata :: Text -> Text -> () -> Maybe (EndpointMetadata ())
twoPageActionEndpointMetadata methodValue pathValue _ =
  staticActionEndpointMetadata twoPageActions methodValue pathValue

subscribeActionMetadata :: EndpointMetadata ()
subscribeActionMetadata =
  mkEndpointMetadata
    (requiredEndpointName "two-pages.subscribe")
    (requiredRouteTemplate "/actions/subscribe")
    ActionEndpoint
    AllowUnauthenticated

-- | Every branch renders a relative path built from this application's own
-- fixed route structure, never from unvalidated caller text, so
-- 'mkSafeUrl' rejecting it here would only ever mean a route itself was
-- defined to render an unsafe URL — a programming mistake in this
-- function, not a runtime condition 'pageLink' callers need to handle.
-- The failure path is extracted into 'twoPageNavigationHref' so a
-- dedicated test can force it directly with deliberately unsafe text, rather
-- than forcing the diagnostic message eagerly at this
-- always-safe call site.
twoPageNavigationPath :: TwoPageNavigationTarget -> SafeUrl
twoPageNavigationPath navigationTarget =
  twoPageNavigationHref renderedPath
  where
    renderedPath = case navigationTarget of
      NavigationPage page -> pageRoutePath page
      NavigationPreview previewSlug -> "/preview/" <> previewSlugText previewSlug

-- | Requires a navigation target's rendered path to already be a safe
-- relative URL so the failure diagnostic can be forced directly by a test.
twoPageNavigationHref :: Text -> SafeUrl
twoPageNavigationHref renderedPath =
  requiredSafeUrlOrDie
    ("twoPageNavigationPath: rendered an unsafe URL: " <> renderedPath)
    (mkSafeUrl renderedPath)

mkPreviewSlug :: Text -> Maybe PreviewSlug
mkPreviewSlug value =
  if not (Text.null value) && Text.all validSlugCharacter value
    then Just (PreviewSlug value)
    else Nothing
  where
    validSlugCharacter character =
      isAsciiLower character || isDigit character || character == '-'

previewSlugText :: PreviewSlug -> Text
previewSlugText (PreviewSlug value) = value

parsePreviewPath :: Text -> Maybe PreviewSlug
parsePreviewPath path =
  Text.stripPrefix "/preview/" path >>= mkPreviewSlug
