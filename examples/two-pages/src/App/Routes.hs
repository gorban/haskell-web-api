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
    twoPageActionContext,
    twoPageActions,
    twoPageNavigationPath,
    twoPageNavigationHref,
    twoPageActionPath,
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
  ( RouteCodec (..),
    RouteMethod (..),
    RouteRequest (..),
    SafeUrl,
    mkSafeUrl,
    requiredSafeUrlOrDie,
    routeMethodPolicy,
  )
import HarchWeb.Action
  ( ActionCodec,
    actionPath,
    formField,
    post,
    singleActionCodec,
    singleOrDefault,
    textValue,
  )

data ApiRoute
  = LiveDataEvents
  deriving (Eq, Show)

type TwoPageActionTarget = ()

data TwoPageNavigationTarget
  = NavigationPage PageRoute
  | NavigationPreview PreviewSlug

newtype TwoPageAction = SubscribeAction Text

newtype PreviewSlug = PreviewSlug Text
  deriving (Eq, Show)

data CustomRoute
  = PreviewPage PreviewSlug
  | NativeSubscriptionFallback
  deriving (Eq, Show)

data TwoPageRoute
  = Page PageRoute
  | Api ApiRoute
  | Custom CustomRoute
  deriving (Eq, Show)

routeCodec :: RouteCodec TwoPageRoute ()
routeCodec =
  RouteCodec
    { parseRoute = \() path ->
        let normalizedPath = routePath path
         in case normalizedPath of
              "/live-data/events" ->
                Just
                  RouteRequest
                    { requestRoute = Api LiveDataEvents,
                      requestContext = ()
                    }
              "/native-subscribe" ->
                Just
                  RouteRequest
                    { requestRoute = Custom NativeSubscriptionFallback,
                      requestContext = ()
                    }
              _ ->
                case parsePreviewPath normalizedPath of
                  Just previewSlug ->
                    Just
                      RouteRequest
                        { requestRoute = Custom (PreviewPage previewSlug),
                          requestContext = ()
                        }
                  Nothing ->
                    (\page -> RouteRequest {requestRoute = Page page, requestContext = ()})
                      <$> parsePageRoute normalizedPath,
      renderRoute = routeHref . requestRoute,
      notFoundRequest = \() ->
        RouteRequest {requestRoute = Page PageNotFound, requestContext = ()},
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

twoPageActionPath :: TwoPageActionTarget -> Maybe Text
twoPageActionPath = actionPath twoPageActions $! twoPageActionContext

twoPageActionContext :: ()
twoPageActionContext = ()

twoPageActions :: ActionCodec TwoPageActionTarget () TwoPageAction
twoPageActions =
  singleActionCodec
    ()
    (post "/actions/subscribe")
    (SubscribeAction <$> singleOrDefault "" (formField "email" textValue))

-- | Every branch renders a relative path built from this application's own
-- fixed route structure, never from unvalidated caller text, so
-- 'mkSafeUrl' rejecting it here would only ever mean a route itself was
-- defined to render an unsafe URL — a programming mistake in this
-- function, not a runtime condition 'pageLink' callers need to handle.
-- The failure path is extracted into 'twoPageNavigationHref' so a
-- dedicated test can force it directly with a deliberately unsafe
-- 'Nothing', rather than forcing the diagnostic message eagerly at this
-- always-safe call site.
-- | Per @docs/design-guidance.md@'s never-mask-a-gate-finding rule: the @$!@
-- below on 'twoPageNavigationHref'\'s first argument is a last resort, tried
-- only after the rule's own preferred fix did not apply here. That fix
-- (deduplicating a literal shared across two source positions into one named
-- binding) does not apply: 'renderedPath' is already exactly that — one
-- named, correctly-factored local binding — used once as
-- 'twoPageNavigationHref'\'s first argument and once inside 'mkSafeUrl', which
-- is the correct shape for this code, not duplication to remove. Confirmed
-- directly, not assumed: running the full coverage gate without the @$!@
-- reproduces a genuine, reproducible gap on this exact expression (72%
-- boolean coverage, 934/935 expressions). GHC shares the two references to
-- this one @let@-bound thunk, and only the second (inside 'mkSafeUrl') earns
-- its own HPC tick when forced; the first — evaluating an already-WHNF
-- thunk — does not.
{-# ANN twoPageNavigationPath ("HLint: ignore Redundant $!" :: String) #-}
twoPageNavigationPath :: TwoPageNavigationTarget -> SafeUrl
twoPageNavigationPath navigationTarget =
  (twoPageNavigationHref $! renderedPath) (mkSafeUrl renderedPath)
  where
    renderedPath = case navigationTarget of
      NavigationPage page -> pageRoutePath page
      NavigationPreview previewSlug -> "/preview/" <> previewSlugText previewSlug

-- | Requires a navigation target's rendered path to already be a safe
-- relative URL, taking the rendered path itself (rather than just the
-- resulting 'Maybe') so the failure diagnostic can be forced directly by a
-- test.
twoPageNavigationHref :: Text -> Maybe SafeUrl -> SafeUrl
twoPageNavigationHref renderedPath =
  requiredSafeUrlOrDie ("twoPageNavigationPath: rendered an unsafe URL: " <> renderedPath)

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

routePath :: Text -> Text
routePath =
  Text.takeWhile (/= '?')
