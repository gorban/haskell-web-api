{-# LANGUAGE OverloadedStrings #-}

-- | Locale-root adaptation for an already-composed local module.  Locale
-- parsing remains root-owned and typed; local Catalog, Orders, and Public
-- modules receive only the selected safe request context.
module App.Composed.Localized
  ( localizeApplicationModule,
    requestContextFromWai,
  )
where

import App.Composed.Model
import Data.ByteString qualified as ByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Action (ActionCodecError, prefixActionCodecByContext)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata (EndpointMetadata (..), requiredRouteTemplateOrDie, routeTemplateText)
import HarchWeb.EndpointSecurity (EndpointGuard (..), EndpointGuardResult (..), EndpointRequest (..))
import HarchWeb.Localization (Locale, localeText)
import HarchWeb.RequestContext (CoreRequestContext (..), RequestContext (..))
import HarchWeb.Routing (RouteCodec (..), RouteLocation (..), RouteParseResult (..), RouteRequest (..), pathSegmentText, requiredPathSegment)
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server (Response, mapResponsePage)
import HarchWeb.Site (RouteDefinition (..))
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

localizeApplicationModule :: LocalePolicy -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> Either ActionCodecError (ApplicationModule RootRoute RootActionTarget RootAction ComposedContext RootAuthorization)
localizeApplicationModule localePolicy localizedModule = do
  localizedActions <-
    prefixActionCodecByContext (localePrefix . requestCore) "/{locale}" (moduleActionCodec localizedModule)
  pure
    ApplicationModule
      { moduleName = requiredModuleNameOrDie "root",
        moduleOwnsRoute = isLocalizedRouteOwned localizedModule,
        moduleRouteMountChain = \(Localized _ localRoute) ->
          requiredModuleNameOrDie "root" NonEmpty.:| NonEmpty.toList (moduleRouteMountChain localizedModule localRoute),
        moduleRouteCodec = localeRootCodec localePolicy localizedModule,
        moduleDeclaredRoutes = map (Localized (defaultLocale localePolicy)) (moduleDeclaredRoutes localizedModule),
        moduleEndpoints = localizedRootDefinition localePolicy localizedModule,
        moduleActionCodec = localizedActions,
        moduleHandleAction = moduleHandleAction localizedModule,
        moduleGuards = map (localizedRootGuard localePolicy localizedModule) (moduleGuards localizedModule)
      }

localeRootCodec :: LocalePolicy -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> RouteCodec RootRoute ComposedContext
localeRootCodec localePolicy localizedModule =
  RouteCodec
    { parseRoute = parseRootRoute,
      renderRoute = renderRootRoute,
      notFoundRequest = \requestContext ->
        let selectedLocale = requestLocale (requestCore requestContext)
            localNotFound = notFoundRequest (moduleRouteCodec localizedModule) (setRequestLocale (defaultLocale localePolicy) selectedLocale requestContext)
         in RouteRequest (Localized selectedLocale (requestRoute localNotFound)) requestContext,
      routeMethods = \(Localized _ localRoute) -> Routing.routeMethods (moduleRouteCodec localizedModule) localRoute
    }
  where
    parseRootRoute rootContext location =
      let (selectedLocale, localLocation) = splitLocalePrefix localePolicy rootContext location
          localizedContext = setRequestLocale (defaultLocale localePolicy) selectedLocale rootContext
       in case parseRoute (moduleRouteCodec localizedModule) localizedContext localLocation of
            RouteNotMatched -> RouteNotMatched
            RouteMalformed routeError -> RouteMalformed routeError
            RouteParsed localRequest -> RouteParsed (RouteRequest (Localized selectedLocale (requestRoute localRequest)) (requestContext localRequest))

    renderRootRoute rootRequest =
      case requestRoute rootRequest of
        Localized selectedLocale localRoute ->
          let localLocation = renderRoute (moduleRouteCodec localizedModule) (RouteRequest localRoute (setRequestLocale (defaultLocale localePolicy) selectedLocale (requestContext rootRequest)))
           in localLocation {routePathSegments = requiredPathSegment (localeText selectedLocale) : routePathSegments localLocation}

localizedRootDefinition :: LocalePolicy -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> RootRoute -> RouteDefinition RootRoute ComposedContext RootAuthorization
localizedRootDefinition localePolicy localizedModule rootRoute =
  case rootRoute of
    Localized selectedLocale localRoute ->
      let localDefinition = moduleEndpoints localizedModule localRoute
       in localDefinition
            { routeMetadata = prefixLocaleMetadata (routeMetadata localDefinition),
              routeResponse = \request rootRequest -> do
                localResponse <- routeResponse localDefinition request (RouteRequest localRoute (setRequestLocale (defaultLocale localePolicy) selectedLocale (requestContext rootRequest)))
                pure (mapLocalizedResponse selectedLocale (requestContext rootRequest) localResponse)
            }

localizedRootGuard :: LocalePolicy -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization -> EndpointGuard LocalizedRoute ComposedContext RootAuthorization -> EndpointGuard RootRoute ComposedContext RootAuthorization
localizedRootGuard localePolicy localizedModule (EndpointGuard guard) =
  EndpointGuard $ \rootRequest ->
    case requestRoute (endpointRouteRequest rootRequest) of
      Localized selectedLocale localRoute -> do
        let parentContext = requestContext (endpointRouteRequest rootRequest)
            localEndpoint = moduleEndpoints localizedModule localRoute
        guardResult <-
          guard
            EndpointRequest
              { endpointWaiRequest = endpointWaiRequest rootRequest,
                endpointRouteRequest = RouteRequest localRoute (setRequestLocale (defaultLocale localePolicy) selectedLocale parentContext),
                endpointMetadata = routeMetadata localEndpoint,
                endpointSecurityEventSink = endpointSecurityEventSink rootRequest,
                endpointDispatchKind = endpointDispatchKind rootRequest
              }
        case guardResult of
          ContinueEndpoint continuedContext -> pure (ContinueEndpoint continuedContext)
          HaltEndpoint response -> pure (HaltEndpoint (mapLocalizedResponse selectedLocale parentContext response))

isLocalizedRouteOwned :: ApplicationModule LocalizedRoute target action ComposedContext authorization -> RootRoute -> Bool
isLocalizedRouteOwned localizedModule rootRoute =
  case rootRoute of
    Localized _ localRoute -> moduleOwnsRoute localizedModule localRoute

mapLocalizedResponse :: Locale -> ComposedContext -> Response LocalizedRoute ComposedContext -> Response RootRoute ComposedContext
mapLocalizedResponse selectedLocale parentContext =
  mapResponsePage $ \page ->
    page {pageRoute = Localized selectedLocale (pageRoute page), pageContext = parentContext}

prefixLocaleMetadata :: EndpointMetadata RootAuthorization -> EndpointMetadata RootAuthorization
prefixLocaleMetadata metadata =
  metadata {endpointRouteTemplate = requiredRouteTemplateOrDie ("/{locale}" <> routeTemplateText (endpointRouteTemplate metadata))}

splitLocalePrefix :: LocalePolicy -> ComposedContext -> RouteLocation -> (Locale, RouteLocation)
splitLocalePrefix localePolicy requestContext location =
  case routePathSegments location of
    firstSegment : remainingSegments ->
      case allowedLocale localePolicy (pathSegmentText firstSegment) of
        Just selectedLocale -> (selectedLocale, location {routePathSegments = remainingSegments})
        Nothing -> (requestLocale (requestCore requestContext), location)
    [] -> (requestLocale (requestCore requestContext), location)

setRequestLocale :: Locale -> Locale -> ComposedContext -> ComposedContext
setRequestLocale fallbackLocale selectedLocale requestContext =
  requestContext
    { requestCore =
        (requestCore requestContext)
          { requestLocale = selectedLocale,
            requestLocaleFallbacks = selectedLocale : filter (/= selectedLocale) [fallbackLocale]
          }
    }

requestContextFromWai :: LocalePolicy -> Wai.Request -> ComposedContext -> ComposedContext
requestContextFromWai localePolicy request requestContext =
  setRequestLocale (defaultLocale localePolicy) selectedLocale requestContext
  where
    selectedLocale =
      resolveLocale
        localePolicy
        LocaleResolutionInput
          { localeExplicitPrefix = listToMaybe (Wai.pathInfo request) >>= allowedLocale localePolicy,
            localeCookieValue = lookupLocaleCookie request,
            localeAcceptLanguage = lookupHeaderText Http.hAcceptLanguage request,
            localeIdentity = requestIdentity requestContext
          }

lookupLocaleCookie :: Wai.Request -> Maybe Text
lookupLocaleCookie request = do
  cookieHeader <- lookup Http.hCookie (Wai.requestHeaders request)
  localeValue <- lookup "locale" (map cookiePair (ByteString.split 59 cookieHeader))
  either (const Nothing) Just (TextEncoding.decodeUtf8' localeValue)
  where
    cookiePair cookie =
      let trimmedCookie = ByteString.dropWhile (== 32) cookie
          (name, valueWithSeparator) = ByteString.break (== 61) trimmedCookie
       in (name, ByteString.drop 1 valueWithSeparator)

lookupHeaderText :: Http.HeaderName -> Wai.Request -> Maybe Text
lookupHeaderText headerName request =
  lookup headerName (Wai.requestHeaders request) >>= either (const Nothing) Just . TextEncoding.decodeUtf8'

localePrefix :: CoreRequestContext -> Text
localePrefix core = "/" <> localeText (requestLocale core)
