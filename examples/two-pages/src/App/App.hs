{-# LANGUAGE OverloadedStrings #-}

module App.App
  ( buildApplication,
    twoPageServerConfig,
    twoPageSite,
    TwoPageAction (..),
  )
where

import App.Components.Layout (twoPageShell)
import App.CustomPages.Preview (previewPageDefinition)
import App.Pages.Generated (pageRouteDefinition)
import App.Pages.Home (subscriptionResultRegion)
import App.Pages.Route.Generated (PageRoute (..))
import App.Routes
  ( ApiRoute (..),
    CustomRoute (..),
    TwoPageAction (..),
    TwoPageActionTarget (..),
    TwoPageRoute (..),
    routeCodec,
    twoPageActionPath,
  )
import Data.Text qualified as Text
import HarchWeb
  ( Application,
    ClientActionDecodeResult (..),
    ClientActionPayload (..),
    ClientActionRequest (..),
    ClientActionResponse (..),
    ListenerConfig (..),
    ListenerScheme (..),
    ObservabilityConfig (..),
    RegionPatch,
    RequestPolicyConfig (..),
    ServerConfig (..),
    ServerSentEvent (..),
    StaticAssetRoot (..),
    StaticAssetsConfig (..),
    defaultCorsPolicyConfig,
    defaultResponseSecurityHeadersConfig,
    defaultStaticAssetContentTypes,
    eventStreamResponse,
    replaceRegion,
    serverSentEventSourceFromList,
  )
import HarchWeb.Site
  ( RouteDefinition (..),
    Site (..),
    buildSiteApplication,
    simpleSite,
  )

buildApplication :: Application TwoPageRoute TwoPageAction ()
buildApplication = buildSiteApplication twoPageSite

twoPageSite :: Site TwoPageRoute TwoPageAction ()
twoPageSite =
  ( simpleSite
      "two-pages-example"
      ()
      routeCodec
      twoPageShell
      [Page HomePage, Page SecondPage, Page LiveDataPage]
      routeDefinition
  )
    { siteStaticAssets = twoPageStaticAssets,
      siteRequestPolicy = twoPageRequestPolicy,
      siteDecodeClientAction = decodeTwoPageAction,
      siteHandleClientAction = twoPageClientAction
    }

routeDefinition :: TwoPageRoute -> RouteDefinition TwoPageRoute ()
routeDefinition route =
  case route of
    Page page -> pageRouteDefinition page
    Api LiveDataEvents -> liveDataEventsRouteDefinition
    Custom (PreviewPage previewSlug) -> previewPageDefinition previewSlug

liveDataEventsRouteDefinition :: RouteDefinition TwoPageRoute ()
liveDataEventsRouteDefinition =
  RouteDefinition
    { routeNavigationLabel = Nothing,
      routeResponse = \_ -> do
        eventSource <-
          serverSentEventSourceFromList
            [ServerSentEvent (Just "update") (Just "example-1") "The live update arrived."]
        pure (eventStreamResponse eventSource)
    }

decodeTwoPageAction :: ClientActionPayload () -> ClientActionDecodeResult TwoPageAction
decodeTwoPageAction actionPayload
  | clientActionMethod actionPayload == "POST",
    clientActionPath actionPayload == twoPageActionPath Subscribe =
      maybe MalformedClientAction (DecodedClientAction . SubscribeAction) (exactlyOneField "email" (clientActionFields actionPayload))
  | otherwise = UnrecognizedClientAction

exactlyOneField :: Text.Text -> [(Text.Text, Text.Text)] -> Maybe Text.Text
exactlyOneField fieldName fields =
  case [fieldValue | (name, fieldValue) <- fields, name == fieldName] of
    [] -> Just Text.empty
    [fieldValue] -> Just fieldValue
    _ -> Nothing

twoPageClientAction :: ClientActionRequest TwoPageAction () -> IO (Maybe ClientActionResponse)
twoPageClientAction actionRequest =
  pure $
    case clientAction actionRequest of
      SubscribeAction emailAddress ->
        Just
          ( case emailAddress of
              value
                | "@" `Text.isInfixOf` value,
                  "." `Text.isInfixOf` value ->
                    ClientActionResponse
                      { clientActionStatus = 200,
                        clientActionPatches = subscriptionPatch "status" "Thanks. Your subscription request is ready.",
                        clientActionFocusId = Nothing,
                        clientActionHeaders = [],
                        clientActionObservabilityAttributes = [],
                        clientActionLogEntries = []
                      }
              _ ->
                ClientActionResponse
                  { clientActionStatus = 422,
                    clientActionPatches = subscriptionPatch "alert" "Enter a valid email address.",
                    clientActionFocusId = Just "subscription-email",
                    clientActionHeaders = [],
                    clientActionObservabilityAttributes = [],
                    clientActionLogEntries = []
                  }
          )

subscriptionPatch :: Text.Text -> Text.Text -> [RegionPatch]
subscriptionPatch liveRole message =
  [replaceRegion (subscriptionResultRegion liveRole message)]

twoPageServerConfig :: ServerConfig
twoPageServerConfig =
  ServerConfig
    { listenerConfigs =
        [ ListenerConfig
            { listenerHost = "127.0.0.1",
              listenerPort = 8080,
              listenerScheme = Http,
              listenerTls = Nothing,
              listenerAcme = Nothing
            }
        ],
      staticAssets = twoPageStaticAssets,
      requestPolicy = twoPageRequestPolicy,
      observability =
        ObservabilityConfig
          { tracingExporter = Nothing,
            metricsExporter = Nothing
          }
    }

twoPageStaticAssets :: StaticAssetsConfig
twoPageStaticAssets =
  StaticAssetsConfig
    { staticAssetRoots = [StaticAssetRoot {staticUrlPrefix = "/assets", staticDirectory = "public"}],
      staticAssetContentTypes = defaultStaticAssetContentTypes,
      staticCacheControlSeconds = Nothing
    }

twoPageRequestPolicy :: RequestPolicyConfig
twoPageRequestPolicy =
  RequestPolicyConfig
    { redirectHttpToHttps = False,
      httpsRedirectPort = Nothing,
      strictTransportSecurity = Nothing,
      trustForwardedHeaders = False,
      corsPolicy = defaultCorsPolicyConfig,
      responseSecurityHeaders = defaultResponseSecurityHeadersConfig
    }
