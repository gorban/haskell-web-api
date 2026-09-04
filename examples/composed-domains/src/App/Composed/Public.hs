{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Anonymous public routes and typed static-asset ownership for the composed
-- root.  Filesystem delivery remains in HarchWeb's single asset interpreter;
-- this module only declares root-owned routing and endpoint metadata.
module App.Composed.Public
  ( buildPublicModule,
    buildPublicModuleWithAdmissionWorkflow,
  )
where

import App.Composed.Admission (AdmissionConfig (..), AdmissionProofConfig, AdmissionSubmissionResult (..), submitAdmission)
import App.Composed.Admission.Types (AdmissionLoginName, AdmissionPrincipalId, mkAdmissionLoginName)
import App.Composed.Model
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import HarchWeb.Action (ActionCodec, ClientActionDecodeResult (DecodedClientAction), ClientActionPayload (..), actionCodec, decodeAction, formField, parseField, post, prefixActionCodecByContext, publicAction, required)
import HarchWeb.ApplicationModule (ApplicationModule (..))
import HarchWeb.Controls qualified as Controls
import HarchWeb.Csrf
  ( CsrfProtection,
    CsrfVerification (..),
    PageSecurity,
    csrfClearCookieHeader,
    csrfProtectionUnavailable,
    pageCsrfValue,
    pageSecurityCsrf,
    verifyCsrfToken,
  )
import HarchWeb.Document (Page (..))
import HarchWeb.EndpointMetadata
  ( AccessRequirement (AllowUnauthenticated),
    EndpointProtocol (ApiEndpoint, AssetEndpoint, HtmlEndpoint),
    mkEndpointMetadata,
    requiredEndpointNameOrDie,
    requiredRouteTemplateOrDie,
  )
import HarchWeb.Localization (localeText)
import HarchWeb.Markup (Attribute, ElementId, Html, autocomplete, buttonTag, element, headingOneTag, inputMode, inputTag, inputType, literalElementId, maxLength, name, paragraphTag, sectionTag, text, value, voidElement)
import HarchWeb.Markup qualified as Markup
import HarchWeb.RequestContext (CoreRequestContext (requestLocale), RequestContext (requestClient, requestCore))
import HarchWeb.Routing
  ( RouteCodec (..),
    RouteLocation (..),
    RouteMethodPolicy (RouteHidden),
    RouteRequest (..),
    mapRouteParseResult,
    pathSegmentText,
    requiredPathSegment,
  )
import HarchWeb.Routing qualified as Routing
import HarchWeb.SecurityEvent (requiredModuleNameOrDie)
import HarchWeb.Server
  ( ActionNavigation (NavigateInternal, StayOnCurrentRoute),
    ClientActionRequest (..),
    ClientActionResponse (..),
    HistoryMode (ReplaceHistory),
    NonPageResponse (NonPageBodyResponse, NonPageProtocolResponse),
    PageResult (RenderedPage),
    RequestBodyReadFailure (RequestBodyLimitExceeded),
    ResponseBody (..),
    nonPageInternalRedirectResponseWithHeaders,
    parseClientActionFields,
    readRequestBodyUpTo,
    unboundedRouteExecutionPolicy,
    validateActionCsrfTransport,
  )
import HarchWeb.Session (OpaqueSession, renderSessionCookie, sessionId)
import HarchWeb.Site (RouteDefinition (..), RouteHandler (..))
import HarchWeb.StaticAssets (StaticAssetsConfig)
import HarchWeb.StaticAssets.Route
  ( StaticAssetRoute (..),
    staticAssetRouteCodec,
    staticAssetRouteResponse,
  )
import HarchWeb.Totp (TotpCode, mkTotpCode)
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai

buildPublicModule :: StaticAssetsConfig -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildPublicModule staticAssetsConfig = buildPublicModuleWithAdmissionWorkflow staticAssetsConfig csrfProtectionUnavailable Nothing

buildPublicModuleWithAdmissionWorkflow :: StaticAssetsConfig -> CsrfProtection ComposedContext -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ApplicationModule LocalizedRoute RootActionTarget RootAction ComposedContext RootAuthorization
buildPublicModuleWithAdmissionWorkflow staticAssetsConfig csrfProtection _admissionWorkflow =
  ApplicationModule
    { moduleName = requiredModuleNameOrDie "root.public",
      moduleOwnsRoute = \case
        Public _ -> True
        _ -> False,
      moduleRouteMountChain = const (requiredModuleNameOrDie "root.public" NonEmpty.:| [requiredModuleNameOrDie "public"]),
      moduleRouteCodec = publicRouteCodec staticAssetsConfig _admissionWorkflow,
      moduleDeclaredRoutes = publicDeclaredRoutes _admissionWorkflow,
      moduleEndpoints = publicRouteDefinition staticAssetsConfig csrfProtection _admissionWorkflow admissionActions,
      moduleActionCodec = admissionActions,
      moduleActionRoute = \_ target ->
        case (_admissionWorkflow, target) of
          (Just _, AdmissionActionTarget) -> Just (Public PublicAdmission)
          _ -> Nothing,
      moduleHandleAction = handleAdmissionAction,
      moduleGuards = []
    }
  where
    admissionActions =
      case _admissionWorkflow of
        Nothing -> either (error . show) id (actionCodec [])
        Just _ ->
          either (error . show) id $
            actionCodec
              [ publicAction
                  AdmissionActionTarget
                  (post admissionActionPath)
                  (requiredEndpointNameOrDie "root.public.admission.submit")
                  (requiredRouteTemplateOrDie admissionActionPath)
                  (SubmitAdmission <$> required (formField "login" (parseField mkAdmissionLoginName)) <*> required (formField "code" (parseField mkTotpCode)) <*> required (formField "return" (parseField mkAdmissionReturnTarget)))
              ]
    handleAdmissionAction actionRequest =
      case (_admissionWorkflow, clientAction actionRequest) of
        (Just (sessionConfig, proofConfig), SubmitAdmission loginName code returnTarget) -> do
          result <- submitAdmission sessionConfig proofConfig (rootClientAddress (requestClient (clientActionContext actionRequest))) loginName code
          pure (Just (admissionResponse sessionConfig actionRequest returnTarget result))
        _ -> pure Nothing
    admissionResponse sessionConfig actionRequest returnTarget submissionResult =
      ClientActionResponse
        { clientActionStatus = admissionSubmissionStatus submissionResult,
          clientActionPatches = [],
          clientActionFocusId = Nothing,
          clientActionNavigation = admissionSubmissionNavigation actionRequest returnTarget submissionResult,
          clientActionHeaders = admissionSubmissionHeaders sessionConfig submissionResult,
          clientActionObservabilityAttributes = [],
          clientActionLogEntries = []
        }

admissionSubmissionStatus :: AdmissionSubmissionResult -> Http.Status
admissionSubmissionStatus submissionResult =
  case submissionResult of
    AdmissionSubmissionAccepted _ -> Http.status200
    AdmissionSubmissionRejected -> Http.status422
    AdmissionSubmissionUnavailable -> Http.status503

admissionSubmissionNavigation :: ClientActionRequest RootAction ComposedContext -> AdmissionReturnTarget -> AdmissionSubmissionResult -> ActionNavigation LocalizedRoute ComposedContext
admissionSubmissionNavigation actionRequest returnTarget submissionResult =
  case submissionResult of
    AdmissionSubmissionAccepted _ -> NavigateInternal ReplaceHistory (RouteRequest (admissionReturnTargetRoute returnTarget) (clientActionContext actionRequest))
    AdmissionSubmissionRejected -> StayOnCurrentRoute
    AdmissionSubmissionUnavailable -> StayOnCurrentRoute

admissionSubmissionHeaders :: AdmissionConfig -> AdmissionSubmissionResult -> [Http.Header]
admissionSubmissionHeaders sessionConfig submissionResult =
  case submissionResult of
    AdmissionSubmissionAccepted session ->
      [ ("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (admissionConfigCookiePolicy sessionConfig) (sessionId session))),
        csrfClearCookieHeader
      ]
    AdmissionSubmissionRejected -> []
    AdmissionSubmissionUnavailable -> []

admissionActionPath :: Text.Text
admissionActionPath = "/public/admission/actions/submit"

publicDeclaredRoutes :: Maybe (AdmissionConfig, AdmissionProofConfig) -> [LocalizedRoute]
publicDeclaredRoutes maybeAdmissionWorkflow =
  [ Public PublicAdmission,
    Public PublicLogin,
    Public (PublicAsset (StaticAssetRoute [requiredPathSegment "public", requiredPathSegment "assets", requiredPathSegment "app.css"])),
    Public PublicNotFound
  ]
    <> [Public PublicAdmissionNativeFallback | isJust maybeAdmissionWorkflow]

publicRouteCodec :: StaticAssetsConfig -> Maybe (AdmissionConfig, AdmissionProofConfig) -> RouteCodec LocalizedRoute ComposedContext
publicRouteCodec staticAssetsConfig maybeAdmissionWorkflow =
  RouteCodec
    { parseRoute = \requestContext location ->
        case routePathSegments location of
          [publicSegment, loginSegment]
            | pathSegmentText publicSegment == "public", pathSegmentText loginSegment == "login" -> Routing.RouteParsed (RouteRequest (Public PublicLogin) requestContext)
          [publicSegment, admissionSegment]
            | pathSegmentText publicSegment == "public", pathSegmentText admissionSegment == "admission" -> Routing.RouteParsed (RouteRequest (Public PublicAdmission) requestContext)
          [publicSegment, admissionSegment, nativeSegment]
            | pathSegmentText publicSegment == "public",
              pathSegmentText admissionSegment == "admission",
              pathSegmentText nativeSegment == "native" ->
                case maybeAdmissionWorkflow of
                  Just _ -> Routing.RouteParsed (RouteRequest (Public PublicAdmissionNativeFallback) requestContext)
                  Nothing -> Routing.RouteNotMatched
          _ -> mapAssetRoute requestContext location,
      renderRoute = \routeRequest ->
        case requestRoute routeRequest of
          Public PublicAdmission -> RouteLocation [requiredPathSegment "public", requiredPathSegment "admission"] []
          Public PublicAdmissionNativeFallback
            | isJust maybeAdmissionWorkflow -> RouteLocation [requiredPathSegment "public", requiredPathSegment "admission", requiredPathSegment "native"] []
          Public PublicLogin -> RouteLocation [requiredPathSegment "public", requiredPathSegment "login"] []
          Public (PublicAsset assetRoute) -> RouteLocation (staticAssetPathSegments assetRoute) []
          Public PublicNotFound -> RouteLocation [requiredPathSegment "public", requiredPathSegment "404"] []
          _ -> error "attempted to render a non-public route through the public module",
      notFoundRequest = RouteRequest (Public PublicNotFound),
      routeMethods = \case
        Public PublicAdmission -> Routing.routeMethodPolicy [Routing.RouteGet]
        Public PublicAdmissionNativeFallback
          | isJust maybeAdmissionWorkflow -> Routing.routeMethodPolicy [Routing.RoutePost]
        Public PublicLogin -> Routing.routeMethodPolicy [Routing.RouteGet]
        Public (PublicAsset _) -> Routing.routeMethodPolicy [Routing.RouteGet]
        Public PublicNotFound -> RouteHidden
        _ -> RouteHidden
    }
  where
    staticCodec = staticAssetRouteCodec staticAssetsConfig
    mapAssetRoute requestContext location =
      mapRouteParseResult (Public . PublicAsset) (parseRoute staticCodec requestContext location)

publicRouteDefinition :: StaticAssetsConfig -> CsrfProtection ComposedContext -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ActionCodec RootActionTarget ComposedContext RootAuthorization RootAction -> LocalizedRoute -> RouteDefinition LocalizedRoute ComposedContext RootAuthorization
publicRouteDefinition staticAssetsConfig csrfProtection maybeAdmissionWorkflow admissionActions routeValue =
  case routeValue of
    Public PublicAdmission ->
      RouteDefinition
        { routeNavigationLabel = Nothing,
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.admission") (requiredRouteTemplateOrDie "/public/admission") HtmlEndpoint AllowUnauthenticated,
          routeMethods = [Routing.RouteGet],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeHandler = PageRouteHandler $ \pageSecurity request ->
            pure
              ( RenderedPage
                  Page
                    { pageTitle = "Admission",
                      pageRoute = Public PublicAdmission,
                      pageContext = requestContext request,
                      pageBody = admissionPage pageSecurity (requestContext request) maybeAdmissionWorkflow admissionActions,
                      pageBootstrapHooks = []
                    }
              )
        }
    Public PublicAdmissionNativeFallback ->
      case maybeAdmissionWorkflow of
        Just (sessionConfig, proofConfig) ->
          RouteDefinition
            { routeNavigationLabel = Nothing,
              routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.admission.native") (requiredRouteTemplateOrDie "/public/admission/native") ApiEndpoint AllowUnauthenticated,
              routeMethods = [Routing.RoutePost],
              routeExecutionPolicy = unboundedRouteExecutionPolicy,
              routeHandler = ProtocolRouteHandler (nativeAdmissionFallbackHandler csrfProtection sessionConfig proofConfig admissionActions)
            }
        Nothing -> error "admission native fallback selected while admission is disabled"
    Public PublicLogin ->
      RouteDefinition
        { routeNavigationLabel = Just "Login",
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.login") (requiredRouteTemplateOrDie "/public/login") HtmlEndpoint AllowUnauthenticated,
          routeMethods = [Routing.RouteGet],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeHandler = PageRouteHandler $ \_ request ->
            pure
              ( RenderedPage
                  Page
                    { pageTitle = "Login",
                      pageRoute = Public PublicLogin,
                      pageContext = requestContext request,
                      pageBody = element headingOneTag [] [text "Login"],
                      pageBootstrapHooks = []
                    }
              )
        }
    Public (PublicAsset assetRoute) ->
      RouteDefinition
        { routeNavigationLabel = Nothing,
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.assets") (requiredRouteTemplateOrDie "/public/assets/*") AssetEndpoint AllowUnauthenticated,
          routeMethods = [Routing.RouteGet],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeHandler = ProtocolRouteHandler $ \request _ ->
            NonPageProtocolResponse <$> staticAssetRouteResponse staticAssetsConfig request assetRoute
        }
    Public PublicNotFound ->
      RouteDefinition
        { routeNavigationLabel = Nothing,
          routeMetadata = mkEndpointMetadata (requiredEndpointNameOrDie "root.public.not-found") (requiredRouteTemplateOrDie "/public/404") HtmlEndpoint AllowUnauthenticated,
          routeMethods = [],
          routeExecutionPolicy = unboundedRouteExecutionPolicy,
          routeHandler = PageRouteHandler $ \_ request ->
            pure
              ( RenderedPage
                  Page
                    { pageTitle = "Not Found",
                      pageRoute = Public PublicNotFound,
                      pageContext = requestContext request,
                      pageBody = element headingOneTag [] [text "Not Found"],
                      pageBootstrapHooks = []
                    }
              )
        }
    _ -> error "attempted to select a non-public route through the public module"

admissionPage :: PageSecurity -> ComposedContext -> Maybe (AdmissionConfig, AdmissionProofConfig) -> ActionCodec RootActionTarget ComposedContext RootAuthorization RootAction -> Html
admissionPage pageSecurity requestContext maybeAdmissionWorkflow admissionActions =
  element
    sectionTag
    []
    ( [element headingOneTag [] [text "Admission"]]
        <> case maybeAdmissionWorkflow of
          Nothing -> [element paragraphTag [] [text "Admission is not enabled."]]
          Just _ -> [admissionForm pageSecurity requestContext admissionActions]
    )

admissionForm :: PageSecurity -> ComposedContext -> ActionCodec RootActionTarget ComposedContext RootAuthorization RootAction -> Html
admissionForm pageSecurity requestContext admissionActions =
  Controls.renderActionForm
    ( Controls.actionForm
        localizedAdmissionActions
        requestContext
        AdmissionActionTarget
        Controls.defaultActionFormAttributes
          { Controls.actionFormAriaLabel = Just "Admission",
            Controls.actionFormCapabilities =
              [ Controls.NativeFallback
                  Controls.NativeActionFallback
                    { Controls.nativeActionFallbackPath = "/" <> localeText (requestLocale (requestCore requestContext)) <> "/public/admission/native",
                      Controls.nativeActionFallbackMethod = Controls.FormPost,
                      Controls.nativeActionFallbackCsrfToken = pageCsrfValue (pageSecurityCsrf pageSecurity)
                    }
              ]
          }
        [ voidElement inputTag [inputType "hidden", name "return", value "login"],
          admissionInput
            (literalElementId "admission-login")
            "Admission name"
            [name "login", inputType "text", autocomplete "username", Markup.required],
          admissionInput
            (literalElementId "admission-code")
            "One-time code"
            [name "code", inputType "text", inputMode "numeric", autocomplete "one-time-code", maxLength "6", Markup.required],
          element buttonTag [inputType "submit"] [text "Continue"]
        ]
    )
  where
    -- The page is rendered inside the locale-root adapter, while this public
    -- module retains its local action codec for independent composition. Use
    -- the same typed context prefix operation as that adapter so markup and
    -- dispatch share /{locale}/public/admission/actions/submit rather than
    -- inventing a string URL at the form boundary.
    localizedAdmissionActions =
      either (error . show) id $
        prefixActionCodecByContext
          (\context -> "/" <> localeText (requestLocale (requestCore context)))
          "/{locale}"
          admissionActions

admissionInput :: ElementId -> Text.Text -> [Attribute] -> Html
admissionInput controlId labelText inputAttributes =
  Controls.accessibleField
    Controls.AccessibleFieldProps
      { Controls.accessibleFieldControlId = controlId,
        Controls.accessibleFieldLabel = text labelText,
        Controls.accessibleFieldHint = Nothing,
        Controls.accessibleFieldValidity = Controls.FieldValid
      }
    (\derived -> voidElement inputTag (Controls.fieldControlIdAttribute derived : Controls.fieldControlRelationshipAttributes derived <> inputAttributes))

nativeAdmissionFallbackHandler :: CsrfProtection ComposedContext -> AdmissionConfig -> AdmissionProofConfig -> ActionCodec RootActionTarget ComposedContext RootAuthorization RootAction -> Wai.Request -> RouteRequest LocalizedRoute ComposedContext -> IO (NonPageResponse LocalizedRoute ComposedContext)
nativeAdmissionFallbackHandler csrfProtection sessionConfig proofConfig admissionActions request routeRequest = do
  requestBodyResult <- readRequestBodyUpTo admissionNativeFallbackBodyBytes request
  case requestBodyResult of
    Left RequestBodyLimitExceeded -> pure (admissionNativeFallbackResponse Http.status413 "Admission request body is too large.")
    Right requestBody
      | admissionNativeFallbackFieldCountExceedsLimit requestBody -> pure (admissionNativeFallbackResponse Http.status413 "Admission request has too many form fields.")
      | otherwise ->
          case parseClientActionFields requestBody of
            Left _ -> pure (admissionNativeFallbackResponse Http.status422 "Admission request is invalid.")
            Right formFields ->
              case validateActionCsrfTransport request formFields of
                Left _ -> pure (admissionNativeFallbackResponse Http.status403 "Admission CSRF validation failed.")
                Right csrfToken -> do
                  verification <- verifyCsrfToken csrfProtection (requestContext routeRequest) csrfToken
                  case verification of
                    CsrfRejected -> pure (admissionNativeFallbackResponse Http.status403 "Admission CSRF validation failed.")
                    CsrfVerificationUnavailable -> pure (admissionNativeFallbackResponse Http.status503 "Admission CSRF protection is unavailable.")
                    CsrfVerified ->
                      case decodeAdmissionSubmission admissionActions (requestContext routeRequest) formFields of
                        Nothing -> pure (admissionNativeFallbackResponse Http.status422 "Admission request is invalid.")
                        Just (loginName, code, returnTarget) -> do
                          submissionResult <- submitAdmission sessionConfig proofConfig (rootClientAddress (requestClient (requestContext routeRequest))) loginName code
                          pure (admissionNativeSubmissionResponse sessionConfig routeRequest returnTarget submissionResult)

decodeAdmissionSubmission :: ActionCodec RootActionTarget ComposedContext RootAuthorization RootAction -> ComposedContext -> [(Text.Text, Text.Text)] -> Maybe (AdmissionLoginName, TotpCode, AdmissionReturnTarget)
decodeAdmissionSubmission admissionActions requestContext formFields =
  case decodeAction admissionActions payload of
    DecodedClientAction (SubmitAdmission loginName code returnTarget) -> Just (loginName, code, returnTarget)
    _ -> Nothing
  where
    payload =
      ClientActionPayload
        { clientActionMethod = "POST",
          clientActionPath = admissionActionPath,
          clientActionFields = formFields,
          clientActionCsrfToken = Nothing,
          clientActionIdempotencyKey = Nothing,
          clientActionPayloadContext = requestContext
        }

admissionNativeSubmissionResponse :: AdmissionConfig -> RouteRequest LocalizedRoute ComposedContext -> AdmissionReturnTarget -> AdmissionSubmissionResult -> NonPageResponse LocalizedRoute ComposedContext
admissionNativeSubmissionResponse sessionConfig routeRequest returnTarget submissionResult =
  case submissionResult of
    AdmissionSubmissionAccepted session ->
      nonPageInternalRedirectResponseWithHeaders
        Http.status303
        (admissionSessionHeaders sessionConfig session)
        (RouteRequest (admissionReturnTargetRoute returnTarget) (requestContext routeRequest))
    AdmissionSubmissionRejected -> admissionNativeFallbackResponse Http.status422 "Admission credentials were not accepted."
    AdmissionSubmissionUnavailable -> admissionNativeFallbackResponse Http.status503 "Admission is temporarily unavailable."

admissionSessionHeaders :: AdmissionConfig -> OpaqueSession AdmissionPrincipalId -> [Http.Header]
admissionSessionHeaders sessionConfig session =
  [ ("Set-Cookie", TextEncoding.encodeUtf8 (renderSessionCookie (admissionConfigCookiePolicy sessionConfig) (sessionId session))),
    csrfClearCookieHeader
  ]

admissionNativeFallbackResponse :: Http.Status -> Text.Text -> NonPageResponse LocalizedRoute ComposedContext
admissionNativeFallbackResponse status message =
  NonPageBodyResponse
    ResponseBody
      { responseStatus = status,
        responseContentType = "text/plain; charset=utf-8",
        responseBody = message,
        responseObservabilityAttributes = [],
        responseLogEntries = [],
        responseDatabaseOperations = []
      }

admissionNativeFallbackBodyBytes :: Int
admissionNativeFallbackBodyBytes = 8192

admissionNativeFallbackFieldCountLimit :: Int
admissionNativeFallbackFieldCountLimit = 8

admissionNativeFallbackFieldCountExceedsLimit :: LazyByteString.ByteString -> Bool
admissionNativeFallbackFieldCountExceedsLimit requestBody =
  admissionNativeFallbackFieldCount requestBody > admissionNativeFallbackFieldCountLimit

admissionNativeFallbackFieldCount :: LazyByteString.ByteString -> Int
admissionNativeFallbackFieldCount requestBody =
  if LazyByteString.null requestBody
    then 0
    else 1 + sum (map (ByteString.count 38) (LazyByteString.toChunks requestBody))
