{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent (forkIO, killThread, readMVar, threadDelay)
import Control.Exception (IOException, SomeException, bracket, displayException, try)
import Control.Monad (forM_)
import Data.ByteString qualified as ByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as TextEncoding
import GHC.Clock (getMonotonicTimeNSec)
import HarchWeb qualified
import HarchWeb.Api (ApiRequestDecodeResult (..), apiRequestDataFromWaiRequest, runRequestCodec)
import HarchWeb.Observability qualified as Observability
import HarchWeb.Session qualified as Session
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import System.IO (hClose)
import System.IO.Error (isAlreadyInUseError)
import System.IO.Temp (withSystemTempDirectory, withSystemTempFile)
import TestCore.Wai (nextRequestBodyChunk, performWaiRequest, readResponseBody, waiRequest)
import TestSupport.RealPostgres (defaultMigrationPostgresConfig, defaultRealPostgresConfig, ensureDefaultPostgresAvailable, withContainerizedPsqlOnPath)
import Unit.WebApi.TestSupport hiding (databaseConfig)
import WebApi (buildApp, run)
import WebApi.Api.Endpoints (noApiRequestFields)
import WebApi.App (buildAppWithDatabase, buildRuntimeAccountWorkflow, buildRuntimeAppWithDatabaseBuilder, runWithConfig)
import WebApi.AppEffect (AccountWorkflow (accountWorkflowEmailDelivery))
import WebApi.Config (AppConfig (..), AppEnvironmentConfig (..), AppMode (..), DatabaseConfig (..), ListenerConfig (..), ListenerScheme (..), ManualTlsCertificateFiles (..), ObservabilityConfig (..), OtlpExporter (..), RequestPolicyConfig (..), TlsCertificateSource (..), TlsConfig (..), databasePoolCapacity, defaultAppConfig, defaultAppEnvironmentConfig, defaultTlsPolicy)
import WebApi.Database (DatabaseError (..), DatabaseOperation (..), DatabaseResult (..), DatabaseSeed (..), PageRepository (..), SecondPageData (..), buildSeededPageRepository, defaultDatabaseSeed, defaultPageRepository)
import WebApi.Page (renderPage)
import WebApi.Postgres.Testing (closePostgresPool, newPostgresPool, runPostgresMigrationsForRuntime, runPostgresSeed)
import WebApi.Response (selectResponse)
import WebApi.Route (AppRequestContext (..), AppRoute (..), defaultRequestContext, parseRoute, renderRoutePath)
import WebApi.Route qualified
import WebApi.SetupPlan (TcpEndpoint (..))

spec = do
  describe "buildApp" $ do
    it "constructs the application description against the HarchWeb facade" $
      HarchWeb.appName pureApplication `shouldBe` "web-api"

    it "stores the account action decoder used by the WAI adapter" $ do
      let recognized =
            case HarchWeb.decodeClientAction
              pureApplication
              HarchWeb.ClientActionPayload
                { HarchWeb.clientActionMethod = "POST",
                  HarchWeb.clientActionPath = "/register",
                  HarchWeb.clientActionFields = [],
                  HarchWeb.clientActionCsrfToken = Nothing,
                  HarchWeb.clientActionIdempotencyKey = Nothing,
                  HarchWeb.clientActionPayloadContext = defaultRequestContext
                } of
              HarchWeb.DecodedClientAction _ -> True
              _ -> False
      recognized `shouldBe` True

    it "uses the account action CSRF authorization policy through the application" $ do
      let decodedRegisterAction =
            HarchWeb.decodeClientAction
              pureApplication
              HarchWeb.ClientActionPayload
                { HarchWeb.clientActionMethod = "POST",
                  HarchWeb.clientActionPath = "/register",
                  HarchWeb.clientActionFields = [],
                  HarchWeb.clientActionCsrfToken = Nothing,
                  HarchWeb.clientActionIdempotencyKey = Nothing,
                  HarchWeb.clientActionPayloadContext = defaultRequestContext
                }
          csrfToken = fromMaybe (error "expected a valid CSRF token fixture") (Session.mkCsrfToken "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
          decodedProfileAction =
            HarchWeb.decodeClientAction
              pureApplication
              HarchWeb.ClientActionPayload
                { HarchWeb.clientActionMethod = "POST",
                  HarchWeb.clientActionPath = "/profile",
                  HarchWeb.clientActionFields = [("intent", "resend-verification")],
                  HarchWeb.clientActionCsrfToken = Nothing,
                  HarchWeb.clientActionIdempotencyKey = Nothing,
                  HarchWeb.clientActionPayloadContext = defaultRequestContext
                }
      case decodedRegisterAction of
        HarchWeb.DecodedClientAction action ->
          HarchWeb.authorizeClientActionCsrf
            pureApplication
            HarchWeb.ClientActionRequest
              { HarchWeb.clientAction = action,
                HarchWeb.clientActionRequestIdempotencyKey = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext
              }
            csrfToken
            `shouldReturn` True
        _ -> expectationFailure "expected registration action to decode"
      case decodedProfileAction of
        HarchWeb.DecodedClientAction action ->
          HarchWeb.authorizeClientActionCsrf
            pureApplication
            HarchWeb.ClientActionRequest
              { HarchWeb.clientAction = action,
                HarchWeb.clientActionRequestIdempotencyKey = Nothing,
                HarchWeb.clientActionContext = defaultRequestContext
              }
            csrfToken
            `shouldReturn` False
        _ -> expectationFailure "expected profile action to decode"

    it "returns a safe bad-request response for duplicate fields in a recognized action" $ do
      actionBodyChunks <- newIORef ["email=first%40example.com&email=second%40example.com&_harch_csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"]
      let actionRequest =
            Wai.setRequestBodyChunks
              (nextRequestBodyChunk actionBodyChunks)
              ( (waiRequest ["register"])
                  { Wai.requestMethod = "POST",
                    Wai.requestHeaders =
                      [ ("X-Harch-Action", "1"),
                        (Http.hContentType, "application/x-www-form-urlencoded"),
                        ("Host", "example.test"),
                        ("Origin", "http://example.test"),
                        ("Cookie", "__Host-harch-csrf=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA")
                      ]
                  }
              )
      response <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) actionRequest
      Wai.responseStatus response `shouldBe` Http.status400
      readResponseBody response `shouldReturn` "{\"patches\":[],\"focusId\":null}"

    it "stores the default request context used by the WAI adapter" $
      HarchWeb.defaultRequestContext pureApplication `shouldBe` defaultRequestContext

    it "ignores forwarded path prefixes by default in the request context used by the WAI adapter" $ do
      let forwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "app, /ignored")]
              }
          emptyForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", ", ")]
              }
          sessionToken = Text.replicate 43 "a"
          cookieRequest =
            (waiRequest ["logout"])
              { Wai.requestHeaders = [("Cookie", TextEncoding.encodeUtf8 ("theme=dark; __Host-harch-session=" <> sessionToken))]
              }
          invalidCookieRequest =
            (waiRequest ["logout"])
              { Wai.requestHeaders = [("Cookie", ByteString.pack [255])]
              }
      HarchWeb.requestContextFromRequest pureApplication forwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext
      HarchWeb.requestContextFromRequest pureApplication emptyForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext
      HarchWeb.requestContextFromRequest pureApplication cookieRequest defaultRequestContext
        `shouldBe` defaultRequestContext {requestSessionId = Session.mkSessionId sessionToken}
      HarchWeb.requestContextFromRequest pureApplication invalidCookieRequest defaultRequestContext
        `shouldBe` defaultRequestContext

    it "derives normalized forwarded path prefixes when forwarded headers are trusted" $ do
      let forwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "app, /ignored")]
              }
          emptyForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", ", ")]
              }
          invalidForwardedPrefixRequest =
            (waiRequest ["second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "\255")]
              }
      HarchWeb.requestContextFromRequest trustedForwardedApplication forwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext {requestPathPrefix = testPathPrefix "/app"}
      HarchWeb.requestContextFromRequest trustedForwardedApplication emptyForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext
      HarchWeb.requestContextFromRequest trustedForwardedApplication invalidForwardedPrefixRequest defaultRequestContext
        `shouldBe` defaultRequestContext

    it "stores the configured static assets used by the WAI adapter" $
      HarchWeb.applicationStaticAssets pureApplication `shouldBe` staticAssets defaultAppConfig

    it "keeps pure-app observability and log reporters as no-ops" $ do
      HarchWeb.reportRequestObservability
        pureApplication
        ( Observability.buildRequestObservability
            Observability.RequestIdentity
              { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                Observability.requestIdentityScheme = "http",
                Observability.requestIdentityPath = "/",
                Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/"
              }
            200
            Observability.PageResponseKind
            []
        )
      HarchWeb.reportConnectionObservability
        pureApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog pureApplication "ignored"

    it "stores the same route codec behavior used by direct route tests" $ do
      let codec = HarchWeb.routeCodec pureApplication
      HarchWeb.parseRoute codec defaultRequestContext "/" `shouldBe` parseRoute defaultRequestContext "/"
      HarchWeb.parseRoute codec defaultRequestContext "/es" `shouldBe` parseRoute defaultRequestContext "/es"
      HarchWeb.parseRoute codec defaultRequestContext "/second" `shouldBe` parseRoute defaultRequestContext "/second"
      HarchWeb.parseRoute codec defaultRequestContext "/api/status" `shouldBe` parseRoute defaultRequestContext "/api/status"
      HarchWeb.parseRoute codec defaultRequestContext "/api/second" `shouldBe` parseRoute defaultRequestContext "/api/second"
      HarchWeb.parseRoute codec defaultRequestContext "/missing" `shouldBe` Nothing
      HarchWeb.renderRoute codec homeRequest `shouldBe` renderRoutePath homeRequest
      HarchWeb.renderRoute codec spanishSecondRequest `shouldBe` renderRoutePath spanishSecondRequest
      HarchWeb.renderRoute codec secondRequest `shouldBe` renderRoutePath secondRequest
      HarchWeb.renderRoute codec apiStatusRequest `shouldBe` renderRoutePath apiStatusRequest
      HarchWeb.renderRoute codec apiSecondRequest `shouldBe` renderRoutePath apiSecondRequest
      HarchWeb.renderRoute codec apiNotFoundRequest `shouldBe` renderRoutePath apiNotFoundRequest
      HarchWeb.renderRoute codec notFoundRequest `shouldBe` renderRoutePath notFoundRequest
      HarchWeb.notFoundRequest codec defaultRequestContext `shouldBe` notFoundRequest
      HarchWeb.routeMethods codec NotFoundRoute `shouldBe` HarchWeb.RouteHidden
      -- 'pureApplication's own codec (above) has its 'HarchWeb.routeMethods'
      -- overridden by 'HarchWeb.buildSiteApplication' to derive from each
      -- route's live 'HarchWeb.RouteDefinition' instead — which, for
      -- 'StatusApiRoute'/'SecondApiRoute', is now 'WebApi.App's own
      -- special-cased typed endpoint 'RouteDefinition', not this codec's
      -- 'Api _' declaration. Test 'WebApi.Route.routeCodec' directly for
      -- those two so this assertion exercises the declaration it names,
      -- not a same-valued but different code path.
      HarchWeb.routeMethods WebApi.Route.routeCodec ApiNotFoundRoute `shouldBe` HarchWeb.RouteHidden
      HarchWeb.routeMethods WebApi.Route.routeCodec StatusApiRoute `shouldBe` HarchWeb.routeMethodPolicy [HarchWeb.RouteGet]
      HarchWeb.routeMethods WebApi.Route.routeCodec SecondApiRoute `shouldBe` HarchWeb.routeMethodPolicy [HarchWeb.RouteGet]

    it "stores the same response-selection behavior used by direct response tests" $ do
      expectedHomeResponse <- selectResponse defaultAppConfig homeRequest
      expectedSecondResponse <- selectResponse defaultAppConfig secondRequest
      expectedSpacesResponse <- selectResponse defaultAppConfig spacesRequest
      expectedNotFoundResponse <- selectResponse defaultAppConfig notFoundRequest
      expectedApiNotFoundResponse <- selectResponse defaultAppConfig apiNotFoundRequest
      HarchWeb.renderResponse pureApplication homeRequest `shouldReturn` expectedHomeResponse
      HarchWeb.renderResponse pureApplication secondRequest `shouldReturn` expectedSecondResponse
      HarchWeb.renderResponse pureApplication spacesRequest `shouldReturn` expectedSpacesResponse
      HarchWeb.renderResponse pureApplication notFoundRequest `shouldReturn` expectedNotFoundResponse
      HarchWeb.renderResponse pureApplication apiNotFoundRequest `shouldReturn` expectedApiNotFoundResponse

    it "dispatches /api/status and /api/second through the typed endpoint boundary, not the shared page/API selector" $ do
      apiStatusResult <- HarchWeb.renderResponse pureApplication apiStatusRequest
      spanishApiStatusResult <- HarchWeb.renderResponse pureApplication spanishApiStatusRequest
      apiSecondResult <- HarchWeb.renderResponse pureApplication apiSecondRequest
      expectAll
        ( (apiStatusResult `shouldBe` HarchWeb.ProtocolResponseResult (expectedApiJsonProtocolResponse "{\"status\":\"ok\",\"locale\":\"en\"}"))
            :| [ spanishApiStatusResult `shouldBe` HarchWeb.ProtocolResponseResult (expectedApiJsonProtocolResponse "{\"status\":\"ok\",\"locale\":\"es\"}"),
                 apiSecondResult `shouldBe` HarchWeb.ProtocolResponseResult (expectedApiJsonProtocolResponse "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}")
               ]
        )

    it "carries database operations through the typed API response boundary" $ do
      let databaseOperation =
            DatabaseOperation
              { databaseOperationName = "load-second-page-summary",
                databaseQueryTemplate = "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;",
                databaseOperationStartedAtNanoseconds = Nothing,
                databaseOperationEndedAtNanoseconds = Nothing
              }
          databaseApplication =
            buildAppWithDatabase
              defaultAppConfig
              defaultPageRepository
                { loadSecondPage =
                    \_ ->
                      pure
                        DatabaseResult
                          { databaseResultValue =
                              Right
                                SecondPageData
                                  { secondPageDataSummary = "Typed operation summary.",
                                    secondPageDataHighlights = []
                                  },
                            databaseResultOperations = [databaseOperation]
                          }
                }
      renderedResponse <- HarchWeb.renderResponse databaseApplication apiSecondRequest
      case renderedResponse of
        HarchWeb.ProtocolResponseResult protocolResponse ->
          HarchWeb.protocolResponseDatabaseOperations protocolResponse
            `shouldBe` [expectedDatabaseOperation "load-second-page-summary" "SELECT summary FROM web_api.page_content WHERE route_slug = ? AND locale = ?;"]
        _ -> expectationFailure "expected a typed API protocol response"

    it "escapes hostile database content through the live /api/second endpoint" $ do
      let hostileApplication =
            buildAppWithDatabase
              defaultAppConfig
              defaultPageRepository
                { loadSecondPage =
                    \_ ->
                      pure
                        DatabaseResult
                          { databaseResultValue =
                              Right
                                SecondPageData
                                  { secondPageDataSummary = "quote\" slash\\ newline\n control\t unicode ☃",
                                    secondPageDataHighlights = ["</script><script>alert(1)</script>", "\b"]
                                  },
                            databaseResultOperations = []
                          }
                }
      HarchWeb.renderResponse hostileApplication apiSecondRequest
        `shouldReturn` HarchWeb.ProtocolResponseResult
          ( expectedApiJsonProtocolResponse
              (TextEncoding.encodeUtf8 "{\"summary\":\"quote\\\" slash\\\\ newline\\n control\\t unicode ☃\",\"highlights\":[\"</script><script>alert(1)</script>\",\"\\u0008\"]}")
          )

    it "maps a database failure at /api/second's typed endpoint boundary into explicit API error diagnostics" $ do
      let failingApplication =
            buildAppWithDatabase
              defaultAppConfig
              ( buildSeededPageRepository
                  DatabaseSeed
                    { englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                      spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                    }
              )
      apiSecondResult <- HarchWeb.renderResponse failingApplication apiSecondRequest
      apiSecondResult
        `shouldBe` HarchWeb.ProtocolResponseResult
          HarchWeb.ProtocolResponse
            { HarchWeb.protocolResponseStatus = Http.status503,
              HarchWeb.protocolResponseHeaders = [(Http.hContentType, "application/json"), (Http.hVary, "Accept")],
              HarchWeb.protocolResponseBody = HarchWeb.ProtocolResponseBytes "{\"error\":\"second-page-unavailable\"}",
              HarchWeb.protocolResponseObservabilityAttributes =
                [ Observability.ObservabilityAttribute
                    { Observability.attributeName = "error.type",
                      Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.failure.code",
                      Observability.attributeValue = Observability.TextAttribute "database.second-page-data"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.route",
                      Observability.attributeValue = Observability.TextAttribute "/second"
                    },
                  Observability.ObservabilityAttribute
                    { Observability.attributeName = "app.surface",
                      Observability.attributeValue = Observability.TextAttribute "api"
                    }
                ],
              HarchWeb.protocolResponseLogEntries =
                ["Database failure while rendering required second-page api response: SecondPageDataError \"seed unavailable\""],
              HarchWeb.protocolResponseDatabaseOperations = []
            }

    it "declares no fields for /api/status and /api/second, decoding an empty request to ()" $
      case runRequestCodec noApiRequestFields (apiRequestDataFromWaiRequest Wai.defaultRequest) of
        ApiRequestDecoded () -> pure ()
        ApiRequestRejected _ -> expectationFailure "expected the no-fields API request codec to decode"
        ApiRequestCodecInvalid -> expectationFailure "expected the no-fields API request codec to be valid"

    it "adapts the pure application to WAI without changing rendered pages" $ do
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["es", "second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      renderedSecondResponse <- readResponseBody secondResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Segunda</h1>" renderedSecondResponse `shouldBe` True
      Text.isInfixOf "<script nonce=\"" renderedSecondResponse `shouldBe` True

      spacesResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["spaces"])
      Wai.responseStatus spacesResponse `shouldBe` Http.status200
      renderedSpacesResponse <- readResponseBody spacesResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Site under construction</h1>" renderedSpacesResponse `shouldBe` True

      apiStatusResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "status"])
      Wai.responseStatus apiStatusResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiStatusResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiStatusResponse
        `shouldReturn` "{\"status\":\"ok\",\"locale\":\"en\"}"

      apiSecondResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "second"])
      Wai.responseStatus apiSecondResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders apiSecondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiSecondResponse
        `shouldReturn` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"

      missingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["missing"])
      Wai.responseStatus missingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders missingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      renderedMissingResponse <- readResponseBody missingResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Not Found</h1>" renderedMissingResponse `shouldBe` True

      apiMissingResponse <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["api", "missing"])
      Wai.responseStatus apiMissingResponse `shouldBe` Http.status404
      lookup Http.hContentType (Wai.responseHeaders apiMissingResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "application/json")
      readResponseBody apiMissingResponse
        `shouldReturn` "{\"error\":\"not-found\"}"

    it "emits second-page observability through the WAI facade" $ do
      requestObservabilityReference <- newIORef Nothing
      let observingApplication =
            pureApplication
              { HarchWeb.reportRequestObservability =
                  writeIORef requestObservabilityReference . Just
              }
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication observingApplication) (waiRequest ["second"])
      Wai.responseStatus secondResponse `shouldBe` Http.status200
      responseBody <- readResponseBody secondResponse
      Text.isInfixOf "<h1 data-page-title=\"true\">Second</h1>" responseBody `shouldBe` True
      Text.isInfixOf "<script nonce=\"" responseBody `shouldBe` True
      maybeRequestObservability <- readIORef requestObservabilityReference
      case maybeRequestObservability of
        Nothing ->
          expectationFailure "expected request observability to be reported for /second"
        Just requestObservability -> do
          let requestSpan = Observability.observabilityRequestSpan requestObservability
              requestAttributes = Observability.requestSpanAttributes requestSpan
          Observability.requestSpanDisplayName requestSpan `shouldBe` "GET /second"
          lookupTextObservabilityAttribute "url.path" requestAttributes `shouldBe` Just "/second"
          lookupTextObservabilityAttribute "http.route" requestAttributes `shouldBe` Just "/second"

    it "adapts forwarded path prefixes through the WAI facade for pages and static assets" $ do
      let prefixedPageRequest =
            (waiRequest ["app", "second"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedAssetRequest =
            (waiRequest ["app", "assets", "navigation.js"])
              { Wai.requestHeaders = [("X-Forwarded-Prefix", "/app")]
              }
          prefixedApplication =
            buildApp
              navigationAppConfig
                { requestPolicy =
                    (requestPolicy navigationAppConfig)
                      { forwardedHeaderTrust = testTrustedForwardedProxy
                      }
                }
      pageResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedPageRequest
      Wai.responseStatus pageResponse `shouldBe` Http.status200
      pageBody <- readResponseBody pageResponse
      Text.isInfixOf "<a href=\"/app\" data-page-link=\"true\">Home</a><a href=\"/app/second\" data-page-link=\"true\" aria-current=\"page\">Second</a>" pageBody `shouldBe` True
      Text.isInfixOf "<script type=\"module\" src=\"/app/assets/navigation.js\" defer></script>" pageBody `shouldBe` True

      assetResponse <- performWaiRequest (HarchWeb.toWaiApplication prefixedApplication) prefixedAssetRequest
      Wai.responseStatus assetResponse `shouldBe` Http.status200
      lookup Http.hContentType (Wai.responseHeaders assetResponse) `shouldBe` Just "application/javascript; charset=utf-8"

    it "fails closed for hostile trusted forwarded prefixes in rendered links and script sources" $ do
      let hostilePrefixes = ["//attacker.example", "/app\\attacker", "/app?next=attacker", "/app%2Fattacker"]
          trustedApplication =
            buildApp
              navigationAppConfig
                { requestPolicy =
                    (requestPolicy navigationAppConfig)
                      { forwardedHeaderTrust = testTrustedForwardedProxy
                      }
                }
      forM_ hostilePrefixes $ \hostilePrefix -> do
        response <-
          performWaiRequest
            (HarchWeb.toWaiApplication trustedApplication)
            ( (waiRequest ["second"])
                { Wai.requestHeaders = [("X-Forwarded-Prefix", hostilePrefix)]
                }
            )
        Wai.responseStatus response `shouldBe` Http.status200
        responseBody <- readResponseBody response
        expectAll
          ( (Text.isInfixOf "href=\"/\"" responseBody `shouldBe` True)
              :| [ Text.isInfixOf "src=\"/assets/navigation.js\"" responseBody `shouldBe` True,
                   Text.isInfixOf "attacker" responseBody `shouldBe` False,
                   Text.isInfixOf "href=\"//" responseBody `shouldBe` False,
                   Text.isInfixOf "src=\"//" responseBody `shouldBe` False
                 ]
          )

    it "returns HTTP 500 for required page failures while keeping unaffected routes unchanged" $ do
      let failingApplication =
            buildAppWithDatabase
              defaultAppConfig
              ( buildSeededPageRepository
                  DatabaseSeed
                    { englishSecondPageData = Left (SecondPageDataError "seed unavailable"),
                      spanishSecondPageData = spanishSecondPageData defaultDatabaseSeed
                    }
              )
      secondResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest ["second"])
      Wai.responseStatus secondResponse `shouldBe` Http.internalServerError500
      lookup Http.hContentType (Wai.responseHeaders secondResponse) `shouldBe` Just (TextEncoding.encodeUtf8 "text/html; charset=utf-8")
      secondResponseBody <- readResponseBody secondResponse
      secondResponseBody `shouldSatisfy` Text.isInfixOf "Second page content is temporarily unavailable."

      homeResponse <- performWaiRequest (HarchWeb.toWaiApplication failingApplication) (waiRequest [])
      Wai.responseStatus homeResponse `shouldBe` Http.status302
      lookup Http.hLocation (Wai.responseHeaders homeResponse) `shouldBe` Just "/spaces"

    it "is structurally complete enough to render supported and not-found shells" $ do
      secondPage <- renderPage defaultAppConfig secondRequest
      notFoundPage <- renderPage defaultAppConfig notFoundRequest
      HarchWeb.documentTitle (HarchWeb.pageShell pureApplication secondPage) `shouldBe` "web-api: Second"
      HarchWeb.documentBootstrapHooks (HarchWeb.pageShell pureApplication secondPage) `shouldBe` ["second-page"]
      HarchWeb.documentTitle (HarchWeb.pageShell pureApplication notFoundPage) `shouldBe` "web-api: Not Found"
      Text.isInfixOf "The requested page could not be found." (HarchWeb.renderHtml (HarchWeb.documentMainContent (HarchWeb.pageShell pureApplication notFoundPage)))
        `shouldBe` True

    it "can grow from page responses to API responses without changing route matching" $ do
      renderedResponse <- HarchWeb.renderResponse pureApplication apiSecondRequest
      case renderedResponse of
        HarchWeb.ProtocolResponseResult protocolResponse ->
          protocolResponseStrictBody protocolResponse
            `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected an API protocol response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.RedirectResponse _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.ClientActionBodyResponse _ -> expectationFailure "expected an API protocol response"
        HarchWeb.EventStreamResponse _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.BodyResponse _ -> expectationFailure "expected an API protocol response"

  describe "buildRuntimeApp" $ do
    it "selects the production and test SMTP authentication policies while constructing runtime workflows"
      $ bracket
        (newPostgresPool (databasePoolCapacity (databaseConfig defaultAppEnvironmentConfig)) (databaseConfig defaultAppEnvironmentConfig))
        closePostgresPool
      $ \pool ->
        forM_ [Production, Test] $ \mode -> do
          let workflow = buildRuntimeAccountWorkflow pool (defaultAppEnvironmentConfig {appMode = mode})
          accountWorkflowEmailDelivery workflow `seq` pure ()

    it "builds the runtime database effect from the environment config" $ do
      let runtimeEnvironmentConfig =
            defaultAppEnvironmentConfig
              { databaseConfig =
                  postgresTestConfig
                    { databaseName = "runtime_db",
                      databaseUser = "runtime_user"
                    }
              }
          runtimeApplication =
            buildRuntimeAppWithDatabaseBuilder
              defaultAppConfig
              ( \databaseRuntimeConfig ->
                  buildSeededPageRepository
                    defaultDatabaseSeed
                      { englishSecondPageData =
                          Right
                            SecondPageData
                              { secondPageDataSummary =
                                  "runtime:" <> databaseName databaseRuntimeConfig <> ":" <> databaseUser databaseRuntimeConfig,
                                secondPageDataHighlights = ["configured-from-environment"]
                              }
                      }
              )
              runtimeEnvironmentConfig
      runtimeResponse <- HarchWeb.renderResponse runtimeApplication apiSecondRequest
      case runtimeResponse of
        HarchWeb.ProtocolResponseResult protocolResponse ->
          protocolResponseStrictBody protocolResponse
            `shouldBe` "{\"summary\":\"runtime:runtime_db:runtime_user\",\"highlights\":[\"configured-from-environment\"]}"
        HarchWeb.PageResponse _ -> expectationFailure "expected an API protocol response"
        HarchWeb.PageResponseWithMetadata _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.RedirectResponse _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.ClientActionBodyResponse _ -> expectationFailure "expected an API protocol response"
        HarchWeb.EventStreamResponse _ _ -> expectationFailure "expected an API protocol response"
        HarchWeb.BodyResponse _ -> expectationFailure "expected an API protocol response"
      HarchWeb.reportRequestObservability
        runtimeApplication
        ( Observability.buildRequestObservability
            Observability.RequestIdentity
              { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                Observability.requestIdentityScheme = "http",
                Observability.requestIdentityPath = "/second",
                Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/second"
              }
            500
            Observability.BodyResponseKind
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "exception.type",
                  Observability.attributeValue = Observability.TextAttribute "SecondPageDataError"
                }
            ]
        )
      HarchWeb.reportConnectionObservability
        runtimeApplication
        ( Observability.buildConnectionObservability
            "CONNECTION insecure-connection-denied"
            [ Observability.ObservabilityAttribute
                { Observability.attributeName = "network.peer.address",
                  Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                }
            ]
        )
      HarchWeb.reportApplicationLog runtimeApplication "runtime failure detail"

    it "redirects to the canonical authority parsed from PUBLIC_BASE_URL, ignoring a forged Host header" $ do
      let runtimeAppConfig = defaultAppConfig {requestPolicy = (requestPolicy defaultAppConfig) {redirectHttpToHttps = True}}
          runtimeEnvironmentConfig = defaultAppEnvironmentConfig {publicBaseUrl = "https://accounts.example.test:8443/"}
          runtimeApplication = buildRuntimeAppWithDatabaseBuilder runtimeAppConfig (const defaultPageRepository) runtimeEnvironmentConfig
          redirectRequest = (waiRequest ["second"]) {Wai.requestHeaders = [("Host", "evil.example")]}
      response <- performWaiRequest (HarchWeb.toWaiApplication runtimeApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://accounts.example.test/second"

    it "falls back to the listener-derived authority when PUBLIC_BASE_URL has no recognizable authority" $ do
      let runtimeAppConfig =
            defaultAppConfig
              { requestPolicy =
                  (requestPolicy defaultAppConfig)
                    { redirectHttpToHttps = True,
                      httpsRedirectAuthority = Just "127.0.0.1"
                    }
              }
          runtimeEnvironmentConfig = defaultAppEnvironmentConfig {publicBaseUrl = "not-a-url"}
          runtimeApplication = buildRuntimeAppWithDatabaseBuilder runtimeAppConfig (const defaultPageRepository) runtimeEnvironmentConfig
          redirectRequest = (waiRequest ["second"]) {Wai.requestHeaders = [("Host", "evil.example")]}
      response <- performWaiRequest (HarchWeb.toWaiApplication runtimeApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://127.0.0.1/second"

    it "falls back to the listener-derived authority when PUBLIC_BASE_URL has no host after its scheme" $ do
      let runtimeAppConfig =
            defaultAppConfig
              { requestPolicy =
                  (requestPolicy defaultAppConfig)
                    { redirectHttpToHttps = True,
                      httpsRedirectAuthority = Just "127.0.0.1"
                    }
              }
          runtimeEnvironmentConfig = defaultAppEnvironmentConfig {publicBaseUrl = "https://"}
          runtimeApplication = buildRuntimeAppWithDatabaseBuilder runtimeAppConfig (const defaultPageRepository) runtimeEnvironmentConfig
          redirectRequest = (waiRequest ["second"]) {Wai.requestHeaders = [("Host", "evil.example")]}
      response <- performWaiRequest (HarchWeb.toWaiApplication runtimeApplication) redirectRequest
      Wai.responseStatus response `shouldBe` Http.status308
      lookup Http.hLocation (Wai.responseHeaders response) `shouldBe` Just "https://127.0.0.1/second"

    it "exports runtime request observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultPageRepository)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability Observability.RequestIdentity {Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET", Observability.requestIdentityScheme = "http", Observability.requestIdentityPath = "/api/status", Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/api/status"} 200 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"service.name\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"web-api\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"GET /api/status\""
        requestBodyText `shouldSatisfy` (not . Text.isInfixOf "\"STATUS_CODE_ERROR\"")

    it "keeps runtime request reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultPageRepository)
                defaultAppEnvironmentConfig
        HarchWeb.reportRequestObservability
          runtimeApplication
          (Observability.buildRequestObservability Observability.RequestIdentity {Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET", Observability.requestIdentityScheme = "http", Observability.requestIdentityPath = "/api/second", Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/api/second"} 500 Observability.BodyResponseKind [])
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

    it "enqueues OTLP exports without blocking the caller, dropping and counting once the bounded queue is full" $
      withSlowOtlpCaptureServer 3000000 Http.ok200 "{}" $ \collectorUrl -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultPageRepository)
                defaultAppEnvironmentConfig
            floodObservability =
              Observability.buildRequestObservability Observability.RequestIdentity {Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET", Observability.requestIdentityScheme = "http", Observability.requestIdentityPath = "/api/status", Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/api/status"} 200 Observability.BodyResponseKind []
        floodStartedAt <- getMonotonicTimeNSec
        -- One export occupies the worker for the full 3s collector delay, so
        -- the other 279 calls must pile into the 256-item bounded queue —
        -- guaranteeing at least a couple dozen are dropped rather than
        -- blocking this thread. The 2s ceiling below still leaves a wide
        -- margin over the ~280 synchronous stderr writes this loop performs
        -- (well under 1s even under load) while staying clearly short of
        -- the 3s collector stall it must not be waiting on.
        mapM_ (const (HarchWeb.reportRequestObservability runtimeApplication floodObservability)) [1 :: Int .. 280]
        floodCompletedAt <- getMonotonicTimeNSec
        (floodCompletedAt - floodStartedAt) `shouldSatisfy` (< 2000000000)
        -- Let the one in-flight export finish and receive its real response
        -- before the collector socket goes away, so the shared background
        -- worker never blocks on a connection that died mid-request (which
        -- would otherwise stall every later test's export behind
        -- http-client's default 30s response timeout).
        threadDelay 3500000

    it "exports runtime connection observability to the configured OTLP tracing endpoint" $
      withOtlpCaptureServer Http.ok200 "{}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = [("x-runtime-trace", "enabled")]
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultPageRepository)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
          ( Observability.buildConnectionObservability
              "CONNECTION insecure-connection-denied"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  },
                Observability.ObservabilityAttribute
                  { Observability.attributeName = "exception.type",
                    Observability.attributeValue = Observability.TextAttribute "InsecureConnectionDenied"
                  }
              ]
          )
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath,
            capturedOtlpHeaders = requestHeaders,
            capturedOtlpBody = requestBody
          } <-
          readMVar capturedRequestReference
        let requestBodyText = TextEncoding.decodeUtf8 requestBody
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"
        lookup "content-type" requestHeaders `shouldBe` Just "application/json"
        lookup "x-runtime-trace" requestHeaders `shouldBe` Just "enabled"
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"name\":\"CONNECTION insecure-connection-denied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"network.peer.address\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"InsecureConnectionDenied\""
        requestBodyText `shouldSatisfy` Text.isInfixOf "\"STATUS_CODE_ERROR\""

    it "keeps runtime connection reporting alive when the OTLP collector rejects the export" $
      withOtlpCaptureServer Http.serviceUnavailable503 "{\"error\":\"collector unavailable\"}" $ \collectorUrl capturedRequestReference -> do
        let runtimeAppConfig =
              defaultAppConfig
                { observability =
                    (observability defaultAppConfig)
                      { tracingExporter =
                          Just
                            OtlpExporter
                              { otlpEndpoint = collectorUrl,
                                otlpHeaders = []
                              }
                      }
                }
            runtimeApplication =
              buildRuntimeAppWithDatabaseBuilder
                runtimeAppConfig
                (const defaultPageRepository)
                defaultAppEnvironmentConfig
        HarchWeb.reportConnectionObservability
          runtimeApplication
          ( Observability.buildConnectionObservability
              "CONNECTION client-closed-connection-prematurely"
              [ Observability.ObservabilityAttribute
                  { Observability.attributeName = "network.peer.address",
                    Observability.attributeValue = Observability.TextAttribute "127.0.0.1"
                  }
              ]
          )
        CapturedOtlpRequest
          { capturedOtlpMethod = requestMethod,
            capturedOtlpPath = requestPath
          } <-
          readMVar capturedRequestReference
        requestMethod `shouldBe` "POST"
        requestPath `shouldBe` "/v1/traces"

  describe "run" $ do
    it "starts the runtime server from an explicit environment and app config" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          completionReference <- newIORef Nothing
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Http,
                            listenerTls = Nothing,
                            listenerAcme = Nothing
                          }
                      ]
                  }
          serverThreadId <- forkIO $ do
            result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either SomeException ())
            writeIORef completionReference (Just result)
          responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
          responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
          completionResult <- readIORef completionReference
          completionResult `shouldSatisfy` isNothing
          killThread serverThreadId
          waitForRuntimeServerExit completionReference
          hClose outputHandle
          readFile outputPath
            `shouldReturn` unlines
              [ "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
              ]

    it "surfaces listener bind failures through the default app config" $
      withDefaultRuntimePortUnavailable $
        withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle ->
          runWithConfig outputHandle defaultAppConfig defaultAppEnvironmentConfig
            `shouldThrow` isAlreadyInUseError

    it "serves database-backed runtime routes from the supplied environment config" $
      withContainerizedPsqlOnPath $ do
        ensureDefaultPostgresAvailable
        runPostgresMigrationsForRuntime defaultMigrationPostgresConfig defaultRealPostgresConfig `shouldReturn` Right ()
        runPostgresSeed defaultMigrationPostgresConfig `shouldReturn` Right ()
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempFile "web-api-runtime-output.txt" $ \_ outputHandle -> do
            completionReference <- newIORef Nothing
            let runtimeAppConfig =
                  defaultAppConfig
                    { listenerConfigs =
                        [ ListenerConfig
                            { listenerHost = tcpEndpointHost unusedEndpoint,
                              listenerPort = tcpEndpointPort unusedEndpoint,
                              listenerScheme = Http,
                              listenerTls = Nothing,
                              listenerAcme = Nothing
                            }
                        ]
                    }
                runtimeEnvironmentConfig =
                  defaultAppEnvironmentConfig
                    { databaseConfig = defaultMigrationPostgresConfig
                    }
            serverThreadId <- forkIO $ do
              result <- try (runWithConfig outputHandle runtimeAppConfig runtimeEnvironmentConfig) :: IO (Either SomeException ())
              writeIORef completionReference (Just result)
            responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/second"
            responseText `shouldBe` "{\"summary\":\"Second page content with stubbed data ready for future loaders.\",\"highlights\":[]}"
            completionResult <- readIORef completionReference
            completionResult `shouldSatisfy` isNothing
            killThread serverThreadId
            waitForRuntimeServerExit completionReference
            hClose outputHandle

    it "announces parsed HTTPS listener configs before surfacing manual TLS startup failures" $
      withUnusedTcpEndpoint $ \unusedEndpoint ->
        withSystemTempFile "web-api-runtime-output.txt" $ \outputPath outputHandle -> do
          let runtimeAppConfig =
                defaultAppConfig
                  { listenerConfigs =
                      [ ListenerConfig
                          { listenerHost = tcpEndpointHost unusedEndpoint,
                            listenerPort = tcpEndpointPort unusedEndpoint,
                            listenerScheme = Https,
                            listenerTls =
                              Just
                                TlsConfig
                                  { certificateSource =
                                      ManualCertificateFiles
                                        ManualTlsCertificateFiles
                                          { certificateFile = "/tmp/missing-cert.pem",
                                            privateKeyFile = "/tmp/missing-key.pem"
                                          },
                                    tlsPolicy = defaultTlsPolicy
                                  },
                            listenerAcme = Nothing
                          }
                      ]
                  }
          result <- try (runWithConfig outputHandle runtimeAppConfig defaultAppEnvironmentConfig) :: IO (Either IOException ())
          hClose outputHandle
          case result of
            Left exception ->
              displayException exception
                `shouldContain` "Manual TLS certificate file does not exist: /tmp/missing-cert.pem"
            Right () ->
              expectationFailure "expected runWithConfig to fail when manual TLS files are missing"
          readFile outputPath
            `shouldReturn` ("Parsed listener config: https://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint) <> "\n")

    it "writes startup output to the supplied handle for isolated tests and serves real requests" $
      withClearedAppEnvironment $
        withUnusedTcpEndpoint $ \unusedEndpoint ->
          withSystemTempDirectory "web-api-run" $ \tempDirectory ->
            withCurrentDirectory tempDirectory $ do
              writeFile ".env" ("LISTENER_0_PORT=" <> show (tcpEndpointPort unusedEndpoint) <> "\nDATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n")
              withSystemTempFile "web-api-output.txt" $ \outputPath outputHandle -> do
                completionReference <- newIORef Nothing
                serverThreadId <- forkIO $ do
                  result <- try (run outputHandle) :: IO (Either SomeException ())
                  writeIORef completionReference (Just result)
                responseText <- waitForRuntimeServerResponse completionReference (tcpEndpointPort unusedEndpoint) "/api/status"
                responseText `shouldBe` "{\"status\":\"ok\",\"locale\":\"en\"}"
                completionResult <- readIORef completionReference
                completionResult `shouldSatisfy` isNothing
                killThread serverThreadId
                waitForRuntimeServerExit completionReference
                hClose outputHandle
                readFile outputPath
                  `shouldReturn` unlines
                    [ "Loaded config file: ./.env",
                      "Config file missing: ./.env.local",
                      "Parsed listener config: http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint),
                      "HTTP Server listening at http://127.0.0.1:" <> show (tcpEndpointPort unusedEndpoint)
                    ]

    it "fails explicitly when the layered runtime startup config is invalid" $
      withClearedAppEnvironment $
        withSystemTempDirectory "web-api-run-invalid" $ \tempDirectory ->
          withCurrentDirectory tempDirectory $ do
            writeFile ".env" "LISTENER_0_PORT=0\nDATABASE_PASSWORD=web_api\nSMTP_PASSWORD=password\nTOTP_ENCRYPTION_KEY=AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA\n"
            result <-
              ( try $
                  withSystemTempFile "web-api-output.txt" $ \_ outputHandle -> do
                    run outputHandle
                    hClose outputHandle
              ) ::
                IO (Either IOException ())
            case result of
              Left exception ->
                displayException exception
                  `shouldContain` "Failed to load app startup config: AppStartupConfigParseError (InvalidConfigValue \"LISTENER_0_PORT\" \"0\")"
              Right () ->
                expectationFailure "expected run to fail on invalid runtime startup config"
