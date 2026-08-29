{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad ()
import Crypto.Error (CryptoFailable (CryptoPassed))
import Data.ByteString qualified as ByteString ()
import Data.ByteString.Builder qualified as Builder ()
import Data.ByteString.Char8 qualified as ByteStringChar8 ()
import Data.ByteString.Lazy qualified as LazyByteString ()
import Data.Char ()
import Data.Either ()
import Data.Functor.Compose ()
import Data.IORef ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe ()
import Data.Text (Text)
import Data.Text qualified as Text (isInfixOf)
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability (ObservabilityAttribute (ObservabilityAttribute, attributeName, attributeValue), ObservabilityAttributeValue (TextAttribute), RequestIdentity (RequestIdentity, requestIdentityMethod, requestIdentityPath, requestIdentityRoutePath, requestIdentityScheme), ResponseKind (PageResponseKind), buildRequestObservability, mkSpanMethodLabel, mkSpanRoutePath)
import HarchWeb.Password qualified as Password
import HarchWeb.Secret qualified as Secret
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http (status200, status202, status422, status500, status503)
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai (Application, defaultRequest)
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO (Handle)
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestContext, TestRoute (DataRoute, KnownRoute, MissingRoute), defaultContext, emptyStaticAssets, renderDocument, sampleApplication, samplePage, spanishContext, testPathPrefix, testRegionPatch, trustedMarkup)

existingSpec :: Spec
existingSpec =
  describe "HarchWeb facade" $
    it "exposes supported framework authoring and extension entry points" $ do
      defaultCaptureKernel `seq`
        defaultNavigationRuntime `seq`
          renderDocumentForTests `seq`
            staticAssetHref `seq`
              routeHref `seq`
                facadeWaiApplication `seq`
                  runRequestMiddlewarePipeline `seq`
                    clientActionResponseBody `seq`
                      planObservabilityStartup `seq`
                        exportRequestObservabilityToOtlp `seq`
                          exportConnectionObservabilityToOtlp `seq`
                            facadeLocalTestServer `seq`
                              facadeRuntimeServer `seq`
                                loadReloadingTlsCredentials `seq`
                                  reloadTlsCredentialsIfChanged `seq`
                                    loadTlsCredentialSnapshotOrThrowWithLoader `seq`
                                      startManualTlsRuntimeServerWithStarter `seq`
                                        startWarpRuntimeServerOnSocket `seq`
                                          (pure () :: IO ())

facadeWaiApplication :: Application Bool () () -> IO Wai.Application
facadeWaiApplication = toWaiApplication

facadeLocalTestServer :: Application Bool () () -> (LocalTestServer -> IO ()) -> IO ()
facadeLocalTestServer = withLocalTestServer

facadeRuntimeServer :: Handle -> ServerConfig -> Application Bool () () -> IO ()
facadeRuntimeServer = runServer

movedSpec :: Spec
movedSpec = do
  describe "public role-safe boundaries" $ do
    it "constructs and applies opaque path, password, secret, TLS, and span roles through their public modules" $ do
      urlPathText (applyRequestPathPrefix (testPathPrefix "/app/") (mkUrlPath "/second")) `shouldBe` "/app/second"
      urlPathText (stripRequestPathPrefix (testPathPrefix "/app") (mkUrlPath "/app/second")) `shouldBe` "/second"
      Password.mkPasswordHashingPolicy (Password.argon2Iterations 1) (Password.argon2MemoryKib 8) (Password.argon2Parallelism 1)
        `shouldBe` Just (Password.defaultPasswordHashingPolicy {Password.passwordHashIterations = 1, Password.passwordHashMemoryKibibytes = 8, Password.passwordHashParallelism = 1})
      let encryptionKey =
            case Secret.mkSecretEncryptionKey "QkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkJCQkI" of
              Just key -> key
              Nothing -> error "expected valid secret-encryption-key fixture"
          encryptionNonce =
            case Secret.mkEncryptionNonce "0123456789ab" of
              Just nonce -> nonce
              Nothing -> error "expected valid encryption-nonce fixture"
      case Secret.encryptSecretWithNonce encryptionKey encryptionNonce (Secret.mkSecretPlaintext "fixture") of
        CryptoPassed envelope ->
          case Secret.decryptSecretText encryptionKey envelope of
            CryptoPassed (Right plaintext) -> plaintext `shouldBe` "fixture"
            _ -> expectationFailure "expected test secret decryption to succeed"
        _ -> expectationFailure "expected test secret encryption to succeed"
      tlsCertificateFilePathValue (tlsCertificateFilePath "certificate.pem") `shouldBe` "certificate.pem"
      tlsPrivateKeyFilePathValue (tlsPrivateKeyFilePath "key.pem") `shouldBe` "key.pem"
      Observability.buildRequestObservability
        Observability.RequestIdentity
          { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
            Observability.requestIdentityScheme = "https",
            Observability.requestIdentityPath = "/second",
            Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/second"
          }
        200
        Observability.PageResponseKind
        []
        `seq` pure ()

  describe "public record coverage" $ do
    it "reads every exported selector from the public request, page, shell, and document records" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          navigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "primary"}
          mainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "true"}
          localTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5001, localServerBaseUrl = "http://127.0.0.1:5001"}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = trustedMarkup "<h1>Known</h1>", pageBootstrapHooks = ["known-page"]}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          navigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/navigation.js", navigationRuntimeScript = "console.log('nav');"}
          stylesheetPath = AssetPath "/assets/sample.css"
          stylesheetValue = stylesheet stylesheetPath
          scopedCssScope = cssScope "sample"
          scopedCssClass = ScopedCssClass scopedCssScope "title"
          globalCssClass = GlobalCssClass "visually-hidden"
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = trustedMarkup "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentStylesheets = [stylesheetValue], documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellStylesheets = [stylesheetValue], shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          responseBodyValue = ResponseBody {responseStatus = Http.status202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}
          clientActionPayload =
            ClientActionPayload
              { clientActionMethod = "POST",
                clientActionPath = "/actions/subscribe",
                clientActionFields = [("email", "ada@example.com")],
                clientActionCsrfToken = Nothing,
                clientActionIdempotencyKey = Nothing,
                clientActionPayloadContext = defaultContext
              }
          clientActionRequest :: ClientActionRequest Text TestContext
          clientActionRequest =
            ClientActionRequest
              { clientAction = "/actions/subscribe",
                clientActionRequestIdempotencyKey = Nothing,
                clientActionContext = defaultContext
              }
          regionPatch = testRegionPatch "status-region" "Ready"
          clientActionResponse = ClientActionResponse {clientActionStatus = Http.status200, clientActionPatches = [regionPatch], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}
          NavigationItem {navigationLabel = navigationItemLabel, navigationRoute = navigationItemRoute} = navigationItem
          ResolvedNavigationItem {navigationLabel = resolvedNavigationItemLabel, navigationRoute = resolvedNavigationItemRoute, navigationHref = resolvedNavigationItemHref, navigationIsActive = resolvedNavigationItemIsActive} = resolvedNavigationItem

      requestRoute request `shouldBe` KnownRoute
      requestContext request `shouldBe` defaultContext
      attributeName attribute `shouldBe` "data-app"
      attributeValue attribute `shouldBe` "sample"
      pageTitle page `shouldBe` "Known"
      pageRoute page `shouldBe` KnownRoute
      pageContext page `shouldBe` defaultContext
      renderHtml (pageBody page) `shouldBe` "<h1>Known</h1>"
      pageBootstrapHooks page `shouldBe` ["known-page"]
      navigationItemLabel `shouldBe` "Known"
      navigationItemRoute `shouldBe` KnownRoute
      navigationRuntimePath navigationRuntime `shouldBe` "/assets/navigation.js"
      navigationRuntimeScript navigationRuntime `shouldBe` "console.log('nav');"
      navigationRuntimeScriptSource (testPathPrefix "/app") navigationRuntime `shouldBe` "/app/assets/navigation.js"
      navigationRuntimeResponse navigationRuntime "/assets/navigation.js"
        `shouldBe` Just
          ResponseBody
            { responseStatus = Http.status200,
              responseContentType = "application/javascript; charset=utf-8",
              responseBody = "console.log('nav');",
              responseObservabilityAttributes = [],
              responseLogEntries = [],
              responseDatabaseOperations = []
            }
      navigationRuntimeResponse navigationRuntime "/assets/missing.js" `shouldBe` Nothing
      navigationRuntimePath defaultNavigationRuntime `shouldBe` "/assets/navigation.js"
      navigationRuntimeScript defaultNavigationRuntime `shouldBe` defaultNavigationRuntimeScript
      Text.isInfixOf "function navigateTo" defaultNavigationRuntimeScript `shouldBe` True
      expectAll
        ( (Text.isInfixOf "const CapturedEvent = Object.freeze({ Submit: 'submit' });" defaultCaptureKernelScript `shouldBe` True)
            :| [ Text.isInfixOf "captureKernel.register(captureKernel.eventTypes.Submit" defaultNavigationRuntimeScript `shouldBe` True,
                 Text.isInfixOf "settlement.completed();" defaultNavigationRuntimeScript `shouldBe` True,
                 Text.isInfixOf "['click', 'input', 'change', 'keydown', 'submit']" defaultCaptureKernelScript `shouldBe` False
               ]
        )
      assetPathText stylesheetPath `shouldBe` "/assets/sample.css"
      stylesheetAsset stylesheetValue `shouldBe` AssetPath "/assets/sample.css"
      cssScopeName scopedCssScope `shouldBe` "sample"
      cssClassText scopedCssClass `shouldBe` "harch-sample-title"
      cssClassText globalCssClass `shouldBe` "visually-hidden"
      resolvedNavigationItemLabel `shouldBe` "Known"
      resolvedNavigationItemRoute `shouldBe` KnownRoute
      resolvedNavigationItemHref `shouldBe` "/known"
      resolvedNavigationItemIsActive `shouldBe` True
      documentTitle document `shouldBe` "Known"
      documentBodyAttributes document `shouldBe` [attribute]
      documentNavigationAttributes document `shouldBe` [navigationAttribute]
      documentNavigation document `shouldBe` [resolvedNavigationItem]
      documentMainId document `shouldBe` "app-main"
      documentMainAttributes document `shouldBe` [mainAttribute]
      renderHtml (documentMainContent document) `shouldBe` "<h1>Known</h1>"
      documentBootstrapHooks document `shouldBe` ["known-page"]
      documentStylesheets document `shouldBe` [stylesheetValue]
      documentRuntimeDescriptors document `shouldBe` [DeferredModule "navigation" "/assets/navigation.js"]
      shellBodyAttributes shell `shouldBe` [attribute]
      shellNavigationAttributes shell `shouldBe` [navigationAttribute]
      shellNavigationItems shell `shouldBe` [navigationItem]
      shellMainId shell `shouldBe` "app-main"
      shellMainAttributes shell `shouldBe` [mainAttribute]
      shellStylesheets shell `shouldBe` [stylesheetValue]
      shellRuntimeDescriptors shell `shouldBe` [DeferredModule "navigation" "/assets/navigation.js"]
      localServerHost localTestServer `shouldBe` "127.0.0.1"
      localServerPort localTestServer `shouldBe` 5001
      localServerBaseUrl localTestServer `shouldBe` "http://127.0.0.1:5001"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      applicationNavigationRuntime sampleApplication `shouldBe` Nothing
      length (applicationRequestMiddleware sampleApplication) `shouldBe` 0
      responseStatus responseBodyValue `shouldBe` Http.status202
      responseContentType responseBodyValue `shouldBe` "application/json"
      responseBody responseBodyValue `shouldBe` "{\"route\":\"data\"}"
      responseObservabilityAttributes responseBodyValue `shouldBe` []
      responseLogEntries responseBodyValue `shouldBe` []
      clientActionMethod clientActionPayload `shouldBe` "POST"
      clientActionPath clientActionPayload `shouldBe` "/actions/subscribe"
      clientActionFields clientActionPayload `shouldBe` [("email", "ada@example.com")]
      clientActionCsrfToken clientActionPayload `shouldBe` Nothing
      clientActionIdempotencyKey clientActionPayload `shouldBe` Nothing
      clientActionPayloadContext clientActionPayload `shouldBe` defaultContext
      clientAction clientActionRequest `shouldBe` "/actions/subscribe"
      clientActionRequestIdempotencyKey clientActionRequest `shouldBe` Nothing
      clientActionContext clientActionRequest `shouldBe` defaultContext
      regionPatchId regionPatch `shouldBe` "status-region"
      regionPatchHtml regionPatch `shouldBe` "<p id=\"status-region\" data-harch-region=\"true\">Ready</p>"
      clientActionStatus clientActionResponse `shouldBe` Http.status200
      clientActionPatches clientActionResponse `shouldBe` [regionPatch]
      clientActionFocusId clientActionResponse `shouldBe` Nothing
      clientActionHeaders clientActionResponse `shouldBe` []
      clientActionObservabilityAttributes clientActionResponse `shouldBe` []
      clientActionLogEntries clientActionResponse `shouldBe` []
      let diagnostics = responseDiagnostics (ClientActionBodyResponse clientActionResponse :: Response TestRoute TestContext)
      diagnosticObservabilityAttributes diagnostics `shouldBe` []
      diagnosticLogEntries diagnostics `shouldBe` []

    it "exercises derived Eq and Show instances for public HarchWeb records and responses" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          otherRequest = RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
          page = Page {pageTitle = "Known", pageRoute = KnownRoute, pageContext = defaultContext, pageBody = trustedMarkup "<h1>Known</h1>", pageBootstrapHooks = ["known-page"]}
          otherPage = Page {pageTitle = "Missing", pageRoute = MissingRoute, pageContext = defaultContext, pageBody = trustedMarkup "<h1>Missing</h1>", pageBootstrapHooks = []}
          attribute = HtmlAttribute {attributeName = "data-app", attributeValue = "sample"}
          otherAttribute = HtmlAttribute {attributeName = "lang", attributeValue = "en"}
          navigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "primary"}
          otherNavigationAttribute = HtmlAttribute {attributeName = "data-navigation-region", attributeValue = "secondary"}
          mainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "true"}
          otherMainAttribute = HtmlAttribute {attributeName = "data-navigation-content", attributeValue = "false"}
          localTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5001, localServerBaseUrl = "http://127.0.0.1:5001"}
          otherLocalTestServer = LocalTestServer {localServerHost = "127.0.0.1", localServerPort = 5002, localServerBaseUrl = "http://127.0.0.1:5002"}
          navigationItem = NavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute}
          otherNavigationItem = NavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute}
          navigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/navigation.js", navigationRuntimeScript = "console.log('nav');"}
          otherNavigationRuntime = NavigationRuntime {navigationRuntimePath = "/assets/other-navigation.js", navigationRuntimeScript = "console.log('other');"}
          inlineBootstrap = InlineBootstrap "capture" "window.capture = true;"
          otherInlineBootstrap = InlineBootstrap "other-capture" "window.capture = false;"
          stylesheetPath = AssetPath "/assets/sample.css"
          otherStylesheetPath = AssetPath "/assets/other.css"
          stylesheetValue = stylesheet stylesheetPath
          otherStylesheetValue = stylesheet otherStylesheetPath
          scopedCssScope = cssScope "sample"
          otherScopedCssScope = cssScope "other"
          scopedCssClass = ScopedCssClass scopedCssScope "title"
          otherScopedCssClass = ScopedCssClass otherScopedCssScope "title"
          globalCssClass = GlobalCssClass "visually-hidden"
          otherGlobalCssClass = GlobalCssClass "other-global"
          liveRegion = PoliteStatus
          otherLiveRegion = AssertiveAlert
          serverSentEvent = ServerSentEvent {serverSentEventName = Just "status", serverSentEventId = Just "42", serverSentEventData = "Ready"}
          otherServerSentEvent = ServerSentEvent {serverSentEventName = Nothing, serverSentEventId = Just "43", serverSentEventData = "Waiting"}
          resolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Known", navigationRoute = KnownRoute, navigationHref = "/known", navigationIsActive = True}
          otherResolvedNavigationItem = ResolvedNavigationItem {navigationLabel = "Missing", navigationRoute = MissingRoute, navigationHref = "/404", navigationIsActive = False}
          document = Document {documentTitle = "Known", documentBodyAttributes = [attribute], documentNavigationAttributes = [navigationAttribute], documentNavigation = [resolvedNavigationItem], documentMainId = "app-main", documentMainAttributes = [mainAttribute], documentMainContent = trustedMarkup "<h1>Known</h1>", documentBootstrapHooks = ["known-page"], documentStylesheets = [], documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          otherDocument = Document {documentTitle = "Missing", documentBodyAttributes = [otherAttribute], documentNavigationAttributes = [otherNavigationAttribute], documentNavigation = [otherResolvedNavigationItem], documentMainId = "other-main", documentMainAttributes = [otherMainAttribute], documentMainContent = trustedMarkup "<h1>Missing</h1>", documentBootstrapHooks = [], documentStylesheets = [], documentRuntimeDescriptors = []}
          shell = PageShell {shellBodyAttributes = [attribute], shellNavigationAttributes = [navigationAttribute], shellNavigationItems = [navigationItem], shellMainId = "app-main", shellMainAttributes = [mainAttribute], shellStylesheets = [], shellRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]}
          otherShell = PageShell {shellBodyAttributes = [otherAttribute], shellNavigationAttributes = [otherNavigationAttribute], shellNavigationItems = [otherNavigationItem], shellMainId = "other-main", shellMainAttributes = [otherMainAttribute], shellStylesheets = [], shellRuntimeDescriptors = []}
          body = ResponseBody {responseStatus = Http.status202, responseContentType = "application/json", responseBody = "{\"route\":\"data\"}", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}
          otherBody = ResponseBody {responseStatus = Http.status200, responseContentType = "text/html", responseBody = "<h1>OK</h1>", responseObservabilityAttributes = [Observability.ObservabilityAttribute {Observability.attributeName = "exception.type", Observability.attributeValue = Observability.TextAttribute "SampleError"}], responseLogEntries = ["ERROR sample"], responseDatabaseOperations = []}
          pageMetadata = ResponseBody {responseStatus = Http.status500, responseContentType = "text/html; charset=utf-8", responseBody = "", responseObservabilityAttributes = [Observability.ObservabilityAttribute {Observability.attributeName = "exception.type", Observability.attributeValue = Observability.TextAttribute "SampleError"}], responseLogEntries = ["ERROR page"], responseDatabaseOperations = []}
          otherPageMetadata = ResponseBody {responseStatus = Http.status503, responseContentType = "text/html; charset=utf-8", responseBody = "", responseObservabilityAttributes = [], responseLogEntries = ["ERROR other page"], responseDatabaseOperations = []}
          pageResponse :: Response TestRoute TestContext
          pageResponse = PageResponse page
          otherPageResponse :: Response TestRoute TestContext
          otherPageResponse = PageResponse otherPage
          pageResponseWithMetadata :: Response TestRoute TestContext
          pageResponseWithMetadata = PageResponseWithMetadata pageMetadata page
          otherPageResponseWithMetadata :: Response TestRoute TestContext
          otherPageResponseWithMetadata = PageResponseWithMetadata otherPageMetadata otherPage
          bodyResponseValue :: Response TestRoute TestContext
          bodyResponseValue = BodyResponse body
          otherBodyResponseValue :: Response TestRoute TestContext
          otherBodyResponseValue = BodyResponse otherBody
          redirectResponseValue :: Response TestRoute TestContext
          redirectResponseValue = RedirectResponse body "/spaces"
          otherRedirectResponseValue :: Response TestRoute TestContext
          otherRedirectResponseValue = RedirectResponse otherBody "/other"
          clientActionRequest :: ClientActionRequest Text TestContext
          clientActionRequest =
            ClientActionRequest
              { clientAction = "/actions/subscribe",
                clientActionRequestIdempotencyKey = Nothing,
                clientActionContext = defaultContext
              }
          otherClientActionRequest :: ClientActionRequest Text TestContext
          otherClientActionRequest =
            ClientActionRequest
              { clientAction = "/actions/other",
                clientActionRequestIdempotencyKey = Nothing,
                clientActionContext = spanishContext
              }
          regionPatch = testRegionPatch "status-region" "Ready"
          otherRegionPatch = testRegionPatch "other-region" "Other"
          clientActionResponse = ClientActionResponse {clientActionStatus = Http.status200, clientActionPatches = [regionPatch], clientActionFocusId = Just "email", clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}
          otherClientActionResponse = ClientActionResponse {clientActionStatus = Http.status422, clientActionPatches = [otherRegionPatch], clientActionFocusId = Nothing, clientActionHeaders = [], clientActionObservabilityAttributes = [], clientActionLogEntries = []}
      runtimeNonce <- generateRuntimeNonce
      otherRuntimeNonce <- generateRuntimeNonce

      (request /= otherRequest) `shouldBe` True
      show request `shouldBe` "RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}}"
      show [request] `shouldBe` "[RouteRequest {requestRoute = KnownRoute, requestContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}}]"
      (page /= otherPage) `shouldBe` True
      show page `shouldBe` "Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}"
      show [page] `shouldBe` "[Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}]"
      (attribute /= otherAttribute) `shouldBe` True
      show attribute `shouldBe` "HtmlAttribute {attributeName = \"data-app\", attributeValue = \"sample\"}"
      (navigationItem /= otherNavigationItem) `shouldBe` True
      show navigationItem `shouldBe` "NavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute}"
      (navigationRuntime /= otherNavigationRuntime) `shouldBe` True
      show navigationRuntime `shouldBe` "NavigationRuntime {navigationRuntimePath = \"/assets/navigation.js\", navigationRuntimeScript = \"console.log('nav');\"}"
      show [navigationRuntime] `shouldBe` "[NavigationRuntime {navigationRuntimePath = \"/assets/navigation.js\", navigationRuntimeScript = \"console.log('nav');\"}]"
      (inlineBootstrap /= otherInlineBootstrap) `shouldBe` True
      show inlineBootstrap `shouldBe` "InlineBootstrap {runtimeDescriptorName = \"capture\", runtimeDescriptorSource = \"window.capture = true;\"}"
      runtimeNonceValue runtimeNonce `shouldNotBe` runtimeNonceValue otherRuntimeNonce
      show runtimeNonce `shouldContain` "RuntimeNonce {runtimeNonceValue = \""
      show [runtimeNonce] `shouldContain` "[RuntimeNonce {runtimeNonceValue = \""
      (stylesheetPath /= otherStylesheetPath) `shouldBe` True
      show stylesheetPath `shouldBe` "AssetPath {assetPathText = \"/assets/sample.css\"}"
      show [stylesheetPath] `shouldBe` "[AssetPath {assetPathText = \"/assets/sample.css\"}]"
      (stylesheetValue /= otherStylesheetValue) `shouldBe` True
      show stylesheetValue `shouldBe` "Stylesheet {stylesheetAsset = AssetPath {assetPathText = \"/assets/sample.css\"}}"
      (scopedCssScope /= otherScopedCssScope) `shouldBe` True
      show scopedCssScope `shouldBe` "CssScope {cssScopeName = \"sample\"}"
      show [scopedCssScope] `shouldBe` "[CssScope {cssScopeName = \"sample\"}]"
      (scopedCssClass /= otherScopedCssClass) `shouldBe` True
      (globalCssClass /= otherGlobalCssClass) `shouldBe` True
      show scopedCssClass `shouldBe` "ScopedCssClass (CssScope {cssScopeName = \"sample\"}) \"title\""
      show globalCssClass `shouldBe` "GlobalCssClass \"visually-hidden\""
      show [scopedCssClass, globalCssClass]
        `shouldBe` "[ScopedCssClass (CssScope {cssScopeName = \"sample\"}) \"title\",GlobalCssClass \"visually-hidden\"]"
      (resolvedNavigationItem /= otherResolvedNavigationItem) `shouldBe` True
      show resolvedNavigationItem `shouldBe` "ResolvedNavigationItem {navigationLabel = \"Known\", navigationRoute = KnownRoute, navigationHref = \"/known\", navigationIsActive = True}"
      (document /= otherDocument) `shouldBe` True
      show document `shouldContain` "documentRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      show [document] `shouldContain` "documentRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      (localTestServer /= otherLocalTestServer) `shouldBe` True
      show localTestServer `shouldBe` "LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}"
      show [localTestServer] `shouldBe` "[LocalTestServer {localServerHost = \"127.0.0.1\", localServerPort = 5001, localServerBaseUrl = \"http://127.0.0.1:5001\"}]"
      (shell /= otherShell) `shouldBe` True
      show shell `shouldContain` "shellRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      show [shell] `shouldContain` "shellRuntimeDescriptors = [DeferredModule {runtimeDescriptorName = \"navigation\", runtimeDescriptorSource = \"/assets/navigation.js\"}]"
      expectAll
        ( ((liveRegion /= otherLiveRegion) `shouldBe` True)
            :| [ show liveRegion `shouldBe` "PoliteStatus",
                 show [liveRegion] `shouldBe` "[PoliteStatus]",
                 (serverSentEvent /= otherServerSentEvent) `shouldBe` True,
                 show serverSentEvent `shouldBe` "ServerSentEvent {serverSentEventName = Just \"status\", serverSentEventId = Just \"42\", serverSentEventData = \"Ready\"}",
                 show [serverSentEvent] `shouldBe` "[ServerSentEvent {serverSentEventName = Just \"status\", serverSentEventId = Just \"42\", serverSentEventData = \"Ready\"}]"
               ]
        )
      (body /= otherBody) `shouldBe` True
      show body `shouldBe` "ResponseBody {responseStatus = Status {statusCode = 202, statusMessage = \"Accepted\"}, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}"
      show [body] `shouldBe` "[ResponseBody {responseStatus = Status {statusCode = 202, statusMessage = \"Accepted\"}, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}]"
      (pageMetadata /= otherPageMetadata) `shouldBe` True
      show pageMetadata `shouldBe` "ResponseBody {responseStatus = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"], responseDatabaseOperations = []}"
      (pageResponse /= otherPageResponse) `shouldBe` True
      show pageResponse `shouldBe` "PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]})"
      (pageResponseWithMetadata /= otherPageResponseWithMetadata) `shouldBe` True
      -- 'Response'\'s 'Eq' short-circuits its '&&': a same-'ResponseBody',
      -- different-'Page' comparison is the only way to reach its second
      -- operand, since 'otherPageResponseWithMetadata' above differs in
      -- both fields at once and never gets that far.
      (pageResponseWithMetadata /= PageResponseWithMetadata pageMetadata otherPage) `shouldBe` True
      show pageResponseWithMetadata `shouldBe` "PageResponseWithMetadata (ResponseBody {responseStatus = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"], responseDatabaseOperations = []}) (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]})"
      (bodyResponseValue /= otherBodyResponseValue) `shouldBe` True
      show bodyResponseValue `shouldBe` "BodyResponse (ResponseBody {responseStatus = Status {statusCode = 202, statusMessage = \"Accepted\"}, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []})"
      (redirectResponseValue /= otherRedirectResponseValue) `shouldBe` True
      -- Same reason as 'pageResponseWithMetadata' above: a same-body,
      -- different-location comparison reaches 'Eq'\'s second '&&' operand,
      -- which 'otherRedirectResponseValue' (differing in both fields) does
      -- not.
      (redirectResponseValue /= RedirectResponse body "/other") `shouldBe` True
      show redirectResponseValue `shouldBe` "RedirectResponse (ResponseBody {responseStatus = Status {statusCode = 202, statusMessage = \"Accepted\"}, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []}) \"/spaces\""
      show [pageResponse, pageResponseWithMetadata, bodyResponseValue] `shouldBe` "[PageResponse (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),PageResponseWithMetadata (ResponseBody {responseStatus = Status {statusCode = 500, statusMessage = \"Internal Server Error\"}, responseContentType = \"text/html; charset=utf-8\", responseBody = \"\", responseObservabilityAttributes = [ObservabilityAttribute {attributeName = \"exception.type\", attributeValue = TextAttribute \"SampleError\"}], responseLogEntries = [\"ERROR page\"], responseDatabaseOperations = []}) (Page {pageTitle = \"Known\", pageRoute = KnownRoute, pageContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}, pageBody = \"<h1>Known</h1>\", pageBootstrapHooks = [\"known-page\"]}),BodyResponse (ResponseBody {responseStatus = Status {statusCode = 202, statusMessage = \"Accepted\"}, responseContentType = \"application/json\", responseBody = \"{\\\"route\\\":\\\"data\\\"}\", responseObservabilityAttributes = [], responseLogEntries = [], responseDatabaseOperations = []})]"
      (clientActionRequest /= otherClientActionRequest) `shouldBe` True
      show clientActionRequest `shouldBe` "ClientActionRequest {clientAction = \"/actions/subscribe\", clientActionRequestIdempotencyKey = Nothing, clientActionContext = TestContext {requestLanguage = \"en\", testContextPathPrefix = \"\"}}"
      show [clientActionRequest] `shouldContain` "ClientActionRequest {clientAction = \"/actions/subscribe\""
      (regionPatch /= otherRegionPatch) `shouldBe` True
      show regionPatch `shouldContain` "ReplaceRegion"
      show [regionPatch] `shouldContain` "ReplaceRegion"
      (clientActionResponse /= otherClientActionResponse) `shouldBe` True
      show clientActionResponse `shouldContain` "ClientActionResponse {clientActionStatus = Status {statusCode = 200, statusMessage = \"OK\"}"
      show [clientActionResponse] `shouldContain` "ClientActionResponse {clientActionStatus = Status {statusCode = 200, statusMessage = \"OK\"}"

    it "reads the Application fields directly without relying on higher-level helpers" $ do
      let request = RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}
          codec = routeCodec sampleApplication

      appName sampleApplication `shouldBe` "sample"
      defaultRequestContext sampleApplication `shouldBe` defaultContext
      requestContextFromRequest sampleApplication Wai.defaultRequest defaultContext `shouldBe` defaultContext
      applicationStaticAssets sampleApplication `shouldBe` emptyStaticAssets
      decodeClientAction
        sampleApplication
        ClientActionPayload
          { clientActionMethod = "POST",
            clientActionPath = "/actions/subscribe",
            clientActionFields = [],
            clientActionCsrfToken = Nothing,
            clientActionIdempotencyKey = Nothing,
            clientActionPayloadContext = defaultContext
          }
        `shouldBe` DecodedClientAction "/actions/subscribe"
      handleClientAction
        sampleApplication
        ClientActionRequest
          { clientAction = "/actions/subscribe",
            clientActionRequestIdempotencyKey = Nothing,
            clientActionContext = defaultContext
          }
        `shouldReturn` Nothing
      parseRoute codec defaultContext "/known" `shouldBe` Just request
      parseRoute codec defaultContext "/data" `shouldBe` Just RouteRequest {requestRoute = DataRoute, requestContext = defaultContext}
      renderRoute codec request `shouldBe` "/known"
      notFoundRequest codec defaultContext `shouldBe` RouteRequest {requestRoute = MissingRoute, requestContext = defaultContext}
      renderResponse sampleApplication request `shouldReturn` PageResponse (samplePage request)
      renderDocument (pageShell sampleApplication (samplePage request))
        `shouldBe` "<!DOCTYPE html><html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"
      Text.isInfixOf
        "<script nonce=\""
        ( renderDocument
            ( (pageShell sampleApplication (samplePage request))
                { documentRuntimeDescriptors = [InlineBootstrap "capture" "window.capture = true;"]
                }
            )
        )
        `shouldBe` True
      reportRequestObservability
        sampleApplication
        ( Observability.buildRequestObservability
            Observability.RequestIdentity
              { Observability.requestIdentityMethod = Observability.mkSpanMethodLabel "GET",
                Observability.requestIdentityScheme = "http",
                Observability.requestIdentityPath = "/known",
                Observability.requestIdentityRoutePath = Observability.mkSpanRoutePath "/known"
              }
            200
            Observability.PageResponseKind
            []
        )
      reportApplicationLog sampleApplication "ignored log entry"

spec = do
  existingSpec
  movedSpec
