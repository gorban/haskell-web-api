module Unit.HarchWeb.FacadeSpec (spec) where

import HarchWeb
import Network.Wai qualified as Wai
import System.IO (Handle)
import Test.Hspec

-- This module intentionally imports only the umbrella facade. It keeps the
-- supported authoring, request, observability, local-server, and TLS extension
-- entry points compile-covered without granting framework users access to the
-- private implementation modules that own them.
spec :: Spec
spec =
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
