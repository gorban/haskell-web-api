{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Control.Concurrent ()
import Control.Exception ()
import Control.Monad ()
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
import Data.Text ()
import Data.Text qualified as Text (isInfixOf, isSuffixOf, length)
import Data.Text.Encoding qualified as TextEncoding ()
import HarchWeb (AssetPath (AssetPath), Document (Document, documentBodyAttributes, documentBootstrapHooks, documentMainAttributes, documentMainContent, documentMainId, documentNavigation, documentNavigationAttributes, documentRuntimeDescriptors, documentStylesheets, documentTitle), HtmlAttribute (HtmlAttribute, attributeName, attributeValue), LiveRegion (AssertiveAlert, PoliteStatus), Page (Page, pageBody, pageBootstrapHooks, pageContext, pageRoute, pageTitle), PageShell (shellNavigationItems), ResolvedNavigationItem (ResolvedNavigationItem, navigationHref, navigationIsActive, navigationLabel, navigationRoute), RouteRequest (RouteRequest, requestContext, requestRoute), RuntimeDescriptor (DeferredModule), RuntimeNonce (runtimeNonceValue), buildNavigation, buildPageShell, generateRuntimeNonce, liveRegionAttributes, stylesheet)
import HarchWeb.Action qualified as Action ()
import HarchWeb.Database qualified as Database ()
import HarchWeb.Markup.Unsafe qualified as MarkupUnsafe ()
import HarchWeb.Observability qualified as Observability ()
import HarchWeb.Security qualified as Security ()
import Network.HTTP.Client qualified as HttpClient ()
import Network.HTTP.Types qualified as Http ()
import Network.Socket qualified as Socket ()
import Network.Socket.ByteString qualified as SocketByteString ()
import Network.Wai qualified as Wai ()
import Network.Wai.Handler.Warp qualified as Warp ()
import System.Directory ()
import System.Environment ()
import System.Exit ()
import System.FilePath ()
import System.IO ()
import System.IO.Error ()
import System.IO.Temp ()
import System.Posix.Signals ()
import System.Process ()
import TestCore.CustomAssertions ()
import TestCore.Wai ()
import Text.Read ()
import Unit.HarchWeb.TestSupport (TestRoute (KnownRoute, MissingRoute), defaultContext, renderDocument, sampleCodec, samplePage, sampleShell, spanishContext, trustedMarkup)

existingSpec :: Spec
existingSpec =
  describe "live-region accessibility helpers" $ do
    it "keeps polite status updates atomic" $
      liveRegionAttributes PoliteStatus
        `shouldBe` [HtmlAttribute "role" "status", HtmlAttribute "aria-live" "polite", HtmlAttribute "aria-atomic" "true"]

    it "uses assertive alerts for errors that require immediate attention" $
      liveRegionAttributes AssertiveAlert
        `shouldBe` [HtmlAttribute "role" "alert", HtmlAttribute "aria-live" "assertive", HtmlAttribute "aria-atomic" "true"]

movedSpec :: Spec
movedSpec = do
  describe "buildNavigation" $
    it "resolves hrefs and active state from the current page context" $
      buildNavigation sampleCodec (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = spanishContext})) (shellNavigationItems sampleShell)
        `shouldBe` [ ResolvedNavigationItem
                       { navigationLabel = "Known",
                         navigationRoute = KnownRoute,
                         navigationHref = "/es/known",
                         navigationIsActive = True
                       },
                     ResolvedNavigationItem
                       { navigationLabel = "Missing",
                         navigationRoute = MissingRoute,
                         navigationHref = "/404",
                         navigationIsActive = False
                       }
                   ]

  describe "buildPageShell" $ do
    it "preserves the generic shell contract separately from app-specific page content" $
      buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext}))
        `shouldBe` Document
          { documentTitle = "Known",
            documentBodyAttributes =
              [ HtmlAttribute
                  { attributeName = "data-app",
                    attributeValue = "sample"
                  }
              ],
            documentNavigationAttributes =
              [ HtmlAttribute
                  { attributeName = "data-navigation-region",
                    attributeValue = "primary"
                  }
              ],
            documentNavigation =
              [ ResolvedNavigationItem
                  { navigationLabel = "Known",
                    navigationRoute = KnownRoute,
                    navigationHref = "/known",
                    navigationIsActive = True
                  },
                ResolvedNavigationItem
                  { navigationLabel = "Missing",
                    navigationRoute = MissingRoute,
                    navigationHref = "/404",
                    navigationIsActive = False
                  }
              ],
            documentMainId = "app-main",
            documentMainAttributes =
              [ HtmlAttribute
                  { attributeName = "data-navigation-content",
                    attributeValue = "true"
                  }
              ],
            documentMainContent = trustedMarkup "<h1>Known</h1>",
            documentBootstrapHooks = [],
            documentStylesheets = [],
            documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js"]
          }

    it "renders typed external stylesheets before nonce-bound runtime descriptors" $ do
      let document =
            (buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})))
              { documentStylesheets = [stylesheet (AssetPath "/assets/sample.css")]
              }
      Text.isInfixOf
        "<title>Known</title><link rel=\"stylesheet\" href=\"/assets/sample.css\"><script type=\"module\" src=\"/assets/navigation.js\" defer></script>"
        (renderDocument document)
        `shouldBe` True

    it "renders the shared HTML document for the supplied page and shell options" $
      renderDocument (buildPageShell sampleCodec sampleShell (samplePage (RouteRequest {requestRoute = KnownRoute, requestContext = defaultContext})))
        `shouldBe` "<!DOCTYPE html><html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\"><h1>Known</h1></main></body></html>"

    it "HTML-escapes the stylesheet href, navigation href/label, main id, and deferred module src sinks" $
      renderDocument
        Document
          { documentTitle = "Known",
            documentBodyAttributes = [],
            documentNavigationAttributes = [],
            documentNavigation =
              [ ResolvedNavigationItem
                  { navigationLabel = "A & <B>",
                    navigationRoute = KnownRoute,
                    navigationHref = "/known?a=1&b=2",
                    navigationIsActive = False
                  }
              ],
            documentMainId = "app-main\" onclick=\"steal()",
            documentMainAttributes = [],
            documentMainContent = trustedMarkup "<h1>Known</h1>",
            documentBootstrapHooks = [],
            documentStylesheets = [stylesheet (AssetPath "/assets/sample.css?a=1&b=2")],
            documentRuntimeDescriptors = [DeferredModule "navigation" "/assets/navigation.js?a=1&b=2"]
          }
        `shouldBe` "<!DOCTYPE html><html><head><title>Known</title><link rel=\"stylesheet\" href=\"/assets/sample.css?a=1&amp;b=2\"><script type=\"module\" src=\"/assets/navigation.js?a=1&amp;b=2\" defer></script></head><body><nav><a href=\"/known?a=1&amp;b=2\" data-page-link=\"true\">A &amp; &lt;B&gt;</a></nav><main id=\"app-main&quot; onclick=&quot;steal()\"><h1>Known</h1></main></body></html>"

    it "renders bootstrap hook metadata only for pages that opt in" $
      renderDocument
        ( buildPageShell
            sampleCodec
            sampleShell
            ( Page
                { pageTitle = "Known",
                  pageRoute = KnownRoute,
                  pageContext = defaultContext,
                  pageBody = trustedMarkup "<h1>Known</h1>",
                  pageBootstrapHooks = ["known-page", "hydrate-known"]
                }
            )
        )
        `shouldBe` "<!DOCTYPE html><html><head><title>Known</title><script type=\"module\" src=\"/assets/navigation.js\" defer></script></head><body data-app=\"sample\"><nav data-navigation-region=\"primary\"><a href=\"/known\" data-page-link=\"true\" aria-current=\"page\">Known</a><a href=\"/404\" data-page-link=\"true\">Missing</a></nav><main id=\"app-main\" data-navigation-content=\"true\" data-bootstrap-hooks=\"known-page,hydrate-known\"><h1>Known</h1></main></body></html>"

    it "generates a fresh unpadded 32-byte CSP nonce for each response" $ do
      firstNonce <- generateRuntimeNonce
      secondNonce <- generateRuntimeNonce
      let firstValue = runtimeNonceValue firstNonce
          secondValue = runtimeNonceValue secondNonce
      expectAll
        ( (Text.length firstValue `shouldBe` 43)
            :| [ Text.length secondValue `shouldBe` 43,
                 Text.isSuffixOf "=" firstValue `shouldBe` False,
                 Text.isSuffixOf "=" secondValue `shouldBe` False
               ]
        )

spec = do
  existingSpec
  movedSpec
