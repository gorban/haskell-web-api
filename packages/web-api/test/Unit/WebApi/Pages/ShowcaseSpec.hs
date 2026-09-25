{-# LANGUAGE OverloadedStrings #-}

{-# SPEC #-}

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified as Text
import HarchWeb qualified
import TestCore.Wai (performWaiRequest, readResponseBody, waiRequest)
import Unit.WebApi.TestSupport (pureApplication)
import WebApi.Config (AppConfig (..), defaultAppConfig)
import WebApi.PageModule (PageFailure (..), PageModule (..), pageModulePage, renderPageFailure)
import WebApi.Route (AppRequestContext, AppRoute (..), RouteMetadata (..), defaultRequestContext, endpointMetadata, renderRoutePath, routeEnhancementHooks, routeMetadata, routePageSegment, routePageTitle)

spec = describe "WebApi.Pages showcase family" $ do
  describe "route presentation (the shared tables)" $ do
    it "projects each generated page's metadata" $ do
      expectAll
        ( (routePageSegment (routeMetadata ShowcaseRoute) `shouldBe` Just "showcase")
            :| [ routePageTitle (routeMetadata ShowcaseRoute) `shouldBe` "Showcase",
                 routeEnhancementHooks (routeMetadata ShowcaseRoute) `shouldBe` ["web-api-showcase"],
                 routePageSegment (routeMetadata ShowcaseAlternateRoute) `shouldBe` Just "showcase-alternate",
                 routePageTitle (routeMetadata ShowcaseAlternateRoute) `shouldBe` "Showcase alternate",
                 routeEnhancementHooks (routeMetadata ShowcaseAlternateRoute) `shouldBe` ["web-api-showcase-alternate"]
               ]
        )

    it "declares each generated page's endpoint identity" $ do
      renderRoutePath
        ( HarchWeb.RouteRequest
            { HarchWeb.requestRoute = ShowcaseRoute,
              HarchWeb.requestContext = defaultRequestContext
            }
        )
        `shouldBe` "/showcase"

  describe "server-rendered pages" $ do
    it "renders the showcase page with its scoped classes and its own stylesheet" $ do
      response <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["showcase"])
      responseBody <- readResponseBody response
      expectAll
        ( (Text.isInfixOf "data-page=\"showcase\"" responseBody `shouldBe` True)
            :| [ Text.isInfixOf "class=\"harch-showcase-root\"" responseBody `shouldBe` True,
                 Text.isInfixOf "class=\"harch-showcase-heading\"" responseBody `shouldBe` True,
                 Text.isInfixOf "class=\"harch-showcase-card\"" responseBody `shouldBe` True,
                 Text.isInfixOf "href=\"/assets/styles/pages/showcase.css\"" responseBody `shouldBe` True,
                 Text.isInfixOf "This card is styled by showcase.css only." responseBody `shouldBe` True,
                 Text.isInfixOf "href=\"/assets/styles/pages/showcase-alternate.css\"" responseBody `shouldBe` False
               ]
        )

    it "renders the alternate page with the same local classes under its own scope" $ do
      response <- performWaiRequest (HarchWeb.toWaiApplication pureApplication) (waiRequest ["showcase-alternate"])
      responseBody <- readResponseBody response
      expectAll
        ( (Text.isInfixOf "data-page=\"showcase-alternate\"" responseBody `shouldBe` True)
            :| [ Text.isInfixOf "class=\"harch-showcase-alternate-root\"" responseBody `shouldBe` True,
                 Text.isInfixOf "class=\"harch-showcase-alternate-card\"" responseBody `shouldBe` True,
                 Text.isInfixOf "href=\"/assets/styles/pages/showcase-alternate.css\"" responseBody `shouldBe` True,
                 Text.isInfixOf "This card is styled by showcase-alternate.css only." responseBody `shouldBe` True,
                 Text.isInfixOf "harch-showcase-root" responseBody `shouldBe` False
               ]
        )

  describe "the typed load-failure rail" $ do
    it "renders the shared failure page when a page load fails" $ do
      let failingModule =
            PageModule
              { pageModuleEndpointMetadata = endpointMetadata ShowcaseRoute,
                pageModuleNavLabel = Nothing,
                pageModuleTitle = "Showcase",
                pageModuleStylesheets = [],
                pageModuleHooks = [],
                pageModuleLoad = pure (Left (PageFailureMessage "database unavailable")),
                pageModuleRender = \_ -> pure (HarchWeb.text "unreachable")
              }
          request =
            HarchWeb.RouteRequest
              { HarchWeb.requestRoute = ShowcaseRoute,
                HarchWeb.requestContext = defaultRequestContext
              }
      page <- pageModulePage failingModule defaultAppConfig request
      let rendered = Text.unpack (HarchWeb.renderHtml (HarchWeb.pageBody page))
      expectAll
        ( (rendered `shouldContain` "Something went wrong")
            :| [ rendered `shouldContain` "database unavailable",
                 HarchWeb.pageTitle page `shouldBe` "web-api: Showcase",
                 HarchWeb.pageBootstrapHooks page `shouldBe` []
               ]
        )

    it "keeps the failure rendering total for any failure message" $ do
      Text.isInfixOf
        "boom"
        (HarchWeb.renderHtml (renderPageFailure (PageFailureMessage "boom")))
        `shouldBe` True
