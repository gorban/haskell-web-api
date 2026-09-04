{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home
  ( pageDefinition,
    homePage,
    nativeSubscriptionResultPage,
    subscriptionResultRegion,
  )
where

import App.Components.Controls
  ( actionForm,
    pageLink,
  )
import App.Components.ExampleAuthor
  ( AuthorIdentity (..),
    AvatarSize (..),
    authorAvatar,
    authorCard,
  )
import App.Components.SubscriptionEmailField
  ( subscriptionEmailField,
  )
import App.Pages.Route.Generated (PageRoute (..))
import App.Routes
  ( CustomRoute (NativeSubscriptionResult),
    TwoPageNavigationTarget (NavigationPage),
    TwoPageRoute,
    twoPageActions,
    twoPageEndpointMetadata,
  )
import App.Routes qualified as Routes
import Data.Text (Text)
import HarchWeb
  ( CssClass (..),
    EndpointProtocol (HtmlEndpoint),
    Html,
    Page (..),
    PageSecurity,
    Region,
    RouteRequest (..),
    buttonTag,
    cssScope,
    dataAttribute,
    element,
    elementId,
    harch,
    headingOneTag,
    inputTag,
    inputType,
    labelFor,
    labelTag,
    literalElementId,
    mkRegionId,
    name,
    pageCsrfValue,
    pageSecurityCsrf,
    paragraphTag,
    region,
    role,
    sectionTag,
    text,
    value,
    voidElement,
  )
import HarchWeb.Controls qualified as Controls
import HarchWeb.Site (RouteDefinition)
import HarchWeb.Site qualified as Site

pageDefinition :: RouteDefinition TwoPageRoute () ()
pageDefinition =
  Site.pageRoute (twoPageEndpointMetadata HtmlEndpoint (Routes.Page HomePage)) (Just "Home") homePage

aboutAuthorName :: Text
aboutAuthorName = "Harch Web team"

aboutAuthorRole :: Text
aboutAuthorRole = "SSR framework maintainers"

homePage :: PageSecurity -> RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
homePage pageSecurity routeRequest =
  pure
    Page
      { pageTitle = "Home",
        pageRoute = Routes.Page HomePage,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
                <section data-page="home" class={ScopedCssClass (cssScope "home") "root"}>
                  <h1>Home</h1>

                  <p>This page is fully server-rendered on direct load and reload.</p>

                  <p><PageLink to={NavigationPage SecondPage}>Go to the second page</PageLink></p>

                  <p><PageLink to={NavigationPage LiveDataPage}>See live updates</PageLink></p>

                  <section data-page-example="about">
                    <h2>About this example</h2>
                    <AuthorCard authorName={aboutAuthorName} authorRole={aboutAuthorRole}>
                      <p>The page and its controls are complete before optional JavaScript loads.</p>
                    </AuthorCard>
                    <AuthorAvatar props={[AuthorIdentity "HW", CompactAvatar]}>
                      <p>Maintained as a small, runnable framework reference.</p>
                    </AuthorAvatar>
                  </section>

                  <ActionForm action={()} aria-label="Subscription">
                    <SubscriptionEmailField />
                    <button name="intent" value="subscribe" type="submit">Subscribe</button>
                  </ActionForm>

                  {nativeSubscriptionFallbackForm pageSecurity}

                  <Region value={subscriptionResultRegion "status" ""} />
                </section>
          |],
        pageBootstrapHooks = []
      }

nativeSubscriptionFallbackForm :: PageSecurity -> Html
nativeSubscriptionFallbackForm pageSecurity =
  Controls.renderActionForm
    ( Controls.staticActionForm
        twoPageActions
        ()
        Controls.defaultActionFormAttributes
          { Controls.actionFormAriaLabel = Just "Native fallback subscription",
            Controls.actionFormCapabilities =
              [ Controls.NativeFallback
                  Controls.NativeActionFallback
                    { Controls.nativeActionFallbackPath = "/native-subscribe",
                      Controls.nativeActionFallbackMethod = Controls.FormPost,
                      Controls.nativeActionFallbackCsrfToken = pageCsrfValue (pageSecurityCsrf pageSecurity)
                    }
              ]
          }
        [ element labelTag [labelFor (literalElementId "native-subscription-email")] [text "Native fallback email address"],
          voidElement inputTag [elementId (literalElementId "native-subscription-email"), name "email", inputType "email", value "fallback@example.com"],
          element buttonTag [inputType "submit"] [text "Submit with native fallback"]
        ]
    )

nativeSubscriptionResultPage :: PageSecurity -> RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
nativeSubscriptionResultPage _ routeRequest =
  pure
    Page
      { pageTitle = "Subscription received",
        pageRoute = Routes.Custom NativeSubscriptionResult,
        pageContext = requestContext routeRequest,
        pageBody = element sectionTag [dataAttribute "page" "native-subscription"] [element headingOneTag [] [text "Subscription received"], element paragraphTag [] [text "The native fallback accepted this submission."]],
        pageBootstrapHooks = []
      }

subscriptionResultRegion :: Text -> Text -> Region
subscriptionResultRegion liveRole message =
  region
    (mkRegionId (literalElementId "subscription-result"))
    paragraphTag
    [role liveRole]
    [text message]
