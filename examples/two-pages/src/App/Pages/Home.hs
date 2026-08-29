{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home
  ( pageDefinition,
    homePage,
    nativeSubscriptionFallbackPage,
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
  ( CustomRoute (NativeSubscriptionFallback),
    TwoPageNavigationTarget (NavigationPage),
    TwoPageRoute,
    twoPageActions,
  )
import App.Routes qualified as Routes
import Data.Text (Text)
import HarchWeb
  ( CssClass (..),
    Html,
    Page (..),
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

pageDefinition :: RouteDefinition TwoPageRoute ()
pageDefinition =
  Site.pageRoute (Just "Home") homePage

aboutAuthorName :: Text
aboutAuthorName = "Harch Web team"

aboutAuthorRole :: Text
aboutAuthorRole = "SSR framework maintainers"

homePage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
homePage routeRequest =
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

                  {nativeSubscriptionFallbackForm}

                  <Region value={subscriptionResultRegion "status" ""} />
                </section>
          |],
        pageBootstrapHooks = []
      }

nativeSubscriptionFallbackForm :: Html
nativeSubscriptionFallbackForm =
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
                      Controls.nativeActionFallbackCsrfToken = "two-pages-native-fallback"
                    }
              ]
          }
        [ element labelTag [labelFor (literalElementId "native-subscription-email")] [text "Native fallback email address"],
          voidElement inputTag [elementId (literalElementId "native-subscription-email"), name "email", inputType "email", value "fallback@example.com"],
          element buttonTag [inputType "submit"] [text "Submit with native fallback"]
        ]
    )

nativeSubscriptionFallbackPage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
nativeSubscriptionFallbackPage routeRequest =
  pure
    Page
      { pageTitle = "Subscription received",
        pageRoute = Routes.Custom NativeSubscriptionFallback,
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
