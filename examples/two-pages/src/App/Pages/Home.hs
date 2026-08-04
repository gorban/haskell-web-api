{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home
  ( pageDefinition,
    homePage,
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
  ( TwoPageActionTarget (Subscribe),
    TwoPageNavigationTarget (NavigationPage),
    TwoPageRoute,
  )
import App.Routes qualified as Routes
import Data.Text (Text)
import HarchWeb
  ( CssClass (..),
    Page (..),
    Region,
    RouteRequest (..),
    buttonTag,
    className,
    cssScope,
    dataAttribute,
    element,
    fragment,
    harch,
    headingOneTag,
    headingTwoTag,
    inputType,
    literalElementId,
    mkRegionId,
    name,
    paragraphTag,
    region,
    regionHtml,
    role,
    sectionTag,
    text,
    value,
  )
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

                  <ActionForm action={Subscribe} aria-label="Subscription">
                    <SubscriptionEmailField />
                    <button name="intent" value="subscribe" type="submit">Subscribe</button>
                  </ActionForm>

                  <Region value={subscriptionResultRegion "status" ""} />
                </section>
          |],
        pageBootstrapHooks = []
      }

subscriptionResultRegion :: Text -> Text -> Region
subscriptionResultRegion liveRole message =
  region
    (mkRegionId (literalElementId "subscription-result"))
    paragraphTag
    [role liveRole]
    [text message]
