{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module App.Pages.Home
  ( homePage,
    subscriptionResultRegion,
  )
where

import App.Components.ExampleAuthor
  ( AuthorIdentity (..),
    AvatarSize (..),
    authorAvatar,
    authorCard,
  )
import App.Components.SubscriptionEmailField
  ( subscriptionEmailField,
  )
import App.Routes (TwoPageRoute (..), routeHref)
import Data.Text (Text)
import HarchWeb
  ( CssClass (..),
    Page (..),
    Region,
    RouteRequest (..),
    anchorTag,
    ariaLabel,
    buttonTag,
    className,
    cssScope,
    dataAttribute,
    dataFlag,
    element,
    formAction,
    formTag,
    fragment,
    harch,
    headingOneTag,
    headingTwoTag,
    href,
    inputType,
    literalElementId,
    method,
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

homePage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
homePage routeRequest =
  pure
    Page
      { pageTitle = "Home",
        pageRoute = HomeRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          [harch|
                <section data-page="home" class={ScopedCssClass (cssScope "home") "root"}>
                  <h1>Home</h1>

                  <p>This page is fully server-rendered on direct load and reload.</p>

                  <p><a href={routeHref SecondRoute} data-page-link="true">Go to the second page</a></p>

                  <p><a href={routeHref LiveDataRoute} data-page-link="true">See live updates</a></p>

                  <section data-page-example="about">
                    <h2>About this example</h2>
                    <AuthorCard authorName="Harch Web team" authorRole="SSR framework maintainers">
                      <p>The page and its controls are complete before optional JavaScript loads.</p>
                    </AuthorCard>
                    <AuthorAvatar props={[AuthorIdentity "HW", CompactAvatar]}>
                      <p>Maintained as a small, runnable framework reference.</p>
                    </AuthorAvatar>
                  </section>

                  <form aria-label="Subscription" data-harch-control data-harch-action="true" action="/actions/subscribe" method="post">
                    <SubscriptionEmailField />
                    <button name="intent" value="subscribe" type="submit">Subscribe</button>
                  </form>

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
