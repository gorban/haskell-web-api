{-# LANGUAGE OverloadedStrings #-}

module App.Pages.Home
  ( homePage,
    subscriptionResultRegion,
  )
where

import App.Routes (TwoPageRoute (..), routeHref)
import Data.Text (Text)
import HarchWeb
  ( CssClass (..),
    Page (..),
    Region,
    RouteRequest (..),
    anchorTag,
    ariaLabel,
    autocomplete,
    buttonTag,
    className,
    cssScope,
    dataAttribute,
    dataFlag,
    element,
    elementId,
    formAction,
    formTag,
    headingOneTag,
    href,
    inputTag,
    inputType,
    labelFor,
    labelTag,
    literalElementId,
    method,
    mkRegionId,
    name,
    paragraphTag,
    region,
    regionHtml,
    required,
    role,
    sectionTag,
    text,
    value,
    voidElement,
  )

homePage :: RouteRequest TwoPageRoute () -> IO (Page TwoPageRoute ())
homePage routeRequest =
  pure
    Page
      { pageTitle = "Home",
        pageRoute = HomeRoute,
        pageContext = requestContext routeRequest,
        pageBody =
          let emailId = literalElementId "subscription-email"
           in element
                sectionTag
                [dataAttribute "page" "home", className (ScopedCssClass (cssScope "home") "root")]
                [ element headingOneTag [] [text "Home"],
                  element paragraphTag [] [text "This page is fully server-rendered on direct load and reload."],
                  element paragraphTag [] [element anchorTag [href (routeHref SecondRoute), dataAttribute "page-link" "true"] [text "Go to the second page"]],
                  element paragraphTag [] [element anchorTag [href (routeHref LiveDataRoute), dataAttribute "page-link" "true"] [text "See live updates"]],
                  element
                    formTag
                    [ ariaLabel "Subscription",
                      dataFlag "harch-control",
                      dataAttribute "harch-action" "true",
                      formAction "/actions/subscribe",
                      method "post"
                    ]
                    [ element labelTag [labelFor emailId] [text "Email address"],
                      voidElement inputTag [elementId emailId, name "email", inputType "email", autocomplete "email", required],
                      element buttonTag [name "intent", value "subscribe", inputType "submit"] [text "Subscribe"]
                    ],
                  regionHtml (subscriptionResultRegion "status" "")
                ],
        pageBootstrapHooks = []
      }

subscriptionResultRegion :: Text -> Text -> Region
subscriptionResultRegion liveRole message =
  region
    (mkRegionId (literalElementId "subscription-result"))
    paragraphTag
    [role liveRole]
    [text message]
