{-# LANGUAGE OverloadedStrings #-}

module App.Pages.Home (homePage) where

import App.Routes (TwoPageRoute (..), routeHref)
import HarchWeb
  ( CssClass (..),
    Page (..),
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
    formTag,
    formAction,
    headingOneTag,
    href,
    inputTag,
    inputType,
    labelFor,
    labelTag,
    method,
    mkElementId,
    name,
    paragraphTag,
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
          case (mkElementId "subscription-email", mkElementId "subscription-result") of
            (Just emailId, Just resultId) ->
              element
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
                  element paragraphTag [elementId resultId, dataAttribute "harch-region" "true", role "status"] []
                ]
            _ -> text "",
        pageBootstrapHooks = []
      }
