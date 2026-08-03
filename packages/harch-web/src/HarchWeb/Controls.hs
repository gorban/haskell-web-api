{-# LANGUAGE OverloadedStrings #-}

-- | Typed authoring controls for enhanced navigation and client actions.
--
-- The low-level markup attributes remain available for deliberately external
-- URLs and HTML interoperation. These controls keep framework-owned targets
-- tied to their route or action values instead of duplicating raw paths.
module HarchWeb.Controls
  ( actionForm,
    pageLink,
  )
where

import Data.Text (Text)
import HarchWeb.Markup
import HarchWeb.Routing (RouteCodec, routeHref)

pageLink :: RouteCodec route context -> context -> route -> [Attribute] -> [Html] -> Html
pageLink codec context target attributes =
  element
    anchorTag
    (href (routeHref codec context target) : dataAttribute "page-link" "true" : attributes)

actionForm :: (action -> Text) -> action -> [Attribute] -> [Html] -> Html
actionForm renderActionTarget target attributes =
  element
    formTag
    ( attributes
        <> [ dataFlag "harch-control",
             dataAttribute "harch-action" "true",
             formAction (renderActionTarget target),
             method "post"
           ]
    )
