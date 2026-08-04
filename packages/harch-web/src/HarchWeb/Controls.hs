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

pageLink :: (route -> Text) -> route -> [Attribute] -> [Html] -> Html
pageLink renderPageTarget target attributes =
  element
    anchorTag
    (href (renderPageTarget target) : dataAttribute "page-link" "true" : attributes)

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
