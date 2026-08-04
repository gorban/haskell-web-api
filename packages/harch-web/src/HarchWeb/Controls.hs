{-# LANGUAGE OverloadedStrings #-}

-- | Typed authoring controls for enhanced navigation and client actions.
--
-- The low-level markup attributes remain available for deliberately external
-- URLs and HTML interoperation. These controls keep framework-owned targets
-- tied to their route or action values instead of duplicating raw paths.
module HarchWeb.Controls
  ( ActionFormAttributes (..),
    actionForm,
    defaultActionFormAttributes,
    pageLink,
  )
where

import Data.Text (Text)
import HarchWeb.Markup

-- | The optional non-routing attributes of a client action form. Framework
-- owned attributes are deliberately absent, so the target, method, and
-- capture markers cannot be overridden by callers.
newtype ActionFormAttributes = ActionFormAttributes
  { actionFormAriaLabel :: Maybe Text
  }

defaultActionFormAttributes :: ActionFormAttributes
defaultActionFormAttributes = ActionFormAttributes {actionFormAriaLabel = Nothing}

pageLink :: (route -> Text) -> route -> [Html] -> Html
pageLink renderPageTarget target =
  element
    anchorTag
    [href (renderPageTarget target), dataAttribute "page-link" "true"]

actionForm :: (action -> Text) -> action -> ActionFormAttributes -> [Html] -> Html
actionForm renderActionTarget target attributes =
  element
    formTag
    ( maybe [] (pure . ariaLabel) (actionFormAriaLabel attributes)
        <> [ dataFlag "harch-control",
             dataAttribute "harch-action" "true",
             formAction (renderActionTarget target),
             method "post"
           ]
    )
