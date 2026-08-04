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
import Data.Text qualified as Text
import HarchWeb.Action
  ( ActionCodec,
    ActionMethod (..),
    actionMethod,
    actionPath,
  )
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

actionForm :: (Eq target) => ActionCodec target context action -> context -> target -> ActionFormAttributes -> [Html] -> Html
actionForm codec context target attributes =
  element
    formTag
    ( maybe [] (pure . ariaLabel) (actionFormAriaLabel attributes)
        <> [ dataFlag "harch-control",
             dataAttribute "harch-action" "true",
             formAction (actionPath codec context target),
             method (formMethod (actionMethod codec target))
           ]
    )

formMethod :: ActionMethod -> Text
formMethod actionMethodValue =
  case actionMethodValue of
    ActionGet -> "get"
    ActionPost -> "post"
    _ -> error ("HTML forms only support GET and POST client actions, not " <> Text.unpack (Text.toLower (showActionMethod actionMethodValue)))

showActionMethod :: ActionMethod -> Text
showActionMethod actionMethodValue =
  case actionMethodValue of
    ActionGet -> "GET"
    ActionPost -> "POST"
    ActionPut -> "PUT"
    ActionPatch -> "PATCH"
    ActionDelete -> "DELETE"
