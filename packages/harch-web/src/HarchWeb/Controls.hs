{-# LANGUAGE OverloadedStrings #-}

-- | Typed authoring controls for enhanced navigation and client actions.
--
-- The low-level markup attributes remain available for deliberately external
-- URLs and HTML interoperation. These controls keep framework-owned targets
-- tied to their route or action values instead of duplicating raw paths.
module HarchWeb.Controls
  ( ActionCapability (..),
    ActionFormAttributes (..),
    ActionRecoveryCopy (..),
    actionForm,
    defaultActionFormAttributes,
    defaultActionRecoveryCopy,
    pageLink,
  )
where

import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action
  ( ActionCodec,
    ActionMethod (..),
    actionMethod,
    actionMethodText,
    actionPath,
  )
import HarchWeb.Markup

-- | The recovery capability an action explicitly declares. The default is an
-- exclusive client handler: it retains work until that handler settles it and
-- does not make a native submission promise.
data ActionCapability
  = ExclusiveClientHandler
  | HandlerSafeRetry
  | ConditionalLeaveConfirmation
  | IdempotentMutationRetry
  | NativeFallback
  deriving (Eq, Show)

-- | Localized copy used by the capture kernel's control-local status region.
-- Applications can replace these strings without reimplementing lifecycle
-- ownership in JavaScript.
data ActionRecoveryCopy = ActionRecoveryCopy
  { actionReadyCopy :: Text,
    actionPendingCopy :: Text,
    actionDelayedCopy :: Text,
    actionRecoverableCopy :: Text,
    actionCancelledCopy :: Text,
    actionCancelCopy :: Text
  }

defaultActionRecoveryCopy :: ActionRecoveryCopy
defaultActionRecoveryCopy =
  ActionRecoveryCopy
    { actionReadyCopy = "Ready.",
      actionPendingCopy = "Submitting…",
      actionDelayedCopy = "Still waiting for this action to be handled.",
      actionRecoverableCopy = "This action needs your attention.",
      actionCancelledCopy = "Action cancelled.",
      actionCancelCopy = "Cancel action"
    }

-- | The optional non-routing attributes of a client action form. Framework
-- owned attributes are deliberately absent, so the target, method, capture
-- markers, and recovery region cannot be overridden by callers.
data ActionFormAttributes = ActionFormAttributes
  { actionFormAriaLabel :: Maybe Text,
    actionFormCapabilities :: [ActionCapability],
    actionFormRecoveryCopy :: ActionRecoveryCopy
  }

defaultActionFormAttributes :: ActionFormAttributes
defaultActionFormAttributes =
  ActionFormAttributes
    { actionFormAriaLabel = Nothing,
      actionFormCapabilities = [ExclusiveClientHandler],
      actionFormRecoveryCopy = defaultActionRecoveryCopy
    }

pageLink :: (route -> Text) -> route -> [Html] -> Html
pageLink renderPageTarget target =
  element
    anchorTag
    [href (renderPageTarget target), dataAttribute "page-link" "true"]

actionForm :: (Eq target) => ActionCodec target context action -> context -> target -> ActionFormAttributes -> [Html] -> Html
actionForm codec context target attributes children =
  element
    formTag
    ( maybe [] (pure . ariaLabel) (actionFormAriaLabel attributes)
        <> [ dataFlag "harch-control",
             dataAttribute "harch-action" "true",
             dataAttribute "harch-action-method" (formMethod (actionMethod codec target)),
             dataAttribute "harch-action-capabilities" (renderCapabilities (actionFormCapabilities attributes)),
             dataAttribute "harch-action-ready-copy" (actionReadyCopy recoveryCopy),
             dataAttribute "harch-action-pending-copy" (actionPendingCopy recoveryCopy),
             dataAttribute "harch-action-delayed-copy" (actionDelayedCopy recoveryCopy),
             dataAttribute "harch-action-recoverable-copy" (actionRecoverableCopy recoveryCopy),
             dataAttribute "harch-action-cancelled-copy" (actionCancelledCopy recoveryCopy),
             formAction (actionPath codec context target),
             method (nativeMethod attributes (actionMethod codec target))
           ]
    )
    ( children
        <> [ actionStatus attributes,
             actionCancel attributes
           ]
    )
  where
    recoveryCopy = actionFormRecoveryCopy attributes

actionStatus :: ActionFormAttributes -> Html
actionStatus attributes =
  element
    paragraphTag
    [ dataFlag "harch-action-status",
      dataAttribute "harch-action-state" "ready",
      role "status",
      ariaLive "polite"
    ]
    [text (actionReadyCopy (actionFormRecoveryCopy attributes))]

actionCancel :: ActionFormAttributes -> Html
actionCancel attributes =
  element
    buttonTag
    [ dataFlag "harch-action-cancel",
      inputType "button",
      hidden
    ]
    [text (actionCancelCopy (actionFormRecoveryCopy attributes))]

nativeMethod :: ActionFormAttributes -> ActionMethod -> Text
nativeMethod attributes actionMethodValue
  | NativeFallback `elem` actionFormCapabilities attributes = formMethod actionMethodValue
  | otherwise = "dialog"

renderCapabilities :: [ActionCapability] -> Text
renderCapabilities = Text.intercalate "," . map renderCapability

renderCapability :: ActionCapability -> Text
renderCapability actionCapability =
  case actionCapability of
    ExclusiveClientHandler -> "exclusive-client-handler"
    HandlerSafeRetry -> "handler-safe-retry"
    ConditionalLeaveConfirmation -> "conditional-leave-confirmation"
    IdempotentMutationRetry -> "idempotent-mutation-retry"
    NativeFallback -> "native-fallback"

formMethod :: ActionMethod -> Text
formMethod actionMethodValue =
  case actionMethodValue of
    ActionGet -> "get"
    ActionPost -> "post"
    _ -> error ("HTML forms only support GET and POST client actions, not " <> Text.unpack (Text.toLower (actionMethodText actionMethodValue)))
