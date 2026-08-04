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
    NativeActionFallback (..),
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

-- | An explicitly authored non-JavaScript submission endpoint. Applications
-- provide the endpoint and CSRF field from their server-side form workflow;
-- enhancement continues to use the action codec's typed endpoint.
data NativeActionFallback = NativeActionFallback
  { nativeActionFallbackPath :: Text,
    nativeActionFallbackMethod :: ActionMethod,
    nativeActionFallbackCsrfToken :: Text
  }

-- | The optional non-routing attributes of a client action form. Framework
-- owned attributes are deliberately absent, so the target, method, capture
-- markers, and recovery region cannot be overridden by callers.
data ActionFormAttributes = ActionFormAttributes
  { actionFormAriaLabel :: Maybe Text,
    actionFormCapabilities :: [ActionCapability],
    actionFormNativeFallback :: Maybe NativeActionFallback,
    actionFormRecoveryCopy :: ActionRecoveryCopy
  }

defaultActionFormAttributes :: ActionFormAttributes
defaultActionFormAttributes =
  ActionFormAttributes
    { actionFormAriaLabel = Nothing,
      actionFormCapabilities = [ExclusiveClientHandler],
      actionFormNativeFallback = Nothing,
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
             dataAttribute "harch-action-path" (actionPath codec context target),
             dataAttribute "harch-action-method" (formMethod (actionMethod codec target)),
             dataAttribute "harch-action-capabilities" (renderCapabilities (actionFormCapabilities attributes)),
             dataAttribute "harch-action-ready-copy" (actionReadyCopy recoveryCopy),
             dataAttribute "harch-action-pending-copy" (actionPendingCopy recoveryCopy),
             dataAttribute "harch-action-delayed-copy" (actionDelayedCopy recoveryCopy),
             dataAttribute "harch-action-recoverable-copy" (actionRecoverableCopy recoveryCopy),
             dataAttribute "harch-action-cancelled-copy" (actionCancelledCopy recoveryCopy),
             formAction (formTarget codec context target nativeFallback),
             method (nativeMethod nativeFallback)
           ]
    )
    ( children
        <> nativeCsrfField nativeFallback
        <> [ actionStatus attributes,
             actionCancel attributes
           ]
    )
  where
    recoveryCopy = actionFormRecoveryCopy attributes
    nativeFallback = requireNativeFallback attributes

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

requireNativeFallback :: ActionFormAttributes -> Maybe NativeActionFallback
requireNativeFallback attributes =
  case (NativeFallback `elem` actionFormCapabilities attributes, actionFormNativeFallback attributes) of
    (False, Nothing) -> Nothing
    (True, Just fallback) -> Just fallback
    (True, Nothing) -> error "NativeFallback requires an explicit NativeActionFallback endpoint and CSRF token"
    (False, Just _) -> error "NativeActionFallback requires the NativeFallback capability"

formTarget :: (Eq target) => ActionCodec target context action -> context -> target -> Maybe NativeActionFallback -> Text
formTarget codec context target = maybe (actionPath codec context target) nativeActionFallbackPath

nativeMethod :: Maybe NativeActionFallback -> Text
nativeMethod = maybe "dialog" (formMethod . nativeActionFallbackMethod)

nativeCsrfField :: Maybe NativeActionFallback -> [Html]
nativeCsrfField =
  maybe [] $ \fallback ->
    [ element
        inputTag
        [ inputType "hidden",
          name "_harch_csrf",
          value (nativeActionFallbackCsrfToken fallback)
        ]
        []
    ]

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
