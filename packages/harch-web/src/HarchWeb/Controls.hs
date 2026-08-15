{-# LANGUAGE OverloadedStrings #-}

-- | Typed authoring controls for enhanced navigation and client actions.
--
-- The low-level markup attributes remain available for deliberately external
-- URLs and HTML interoperation. These controls keep framework-owned targets
-- tied to their route or action values instead of duplicating raw paths.
module HarchWeb.Controls
  ( ActionCapability (..),
    ActionFormAttributes (..),
    ActionIdempotency,
    ActionRecoveryCopy (..),
    FormMethod (..),
    NativeActionFallback (..),
    actionForm,
    actionIdempotency,
    actionIdempotencyKey,
    defaultActionFormAttributes,
    defaultActionRecoveryCopy,
    pageLink,
  )
where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action
  ( ActionCodec,
    actionMethod,
    actionMethodText,
    actionPath,
  )
import HarchWeb.Markup

-- | The recovery capability an action explicitly declares, each paired with
-- whatever evidence that capability requires. Attaching the evidence to the
-- constructor (rather than a same-shaped 'Maybe' field elsewhere) makes an
-- inconsistent pairing — a native fallback with no 'NativeFallback'
-- capability, or vice versa — impossible to construct, closing the four
-- render-time 'error' calls this module previously needed to reject it. The
-- default is an exclusive client handler: it retains work until that handler
-- settles it and does not make a native submission promise.
data ActionCapability
  = ExclusiveClientHandler
  | HandlerSafeRetry
  | ConditionalLeaveConfirmation
  | IdempotentMutationRetry ActionIdempotency
  | NativeFallback NativeActionFallback
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
    actionRetryCopy :: Text,
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
      actionRetryCopy = "Retry action",
      actionCancelCopy = "Cancel action"
    }

-- | The stable identity supplied with every attempt of an explicitly
-- idempotent client action. The server action handler receives the same value
-- in 'HarchWeb.ClientActionRequest' and is responsible for its durable
-- deduplication boundary. Opaque: 'actionIdempotency' is the only
-- constructor, so a key that could never distinguish attempts cannot exist.
newtype ActionIdempotency = ActionIdempotency
  { actionIdempotencyKey :: Text
  }
  deriving (Eq, Show)

-- | Reject an empty key: it could not distinguish one attempt from another,
-- defeating the deduplication the capability names.
actionIdempotency :: Text -> Maybe ActionIdempotency
actionIdempotency key
  | Text.null key = Nothing
  | otherwise = Just (ActionIdempotency key)

-- | The HTML @method@ attribute accepts only these two values. Restricting a
-- native fallback's method to this type, rather than the full
-- 'HarchWeb.Action.ActionMethod', makes a PUT\/PATCH\/DELETE fallback
-- unrepresentable instead of a render-time 'error'.
data FormMethod = FormGet | FormPost
  deriving (Eq, Show)

-- | An explicitly authored non-JavaScript submission endpoint. Applications
-- provide the endpoint and CSRF field from their server-side form workflow;
-- enhancement continues to use the action codec's typed endpoint.
data NativeActionFallback = NativeActionFallback
  { nativeActionFallbackPath :: Text,
    nativeActionFallbackMethod :: FormMethod,
    nativeActionFallbackCsrfToken :: Text
  }
  deriving (Eq, Show)

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
             dataAttribute "harch-action-path" (actionPath codec context target),
             dataAttribute "harch-action-method" (Text.toLower (actionMethodText (actionMethod codec target))),
             dataAttribute "harch-action-capabilities" (renderCapabilities (actionFormCapabilities attributes))
           ]
        <> maybe [] (pure . dataAttribute "harch-action-idempotency-key" . actionIdempotencyKey) idempotency
        <> [ dataAttribute "harch-action-ready-copy" (actionReadyCopy recoveryCopy),
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
             actionRetry attributes,
             actionCancel attributes
           ]
    )
  where
    recoveryCopy = actionFormRecoveryCopy attributes
    nativeFallback = nativeFallbackFor attributes
    idempotency = idempotencyFor attributes

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

actionRetry :: ActionFormAttributes -> Html
actionRetry attributes =
  element
    buttonTag
    [ dataFlag "harch-action-retry",
      inputType "button",
      hidden
    ]
    [text (actionRetryCopy (actionFormRecoveryCopy attributes))]

nativeFallbackFor :: ActionFormAttributes -> Maybe NativeActionFallback
nativeFallbackFor attributes =
  listToMaybe [fallback | NativeFallback fallback <- actionFormCapabilities attributes]

idempotencyFor :: ActionFormAttributes -> Maybe ActionIdempotency
idempotencyFor attributes =
  listToMaybe [idempotency | IdempotentMutationRetry idempotency <- actionFormCapabilities attributes]

formTarget :: (Eq target) => ActionCodec target context action -> context -> target -> Maybe NativeActionFallback -> Text
formTarget codec context target = maybe (actionPath codec context target) nativeActionFallbackPath

nativeMethod :: Maybe NativeActionFallback -> Text
nativeMethod = maybe "dialog" (formMethodText . nativeActionFallbackMethod)

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
    IdempotentMutationRetry _ -> "idempotent-mutation-retry"
    NativeFallback _ -> "native-fallback"

formMethodText :: FormMethod -> Text
formMethodText formMethodValue =
  case formMethodValue of
    FormGet -> "get"
    FormPost -> "post"
