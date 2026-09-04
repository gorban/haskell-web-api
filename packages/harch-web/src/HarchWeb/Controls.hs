{-# LANGUAGE OverloadedStrings #-}

-- | Typed authoring controls for enhanced navigation and client actions.
--
-- The low-level markup attributes remain available for deliberately external
-- URLs and HTML interoperation. These controls keep framework-owned targets
-- tied to their route or action values instead of duplicating raw paths.
module HarchWeb.Controls
  ( ActionCapability (..),
    ActionFormAttributes (..),
    ActionFormRendering,
    ActionIdempotency,
    ActionRecoveryCopy (..),
    AccessibleName,
    AccessibleFieldProps (..),
    DescribedContent (..),
    DialogControlProps (..),
    DialogLinkTrigger (..),
    ErrorSummary (..),
    FieldControlAttributes (..),
    FieldErrorLink (..),
    FieldValidity (..),
    FormMethod (..),
    NativeActionFallback (..),
    RetainedActionLifetime,
    actionForm,
    actionIdempotency,
    actionIdempotencyKey,
    defaultActionFormAttributes,
    defaultActionRecoveryCopy,
    accessibleField,
    accessibleNameText,
    dialogControl,
    errorSummary,
    mkAccessibleName,
    mkRetainedActionLifetime,
    requiredAccessibleNameOrDie,
    pageLink,
    renderActionForm,
    retainedActionLifetimeMilliseconds,
    staticActionForm,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import HarchWeb.Action
  ( ActionCodec,
    ActionMethod,
    actionMethod,
    actionMethodText,
    actionPath,
    staticActionPath,
  )
import HarchWeb.Csrf (CsrfToken, csrfTokenText)
import HarchWeb.Markup
import HarchWeb.StaticAssets (CssClass)

-- | A non-empty name that a framework control exposes to assistive
-- technology. Whitespace-only names are rejected because they are empty for
-- users even though their source text is not.
newtype AccessibleName = AccessibleName Text
  deriving (Eq, Show)

accessibleNameText :: AccessibleName -> Text
accessibleNameText (AccessibleName nameText) = nameText

mkAccessibleName :: Text -> Maybe AccessibleName
mkAccessibleName nameText
  | Text.null (Text.strip nameText) = Nothing
  | otherwise = Just (AccessibleName nameText)

-- | Require text authored by a closed route or localization table to remain
-- a usable accessible name. Dynamic input must use 'mkAccessibleName' and
-- handle rejection instead.
requiredAccessibleNameOrDie :: Text -> Maybe AccessibleName -> AccessibleName
requiredAccessibleNameOrDie context =
  fromMaybe (error ("HarchWeb.Controls: " <> Text.unpack context))

-- | A native link trigger whose typed route remains usable as a complete SSR
-- destination before the deferred dialog runtime loads and when scripts are
-- disabled. The route stays abstract until 'dialogControl' receives the
-- application's renderer, matching 'pageLink' rather than duplicating a
-- path. The explicit name cannot be displaced by decorative trigger content.
data DialogLinkTrigger route = DialogLinkTrigger
  { dialogTriggerRoute :: route,
    dialogTriggerName :: AccessibleName,
    dialogTriggerContent :: Html,
    dialogTriggerClass :: Maybe CssClass
  }
  deriving (Eq, Show)

-- | Cohesive inputs for one always-dismissible native dialog.
--
-- Decision (AHI-6, 2026-08-31): extend the existing typed control and capture
-- boundaries with a link-fallback dialog adapter. Harch owns naming,
-- modality hooks, early-activation capture, dismissal, and focus restoration;
-- applications own the heading/body and optional typed styling classes. The
-- first shipped surface deliberately omits a speculative button trigger and
-- required-decision mode. A replaceable deferred runtime remains an ordinary
-- shell runtime asset rather than application JavaScript embedded in props.
data DialogControlProps route = DialogControlProps
  { dialogControlId :: ElementId,
    dialogHeadingId :: ElementId,
    dialogHeading :: Html,
    dialogInitialFocus :: ElementId,
    dialogTrigger :: DialogLinkTrigger route,
    dialogBody :: [Html],
    dialogCloseName :: AccessibleName,
    dialogClass :: Maybe CssClass,
    dialogCloseClass :: Maybe CssClass
  }
  deriving (Eq, Show)

-- | Render the typed fallback link and native dialog together. The renderer
-- is application-supplied, so Harch's dialog contract does not choose a route
-- algebra or URL policy. Framework-owned attributes are not caller-
-- overridable, so the trigger, target, accessible name, labelled-by
-- relationship, initial focus, and close control cannot drift apart.
dialogControl :: (route -> SafeUrl) -> DialogControlProps route -> Html
dialogControl renderRouteTarget props =
  element
    divTag
    [dataFlag "harch-dialog-control"]
    [ renderDialogTrigger renderRouteTarget props,
      element
        dialogTag
        ( [ elementId (dialogControlId props),
            ariaLabelledBy (dialogHeadingId props),
            dataFlag "harch-dialog-root",
            dataAttribute "harch-dialog-initial-focus-id" (elementIdText (dialogInitialFocus props))
          ]
            <> optionalClass (dialogClass props)
        )
        ( [ element headingTwoTag [elementId (dialogHeadingId props)] [dialogHeading props]
          ]
            <> dialogBody props
            <> [renderDialogClose props]
        )
    ]

renderDialogTrigger :: (route -> SafeUrl) -> DialogControlProps route -> Html
renderDialogTrigger renderRouteTarget props =
  element
    anchorTag
    ( [ href fallback,
        ariaLabel (accessibleNameText (dialogTriggerName trigger)),
        ariaHasPopupDialog,
        ariaControls (dialogControlId props),
        ariaExpanded False,
        dataAttribute "harch-dialog-trigger" "true",
        dataAttribute "harch-dialog-id" (elementIdText (dialogControlId props)),
        dataAttribute "harch-dialog-fallback" (safeUrlText fallback)
      ]
        <> optionalClass (dialogTriggerClass trigger)
    )
    [dialogTriggerContent trigger]
  where
    trigger = dialogTrigger props
    fallback = renderRouteTarget (dialogTriggerRoute trigger)

renderDialogClose :: DialogControlProps route -> Html
renderDialogClose props =
  element
    buttonTag
    ( [ inputType "button",
        ariaLabel (accessibleNameText (dialogCloseName props)),
        dataFlag "harch-dialog-close"
      ]
        <> optionalClass (dialogCloseClass props)
    )
    [text (accessibleNameText (dialogCloseName props))]

optionalClass :: Maybe CssClass -> [Attribute]
optionalClass = maybe [] (pure . className)

-- | Stable described content used as either a hint or a field-local error.
-- Its ID and body travel together so a caller cannot render one while
-- pointing the control at another.
data DescribedContent = DescribedContent
  { describedContentId :: ElementId,
    describedContentBody :: Html
  }
  deriving (Eq, Show)

-- | The only two validity states a rendered field can have.  An invalid
-- field necessarily owns concrete error content.
data FieldValidity
  = FieldValid
  | FieldInvalid DescribedContent
  deriving (Eq, Show)

-- | Structural inputs shared by native input, select, and future controls.
--
-- Decision (AHI-7, 2026-08-31): the registration experiment and the distinct
-- login form both reproduced the same inseparable label/ID/description
-- relationship.  Extend the existing control boundary with this higher-order
-- component; do not add a form-builder DSL or leave applications to transpose
-- raw ARIA IDs.  Validation remains application-owned.
data AccessibleFieldProps = AccessibleFieldProps
  { accessibleFieldControlId :: ElementId,
    accessibleFieldLabel :: Html,
    accessibleFieldHint :: Maybe DescribedContent,
    accessibleFieldValidity :: FieldValidity
  }
  deriving (Eq, Show)

-- | Attributes derived by 'accessibleField'.  The renderer must attach both
-- fields to its one native control; separating the ID from relationships keeps
-- attribute ordering deterministic without exposing unchecked attributes.
data FieldControlAttributes = FieldControlAttributes
  { fieldControlIdAttribute :: Attribute,
    fieldControlRelationshipAttributes :: [Attribute]
  }
  deriving (Eq, Show)

-- | One error-summary link.  Non-emptiness belongs to 'ErrorSummary', not to
-- each link.
data FieldErrorLink = FieldErrorLink
  { fieldErrorControlId :: ElementId,
    fieldErrorBody :: Html
  }
  deriving (Eq, Show)

-- | A programmatically focusable summary with at least one target.
data ErrorSummary = ErrorSummary
  { errorSummaryId :: ElementId,
    errorSummaryHeading :: Html,
    errorSummaryItems :: NonEmpty FieldErrorLink
  }
  deriving (Eq, Show)

-- | Render a visible label, exactly one caller-supplied native control, and
-- the present hint/error nodes.  Description references are derived in hint,
-- error order and invalid-only ARIA attributes cannot leak into valid state.
accessibleField :: AccessibleFieldProps -> (FieldControlAttributes -> Html) -> Html
accessibleField props renderControl =
  element
    divTag
    [dataFlag "accessible-field"]
    ( [ element labelTag [labelFor controlId] [accessibleFieldLabel props],
        renderControl
          FieldControlAttributes
            { fieldControlIdAttribute = elementId controlId,
              fieldControlRelationshipAttributes = relationshipAttributes
            }
      ]
        <> maybe [] (pure . renderDescribedContent "field-hint") (accessibleFieldHint props)
        <> case accessibleFieldValidity props of
          FieldValid -> []
          FieldInvalid fieldError -> [renderDescribedContent "field-error" fieldError]
    )
  where
    controlId = accessibleFieldControlId props
    describedIds =
      maybe [] (pure . describedContentId) (accessibleFieldHint props)
        <> case accessibleFieldValidity props of
          FieldValid -> []
          FieldInvalid fieldError -> [describedContentId fieldError]
    relationshipAttributes =
      maybe [] (pure . ariaDescribedBy) (NonEmpty.nonEmpty describedIds)
        <> case accessibleFieldValidity props of
          FieldValid -> []
          FieldInvalid fieldError -> [ariaInvalid True, ariaErrorMessage (describedContentId fieldError)]

renderDescribedContent :: DataAttributeSuffix -> DescribedContent -> Html
renderDescribedContent kind content =
  element
    paragraphTag
    [elementId (describedContentId content), dataFlag kind]
    [describedContentBody content]

-- | Render ordinary labelled content rather than another live region.  The
-- action response focuses this node only for multiple errors, which announces
-- the new summary without duplicating a @role=alert@ announcement; each link
-- remains keyboard-usable and targets its typed field ID.
errorSummary :: ErrorSummary -> Html
errorSummary summary =
  element
    sectionTag
    [ elementId (errorSummaryId summary),
      tabIndex (-1),
      dataFlag "error-summary"
    ]
    [ element headingTwoTag [] [errorSummaryHeading summary],
      element
        listTag
        []
        [ element
            listItemTag
            []
            [element anchorTag [fragmentHref (fieldErrorControlId item)] [fieldErrorBody item]]
        | item <- NonEmpty.toList (errorSummaryItems summary)
        ]
    ]

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

-- | An explicitly authored non-JavaScript submission endpoint. Its CSRF field
-- is the opaque token from the same pre-render 'PageSecurity' that sets the
-- host cookie; enhancement continues to use the action codec's typed endpoint.
data NativeActionFallback = NativeActionFallback
  { nativeActionFallbackPath :: Text,
    nativeActionFallbackMethod :: FormMethod,
    nativeActionFallbackCsrfToken :: CsrfToken
  }
  deriving (Eq, Show)

-- | A positive tab-memory lifetime for the one captured action that may wait
-- for deliberate reauthentication.  The value is rendered only as framework
-- control metadata; the captured fields themselves never leave the kernel.
newtype RetainedActionLifetime = RetainedActionLifetime Int
  deriving (Eq, Show)

mkRetainedActionLifetime :: Int -> Maybe RetainedActionLifetime
mkRetainedActionLifetime milliseconds
  | milliseconds > 0 = Just (RetainedActionLifetime milliseconds)
  | otherwise = Nothing

retainedActionLifetimeMilliseconds :: RetainedActionLifetime -> Int
retainedActionLifetimeMilliseconds (RetainedActionLifetime milliseconds) = milliseconds

defaultRetainedActionLifetime :: RetainedActionLifetime
defaultRetainedActionLifetime = RetainedActionLifetime 600000

-- | The optional non-routing attributes of a client action form. Framework
-- owned attributes are deliberately absent, so the target, method, capture
-- markers, and recovery region cannot be overridden by callers.
data ActionFormAttributes = ActionFormAttributes
  { actionFormAriaLabel :: Maybe Text,
    actionFormCapabilities :: [ActionCapability],
    actionFormRetainedActionLifetime :: RetainedActionLifetime,
    actionFormRecoveryCopy :: ActionRecoveryCopy
  }

defaultActionFormAttributes :: ActionFormAttributes
defaultActionFormAttributes =
  ActionFormAttributes
    { actionFormAriaLabel = Nothing,
      actionFormCapabilities = [ExclusiveClientHandler],
      actionFormRetainedActionLifetime = defaultRetainedActionLifetime,
      actionFormRecoveryCopy = defaultActionRecoveryCopy
    }

pageLink :: (route -> SafeUrl) -> route -> [Html] -> Html
pageLink renderPageTarget target =
  element
    anchorTag
    [href (renderPageTarget target), dataAttribute "page-link" "true"]

actionForm :: (Eq target) => ActionCodec target context authorization action -> context -> target -> ActionFormAttributes -> [Html] -> ActionFormRendering
actionForm codec context target attributes children =
  case (actionPath codec context target, actionMethod codec target) of
    (Just targetPath, Just targetMethod) -> CapturingActionForm (renderCapturingActionForm targetPath targetMethod attributes children)
    _ -> UndeclaredActionForm (renderUndeclaredActionForm children)

-- | Render a form only for an action whose declaration proves that its path
-- does not depend on request context. Dynamic action declarations remain
-- available through 'actionForm'.
staticActionForm :: (Eq target) => ActionCodec target context authorization action -> target -> ActionFormAttributes -> [Html] -> ActionFormRendering
staticActionForm codec target attributes children =
  case (staticActionPath codec target, actionMethod codec target) of
    (Just targetPath, Just targetMethod) -> CapturingActionForm (renderCapturingActionForm targetPath targetMethod attributes children)
    _ -> UndeclaredActionForm (renderUndeclaredActionForm children)

-- | A rendered action control is either capture-ready, or an explicit
-- configuration result. Keeping this separate from 'Html' prevents a caller
-- from accidentally treating an undeclared action target as a usable form.
-- 'renderActionForm' preserves the supplied content and displays an
-- accessible configuration failure; it never emits capture attributes for an
-- undeclared target.
data ActionFormRendering
  = CapturingActionForm Html
  | UndeclaredActionForm Html

renderActionForm :: ActionFormRendering -> Html
renderActionForm actionFormRendering =
  case actionFormRendering of
    CapturingActionForm renderedForm -> renderedForm
    UndeclaredActionForm renderedFallback -> renderedFallback

renderCapturingActionForm :: Text -> ActionMethod -> ActionFormAttributes -> [Html] -> Html
renderCapturingActionForm targetPath targetMethod attributes children =
  element
    formTag
    ( maybe [] (pure . ariaLabel) (actionFormAriaLabel attributes)
        <> [ dataFlag "harch-control",
             dataAttribute "harch-action" "true",
             dataAttribute "harch-action-path" targetPath,
             dataAttribute "harch-action-method" (Text.toLower (actionMethodText targetMethod)),
             dataAttribute "harch-action-capabilities" (renderCapabilities (actionFormCapabilities attributes)),
             dataAttribute "harch-action-retention-ms" (Text.pack (show (retainedActionLifetimeMilliseconds (actionFormRetainedActionLifetime attributes))))
           ]
        <> maybe [] (pure . dataAttribute "harch-action-idempotency-key" . actionIdempotencyKey) idempotency
        <> [ dataAttribute "harch-action-ready-copy" (actionReadyCopy recoveryCopy),
             dataAttribute "harch-action-pending-copy" (actionPendingCopy recoveryCopy),
             dataAttribute "harch-action-delayed-copy" (actionDelayedCopy recoveryCopy),
             dataAttribute "harch-action-recoverable-copy" (actionRecoverableCopy recoveryCopy),
             dataAttribute "harch-action-cancelled-copy" (actionCancelledCopy recoveryCopy),
             formAction (formTarget targetPath nativeFallback),
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

renderUndeclaredActionForm :: [Html] -> Html
renderUndeclaredActionForm children =
  fragment
    [ element
        paragraphTag
        [role "alert", dataFlag "harch-action-configuration-error"]
        [text "This action is unavailable because its target is not declared."],
      fragment children
    ]

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

formTarget :: Text -> Maybe NativeActionFallback -> Text
formTarget targetPath = maybe targetPath nativeActionFallbackPath

nativeMethod :: Maybe NativeActionFallback -> Text
nativeMethod = maybe "dialog" (formMethodText . nativeActionFallbackMethod)

nativeCsrfField :: Maybe NativeActionFallback -> [Html]
nativeCsrfField =
  maybe [] $ \fallback ->
    [ voidElement
        inputTag
        [ inputType "hidden",
          name "_harch_csrf",
          value (csrfTokenText (nativeActionFallbackCsrfToken fallback))
        ]
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
