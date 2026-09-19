{-# LANGUAGE OverloadedStrings #-}

-- | App-owned placement and presentation for the AHI-6 reference controls.
-- Harch owns the dialog's typed semantic/capture contract; this module owns
-- the concrete language choices and the Help link's floating presentation.
-- The Help action remains ordinary typed native-link composition because that
-- already closes every state this example needs. Harch therefore does not
-- claim a general FAB API or a command-style floating button from this case.
module WebApi.Components.AppControls
  ( appControls,
    languageSelectionLinks,
    requiredAppAccessibleName,
  )
where

import Data.Text (Text)
import HarchWeb qualified
import WebApi.Localization (AppMessage (..), localizedMessage)
import WebApi.Route
  ( AppLocale (..),
    AppRequestContext (..),
    AppRoute (..),
    renderRouteUrl,
  )

appControls :: AppRequestContext -> AppRoute -> HarchWeb.Html
appControls context route =
  HarchWeb.fragment
    ( languagePicker context
        : [helpFab context | route /= HelpRoute]
    )

languagePicker :: AppRequestContext -> HarchWeb.Html
languagePicker context =
  HarchWeb.dialogControl
    (routeUrl context)
    HarchWeb.DialogControlProps
      { HarchWeb.dialogControlId = languageDialogId,
        HarchWeb.dialogHeadingId = languageHeadingId,
        HarchWeb.dialogHeading = HarchWeb.text (localizedMessage locale ChooseLanguage),
        HarchWeb.dialogInitialFocus = languageChoiceId locale,
        HarchWeb.dialogTrigger =
          HarchWeb.DialogLinkTrigger
            { HarchWeb.dialogTriggerRoute = LanguageRoute,
              HarchWeb.dialogTriggerName = requiredAppAccessibleName (localizedMessage locale Language),
              HarchWeb.dialogTriggerContent = HarchWeb.text (localizedMessage locale Language),
              HarchWeb.dialogTriggerClass = Just (controlClass "language-trigger")
            },
        HarchWeb.dialogBody = [dialogLanguageSelectionLinks context],
        HarchWeb.dialogCloseName = requiredAppAccessibleName (localizedMessage locale CloseLanguagePicker),
        HarchWeb.dialogClass = Just (controlClass "language-dialog"),
        HarchWeb.dialogCloseClass = Just (controlClass "language-close")
      }
  where
    locale = requestLocale context

-- | The complete route and enhanced dialog deliberately share this exact
-- typed link renderer, so their locale URLs and current-language semantics
-- cannot drift.
languageSelectionLinks :: AppRequestContext -> HarchWeb.Html
languageSelectionLinks context =
  HarchWeb.element
    HarchWeb.listTag
    [HarchWeb.className (controlClass "language-list")]
    [languageChoice Nothing context English EnglishLanguage, languageChoice Nothing context Spanish SpanishLanguage]

dialogLanguageSelectionLinks :: AppRequestContext -> HarchWeb.Html
dialogLanguageSelectionLinks context =
  HarchWeb.element
    HarchWeb.listTag
    [HarchWeb.className (controlClass "language-list")]
    [ languageChoice (Just (languageChoiceId English)) context English EnglishLanguage,
      languageChoice (Just (languageChoiceId Spanish)) context Spanish SpanishLanguage
    ]

languageChoice :: Maybe HarchWeb.ElementId -> AppRequestContext -> AppLocale -> AppMessage -> HarchWeb.Html
languageChoice maybeChoiceId context choice messageKey =
  HarchWeb.element
    HarchWeb.listItemTag
    []
    [ HarchWeb.element
        HarchWeb.anchorTag
        ( maybe [] (pure . HarchWeb.elementId) maybeChoiceId
            <> [ HarchWeb.href (routeUrl (explicitLocale choice context) LanguageRoute),
                 HarchWeb.dataAttribute "page-link" "true"
               ]
            <> [HarchWeb.ariaCurrentPage | requestLocale context == choice]
        )
        [HarchWeb.text (localizedMessage (requestLocale context) messageKey)]
    ]

helpFab :: AppRequestContext -> HarchWeb.Html
helpFab context =
  HarchWeb.element
    HarchWeb.anchorTag
    [ HarchWeb.href (routeUrl context HelpRoute),
      HarchWeb.ariaLabel label,
      HarchWeb.dataAttribute "page-link" "true",
      HarchWeb.dataFlag "help-fab",
      HarchWeb.className (controlClass "help-fab")
    ]
    [ HarchWeb.element
        HarchWeb.spanTag
        [HarchWeb.ariaHidden True, HarchWeb.className (controlClass "help-icon")]
        [HarchWeb.text "?"],
      HarchWeb.element
        HarchWeb.spanTag
        [HarchWeb.className (controlClass "help-label")]
        [HarchWeb.text label]
    ]
  where
    label = HarchWeb.accessibleNameText (requiredAppAccessibleName (localizedMessage (requestLocale context) HelpAndSupport))

explicitLocale :: AppLocale -> AppRequestContext -> AppRequestContext
explicitLocale locale context =
  context
    { requestLocale = locale,
      requestLocaleIsExplicit = True
    }

routeUrl :: AppRequestContext -> AppRoute -> HarchWeb.SafeUrl
routeUrl context route =
  renderRouteUrl (HarchWeb.RouteRequest route context)

-- | Require an application-owned catalog value to remain a usable control
-- name. Exporting this invariant helper lets its failure rail be tested
-- directly instead of forcing diagnostics at always-valid call sites.
requiredAppAccessibleName :: Text -> HarchWeb.AccessibleName
requiredAppAccessibleName nameText =
  HarchWeb.requiredAccessibleNameOrDie
    "WebApi.Components.AppControls: empty accessible-name catalog entry"
    (HarchWeb.mkAccessibleName nameText)

languageDialogId :: HarchWeb.ElementId
languageDialogId = HarchWeb.literalElementId "language-dialog"

languageHeadingId :: HarchWeb.ElementId
languageHeadingId = HarchWeb.literalElementId "language-dialog-heading"

languageChoiceId :: AppLocale -> HarchWeb.ElementId
languageChoiceId locale =
  HarchWeb.literalElementId
    ( case locale of
        English -> "language-choice-en"
        Spanish -> "language-choice-es"
    )

controlClass :: Text -> HarchWeb.CssClass
controlClass = HarchWeb.ScopedCssClass (HarchWeb.cssScope "app-controls")
