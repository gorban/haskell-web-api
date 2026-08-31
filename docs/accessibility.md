# Accessibility guide

Harch starts with semantic HTML and native controls, then adds typed
relationships and progressive behavior. Every supported page route renders a
complete server document. A client-action patch is an accessibility surface in
its own right: labels, hints, errors, focus targets, and status announcements
must remain correct after replacement.

## Enhanced navigation lifecycle

The `web-api` shell uses Harch's default `NavigationLifecycle` adapter. Its
localized skip link is the first focusable body control and targets the same
typed main landmark that receives focus after an enhanced navigation. The
main is focused with scroll suppression before an explicit scroll, and the
application stylesheet gives both the skip link and focused main visible,
unobscured treatment. The route-status node is empty on initial SSR and has
fixed polite, atomic semantics.

After a compatible click or Back/Forward fetch, the existing navigation
runtime validates the destination regions, lifecycle bindings, and final
same-origin response URL before changing the document. It then replaces the
regions, commits history, focuses main, and mutates the route status exactly
once with the destination document title. A newer request aborts and
supersedes an older one. Redirects use their final same-origin URL; failed,
malformed, cross-origin, or incompatible responses use native navigation and
do not announce success. Direct loads, activation before the deferred module
arrives, and scripts-disabled links remain browser-native.

The adapter is an ordinary declarative value, not a closed application UX.
Apps can select a typed element focus target and element-text announcement
source inside the replaced regions, or replace the existing
`NavigationRuntime` for a genuinely different algorithm. Harch keeps status
semantics fixed and does not accept arbitrary selectors or script callbacks.

## Authentication control inventory

This table is the review contract for the `web-api` reference application.
Autocomplete values are application policy expressed with Harch's open
`autocomplete :: Text` attribute; they are not a partial framework enum.

| Workflow and control | Autocomplete | Input mode | Accessible relationship | Scripts-disabled behavior |
| --- | --- | --- | --- | --- |
| Registration username | `username` | default text | visible label; linked field error | visible and keyboard-usable in the semantic client-only form |
| Registration email | `email` | email type | visible label; linked field error | same |
| Registration display name | `name` | default text | visible label | same |
| Registration password | `new-password` | password type | visible label; length hint; linked field error | same; password managers and paste remain available |
| Login email or username | `username` | default text | visible label; linked field error | visible and keyboard-usable |
| Login password | `current-password` | password type | visible label; linked field error | same; password managers and paste remain available |
| Login proof choice | n/a | native select | visible label; closed authenticator/recovery choices | both choices and both inputs remain present without script |
| Login authenticator code | `one-time-code` | `numeric` | visible label; paste instruction; linked field error | visible; no pattern or key handler blocks assistive input |
| Login recovery code | omitted intentionally | default text | separate visible label; saved-code instruction; linked field error | visible independently of the neighboring choice |
| Email verification token | `one-time-code` | default text | visible label; verification-link and clearing hint; linked field error | a query token is prefilled on the complete document |
| MFA enrollment confirmation | `one-time-code` | `numeric` | visible label; six-digit paste hint; linked field error | rendered after a server-confirmed enrollment start |
| Generated recovery codes | n/a | n/a | heading, save-once instruction, and semantic list of code values | complete server content when issued |

AHI-4 admission controls are not implemented. When that workflow lands, its
principal and TOTP controls must be added as separate inventory rows rather
than reusing account-MFA state or silently inheriting these semantics.

## Validation, focus, and secret handling

Independent syntax checks accumulate in declaration order before an effectful
workflow starts. One invalid field receives focus; multiple invalid fields
focus the labelled error summary, whose links use ordinary same-document
fragment navigation. Expected authentication outcomes remain ordinary domain
results on the existing application rail.

Identifiers and proof choice may be preserved in a patch. Passwords,
authenticator codes, recovery codes, verification tokens, enrollment secrets,
and generated recovery codes are never copied into rejection response models,
logs, observability, or diagnostic `Show` output. Browser tests use the real
clipboard and autofill-compatible replacement input events; this proves event
compatibility, not interoperability with every password-manager product.

The current account actions declare `exclusive-client-handler` and render
`method="dialog"`. With scripts disabled, their names, instructions, choices,
and delivered values remain available, but the application deliberately does
not claim a native network-submission fallback.

## Verification checklist

- Unit tests freeze exact autocomplete/input-mode markup and every
  label/hint/error relationship.
- Browser tests use keyboard traversal, real clipboard paste, delayed module
  loading, direct replacement-input events, single/multiple-error focus, and
  secret clearing after a patch.
- Narrow-viewport and 200% layout-zoom coverage verifies that the focused
  recovery control is scrolled into view.
- Scripts-disabled tests verify semantic content and the declared
  client-only fallback rather than assuming a page POST exists.
