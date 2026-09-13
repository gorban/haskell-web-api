# composed-domains

**Status:** Executable reference application

This application mounts independent Catalog and Orders modules below a
locale-aware root. It is a composition reference: the framework owns typed
module mounting, routes, complete SSR documents, action transport, and declared
enhancement lifecycle; the application owns its domain capabilities, locale
policy, styling, and admission policy.

Run it from the repository root:

```sh
cabal run composed-domains
```

Then open <http://127.0.0.1:8080/>. It uses in-process development capabilities
and a fresh process-local CSRF signing key, so it is a runnable composition
example rather than a production admission deployment.

The reference includes:

- localized SSR routes with typed mounted Catalog and Orders modules;
- an accessible language-picker: a normal link to a complete language page
  without scripts, enhanced into a native dialog only when its declared runtime
  is available;
- a Help/support floating link that remains ordinary navigation, has a complete
  destination, and is absent at that destination;
- an application-owned admission flow that is deliberately separate from the
  `web-api` account/MFA model.

Run the Unit and real-browser proof with:

```sh
cabal test composed-domains-tests --test-show-details=direct
```

The browser suite covers direct and enhanced navigation, scripts-disabled
fallback, the language dialog's keyboard/focus behavior, and narrow/mobile Help
link layout. The Help link is a reference control, not a general Harch FAB API;
applications remain responsible for choosing when a floating control is
appropriate and for its accessible name, target size, focus treatment, and
non-obstructive layout.
