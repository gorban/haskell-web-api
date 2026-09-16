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

## Admission CSRF guard outcomes

The admission form uses Harch's one action rail for both the signed reference
protection used by this executable and the durable synchronizer fixture used by
the browser suite.  Its order is deliberate: the framework first requires one
host-only CSRF cookie and one submitted value with constant-work equality; only
then does the selected protection check the token's current binding; only a
successful result reaches the admission TOTP workflow.  The action itself clears
the CSRF cookie when it establishes an admission session, so a form rendered
before that transition cannot be reused accidentally.

| Browser state at submission | Signed protection outcome | Durable synchronizer outcome | Guard/action result and browser evidence |
| --- | --- | --- | --- |
| Current page cookie and hidden value; current anonymous or admission binding | Signature, expiry, and binding verify. | Digest is present, unexpired, and bound to the current grant set. | The admission handler runs; valid TOTP establishes the session and navigates. Both variants use the same guarded enhanced action transport. |
| Missing cookie/value, duplicate value, or cookie/value mismatch | Not reached. | Not reached. | The framework transport rail rejects before either verifier or the admission handler; the browser remains on the form with its draft. |
| Expired, malformed, or binding-mismatched signed token | Rejected after transport equality. | N/A. | The handler is not called and the draft remains editable; signed CSRF expiry/signature/binding coverage is in the Harch CSRF suite. |
| Stored synchronizer digest revoked or expired after SSR | N/A. | Rejected after transport equality because the current durable row is absent or expired. | The handler is not called and the draft remains editable; `rejects a synchronizer token revoked after SSR` proves one backend verification and no navigation. |
| A second tab successfully establishes admission while the first tab retains an older form | The shared CSRF cookie is cleared by the successful action, so the older hidden value cannot pass the transport rail. | The same transport mismatch rejects before a third durable lookup. | No admission handler runs and the stale draft remains. The signed and synchronizer cross-tab scenarios prove this; the synchronizer fixture observes exactly two verifications (one successful page/action flow per tab), not three. |
| An expired or missing required admission grant and an expired or mismatched CSRF token occur together | The endpoint admission guard handles the expired grant before the action can reach CSRF validation. | The same endpoint guard ordering applies; no synchronizer lookup occurs. | The browser receives the ordinary admission challenge, not a replay capability. Account reauthentication cannot bypass the expired required admission grant, as proved by the `web-api` browser fixture. |

These rows describe the actual ordering, rather than treating a CSRF rejection
as a recoverable login state: it never grants replay.  A new guarded GET issues
fresh page security when the user needs to continue.  Admission and account
guards remain independent, so account login cannot bypass an expired required
admission grant; that recovery boundary is covered by the `web-api` browser
fixture.
