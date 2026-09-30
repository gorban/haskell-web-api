# localization-example

This independent example shows an application-owned message enum and ICU
catalog layered on `HarchWeb.Localization`. It renders English, Spanish, and
Icelandic fields directly into SSR HTML; Icelandic's `11 hlutir` demonstrates
that the `plural` template uses CLDR categories rather than an English-only
singular branch. Its in-memory favorites adapter also returns a typed duplicate
failure which an API boundary turns into a localized public message.

Run it from the repository root:

```sh
cabal test localization-example-tests --test-show-details=direct
```
