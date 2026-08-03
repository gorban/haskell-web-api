# multilanguage-routing

**Status:** Implemented guide

The full `web-api` reference application models locale as `AppLocale = English | Spanish` in its
typed request context and route codec.

Current behavior:

- an unprefixed path such as `/spaces` uses the default English locale,
- `/en/spaces` records an explicit English locale,
- `/es/spaces` selects Spanish,
- route rendering retains explicit English and always prefixes Spanish,
- a trusted reverse-proxy path prefix composes ahead of the locale prefix, and
- localized account actions, validation patches, navigation, email links, and API data reuse the
  request locale.

The implementation is in [WebApi.Route](../../packages/web-api/src/WebApi/Route.hs). The
[real-browser suite](../../packages/web-api/test/E2E/WebApiSpec.hs) proves scripts-disabled Spanish
root redirects, localized SSR navigation, registration patches, profile content, and logout behavior.
Unit coverage exercises parsing/rendering for implicit English, explicit English, Spanish, and proxied
paths.

For an app with different locales, change the locale ADT, prefix codec, translations, and defaulting
policy together. Keep locale in the typed request context so page links, forms, client actions, and
server-rendered responses cannot silently disagree.
