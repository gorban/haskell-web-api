# custom-js

**Status:** Implemented guide

Show how a page can opt into a small custom browser behavior without forcing a front-end framework.

Current repo alignment:

- the repo ships a tiny framework navigation runtime,
- `examples/two-pages` includes a page-scoped deferred module at
  `/assets/live-data.js` that opens `EventSource` only after complete SSR HTML
  is usable,
- scripts-disabled browser coverage proves the server-rendered state remains
  meaningful without the optional enhancement.

Use a page marker and a same-origin deferred module rather than replacing the
whole document. The module should do nothing when its marker is absent:

- [live-data module](../two-pages/public/live-data.js)
- [SSR page](../two-pages/src/App/Pages/LiveData.hs)
- [browser proof](../two-pages/test/E2E/AppSpec.hs)
