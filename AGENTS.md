# Design rules

- Always render complete SSR HTML for every supported route.
- Every framework control must have its event-capture path ready before it can be used; never lose an event or input while waiting for hydration or a later module.
- Framework mutations use client actions and region patches, not page POSTs or full-page reloads.
- Keep components composable typed functions with explicit props; prefer declarative markup, styling, and behavior EDSLs.
- Keep runtime minimal: a nonce-protected inline capture kernel is allowed; larger behavior modules load afterward.
- Prove immediate interaction and input preservation with real-browser E2E tests.
