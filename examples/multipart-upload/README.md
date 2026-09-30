# multipart-upload example

This example intentionally builds its page markup **manually** with the
programmatic `HarchWeb.Markup` element combinators. It is the repository's
verbose reference for the non-EDSL authoring path: every manual element
construction carries a comment saying so and pointing to the cleaner
`[harch| ... |]` quasiquoter style used by `web-api`'s
`src/WebApi/Pages/Showcase.hs` and by `examples/two-pages`.

Keep this example manual on purpose; new pages should use the quasiquoter.
