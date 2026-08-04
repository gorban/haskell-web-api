# Changelog

## 0.1.2.0

1. Added declarative `ActionCodec` endpoint definitions shared by typed client-action forms and server
   decoding. The protocol now distinguishes unknown paths (`404`), method mismatches (`405` with `Allow`),
   malformed matched fields (`400`), and application validation responses (`422`).
2. Added a new `HarchWeb.Site` wrapper that lets composition roots assemble typed route definitions, shared page-shell configuration, and route-derived navigation without directly building the lower-level `Application` record.
3. Switched the first-party `web-api` composition root onto that `HarchWeb.Site` path while preserving current page/API behavior and keeping the route-aware shell output stable under the existing test suite.

## 0.1.1.0

1. Began extracting the future SSR-first web framework into the separate `harch-web` package so shared web architecture can evolve independently from the example app.
2. Defined the first `HarchWeb` facade boundary around application description, route matching, page rendering, not-found handling, shared page shell attachment, and the final server startup seam.
3. Repositioned `web-api` as the application composition root over that facade, keeping app-specific routes, pages, config, and startup wiring outside the framework package.
4. Added initial unit/integration coverage for the stub facade path in `web-api` and introduced a first dedicated `harch-web` test suite for route matching and server-boundary behavior.
5. Documented the new package layout in the root README and expanded local debugger/setup guidance for running package test suites and spec preprocessors from editor tooling.
6. Cleaned related repository metadata while landing the new framework boundary, including `core` package terminology updates, `hspec-expectations-match` extra-source-file cleanup, and repository-wide changelog heading normalization without inline dates.
7. Established the intended PR scope for `HarchWeb` as tracked in `TASKS.md`: pure SSR route/page/layout/config seams first, then a thin server adapter, then effect-backed data access and a tiny progressive-enhancement navigation runtime.

## 0.1.0.1

1. Dropped Windows native support. Windows native was last fully supported in
   [0.1.0.0](https://github.com/gorban/haskell-web-api/tree/v0.1.0.0), but was later removed because, even
   after an immense effort to produce JavaScript output from Haskell (e.g. from GHCJS or the newer GHC with
   JavaScript backend), it just is not community-supported (see WIP PRs for unresolved issue
   [ghcjs#834](https://github.com/ghcjs/ghcjs/issues/830), for
   [ghcjs#1](https://github.com/ghcjs/ghcjs/issues/833) and its specific
   [ghc#1](https://github.com/gorban/ghc/pull/1) version it needs). If you want to run this on Windows, you
   can use WSL2 or Docker (configured for Linux containers).
2. Simplified generate-code-coverage-report to just be an .sh script (no more PowerShell polyglot).
3. Split off this setup guide into its own file, and added a link to it from the main README.
4. Upgraded GHC to 9.14.1 and Base to 4.22.0.0.

## 0.1.0.0

1. Initial release.
2. Completely lacks a real web API, but has a putStrLn in the main application executable as a placeholder.
   The main focus of this release is to set up the project structure, build system, testing framework, and
   CI/CD pipeline with code coverage reporting. The actual web API implementation will come in future
   releases.
