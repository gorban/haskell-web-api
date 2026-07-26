# Design rules

- Always render complete SSR HTML for every supported route.
- Every framework control must have its event-capture path ready before it can be used; never lose an event or input while waiting for hydration or a later module.
- Framework mutations use client actions and region patches, not page POSTs or full-page reloads.
- Keep components composable typed functions with explicit props; prefer declarative markup, styling, and behavior EDSLs.
- Keep runtime minimal: a nonce-protected inline capture kernel is allowed; larger behavior modules load afterward.
- Prove immediate interaction and input preservation with real-browser E2E tests.

# Haskell code style

- Model expected states and failures with precise ADTs and newtypes; prefer total functions and make invalid states unrepresentable where practical.
- Keep parsing and validation pure. Use `Either` for failure, and reserve `Maybe` for cases where absence is a valid result rather than an explanation being discarded.
- Use `ExceptT DomainError IO` for multi-step effectful workflows that stop at the first domain failure. Interpret the error once at the public or transport boundary instead of manually forwarding `Left`, `Right`, `Nothing`, or `Just` through nested cases.
- Keep expected business outcomes as ordinary result constructors. Do not turn validation failures, authentication rejection, not-found results, or other expected alternatives into exceptions or infrastructure errors.
- Catch exceptions only at the `IO` boundary that can explain or recover from them. Public responses must remain safe; detailed causes belong in private structured logs and observability.
- Introduce a custom Monad newtype only when it names a stable capability stack, such as shared services plus typed request failure. Prefer standard transformers for a single local workflow and avoid polymorphic Monad constraints when no alternate interpreter is useful.
- Use records for cohesive dependencies or inputs when a long positional argument list obscures meaning. Keep framework components as typed functions with explicit props rather than ambient application state.
- Keep observability failure codes stable and low-cardinality. Detailed causes stay in private logs; HTTP server spans leave expected 4xx outcomes unset and mark 5xx responses as errors.
- Treat complexity metrics as review signals. Exhaustive folds over closed ADTs and encoding tables may remain branch-heavy when they are total, direct, and tested; error forwarding, mixed responsibilities, and deep nesting should be refactored.
- In tests, keep dependent actions fail-fast. For independent, consecutive checks, use `expectAll` to report every ordinary assertion failure, or browser `assertAll` to retry one composed `BrowserObservation` and then aggregate its checks.

# CI-equivalent checks

Before pushing, run the same checks that CI runs from the repository root. Ensure the local PostgreSQL and Jaeger prerequisites are available, then seed the test database and run:

```sh
cabal run haskell-web-api-db -- migrate-and-seed
./generate-code-coverage.sh
./.github/scripts/formatting-checks.sh
./tools/check-vscode-ormolu-formatter.sh
cabal build all -O2 --ghc-options=-Werror
cabal test all -O2 --test-options="--skip Unit"
```

The coverage script cleans and rebuilds all packages, runs Unit tests package by package, and requires 100% coverage for every package. Do not run another Cabal command while it is active.

📦
