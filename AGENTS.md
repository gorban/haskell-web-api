# Before large or foundational work

Read [`docs/design-guidance.md`](docs/design-guidance.md) before starting any task scoped Large,
foundational, or security-critical — including one whose own text says its shape is "confirmed"
and needs no further design approval. It is not only a status report: read its "Design decisions
before you build" section to understand how to make the same kind of decision the current design
direction depends on, including the extend-vs-new-abstraction rule, the mid-task
framework-capability-gap protocol, and the untrusted-input resource-ownership principle — each
illustrated with a worked example of a real decision that went wrong. Apply that section's
framework rather than deciding silently.

Record the decision made under that framework — not just the code that resulted from it — in the
touched module's Haddock and, when it changes what an area of the framework can do, in
`docs/design-guidance.md` itself. Do not describe a Large/foundational task's shipped surface as a
completed version of what the task asked for when it is a narrower slice: name the concrete gap
and the follow-up task that closes it, in both places.

# Design rules

- Always render complete SSR HTML for every supported page route; API, SSE, and asset routes keep their
  explicit protocol response types.
- Every framework control must have its event-capture path ready before it can be used; never lose an event or input while waiting for hydration or a later module.
- Framework mutations use client actions and region patches, not page POSTs or full-page reloads.
- Keep components composable typed functions with explicit props; prefer declarative markup, styling, and behavior EDSLs.
- Keep runtime minimal: a nonce-protected inline capture kernel is allowed; larger behavior modules load afterward.
- Prove immediate interaction and input preservation with real-browser E2E tests.

# Haskell code style

- Model expected states and failures with precise ADTs and newtypes; prefer total functions and make invalid states unrepresentable where practical.
- Keep parsing and validation pure. Use `Either` for failure, and reserve `Maybe` for cases where absence is a valid result rather than an explanation being discarded.
- Use `ExceptT DomainError IO` for multi-step effectful workflows that stop at the first domain failure. Interpret the error once at the public or transport boundary instead of manually forwarding `Left`, `Right`, `Nothing`, or `Just` through nested cases.
- For a recurring `IO (Either sourceError value)` adapter, use `liftEitherWith` to map its error into the workflow rail. Do not alias `liftIO` merely to hide it; instead, give a cohesive group of IO steps a domain name and lift that operation once.
- Keep expected business outcomes as ordinary result constructors. Do not turn validation failures, authentication rejection, not-found results, or other expected alternatives into exceptions or infrastructure errors.
- Catch exceptions only at the `IO` boundary that can explain or recover from them. Public responses must remain safe; detailed causes belong in private structured logs and observability.
- Introduce a custom Monad newtype only when it names a stable capability stack, such as shared services plus typed request failure. Prefer standard transformers for a single local workflow and avoid polymorphic Monad constraints when no alternate interpreter is useful.
- Use records for cohesive dependencies or inputs when a long positional argument list obscures meaning. Keep framework components as typed functions with explicit props rather than ambient application state.
- Keep observability failure codes stable and low-cardinality. Detailed causes stay in private logs; HTTP server spans leave expected 4xx outcomes unset and mark 5xx responses as errors.
- Treat complexity metrics as review signals. Exhaustive folds over closed ADTs and encoding tables may remain branch-heavy when they are total, direct, and tested; error forwarding, mixed responsibilities, and deep nesting should be refactored.
- Before an implementation commit, run `tools/haskell-quality-report.sh` and create a follow-up task when a non-facade production module exceeds 500 lines plus 20 imports or 10 local dependencies, a non-facade public API has more than 40 exports, there is an import cycle, a cohesive function has 6 or more positional inputs, or an Argon hotspot also has a module-health signal. Do not split declarative test `spec`s, re-export facades, or direct total ADT folds solely to lower a metric. Do not add strictness annotations (`$!`, `seq`, `deepseq`), no-op/fake computation, or HLint-ignore pragmas solely to make Argon or HPC coverage tick a branch or expression that is already correct and already exercised by a test — extend or fix the test to exercise real behavior, or restructure the code, instead.
- In tests, keep dependent actions fail-fast. For independent, consecutive checks, use `expectAll` to report every ordinary assertion failure, or browser `assertAll` to retry one composed `BrowserObservation` and then aggregate its checks.

# CI-equivalent checks

Before pushing, run the same checks that CI runs from the repository root. Ensure the local PostgreSQL,
Jaeger, and LLVM `ld.lld` prerequisites are available, then seed the test database and run:

```sh
cabal run haskell-web-api-db -- migrate-and-seed
./tools/run-optimized-build-check.sh
./tools/run-code-coverage-check.sh
./.github/scripts/formatting-checks.sh
./tools/check-vscode-ormolu-formatter.sh
cabal test all -O2 --test-options="--skip Unit"
```

Before the formatting checks, run `.github/scripts/install-formatting-tools.sh` whenever the
local tools are not known to be the CI-pinned versions; it installs the exact `cabal-gild`, HLint,
and Ormolu toolchain used by CI.

The optimized-build wrapper first tests the warning classifier, then rejects actionable compiler or
linker warnings before the longer coverage run. The coverage wrapper cleans and rebuilds all
packages, runs Unit tests package by package, requires 100% coverage in every project report and
the complete multi-package report, and applies the same diagnostic gate. They record only the
documented GHC HPC diagnostic without masking it; see `docs/build-diagnostics.md`. Do not run
another Cabal command while either wrapper is active.

Treat its process exit status as a hard pre-push gate: it must be zero. Both the red
`Per-project reports found with <100% coverage` section and the red `Aggregate coverage report
found <100% coverage` section are failures. Capture the complete output and verify neither
condition appears before pushing:

```sh
./tools/run-code-coverage-check.sh
```

Do not treat a truncated terminal response, an HTML report existing on disk, a focused package run,
or a successful aggregate-report generation as proof that this gate passed. If the coverage command
was interrupted, its status cannot be recovered, or its output was not captured, rerun it from a
cleanly idle Cabal workspace before pushing.

Run `.github/scripts/install-formatting-tools.sh` before every pre-push formatting check unless the
installed tools have been verified as CI-pinned in the current workspace session. A formatting check
using a different Ormolu version is not CI-equivalent and must not justify a push. Run the listed
commands sequentially and push only after every one has completed successfully; do not assume a
format-only change is exempt from the formatter check.

📦
