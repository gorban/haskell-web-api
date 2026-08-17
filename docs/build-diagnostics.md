# Build diagnostics

The optimized build and coverage build are diagnostic boundaries, not merely test runs. Use
`tools/run-optimized-build-check.sh` followed by `tools/run-code-coverage-check.sh` locally. CI
runs those same wrappers before formatting or integration work; it does not maintain a second
coverage pipeline in YAML. The coverage wrapper first executes the coverage-gate fixture suite,
then captures the real coverage output and applies the same warning classifier, so a local run and
a CI run prove the same failure contract.

Both commands require LLVM `ld.lld` and pass `-fuse-ld=lld` to GHC's native linker invocation. This
removes the GNU `ld.bfd` dynamic-closure-symbol warning rather than suppressing it. CI installs
`lld`; local developers must install an `ld.lld` executable before either wrapper starts Cabal. See
the [setup guide](../SETUP.md#ghcup-prerequisites) for the Ubuntu and Fedora/Distrobox package lists.

## Runtime coverage scope

The coverage gate requires 100% expressions, alternatives, and top-level declarations from every
package's executable runtime code. Its only exclusions are the exact internal Template Haskell
modules `HarchWeb.Markup.Quasi`, `HarchWeb.Markup.Quasi.Lowering`, and
`HarchWeb.Markup.Quasi.Parser`. GHC runs those while compiling a quasiquote, before the test process
and its `.tix` file exist; standard HPC therefore cannot attribute that compiler-process execution to
the test run. The quasiquoter remains covered by compile-time acceptance and rejection specs.

This is not a general generated-code exemption: ordinary generated instances, application code,
runtime error paths, and all other production modules remain in the gate. Add no module to this list
without demonstrating that it executes exclusively in GHC's compiler process and documenting the
corresponding compile-time acceptance test.

## Fatal diagnostics

Any line containing `warning:` or `Warning:` is fatal unless it matches the exact documented
external exception below. This deliberately includes compiler warnings and all linker warnings,
warnings such as missing libraries, unresolved symbols, duplicate definitions, or incompatible
linkage. GHC's optimized build also uses `-Werror`, but the coverage build needs this additional
gate because it is compiled with coverage instrumentation.

## Documented GHC HPC deprecation

The same GHC 9.14.1 coverage mode can run instrumented custom-Setup and source-preprocessor
helpers. Their invocations do not receive a test suite's RTS arguments, so GHC can report this
exact five-line block when a helper reopens its own `.tix` file:

```
Deprecation warning:
I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.
GHC 9.14 will cease looking for an existing tix file by default.
If you positively want to add hpc info to the current tix file, use the RTS option --read-tix-file=yes.
More information can be found in the accepted GHC proposal 612.
```

The coverage command already supplies `--read-tix-file=no` to test suites and cleans stale
`.tix` data. No supported option reaches every Cabal-launched helper without producing a new
compiler diagnostic. The gate accepts this complete, version-specific block only; a different
deprecation warning is fatal.
