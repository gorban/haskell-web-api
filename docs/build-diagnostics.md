# Build diagnostics

The optimized build and coverage build are diagnostic boundaries, not merely test runs. Use
`tools/run-optimized-build-check.sh` followed by `tools/run-code-coverage-check.sh` locally. CI
runs the optimized diagnostic gate before the long coverage command, then applies the same warning
classifier to coverage output before formatting or integration work.

## Fatal diagnostics

Any line containing `warning:` or `Warning:` is fatal unless it matches one of the exact documented
external exceptions below. This deliberately includes compiler warnings and ordinary linker
warnings such as missing libraries, unresolved symbols, duplicate definitions, or incompatible
linkage. GHC's optimized build also uses `-Werror`, but the coverage build needs this additional
gate because it is compiled with coverage instrumentation.

## Documented GHC dynamic-link exception

The observed affected mode is GHC 9.14.1 and Cabal 3.16.1.0 coverage/dynamic builds with GNU
`ld.bfd`; it can emit this exact form:

```
/usr/bin/ld.bfd: warning: type and size of dynamic symbol `..._closure' are not defined
```

It refers to a generated GHC closure symbol (for example
`HarchWeb.Email.smtpServerHost_closure`), rather than reporting an unresolved application symbol.
The linker lacks ELF type/size metadata for that dynamic closure. The gate accepts only the absolute `/usr/bin/ld` or
`/usr/bin/ld.bfd` form ending in an alphanumeric/underscore GHC `_closure` symbol and the exact
`type and size ... are not defined` wording. Any variation is fatal and must be investigated.

The checked build environment has no alternative linker installed, and GHC exposes no supported
linker-selection switch there, so no project configuration has been adopted to suppress it.
Re-evaluate this exception whenever GHC, Cabal, the system linker, or the coverage build mode
changes. Prefer removing it through a supported toolchain configuration over broadening the
allowlist.

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
