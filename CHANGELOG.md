# Changelog

## v0.1.0.1

1. Dropped Windows native support. Windows native was last fully supported in
   [v0.1.0.0](https://github.com/gorban/haskell-web-api/tree/v0.1.0.0), but was later removed because, even
   after an immense effort to produce JavaScript output from Haskell (e.g. from GHCJS or the newer GHC with
   JavaScript backend), it just is not community-supported (see WIP PRs for unresolved issue
   [ghcjs#834](https://github.com/ghcjs/ghcjs/issues/830), for
   [ghcjs#1](https://github.com/ghcjs/ghcjs/issues/833) and its specific
   [ghc#1](https://github.com/gorban/ghc/pull/1) version it needs). If you want to run this on Windows, you
   can use WSL2 or Docker (configured for Linux containers).
2. Simplified generate-code-coverage-report to just be an .sh script (no more PowerShell polyglot).
3. Split off this setup guide into its own file, and added a link to it from the main README.

## v0.1.0.0

1. Initial release.
2. Completely lacks a real web API, but has a putStrLn in the main application executable as a placeholder.
   The main focus of this release is to set up the project structure, build system, testing framework, and
   CI/CD pipeline with code coverage reporting. The actual web API implementation will come in future
   releases.
