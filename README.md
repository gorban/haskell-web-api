# haskell-web-api

An opinionated template for a simple web api in Haskell.
- Uses Cabal without Stack build support
- Reduces boilerplate:
  - Uses pre-processors to reduce boilerplate:
    - `hspec-discover` (from Hackage) turns test/Main.hs into a test suite that automatically discovers and
      runs all tests in the `test` directory.
    - `spec-preprocessor` (defined in this repo) writes the module and common imports, making concise
      tests.
  - Rolls up common imports into `Core.Prelude` and `TestCore.Prelude` modules, to reduce all imports.
- Runs tests in GitHub Actions as its CI/CD pipeline:
  - Requires a clean `cabal-gild` to ensure .cabal files are well formatted and consistent.
  - Requires a clean `ormulo` to ensure the code is well formatted and consistent.
  - Requires a clean `hlint` to ensure no linting issues are present.
  - Requires a warning-free build to ensure no warnings are present.
  - Ensures that just Unit folder tests alone have 100% code coverage of their same package, and that
    overall code coverage is also 100%, both top-level-expressions **and** alternations.
  - Includes a local helper to generate a coverage report locally, combining every project's coverage
    report in a custom root report. The coverage report is published (with slight modifications) to GitHub
    Pages for easy access.
  - Requires non-third party packages also pass some integration tests of their actual application
    executables.
- Dockerfile provides an alternative to produce the coverage report and run the actual application in a
  consistent environment, without needing to set up the Haskell environment locally.
- Supports only Linux and MacOS. Windows native was last fully supported in
  [v0.1.0.0](https://github.com/gorban/haskell-web-api/tree/v0.1.0.0), but was later removed because, even
  after an immense effort to produce JavaScript output from Haskell (e.g. from GHCJS or the newer GHC with
  JavaScript backend), it just is not community-supported (see WIP PRs for unresolved issue
  [ghcjs#830](https://github.com/ghcjs/ghcjs/issues/830), for
  [ghcjs#1](https://github.com/ghcjs/ghcjs/issues/830) and its specific
  [ghc#1](https://github.com/gorban/ghc/pull/1) version it needs). If you want to run this on Windows, you
  can use WSL2 or Docker (configured for Linux containers).

## Changelog

See [CHANGELOG](CHANGELOG.md) for a detailed changelog.

## Components

packages/
  - web-api: The main application package providing currently only an putStrLn (not a real web API yet).
  - core: Shared utility functions and setup helpers used across application and test packages.
  - test-core: A library with shared test utilities and custom preprocessors.
  - hspec-expectations-match: A fork of the third-party package `hspec-expectations-match` with local
    changes to get it to work with current versions of Template Haskell.
    - No public GitHub repo, so not sure if we can get these changes upstreamed to Hackage:\
      <https://hackage.haskell.org/package/hspec-expectations-match>

### Build Status

[![CI](https://github.com/gorban/haskell-web-api/actions/workflows/ci.yml/badge.svg)
](https://github.com/gorban/haskell-web-api/actions/workflows/ci.yml)

Coverage report:\
<https://gorban.github.io/haskell-web-api/>

## Prerequisites

The following is a detailed guide to set up a Haskell development environment on Windows, MacOS, and Linux
(e.g. WSL2 / Ubuntu or Fedora).

- Haskell GHC Compiler and Cabal
  - (recommended) GHCup, which provides an easy way to install and manage multiple versions of GHC and
    Cabal. It also includes tools like HLS and hlint.
- (recommended) IDE/editor with Haskell support (e.g. Visual Studio Code with Haskell extension)
  - (recommended IDE-helpers) Haskell Language Server (HLS), hlint, Haskell debugger (hdb), and Haskell
    Debugger
- (optional) Stack (some third party projects might require it)

### MacOS / Linux

See [Setup](SETUP.md) for detailed instructions on setting up the Haskell environment on MacOS and Linux.

### Alternative: Docker

You can build and run the project including running tests and generating coverage report with Docker. The
base image is Linux-based so will only work on Windows if the docker engine supports Linux containers (not
Windows containers).

### Windows

- Install WSL2 (Windows Subsystem for Linux) and set up a Linux distribution (e.g. Ubuntu or Fedora) to run
  the Haskell environment in a Linux-like environment, which is more compatible with Haskell tooling.
- Alternatively, you can use Docker with Linux containers. Docker Desktop can be switched to use Linux
  containers from its System Tray icon menu,
"Switch to Linux Containers", if you set up Docker Desktop correctly:\
<https://docs.docker.com/desktop/windows/wsl/>
