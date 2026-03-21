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
  - harch-web: Haskell Architecture Web - The SSR-first web framework package. It currently provides the
    initial facade boundary that `web-api` composes against, and it is intended to grow into the shared
    route, page shell, server adapter, and progressive enhancement infrastructure tracked in `TASKS.md`.
  - web-api: The example application package and composition root that wires app-specific routes, pages,
    config, and startup into the shared `HarchWeb` facade.
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

### Configuration

The example application ships with committed localhost-friendly defaults, so you can run it for local
development without setting any configuration first. When you do need to reconfigure it, the current app
understands the following values:

| Config value | Description | Default |
| --- | --- | --- |
| `APP_MODE` | Application environment mode for app-level behavior. Supported values are `development`, `test`, and `production`. | (`development`) |
| `DATABASE_HOST` | Database host for app environment config. Using loopback addresses such as `127.0.0.1` or `::1` keeps the database reachable only from the local machine, not from external clients. | (`127.0.0.1`) |
| `DATABASE_PORT` | Database port for app environment config. | (`5432`) |
| `DATABASE_NAME` | Database name for app environment config. | (`web_api_dev`) |
| `DATABASE_USER` | Database username for app environment config. | (`web_api`) |
| `DATABASE_PASSWORD` | Database password for app environment config. | (`web_api`) |
| `APP_TITLE_PREFIX` | Prefix used in rendered HTML page titles. | (`web-api`) |
| `LISTENER_<n>_HOST` | Host/interface to bind for listener `n`. | (`LISTENER_0_HOST=127.0.0.1`) |
| `LISTENER_<n>_PORT` | Port to bind for listener `n`. | (`LISTENER_0_PORT=5001`) |
| `LISTENER_<n>_SCHEME` | Listener scheme, either `http` or `https`. | (`LISTENER_0_SCHEME=http`) |
| `LISTENER_<n>_TLS_SOURCE` | TLS source for HTTPS listeners, either `manual` or `acme`. | (`unset`) |
| `LISTENER_<n>_TLS_CERTIFICATE_FILE` | Certificate file path for manual TLS. | (`unset`) |
| `LISTENER_<n>_TLS_PRIVATE_KEY_FILE` | Private key file path for manual TLS. | (`unset`) |
| `LISTENER_<n>_ACME_DIRECTORY_URL` | ACME directory URL for ACME-backed TLS. | (`unset`) |
| `LISTENER_<n>_ACME_CONTACT_EMAILS` | Comma-delimited ACME contact email list. | (`unset`) |
| `LISTENER_<n>_ACME_CHALLENGE_BACKEND` | ACME challenge backend, either `in-process-http01` or `certbot-http01`. | (`unset`) |
| `LISTENER_<n>_ACME_CERTBOT_EXECUTABLE` | Executable path for the `certbot-http01` backend. | (`unset`) |
| `LISTENER_<n>_ACME_CERTBOT_ARGUMENTS` | Comma-delimited extra arguments passed to certbot. | (`unset`) |
| `STATIC_ASSET_ROOT_<n>_URL_PREFIX` | URL prefix served from static asset root `n`. | (`unset`) |
| `STATIC_ASSET_ROOT_<n>_DIRECTORY` | Filesystem directory for static asset root `n`. | (`unset`) |
| `STATIC_CACHE_CONTROL_SECONDS` | Cache-Control max-age for configured static assets. | (`unset`) |
| `OTLP_TRACING_ENDPOINT` | OTLP endpoint for tracing export. | (`unset`) |
| `OTLP_TRACING_HEADERS` | Comma-delimited OTLP tracing headers in `name=value` form. | (`unset`) |
| `OTLP_METRICS_ENDPOINT` | OTLP endpoint for metrics export. | (`unset`) |
| `OTLP_METRICS_HEADERS` | Comma-delimited OTLP metrics headers in `name=value` form. | (`unset`) |

If you want a concrete local override file convention, create `./.env.local` in the repository root,
next to `README.md` and `cabal.project`.

1. Create `./.env.local`.
2. Add one `KEY=value` entry per line.
3. Blank lines are allowed, and lines starting with `#` are treated as comments.
4. Add only the keys you want to change; any key you omit will continue using its committed default.

Today, `./.env.local` is a documented convention for the parser/test seam, not an auto-loaded runtime
file. The current `cabal run haskell-web-api` path does not yet read `./.env.local` or process
environment variables automatically; it still boots with the committed in-process defaults directly.

In practice, that means:

1. To run the default localhost setup, just use `cabal run haskell-web-api` and do not create any
   override file unless you want one for future wiring.
2. To prepare overrides in the format already supported by the config parser, put them in
   `./.env.local`.
3. Once startup wiring loads overrides, the intended precedence is: committed defaults in source,
   then `./.env.local`, then process environment variables.

Example fully populated `./.env.local` body:

```dotenv
# App environment values
APP_MODE=production
DATABASE_HOST=192.0.2.10
DATABASE_PORT=55432
DATABASE_NAME=web_api_prod
DATABASE_USER=web_api_app
DATABASE_PASSWORD=replace-me

# App/runtime values
APP_TITLE_PREFIX=web-api-prod

# Listener 0: plain HTTP
LISTENER_0_HOST=127.0.0.1
LISTENER_0_PORT=5001
LISTENER_0_SCHEME=http

# Listener 1: HTTPS with manual certificate files
LISTENER_1_HOST=0.0.0.0
LISTENER_1_PORT=5443
LISTENER_1_SCHEME=https
LISTENER_1_TLS_SOURCE=manual
LISTENER_1_TLS_CERTIFICATE_FILE=/etc/web-api/tls/fullchain.pem
LISTENER_1_TLS_PRIVATE_KEY_FILE=/etc/web-api/tls/privkey.pem

# Listener 2: HTTPS with ACME and certbot
LISTENER_2_HOST=0.0.0.0
LISTENER_2_PORT=8443
LISTENER_2_SCHEME=https
LISTENER_2_TLS_SOURCE=acme
LISTENER_2_ACME_DIRECTORY_URL=https://acme-v02.api.letsencrypt.org/directory
LISTENER_2_ACME_CONTACT_EMAILS=ops@example.com,security@example.com
LISTENER_2_ACME_CHALLENGE_BACKEND=certbot-http01
LISTENER_2_ACME_CERTBOT_EXECUTABLE=/usr/bin/certbot
LISTENER_2_ACME_CERTBOT_ARGUMENTS=certonly,--non-interactive,--agree-tos,--email,ops@example.com

# Static assets
STATIC_ASSET_ROOT_0_URL_PREFIX=/assets
STATIC_ASSET_ROOT_0_DIRECTORY=public/assets
STATIC_ASSET_ROOT_1_URL_PREFIX=/uploads
STATIC_ASSET_ROOT_1_DIRECTORY=/var/lib/web-api/uploads
STATIC_CACHE_CONTROL_SECONDS=3600

# Observability
OTLP_TRACING_ENDPOINT=http://127.0.0.1:4318/v1/traces
OTLP_TRACING_HEADERS=authorization=Bearer demo-token,x-service-name=web-api
OTLP_METRICS_ENDPOINT=http://127.0.0.1:4318/v1/metrics
OTLP_METRICS_HEADERS=authorization=Bearer demo-token,x-service-name=web-api
```

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
  containers from its System Tray icon menu, "Switch to Linux Containers", if you set up Docker Desktop
  correctly:\
  <https://docs.docker.com/desktop/windows/wsl/>

## Local Development Runtime

With the default configuration, `cabal run haskell-web-api` is enough to boot the example application
locally. It uses the committed localhost listener defaults, the built-in page/API response stubs, and no
external database, telemetry backend, TLS certificate, ACME service, or static-asset root is required
unless you explicitly reconfigure one.

In practice that means a fresh clone can usually be started with:

```bash
cabal run haskell-web-api
```

By default the app binds an HTTP listener on `127.0.0.1:5001` and serves the example SSR/API behavior in
place, so external dependencies only become necessary when you override the defaults for your own
environment.
