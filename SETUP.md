# Haskell Development Environment Setup

This guide provides detailed instructions for setting up a Haskell development environment on MacOS and
Linux (e.g. Ubuntu or Fedora).

For Windows, see deprecation in [changelog v0.1.0.1](CHANGELOG.md#v0101).

## GHCup Prerequisites

Fulfill any system requirements outlined by the
[GHCup official instructions](https://www.haskell.org/ghcup/install/#system-requirements).

### Example: Ubuntu Prerequisites

Commands to install prerequisites in Ubuntu. Also tested in WSL2 on Windows:

```bash
sudo apt update
sudo apt upgrade -y
sudo apt install -y build-essential curl libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev pkg-config zlib1g-dev git dos2unix
```

These Ubuntu packages already include the development libraries needed by the optional Haskell Debugger:
`libncurses-dev` provides the ncurses / `tinfo` linker files, and `zlib1g-dev` provides the `z` linker
files.

### Example: Fedora Prerequisites

Prerequisites for Fedora provided by Distrobox container, making them compatible whether you are using an
Atomic/immutable edition of Fedora or not. So first, if you don't have [Distrobox](https://distrobox.it/):
```bash
curl -s https://raw.githubusercontent.com/89luca89/distrobox/main/install | sudo sh
```

Example Distrobox container definition, e.g. save as `distrobox.ini`:
```ini
[haskellbox]
additional_packages="gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs ncurses-devel zlib-ng-compat-devel xz perl git vim-enhanced dos2unix podman-remote"
image="registry.fedoraproject.org/fedora:latest"
root=false
additional_flags="--env GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig"
init_hooks="install -d /var/tmp/distrobox-git /var/tmp/distrobox-git/bin"
init_hooks="if [ -f \$HOME/.gitconfig ]; then cp \$HOME/.gitconfig /var/tmp/distrobox-git/gitconfig; else : > /var/tmp/distrobox-git/gitconfig; fi"
init_hooks="GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get diff.tool >/dev/null 2>&1 || GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global diff.tool vimdiff; GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get merge.tool >/dev/null 2>&1 || GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global merge.tool vimdiff"
init_hooks="resolve_git_tool_bin() { case \$1 in bc|bc3|bc4) printf %s bcompare ;; gvimdiff|gvimdiff1|gvimdiff2|gvimdiff3) printf %s gvim ;; nvimdiff|nvimdiff1|nvimdiff2|nvimdiff3) printf %s nvimdiff ;; vimdiff|vimdiff1|vimdiff2|vimdiff3) printf %s vimdiff ;; vscode) printf %s code ;; *) printf %s \$1 ;; esac; }; if command -v distrobox-host-exec >/dev/null 2>&1; then for tool_key in diff.tool merge.tool; do tool_name=\$(GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get \$tool_key || true); test x\$tool_name = x && continue; tool_bin=\$(resolve_git_tool_bin \$tool_name); if ! command -v \$tool_bin >/dev/null 2>&1; then ln -sf /usr/bin/distrobox-host-exec /var/tmp/distrobox-git/bin/\$tool_bin; case \$tool_key in diff.tool) GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global difftool.\$tool_name.path /var/tmp/distrobox-git/bin/\$tool_bin ;; merge.tool) GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global mergetool.\$tool_name.path /var/tmp/distrobox-git/bin/\$tool_bin ;; esac; fi; done; fi"
init_hooks="ln -sf /run/user/$(id -u)/podman/podman.sock /var/run/docker.sock"
init_hooks="ln -sf /usr/bin/podman-remote /usr/local/bin/podman"
```

- In that Fedora package list, `ncurses-devel` and `zlib-ng-compat-devel` are specifically needed for the
  optional Haskell Debugger. They are bundled into the example container definition so debugger setup works
  without an extra system package step later. `vim-enhanced` is included so git can always fall back to the
  built-in `vimdiff` tool inside the container.
- The web-api project setup also tries to start missing prerequisites like PostgreSQL and Jaeger with
  `docker` or `podman`, so the example container definition also includes `podman-remote`, a socket
  symlink for it, and a symlink for the `podman` binary, so that the container can control host containers
  if needed.
  - **NOTE**: It is not required for the app to start up its own prerequisites if they are provided
    separately connections are configured appropriately, but otherwise you must have the Podman socket
    enabled on your host:
    ```bash
    systemctl --user enable --now podman.socket
    ```
- The `init_hooks` do git setup overrides inside the container. `additional_flags` sets `GIT_CONFIG_GLOBAL`
  on every container start, and the hooks copy the host `~/.gitconfig` into that container-local file if
  it exists. They then inspect the selected `diff.tool` and `merge.tool` values from that copied config.
  If a selected tool is not installed locally, the example creates a symlink under
  `/var/tmp/distrobox-git/bin` that reroutes that executable via `distrobox-host-exec`, and points git at
  that symlink with `difftool.<tool>.path` or `mergetool.<tool>.path`. They do not override any existing
  `diff.tool` or `merge.tool` setting copied from the host; they only default missing tool entries to
  `vimdiff`.

Then to assemble and run the container:
```bash
distrobox assemble create --file distrobox.ini
distrobox enter haskellbox
```

### Example: MacOS Prerequisites

According to the [GHCup official instructions](https://www.haskell.org/ghcup/install/#system-requirements),
simply running the GHCup installer below should install some prerequisites, but it notes:

> On Darwin M1 you might also need a working llvm installed (e.g. via brew) and have the toolchain exposed
> in PATH.

But, we also have a custom pre-commit hook that uses `dos2unix` for formatting checks, so you may want to
install that as well with Homebrew:

```bash
brew install llvm dos2unix
```

## Install GHCup

Install GHCup itself; answering as provided any questions asked during execution (some answers or
questions may be different or omitted entirely depending on target operating system):
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

1.  Press ENTER to accept the only available ghcup install location ($HOME/.ghcup).
2.  Press ENTER to acknowledge that the System requirements had just been installed.
3.  Use 'D' (or 'G' on MacOS and Fedora) to use the Default (GHCup maintained) binary distribution
    "channel".
4.  Answer 'N' to disable the pre-releases channel.
5.  Answer 'Y' to enable the cross channel (GHCJS, WASM, etc.).
6.  Answer 'P' to automatically add (prepend) the required PATH variable to "$HOME/.bashrc" (or
    "$HOME/zshrc" on MacOS).
7.  Answer 'Y' to install HLS (Haskell Language Server), we need it for IDE support.
8.  Answer 'Y' to install stack (we might be able to build our project with Cabal alone, but other
    projects might need stack).
9.  Answer 'Y' to enable better integration of stack with GHCup (so stack uses GHCup's own GHC versions).
10. MacOS only: look for security warnings installing with GHCup. They may appear as popups like "llc" Not
    Opened - Apple could not verify "llc" ...
    1. Select "Done" to not move each file to the Trash.
    2. After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have to
       unblock the last executable in System Preferences:
       1. Open System Preferences -> Privacy & Security -> scroll down to Security section.
       2. There should be a message like "llc" was blocked to protect your Mac.
       3. Click Allow Anyway.
    3. Repeat for other files like "opt", etc. until no more popups appear.
    4. If any popups appeared, you may want to kill the ghcup installer (Ctrl+C), clean out
       "$HOME/.ghcup", and re-run it, so that it can proceed with installing with now unblocked
       executables.
    5. On rerun you may get popups again for the same executables like Open "llc"? This time, click Open
       Anyway for each. It may also ask for your password to allow the installation to proceed.
11. For the new PATH variable to take effect:
    1. You can immediately get `ghcup` on the system PATH by running command:
       ```bash
       . "$HOME/.ghcup/env"
       ```
    2. To ensure it comes up automatically with a terminal restart, GHCup may have tried to update
       `$HOME/.bashrc`(or `$HOME/.zshrc` on MacOS), but it may not respect if you are using a non-default
       terminal (e.g. Zsh on Ubuntu / Fedora). So you may need to copy this line (e.g. to `$HOME/.zshrc`):
       ```bash
       [ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
       ```

## Install Haskell Toolchain

In the past, the latest versions had not all been compatible with each other or an otherwise compatible
version set segfaulted on specific operating systems. So we maintain a list of known compatible versions
here:
- GHC 9.14.1
- Cabal 3.16.1.0
- Stack 3.9.1
- HLS 2.13.0.0

Run `ghcup tui` to manage installed versions of GHC, Cabal, Stack, HLS, etc.
1. Immediately change GHC to the latest version that is "hls-powered" (compatible with Haskell Language
   Server) by selecting it in the list and pressing 's'. On MacOS, I did not see any "hls-powered"
   versions, so just select a version that would be "hls-powered" on Linux (can test via a Linux VM or
   another machine), or use GHC 9.14.1 if it's still current enough.
   1. Answer 'Y' for whether you would like to install the selected version if it is not already installed.
   2. MacOS only: look for security warnings installing with GHCup. They may appear as popups like
      "clang-18" Not Opened - Apple could not verify "clang-18" ...
      1. Select "Done" to not move each file to the Trash.
      2. After clicking "Done" before addressing any subsequent popup that appears (if any), you'll have
         to unblock the last executable in System Preferences:
         1. Open System Preferences -> Privacy & Security -> scroll down to Security section.
         2. There should be a message like "llc" was blocked to protect your Mac.
         3. Click Allow Anyway.
      3. Repeat for other files until no more popups appear.
   3. Press enter if asked to "continue" after each successful install.
      1. MacOS only: if any popups appeared, you may want to use undo by setting the GHC version to the
         "recommended" one with 's', selecting the newer one again and uninstalling with 'u', then
         pressing 's' to install and set it again, so that it can proceed with installing with now
         unblocked executables.
      2. On rerun you may get popups again for the same executables like Open "clang-18"? This time, click
         Open Anyway for each. It may also ask for your password to allow the installation to proceed.
2. You should be returned back into `ghcup tui`. For cabal, HLS, and stack, select the latest versions and
   press 's' to set them as default.
   1. Similarly answer 'Y' for whether you would like to install the selected version if it is not already
      installed.
   2. Also, press enter after each success and finally answer 'q' to exit ghcup.
3. Also, install with 'i', but do not 's'et the latest GHC version with a JavaScript backend. It may not be
   a corresponding version to the latest "hls-powered" GHC. For example if 9.14.1 is "hls-powered", the
   latest with a JavaScript backend might be:\
   `javascript-unknown-ghc-9.12.2`
   - **Note** any messages that is pauses on right after you start the 'i'nstall like:
     > [ Warn  ] To use this bindist, you have to use emscripten version 3.1.74

     We will need to install this exact emscripten version.
4. Check the versions with `ghc --version`, `cabal --version`,
   `haskell-language-server-wrapper --version`, and `stack --version` for last installed, respectively.

### Build Prerequisites for This Repository

Before running `cabal build all` in this repository, install the external build tool that is currently
required globally:

```bash
cabal install --ignore-project hspec-discover --overwrite-policy=always
```

Notes:
- Run this from anywhere, including the repository root. The `--ignore-project` flag prevents
  `cabal install` from trying to package the local workspace first.
- The previous instructions to enhance the system PATH from "$HOME/.ghcup/env" mean that `ghc` should now
  also be available with `which ghc`.

After that, the build command should at least have the required external build tool available:

```bash
cabal build all
```

## Repository Runtime Prerequisites

In addition to the Haskell toolchain, the current repository is easiest to work with when the following
commands are also available on your `PATH`:

- `node` for the current browser-harness-backed e2e spec. No Playwright install is required yet; the
  current e2e path only needs a basic Node.js runtime.
- `psql` plus a local PostgreSQL server if you want to exercise the PostgreSQL adapter, migrations, or seed
  data locally.

Example package-manager installs:

### Ubuntu

```bash
sudo apt install -y nodejs postgresql postgresql-client
```

### Fedora

```bash
sudo dnf install -y nodejs postgresql-server postgresql
```

### MacOS

```bash
brew install node postgresql@17
```

If you only want to boot the example app with its current committed stub data, PostgreSQL is optional
today. `cabal run haskell-web-api` still starts from `defaultAppConfig` and the in-process default
database effect, so the local database only becomes necessary when you are explicitly exercising the
PostgreSQL path.

## Repository Configuration Layers

The example app's intended configuration layout has three layers, all rooted at the repository root:

1. Code defaults in source. These are the committed defaults in `packages/web-api/src/WebApi/Config.hs`.
2. `./.env` for shared, checked-in, non-secret development overrides.
3. `./.env.local` for machine-specific or deployed overrides. This file may contain secrets and is already
   excluded by `.gitignore`.

Both `./.env` and `./.env.local` use simple `KEY=value` lines. Blank lines are allowed, and lines beginning
with `#` are comments.

When the file-based startup path is wired in, the intended precedence is:

1. Code defaults in source.
2. `./.env`
3. `./.env.local`

Today, the parser seam and precedence rules already exist, but the default `cabal run haskell-web-api` path
still starts directly from committed defaults. In other words, these two files describe the intended local
layout now, even though the default executable path does not yet read them automatically.

Practical steps:

1. If the committed defaults are good enough, create neither file and just run the app.
2. If your team wants shared non-secret development overrides, create `./.env` in the repository root.
3. If your machine or deployment needs secrets or different values, create `./.env.local` in the repository
   root.

For the exhaustive list of supported keys and example file bodies, see the `Configuration` section in
`README.md`.

## Local PostgreSQL Startup Example

If you want a local PostgreSQL instance that matches the current committed development defaults:

- Host: `127.0.0.1`
- Port: `5432`
- Database: `web_api_dev`
- Runtime app user: `web_api`
- Runtime app password: `web_api`

Today, prerequisite autostart from `Setup.hs` is still planned work. For now, manually start a local
PostgreSQL instance that matches your configured values. One straightforward option is a local container,
using either Docker or Podman.

```bash
docker run --name web-api-postgres \
  -e POSTGRES_USER=web_api_owner \
  -e POSTGRES_PASSWORD=web_api_owner \
  -e POSTGRES_DB=web_api_dev \
  -p 5432:5432 \
  -d postgres:17
```
- **podman**: just replace `docker` with `podman` in the above command, but make sure you have the Podman
  socket enabled on your host.

That container example creates a database owner up front, so you do not need separate `psql` owner calls
just to bootstrap the database. Then export dedicated migration credentials before running the Haskell
migration command:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner
```

Then apply the Haskell-managed migrations and seed data from this repository:

```bash
cabal run haskell-web-api-db -- migrate-and-seed
```

That command intentionally does **not** read the runtime app config files. Instead, it requires separate
owner-level credentials from the environment variables shown above so migrations do not depend on the
runtime application's minimal-access user.

Your runtime `./.env` / `./.env.local` values should keep describing the application's own connection user.
The future database-backed runtime path should use a minimal-access account there, while migrations should
continue to use separate owner credentials.

No extra migration tool installation is required. If you only want the schema without the sample content,
run:

```bash
cabal run haskell-web-api-db -- migrate
```

If you change the owner-level database connection values, update the exported
`WEB_API_MIGRATION_DATABASE_*` environment variables to match. The later `Setup.hs` prerequisite-autostart
work should reuse this same migration path with its own owner credentials instead of requiring manual SQL.

When you are done with the container example, stop and remove it with either Docker or Podman:

```bash
docker rm -f web-api-postgres
```
- **podman**: just replace `docker` with `podman` in the above comman.

## Local Jaeger All-in-One Startup Example

If you want a local tracing backend that matches the current OTLP tracing configuration seam, point
`OTLP_TRACING_ENDPOINT` at Jaeger's OTLP HTTP listener, for example:

```dotenv
OTLP_TRACING_ENDPOINT=http://127.0.0.1:4318/v1/traces
```

Then start Jaeger all-in-one with OTLP enabled.

```bash
docker run --name web-api-jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  -p 16686:16686 \
  -p 4318:4318 \
  -d jaegertracing/all-in-one
```
- **podman**: just replace `docker` with `podman` in the above command, but make sure you have the Podman
  socket enabled on your host.

Useful endpoints after startup:

- Jaeger UI: `http://127.0.0.1:16686`
- OTLP HTTP ingest: `http://127.0.0.1:4318/v1/traces`

When you are done, stop and remove it with either Docker or Podman:

```bash
docker rm -f web-api-jaeger
```
- **podman**: just replace `docker` with `podman` in the above command.

#### Additional Build Prerequisites for CI Builds

The .github workflow `ci.yml` requires formatting checks with `cabal-gild`, `hlint`, and `ormolu` for the
`Formatting checks` step.

You may want to install the formatting tools and set up the pre-commit hook that the CI workflow uses to
fail fast on formatting regressions before a push or pull request.

```bash
.github/scripts/install-formatting-tools.sh
```

Those commands install:
- `cabal-gild`
- `hlint`
- `ormolu`

To fail fast on formatting regressions before a push or pull request, copy the tracked pre-commit hook into
your local git hooks directory:

```bash
install -Dm755 .github/hooks/pre-commit .git/hooks/pre-commit
```

After copying it, any git commit will automatically run formatting checks, or you can also run it yourself:

```bash
git hook run pre-commit
```

## (optional) IDE Setup with VS Code-likes

For editor support, install Visual Studio Code or another editor with Haskell support.

### VS Code with Distrobox

If you are using a container (e.g. Distrobox) for development with Visual Studio Code, install the
[Dev Containers extension
](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers) on the host VS
Code instance.

After that, use the Command Palette and run `Dev Containers: Attach to Running Container...`, then select
the running container.

- **NOTE:** Immediately open an integrated terminal in the attached container, and run `whoami` to make
  sure it's not `root`. Attaching to a running container might not carry through the current user from the
  host, and VS Code can end up using the container engine's default user instead. If you leave it `root`,
  files created from that session may not be writable or readable as expected from the host user.
  \
  If you find the user is `root`, from either the host VS Code or the attached container, immediately run
  `Dev Containers: Open Attached Container Configuration File` from the Command Palette and add:
  \
  ```jsonc
  {
     // ...keep existing config...
     "remoteUser": "your-username",
     "containerUser": "your-username"
  }
  ```
  \
  At minimum, `remoteUser` should be set. `containerUser` is also reasonable if you want the whole attached
  session to stay on that user consistently. After attaching, run `whoami` in the VS Code terminal before
  building anything.
  \
  VS Code stores that attached-container override outside the repository, so each developer needs to set it
  on their own machine the first time they attach.
  \
  **Finally** if you had to set the user override, close the attached VS Code window and re-attach to the
  container again for the override to take effect. Then run `whoami` again to confirm it's correct before
  continuing.
- **Expected subsequent issue**: If you had to set the user override, the reopening remote VS Code may fail
  with error like:
  > [6838 ms] cat: $HOME/.vscode-server/data/Machine/.connection-token-...: Permission denied
  > [6838 ms] Exit code 1
  \
  Fix this on the host machine by running command:
  ```bash
  sudo chown -LR "$(whoami):$(whoami)" $HOME/.vscode-server
  ```
  \
  Then, close the attached VS Code window and re-attach to the container again. Finally `whoami` in the
  attached VS Code terminal should show the correct non-root user.

### Recommended VS Code Extensions

- Haskell by the Haskell Foundation
- Haskell Debugger by Well-Typed
  - With GHC 9.14.1, use the Haskell Debugger extension rather than the older GHCi Debug Adapter / Phoityne
    tooling.

You might need the pre-release versions for compatibility with the latest GHC and HLS releases.

### Global Tools for IDE Extensions

Install the debugger backend with:

```bash
cabal install --ignore-project -w ghc-9.14.1 haskell-debugger --allow-newer=base,containers,ghc,time,template-haskell --enable-executable-dynamic
```

If your editor starts outside a login shell, make sure the shell startup file it does read contains the
GHCup environment line shown earlier so the editor can find `ghc`, `hlint`, `ormolu`, `cabal-gild`, and
`hdb`.

Haskell Debugger will fail to start without installing our spec-preprocessor executable in its path:

```bash
cabal install core:core-spec-preprocessor
cabal install test-core:spec-preprocessor
```
