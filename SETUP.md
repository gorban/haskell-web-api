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

Before creating the box, enable both the normal user Podman socket and the host rootful Podman socket:

```bash
systemctl --user enable --now podman.socket
sudo systemctl enable --now podman.socket
```

Keep the Distrobox itself **rootless** (`root=false`). That keeps VS Code Remote Containers and similar
editor integrations aligned with your normal user session, lets ordinary development services keep using the
rootless Podman socket, and avoids running the whole development environment as root. The main case that
still needs host root privileges is binding privileged ports such as 80/443, and from inside the rootless
box you can handle that explicitly with `distrobox-host-exec sudo podman` when needed.

Example Distrobox container definition, e.g. save as `distrobox.ini`:
```ini
[haskellbox]
additional_packages="gcc gcc-c++ gmp gmp-devel make ncurses ncurses-compat-libs ncurses-devel zlib-ng-compat-devel xz perl git vim-enhanced dos2unix podman-remote postgresql"
image="registry.fedoraproject.org/fedora:latest"
root=false
additional_flags="--env GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig"
init_hooks="install -d /var/tmp/distrobox-git /var/tmp/distrobox-git/bin"
init_hooks="if [ -f \$HOME/.gitconfig ]; then cp \$HOME/.gitconfig /var/tmp/distrobox-git/gitconfig; else : > /var/tmp/distrobox-git/gitconfig; fi"
init_hooks="GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get diff.tool >/dev/null 2>&1 || GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global diff.tool vimdiff; GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get merge.tool >/dev/null 2>&1 || GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global merge.tool vimdiff"
init_hooks="resolve_git_tool_bin() { case \$1 in bc|bc3|bc4) printf %s bcompare ;; gvimdiff|gvimdiff1|gvimdiff2|gvimdiff3) printf %s gvim ;; nvimdiff|nvimdiff1|nvimdiff2|nvimdiff3) printf %s nvimdiff ;; vimdiff|vimdiff1|vimdiff2|vimdiff3) printf %s vimdiff ;; vscode) printf %s code ;; *) printf %s \$1 ;; esac; }; if command -v distrobox-host-exec >/dev/null 2>&1; then for tool_key in diff.tool merge.tool; do tool_name=\$(GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get \$tool_key || true); test x\$tool_name = x && continue; tool_bin=\$(resolve_git_tool_bin \$tool_name); if ! command -v \$tool_bin >/dev/null 2>&1; then ln -sf /usr/bin/distrobox-host-exec /var/tmp/distrobox-git/bin/\$tool_bin; case \$tool_key in diff.tool) GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global difftool.\$tool_name.path /var/tmp/distrobox-git/bin/\$tool_bin ;; merge.tool) GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global mergetool.\$tool_name.path /var/tmp/distrobox-git/bin/\$tool_bin ;; esac; fi; done; fi"
init_hooks="if command -v distrobox-host-exec >/dev/null 2>&1 && ! command -v code >/dev/null 2>&1; then set -- \$(GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get core.editor || true); if ! GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global --get core.editor >/dev/null 2>&1 || test x\$1 = xcode || test x\$1 = xvscode; then printf %b \\\\043\\\\041/bin/sh\\\\012exec\\\\040/usr/bin/distrobox-host-exec\\\\040code\\\\040\\\\055w\\\\040\\\\042\\\\044\\\\100\\\\042\\\\012 > /var/tmp/distrobox-git/bin/git-editor && chmod +x /var/tmp/distrobox-git/bin/git-editor && GIT_CONFIG_GLOBAL=/var/tmp/distrobox-git/gitconfig git config --global core.editor /var/tmp/distrobox-git/bin/git-editor; fi; fi"
init_hooks="ln -sf /run/user/$(id -u)/podman/podman.sock /var/run/docker.sock 2>/dev/null || true"
init_hooks="ln -sf /usr/bin/podman-remote /usr/local/bin/podman 2>/dev/null || true"
```

- In that Fedora package list, `ncurses-devel` and `zlib-ng-compat-devel` are specifically needed for the
  optional Haskell Debugger. They are bundled into the example container definition so debugger setup works
  without an extra system package step later. `vim-enhanced` is included so git can always fall back to the
  built-in `vimdiff` tool inside the container, and `postgresql` keeps the `psql` CLI available without a
  separate install step.
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
  - Normal development services such as PostgreSQL on `5432`, Jaeger on `4318` / `16686`, and the app on
    `5001` are unprivileged and should keep using plain `podman` inside the rootless box.
  - For privileged ports below `1024` such as `80` for ACME `http-01`, use `distrobox-host-exec sudo podman`
    from inside the rootless box. That runs the container rootfully on the host only for the port-binding
    case and still requires your sudo approval.
  - A simpler development alternative is to bind the app on a higher port such as `5001` and use a router or
    host redirect from `80` to that higher port instead of running anything rootfully just to claim port 80.
  - If you need the app binary itself to bind `80` / `443` directly outside a container, `setcap
    cap_net_bind_service+ep <binary>` or a host redirect such as `nft add rule nat PREROUTING tcp dport 80
    redirect to 5001` are narrower-scope alternatives to running the full app as root.
- The `init_hooks` do git setup overrides inside the container. `additional_flags` sets `GIT_CONFIG_GLOBAL`
  on every container start, and the hooks copy the host `~/.gitconfig` into that container-local file if
  it exists. They then inspect the selected `diff.tool` and `merge.tool` values from that copied config.
  If a selected tool is not installed locally, the example creates a symlink under
  `/var/tmp/distrobox-git/bin` that reroutes that executable via `distrobox-host-exec`, and points git at
  that symlink with `difftool.<tool>.path` or `mergetool.<tool>.path`. They do not override any existing
  `diff.tool` or `merge.tool` selection copied from the host; they only default missing tool entries to
  `vimdiff`. That works well for executable-based tools such as `vimdiff`, `nvimdiff`, `gvimdiff`,
  `bc3`/`bc4`, and `vscode` (mapped to the host `code` binary). If you use a fully custom
  `difftool.<tool>.cmd` or `mergetool.<tool>.cmd`, keep that custom command available inside the container
  or add a dedicated wrapper instead of relying on this generic executable bridge.
- The same example also handles the common VS Code editor case for `git commit`. If `core.editor` is unset,
  or already starts with `code` / `vscode`, and the container does not have a local `code` binary, the
  hook generates `/var/tmp/distrobox-git/bin/git-editor` as a small wrapper that runs host
  `code -w "$@"`, then rewrites `core.editor` inside the container-local git config to that wrapper. That
  makes `git commit`, `git config --edit`, and similar commands open the host VS Code and wait for it to
  close before git continues. If your copied host config already uses a different editor such as `vim`,
  `nvim`, `nano`, or another executable available inside the container, the hook leaves that editor alone.
- The Podman convenience symlinks are intentionally best-effort here. Some Distrobox setups run
  `init_hooks` without permission to write `/var/run` or `/usr/local/bin`; in that case those lines now
  quietly skip instead of aborting `distrobox enter`. If they are skipped, host-container control from
  inside the box may still need separate manual setup.

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

- `node` for the current browser-harness-backed e2e spec. The long-term Playwright direction is to keep
  Haskell-authored specs talking to an external runner process and let that runner use Playwright's
  official Node client, rather than implementing Playwright's server/MCP protocol directly in Haskell.
  No Playwright install is required yet; the current e2e path only needs a basic Node.js runtime. When a
  Playwright-backed runner is introduced, the intended handoff is `TEST_CORE_BROWSER_RUNNER=node` plus
  `TEST_CORE_BROWSER_RUNNER_ARGUMENTS=path/to/playwright-runner.js[,extra,args]`.
- `psql` plus a local PostgreSQL server if you want to exercise the PostgreSQL adapter, migrations, or seed
  data locally.

For real PostgreSQL coverage, the repository currently claims support for PostgreSQL `17.x`. The local
autostart path, documented container examples, and live integration tests all target that major version.

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
today. `cabal run exe:haskell-web-api` loads code defaults, then `./.env`, then `./.env.local`, and uses
the in-process default database effect unless you explicitly point it at PostgreSQL.

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

`cabal run exe:haskell-web-api` now reads those files on startup with exactly that precedence, so these two
files are the active local override path rather than just a documented future layout.

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
- Runtime app user: `web_api_runtime`
- Runtime app password: `web_api`

The current committed support target for those real-database flows is PostgreSQL `17.x`.

`Setup.hs` now tries to start a missing local PostgreSQL instance during `cabal build` / `cabal test`
when `SETUP_AUTOSTART_DATABASE=true`, `DATABASE_HOST` is `127.0.0.1` or `0.0.0.0`, and the configured
database is unreachable. It tries Podman first, then Docker, uses the configured `DATABASE_HOST`,
`DATABASE_PORT`, and `DATABASE_NAME` values for reachability/binding, and bootstraps the local container
with the fixed PostgreSQL migration superuser `web_api_owner` / `web_api_owner`.

If you prefer to start PostgreSQL yourself, want to use a remote/shared database, or need a different
bootstrap shape, disable that behavior with `SETUP_AUTOSTART_DATABASE=false` and start a matching local
container manually instead. One straightforward option is:

```bash
docker run --name web-api-postgres \
  -e POSTGRES_USER=web_api_owner \
  -e POSTGRES_PASSWORD=web_api_owner \
  -e POSTGRES_DB=web_api_dev \
  -p 127.0.0.1:5432:5432 \
  -d docker.io/library/postgres:17
```
- **podman**: just replace `docker` with `podman` in the above command, but make sure you have the Podman
  socket enabled on your host.

After `Setup.hs` starts a missing local PostgreSQL instance during `cabal build` / `cabal test`, it also
runs `haskell-web-api-db migrate-and-seed` automatically. If you prefer to run the Haskell migration
command yourself, export the owner-level migration credentials you want `haskell-web-api-db` to use. If
you used the default setup-time container behavior above, the simplest option is:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner
```

Then apply the Haskell-managed migrations and seed data from this repository:

```bash
cabal run exe:haskell-web-api-db -- migrate-and-seed
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
cabal run exe:haskell-web-api-db -- migrate
```

If you change the owner-level database connection values, update the exported
`WEB_API_MIGRATION_DATABASE_*` environment variables to match. The remaining `Setup.hs` database bootstrap
work should reuse this same migration path with its own owner credentials instead of requiring manual SQL.

When you are done with the container example, stop and remove it with either Docker or Podman:

```bash
docker rm -f web-api-postgres
```
- **podman**: just replace `docker` with `podman` in the above command.

## Local Jaeger All-in-One Startup Example

If you want a local tracing backend that matches the current OTLP tracing configuration seam, the
shortest path is:

```dotenv
OTLP_TRACING_ENABLED=true
```

That uses the default local OTLP tracing endpoint `http://127.0.0.1:4318/v1/traces`. If you need a
different collector address, override `OTLP_TRACING_ENDPOINT` explicitly instead.

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

## Podman End-to-End Bring-Up / Tear-Down Example

If you want one reproducible Podman-based local stack for PostgreSQL, Jaeger, and the runtime app
itself, a single rootless Podman pod is the simplest path. The pod keeps the default localhost
database and tracing endpoints working between containers, and only publishes the unprivileged ports
you need on the host:

- PostgreSQL: `127.0.0.1:5432`
- Jaeger OTLP HTTP ingest: `127.0.0.1:4318`
- Jaeger UI: `127.0.0.1:16686`
- web-api HTTP listener: `127.0.0.1:5001`

This example intentionally keeps the app on plain HTTP port `5001` so everyday development avoids
privileged port handling. The runtime can also start manual TLS listeners plus ACME listeners when you
explicitly configure them.

1. Build the runtime image from this repository:

```bash
podman build -t localhost/haskell-web-api:dev .
```

2. Create a pod that exposes all three services on localhost:

```bash
podman pod create --name web-api-dev \
  -p 127.0.0.1:5432:5432 \
  -p 127.0.0.1:4318:4318 \
  -p 127.0.0.1:16686:16686 \
  -p 127.0.0.1:5001:5001
```

3. Start PostgreSQL and Jaeger inside that pod:

```bash
podman run -d --pod web-api-dev --name web-api-postgres \
  -e POSTGRES_USER=web_api_owner \
  -e POSTGRES_PASSWORD=web_api_owner \
  -e POSTGRES_DB=web_api_dev \
  docker.io/library/postgres:17

podman run -d --pod web-api-dev --name web-api-jaeger \
  -e COLLECTOR_OTLP_ENABLED=true \
  docker.io/jaegertracing/all-in-one
```

4. Write container-local config files for the runtime app. These are mounted into `/app/.env` and
   `/app/.env.local` because the runtime executable loads those files from its working directory:

```bash
cat > ./podman.env <<'EOF'
APP_MODE=development
DATABASE_HOST=127.0.0.1
DATABASE_PORT=5432
DATABASE_NAME=web_api_dev
DATABASE_USER=web_api_runtime
DATABASE_PASSWORD=web_api
EOF

cat > ./podman.env.local <<'EOF'
APP_TITLE_PREFIX=web-api-podman
LISTENER_0_HOST=0.0.0.0
LISTENER_0_PORT=5001
LISTENER_0_SCHEME=http
OTLP_TRACING_ENDPOINT=http://127.0.0.1:4318/v1/traces
EOF
```

5. Seed the database once with the owner-level migration credentials:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner

cabal run exe:haskell-web-api-db -- migrate-and-seed
```

6. Start the runtime app container in the same pod:

```bash
podman run -d --pod web-api-dev --name web-api \
  -v "$PWD/podman.env:/app/.env:ro" \
  -v "$PWD/podman.env.local:/app/.env.local:ro" \
  localhost/haskell-web-api:dev
```

7. Useful checks after startup:

```bash
curl http://127.0.0.1:5001/api/status
curl http://127.0.0.1:5001/second
xdg-open http://127.0.0.1:16686
podman pod ps
podman ps --pod
```

When you are done, stop and remove the whole stack together:

```bash
podman pod rm -f web-api-dev
rm -f ./podman.env ./podman.env.local
```

That pod removal also removes the `web-api-postgres`, `web-api-jaeger`, and `web-api` containers that
were created inside it. The built image `localhost/haskell-web-api:dev` is left in your local image
store so you can restart the stack quickly.

## Reverse-Proxy Compose Example

The repository now includes a canonical reverse-proxy container example under
`examples/reverse-proxy/`:

- `docker-compose.yml`
- `generate-local-tls.sh`
- `podman-compose.yml`
- `app.env`
- `app.env.local`
- `nginx/default.conf`
- `nginx/prefixed.conf`

That stack keeps the application container itself on unprivileged HTTP port `5001`, while nginx owns
public ports `80` and `443` and forwards `Host`, `X-Forwarded-For`, and `X-Forwarded-Proto` to
`haskell-web-api`. `nginx/default.conf` mounts the app at `/`, while `nginx/prefixed.conf` shows the
same TLS-offload pattern mounted under `/app/*`. The two compose files describe the same stack so you
can keep `docker compose -f docker-compose.yml ...` and `podman compose -f podman-compose.yml ...`
spelled explicitly in local notes or automation without relying on tool-specific default filename
discovery.

Because nginx publishes low-numbered host ports, run this example with Docker, with rootful Podman,
or with a host configuration that allows rootless low-port binds. From a Distrobox shell, the
rootful Podman form is:

```bash
distrobox-host-exec sudo podman compose -f podman-compose.yml up -d postgres jaeger
```

Use the same `distrobox-host-exec sudo podman compose -f podman-compose.yml ...` prefix for the later
`up` and `down` commands in this section when you need the rootful path.

1. Generate a local root CA plus a `localhost` / `127.0.0.1` server certificate for the nginx TLS
   listener:

```bash
./examples/reverse-proxy/generate-local-tls.sh
```

   That helper writes `examples/reverse-proxy/tls/local-root-ca.pem`,
   `examples/reverse-proxy/tls/fullchain.pem`, and `examples/reverse-proxy/tls/privkey.pem`. The CA
   is only for local development; do not reuse it anywhere else.

2. Start only PostgreSQL and Jaeger first so the database can be migrated and seeded before the app
   container joins the stack:

```bash
podman compose -f examples/reverse-proxy/podman-compose.yml up -d postgres jaeger
# or: docker compose -f examples/reverse-proxy/docker-compose.yml up -d postgres jaeger
```

3. From the repository root, seed the database with the owner-level migration credentials against the
   compose-exposed PostgreSQL port:

```bash
export WEB_API_MIGRATION_DATABASE_HOST=127.0.0.1
export WEB_API_MIGRATION_DATABASE_PORT=5432
export WEB_API_MIGRATION_DATABASE_NAME=web_api_dev
export WEB_API_MIGRATION_DATABASE_USER=web_api_owner
export WEB_API_MIGRATION_DATABASE_PASSWORD=web_api_owner

cabal run exe:haskell-web-api-db -- migrate-and-seed
```

4. Bring up the app container and nginx reverse proxy:

```bash
podman compose -f examples/reverse-proxy/podman-compose.yml up -d web-api nginx
# or: docker compose -f examples/reverse-proxy/docker-compose.yml up -d web-api nginx
```

5. Verify the end-to-end behavior:

```bash
curl -I http://127.0.0.1/
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/api/status
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/second
xdg-open http://127.0.0.1:16686
```

Expected results:

- plain HTTP on port `80` redirects to HTTPS because the app sees `X-Forwarded-Proto: http`,
- HTTPS on port `443` succeeds with the local certificate chain from `tls/local-root-ca.pem`, and the app sees
  `X-Forwarded-Proto: https`,
- Jaeger stays available on `127.0.0.1:16686`.

6. Tear the stack down when finished:

```bash
podman compose -f examples/reverse-proxy/podman-compose.yml down
# or: docker compose -f examples/reverse-proxy/docker-compose.yml down
rm -f examples/reverse-proxy/tls/fullchain.pem \
  examples/reverse-proxy/tls/localhost.pem \
  examples/reverse-proxy/tls/local-root-ca.key \
  examples/reverse-proxy/tls/local-root-ca.pem \
  examples/reverse-proxy/tls/privkey.pem
```

### Path-prefix variant (`/app/*`)

If the reverse proxy should publish the app below a path prefix instead of at `/`, switch the nginx
volume in `examples/reverse-proxy/docker-compose.yml` or `examples/reverse-proxy/podman-compose.yml`
from `./nginx/default.conf` to `./nginx/prefixed.conf` and bring the `web-api` / `nginx` services
back up.

That alternate config keeps the backend on the same internal HTTP listener (`web-api:5001`) but now
forwards:

- `X-Forwarded-Proto: http` / `https` based on the public listener,
- `X-Forwarded-Prefix: /app` on every proxied request below `/app`.

With those headers in place, `haskell-web-api` still matches internal routes like `/second`, while
rendered links, redirects, and static asset URLs stay rooted at `/app`.

Verify the prefixed flow with:

```bash
curl -I http://127.0.0.1/app
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/app
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/app/second
curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/app/assets/navigation.js
```

Expected results:

- plain HTTP on `/app` redirects to `https://.../app` because the app sees
  `X-Forwarded-Proto: http`,
- HTTPS responses under `/app` keep working without any backend TLS listener,
- HTML navigation links and app-owned assets render as `/app/...`, not `/...`.

### Let's Encrypt / port 80 note for Podman

The earlier rootless Podman pod example works because every published port there is above `1024`.
ACME `http-01` validation is different: the challenge flow needs TCP/80, and `80` is a privileged
low-numbered port.

For that case you need one of these approaches:

- Run the relevant Podman container or pod rootfully on the host, for example
  `distrobox-host-exec sudo podman ...` from inside a Distrobox shell.
- Use a runtime path that actually grants low-port bind privileges end-to-end. The tracked image already
  sets `cap_net_bind_service` on `/app/haskell-web-api`, but rootless Podman may still need a rootful run
  or explicit runtime capability/host-network allowances before port `80` binding succeeds.

If you do not actually need ACME `http-01`, keep the rootless pod on port `5001` and avoid privileged
port binding entirely.

### Rootful host-network app container on port 80

If you are developing from inside Distrobox and want the app itself to listen on the real host port
`80`, keep PostgreSQL and Jaeger on the rootless Podman pod from the section above, but run only the
runtime app container rootfully on the host network.

1. Follow steps 1-5 from the Podman end-to-end example above, but stop before step 6 so the regular
   rootless `web-api` container is not started on port `5001`.

2. Write a host-port override file for the runtime app:

   ```bash
   cat > ./podman.env.port80 <<'EOF'
   APP_TITLE_PREFIX=web-api-host80
   LISTENER_0_HOST=0.0.0.0
   LISTENER_0_PORT=80
   LISTENER_0_SCHEME=http
   OTLP_TRACING_ENDPOINT=http://127.0.0.1:4318/v1/traces
   EOF
   ```

3. Start the runtime app rootfully on the host network from the Distrobox shell:

   ```bash
   distrobox-host-exec sudo podman run --rm --name web-api-host80 \
     --network=host \
     -v "$PWD/podman.env:/app/.env:ro" \
     -v "$PWD/podman.env.port80:/app/.env.local:ro" \
     localhost/haskell-web-api:dev
   ```

   Because this container is sharing the host network namespace directly, `127.0.0.1:5432` still
   reaches the PostgreSQL container from the earlier pod and `127.0.0.1:4318` still reaches Jaeger.

4. From another terminal, verify the runtime is actually serving traffic on host port `80`:

   ```bash
   curl http://127.0.0.1/api/status
   curl http://127.0.0.1/second
   distrobox-host-exec sudo ss -ltnp '( sport = :80 )'
   distrobox-host-exec sudo podman logs web-api-host80
   ```

5. When you are done, stop the rootful app container and remove the temporary override file:

   ```bash
   distrobox-host-exec sudo podman stop web-api-host80
   rm -f ./podman.env.port80
   ```

This is the simplest local path for proving that the implemented HTTP runtime can bind the real host
port `80`. For longer-lived dev/prod container setups, prefer a reverse proxy in front of the app so
the application container itself can stay unprivileged on port `5001`.

### Rootful host-network app container on ports 80 and 443

If you want the app container itself to own both public ports and prove the listener-aware default
redirect behavior without a reverse proxy, run the same host-network pattern with one HTTP listener and
one manual-TLS HTTPS listener.

1. Follow steps 1-5 from the Podman end-to-end example above, but stop before step 6 so the regular
   rootless `web-api` container is not started on port `5001`.

2. Generate a local root CA plus a `localhost` / `127.0.0.1` certificate chain for the HTTPS listener:

   ```bash
   ./examples/reverse-proxy/generate-local-tls.sh
   ```

3. Write a dual-listener override file for the runtime app:

   ```bash
   cat > ./podman.env.ports80-443 <<'EOF'
   APP_TITLE_PREFIX=web-api-host443
   LISTENER_0_HOST=0.0.0.0
   LISTENER_0_PORT=80
   LISTENER_0_SCHEME=http
   LISTENER_1_HOST=0.0.0.0
   LISTENER_1_PORT=443
   LISTENER_1_SCHEME=https
   LISTENER_1_TLS_SOURCE=manual
   LISTENER_1_TLS_CERTIFICATE_FILE=/app/tls/fullchain.pem
   LISTENER_1_TLS_PRIVATE_KEY_FILE=/app/tls/privkey.pem
   OTLP_TRACING_ENDPOINT=http://127.0.0.1:4318/v1/traces
   EOF
   ```

4. Start the runtime app rootfully on the host network from the Distrobox shell:

   ```bash
   distrobox-host-exec sudo podman run --rm --name web-api-host443 \
     --network=host \
     -v "$PWD/podman.env:/app/.env:ro" \
     -v "$PWD/podman.env.ports80-443:/app/.env.local:ro" \
     -v "$PWD/examples/reverse-proxy/tls:/app/tls:ro" \
     localhost/haskell-web-api:dev
   ```

5. From another terminal, verify both low ports are bound and that plain HTTP now redirects to HTTPS by
   default:

   ```bash
   curl -I http://127.0.0.1/api/status
   curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/api/status
   curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/second
   distrobox-host-exec sudo ss -ltnp '( sport = :80 or sport = :443 )'
   distrobox-host-exec sudo podman logs web-api-host443
   ```

   Expected results:

   - `curl -I http://127.0.0.1/api/status` returns `308 Permanent Redirect`.
   - The `Location` header points at `https://127.0.0.1/api/status`.
   - The HTTPS requests succeed with the local certificate chain from
     `examples/reverse-proxy/tls/local-root-ca.pem`.

6. If you want a dedicated verification pass for the redirect override, add
   `REDIRECT_HTTP_TO_HTTPS=false` to `./podman.env.ports80-443`, restart the container, then prove that
   both listeners now serve real traffic side by side:

   ```bash
   printf '\nREDIRECT_HTTP_TO_HTTPS=false\n' >> ./podman.env.ports80-443
   distrobox-host-exec sudo podman stop web-api-host443
   distrobox-host-exec sudo podman run --rm --name web-api-host443 \
     --network=host \
     -v "$PWD/podman.env:/app/.env:ro" \
     -v "$PWD/podman.env.ports80-443:/app/.env.local:ro" \
     -v "$PWD/examples/reverse-proxy/tls:/app/tls:ro" \
     localhost/haskell-web-api:dev
   ```

   Then from another terminal:

   ```bash
   curl -i http://127.0.0.1/api/status
   curl -I http://127.0.0.1/second
   curl --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/api/status
   curl -I --cacert examples/reverse-proxy/tls/local-root-ca.pem https://127.0.0.1/second
   ```

   Expected results:

   - `http://127.0.0.1/api/status` returns `200 OK` with the same JSON status payload that HTTPS serves.
   - `http://127.0.0.1/second` returns `200 OK` instead of redirecting to HTTPS.
   - The HTTPS `api/status` and `/second` requests still succeed with the local certificate chain from
     `examples/reverse-proxy/tls/local-root-ca.pem`.
   - No `Location: https://...` redirect header appears while the override is enabled.

7. When you are done, stop the rootful app container and remove the temporary override file. If you do
   not need the generated local CA anymore, remove those TLS files too:

   ```bash
   distrobox-host-exec sudo podman stop web-api-host443
   rm -f ./podman.env.ports80-443 \
     examples/reverse-proxy/tls/fullchain.pem \
     examples/reverse-proxy/tls/localhost.pem \
     examples/reverse-proxy/tls/local-root-ca.key \
     examples/reverse-proxy/tls/local-root-ca.pem \
     examples/reverse-proxy/tls/privkey.pem
   ```

### Other low-port binding options

If you do not want the rootful host-network path above, the remaining low-port options from
`TASKS.md` are still viable:

1. **Host-level nftables / iptables redirect**: keep the app itself on `5001`, then redirect host
   port `80` to that unprivileged listener.

   ```bash
   sudo nft add table ip nat
   sudo nft 'add chain ip nat PREROUTING { type nat hook prerouting priority dstnat; }'
   sudo nft add rule ip nat PREROUTING tcp dport 80 redirect to :5001

   curl http://127.0.0.1/api/status
   sudo nft list table ip nat
   ```

   If your host still uses iptables tooling instead of nftables, the equivalent redirect is:

   ```bash
   sudo iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-ports 5001
   ```

2. **`setcap` on the runtime binary inside the image**: the tracked `Dockerfile` now grants only the
   bind-low-port capability to `/app/haskell-web-api`, then keeps the container running as the non-root
   `app` user.

   ```dockerfile
   RUN apk add --no-cache libcap \
    && setcap cap_net_bind_service+ep /app/haskell-web-api \
    && getcap /app/haskell-web-api \
    && apk del libcap
   ```

   The runtime stage now does exactly that, but whether `80` / `443` actually bind still depends on the
   container runtime honoring that file capability. If a rootless runtime still fails with `permission
   denied`, use the rootful host-network path above, a host redirect, or explicit runtime capability
   grants.

3. **Rootless Podman with `--cap-add=NET_BIND_SERVICE --network=host`**: this keeps the container
   rootless, but it only works when the host allows unprivileged low ports.

   ```bash
   distrobox-host-exec sysctl net.ipv4.ip_unprivileged_port_start

   distrobox-host-exec podman run --rm --name web-api-host80 \
     --network=host \
     --cap-add=NET_BIND_SERVICE \
     -v "$PWD/podman.env:/app/.env:ro" \
     -v "$PWD/podman.env.port80:/app/.env.local:ro" \
     localhost/haskell-web-api:dev
   ```

   The sysctl output must be `80` or lower for that rootless bind to succeed.

## Local ACME runtime exercise

Both ACME backends now run through the real runtime path: `web-api` parses the environment into an
ACME-backed listener config, `harch-web` translates that into an ACME startup plan, and `HarchWeb.runServer`
can either invoke certbot or complete the `in-process-http01` flow before starting the HTTPS listener.
The native backend needs `openssl` on `PATH` for RSA key generation, CSR generation, and RS256 signing.

When a config includes both the plain HTTP challenge listener and an HTTPS listener, leaving
`REDIRECT_HTTP_TO_HTTPS` unset now defaults non-ACME traffic to HTTPS redirects while keeping
`/.well-known/acme-challenge/*` exempt for `http-01`.

For development, prefer a staging ACME directory rather than the production Let's Encrypt endpoint. Add a
temporary ACME listener block like this to `./.env.local`:

```env
# Listener 0: plain HTTP for ACME http-01 challenge traffic
LISTENER_0_HOST=0.0.0.0
LISTENER_0_PORT=80
LISTENER_0_SCHEME=http

# Listener 1: HTTPS with ACME + certbot
LISTENER_1_HOST=0.0.0.0
LISTENER_1_PORT=443
LISTENER_1_SCHEME=https
LISTENER_1_TLS_SOURCE=acme
LISTENER_1_ACME_DIRECTORY_URL=https://acme-staging-v02.api.letsencrypt.org/directory
LISTENER_1_ACME_CONTACT_EMAILS=ops@example.com
LISTENER_1_ACME_DOMAINS=example.com,www.example.com
LISTENER_1_ACME_CHALLENGE_BACKEND=certbot-http01
LISTENER_1_ACME_CERTBOT_EXECUTABLE=certbot
LISTENER_1_ACME_CERTBOT_ARGUMENTS=certonly,--non-interactive,--agree-tos,--email,ops@example.com,--staging,--http-01-port,80
```

Set `LISTENER_<n>_ACME_DOMAINS` to the certificate domains you want the ACME order to cover. The
certbot runtime path reuses that list when its arguments do not already declare `-d` / `--domain` /
`--domains`, and the native in-process backend uses the same list for its ACME order identifiers and CSR.

Then exercise the ACME path in four layers:

1. Parse the runtime config shape from environment values:

   ```bash
   cabal test haskell-web-api-tests --test-options='--match "parses manual and ACME-backed HTTPS listeners distinctly"'
   ```

2. Confirm the listener plan keeps the ACME settings intact:

   ```bash
   cabal test harch-web-tests --test-options='--match "translates ACME-backed HTTPS listeners into certificate-management plans"'
   ```

3. Confirm the native in-process runtime path can acquire TLS material before the HTTPS listener starts:

   ```bash
   cabal test harch-web-tests --test-options='--match "starts in-process ACME listeners on the configured http-01 port and stays running until signalled to stop"'
   ```

4. Confirm the certbot-backed runtime path still starts successfully once the HTTP challenge listener exists:

   ```bash
   cabal test harch-web-tests --test-options='--match "starts certbot-backed ACME listeners on the declared http-01 port and stays running until signalled to stop"'
   ```

If you also run the executable with that `./.env.local` and the challenge listener is reachable on the
declared `http-01` port, it should now start both listeners instead of stopping at the runtime boundary.
To exercise the native backend directly, switch `LISTENER_1_ACME_CHALLENGE_BACKEND` to
`in-process-http01`, remove the certbot-specific variables, and keep `openssl` installed on the machine or
in the container image. For real `http-01` testing on port `80`, reuse the same listener block together
with the privileged-port guidance above.

## Request Context In Logs And Traces

For direct requests, runtime traces record the socket peer as both `client.address` and
`network.peer.address`, and `url.scheme` follows the actual listener (`http` vs `https`).

When the app sits behind a reverse proxy or TLS-terminating load balancer, set
`X-Forwarded-For` and `X-Forwarded-Proto` on the hop into `haskell-web-api`, plus
`X-Forwarded-Prefix` when the proxy mounts the app below a path such as `/app`. The runtime then:

- records `client.address` from the first `X-Forwarded-For` value,
- keeps the immediate socket peer in `network.peer.address`,
- derives `url.scheme` from `X-Forwarded-Proto` when it is `http` or `https`,
- strips `X-Forwarded-Prefix` from the incoming request path for internal route/static matching and
  reapplies it to rendered links and redirects,
- preserves the raw forwarded header values as
  `http.request.header.x_forwarded_for` / `http.request.header.x_forwarded_proto` /
  `http.request.header.x_forwarded_prefix`,
- prefixes application `ERROR` log entries with the same request context that appears in
  `TRACE` request observability output.

Example proxy headers:

```text
X-Forwarded-For: 203.0.113.10, 10.0.0.15
X-Forwarded-Proto: https
X-Forwarded-Prefix: /app
```

## TLS-Offload Redirect And HSTS Example

If TLS terminates at a reverse proxy and `haskell-web-api` only sees plain HTTP on the backend hop,
enable redirect/HSTS policy in the app and make sure the proxy forwards `X-Forwarded-Proto`.

```env
REDIRECT_HTTP_TO_HTTPS=true
HSTS_MAX_AGE_SECONDS=31536000
HSTS_INCLUDE_SUBDOMAINS=true
HSTS_PRELOAD=true
```

With that policy:

- requests whose effective scheme is `http` return an HTTPS redirect before app or static handling,
- requests whose effective scheme is `https` keep their normal response and add
  `Strict-Transport-Security`,
- `X-Forwarded-Proto=https` is enough for TLS-offload deployments where the backend listener itself stays
  on plain HTTP.

That lets a common `80 -> 443` proxy setup enforce browser upgrades and HSTS now, even before native
runtime HTTPS listener startup is the active deployment path.

For direct app-managed dual listeners, you can now omit `REDIRECT_HTTP_TO_HTTPS` and let the runtime
default it on whenever both HTTP and HTTPS listeners are configured together. Set
`REDIRECT_HTTP_TO_HTTPS=false` only when you intentionally want both listeners to serve real traffic.
If more than one distinct HTTPS listener port is configured, the runtime keeps redirects on but omits
an explicit port in the redirect target, which means browsers fall back to the default HTTPS authority
on port `443`.

## External Port 80 Reachability for ACME / http-01

If you want to test Let's Encrypt or another external `http-01` style flow against this machine from the
internet, two separate network steps matter:

1. The host firewall must accept inbound TCP/80.
2. Your router must forward WAN TCP/80 to this host's LAN IP.

That setup is only needed for outside reachability. Local HTTPS binding and other local listener testing can
still be exercised without exposing the machine publicly, so for ACME-style reachability tests it is usually
enough to forward **only** TCP/80.

### Firewalld zone awareness

On Fedora-family hosts, do not assume the active firewalld zone is `public`. Check first:

```bash
sudo firewall-cmd --get-active-zones
```

Then add the port rule against the active zone you actually see there:

```bash
sudo firewall-cmd --add-port=80/tcp --zone=<active_zone>
sudo firewall-cmd --permanent --add-port=80/tcp --zone=<active_zone>
```

Replace `<active_zone>` with the real zone name from the first command.

This matters especially when you run a container with `--network=host`. With host networking, Podman is
sharing the host network namespace directly, so it does **not** automatically open firewall rules for you.
That differs from `-p 80:80`, where the container engine can often arrange the port mapping on its own.

### Router port-forwarding

A host firewall "accept" rule alone is not enough for internet reachability. Your router still needs a DNAT
/ port-forward rule that rewrites inbound WAN TCP/80 traffic to this machine's LAN IP on TCP/80.

Router UIs differ a lot, but the general steps are:

1. Find this machine's current LAN IP address.
2. Make that address stable with either a DHCP reservation in the router or a static host configuration.
3. Add a new router port-forward rule from WAN / internet TCP port `80` to that same host LAN IP on TCP
   port `80`.
4. If your router distinguishes firewall-accept rules from port-forward rules, make sure the forward/DNAT
   rule exists; an accept rule by itself is insufficient.

Without a stable LAN IP, the forward rule can silently break the next time DHCP assigns a different address
to the host.

## Manual Off-LAN Reachability Verification

Once one of the public listener setups above is running, the final "does real outside traffic reach the
app?" check is an operator step rather than an automated test. Run it from a phone on cellular or from a
different external network so the request really arrives through the router / WAN path instead of through
local NAT loopback.

Use one of these public listener shapes:

- **Reverse proxy (recommended)**: the nginx example on ports `80` / `443`.
- **Direct app bind**: the rootful host-network app flow on port `80`, or on ports `80` + `443`.

If you test HTTPS from off-LAN, use a publicly trusted certificate. The local CA from
`examples/reverse-proxy/generate-local-tls.sh` is only for loopback-style local verification on the same
machine.

1. Start watching the app logs on the host:

   ```bash
   podman compose -f examples/reverse-proxy/podman-compose.yml logs -f web-api nginx
   # or, for the direct host-network container:
   distrobox-host-exec sudo podman logs -f web-api-host443
   ```

2. Open Jaeger locally so you can search for the request trace after the off-LAN request lands:

   ```bash
   xdg-open http://127.0.0.1:16686
   ```

3. From the external client, request the public endpoint:

   ```text
   http://<public-host-or-ip>/api/status
   https://<public-host-or-domain>/api/status
   ```

   Use the HTTP form when you are only exposing port `80`. Use the HTTPS form when you have a real TLS
   listener on `443` with a cert that the external client already trusts.

4. Confirm the request reached the runtime:

   - the app or proxy logs now show the incoming request,
   - Jaeger has a new request span for `/api/status`,
   - `client.address` matches the external client IP (or the first `X-Forwarded-For` value when a proxy is
     in front),
   - `network.peer.address` still shows the immediate peer on the last hop into `haskell-web-api`.

If the request does not appear, re-check the firewall and router steps above first, then confirm the test
device is really off the LAN (for example, disable Wi-Fi on the phone before retrying).

#### Additional Build Prerequisites for CI Builds

The .github workflow `ci.yml` requires formatting checks with `cabal-gild`, `hlint`, and `ormolu` for the
`Formatting checks` step.

You may want to install the formatting tools and set up the pre-commit hook that the CI workflow uses to
fail fast on formatting regressions before a push or pull request.

```bash
.github/scripts/install-formatting-tools.sh
```

Those commands install:
- `cabal-gild` `1.8.4.1`
- `hlint`
- `ormolu`

The formatting-check script requires the positional `cabal-gild FILE --mode check` interface, and the
installer intentionally pins `cabal-gild` to `1.8.4.1` so local installs match CI.

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
