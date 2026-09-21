#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
playwright_runner="$repo_root/packages/test-core/playwright-runner"

if ! command -v node >/dev/null 2>&1; then
  printf '%s\n' 'Node.js 24 is required for the Playwright integration and E2E gate, but node is not on PATH.' >&2
  exit 2
fi

node_version="$(node --version)"
case "$node_version" in
  v24.*) ;;
  *)
    printf 'Node.js 24 is required for the Playwright integration and E2E gate; found %s.\n' "$node_version" >&2
    exit 2
    ;;
esac

if ! (
  cd "$playwright_runner"
  node --eval "require.resolve('@playwright/test')"
) >/dev/null 2>&1; then
  printf '%s\n' "Playwright dependencies are unavailable. Run: npm ci --prefix $playwright_runner" >&2
  exit 2
fi

cd "$repo_root"

# Cabal can start a test component before its executable is linked when a
# clean @test all@ evaluates the project in parallel. Build every component
# first, then run the non-Unit integration/E2E selection from those concrete
# test executables.
cabal build all -O2 --ghc-options=-optl-fuse-ld=lld
exec cabal test all -O2 --ghc-options=-optl-fuse-ld=lld --test-options="--skip Unit"
