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
exec cabal test all -O2 --ghc-options=-optl-fuse-ld=lld --test-options="--skip Unit"
