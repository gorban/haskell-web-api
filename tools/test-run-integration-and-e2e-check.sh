#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
checker="$repo_root/tools/run-integration-and-e2e-check.sh"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

mkdir -p "$fixture_root/bin"

write_node() {
  local version="$1"
  local resolve_status="$2"

  printf '#!/usr/bin/env bash\nif [ "$1" = "--version" ]; then\n  printf %%s\\n %q\n  exit 0\nfi\nif [ "$1" = "--eval" ]; then\n  exit %s\nfi\nexit 1\n' "$version" "$resolve_status" >"$fixture_root/bin/node"
  chmod +x "$fixture_root/bin/node"
}

printf '%s\n' '#!/usr/bin/env bash' 'printf "%s\\n" "$*"' >"$fixture_root/bin/cabal"
chmod +x "$fixture_root/bin/cabal"

expect_failure() {
  local description="$1"
  local expected_message="$2"
  local output

  if output="$(PATH="$fixture_root/bin:$PATH" "$checker" 2>&1)"; then
    printf 'Integration and E2E gate accepted %s.\n' "$description" >&2
    exit 1
  fi
  if ! printf '%s' "$output" | grep -Fq "$expected_message"; then
    printf 'Integration and E2E gate did not explain %s. Output:\n%s\n' "$description" "$output" >&2
    exit 1
  fi
}

write_node 'v22.22.2' 1
expect_failure 'a Node version different from CI' 'Node.js 24 is required'

write_node 'v24.0.0' 1
expect_failure 'missing Playwright dependencies' 'Playwright dependencies are unavailable'

write_node 'v24.0.0' 0
success_output="$(PATH="$fixture_root/bin:$PATH" "$checker")"
if ! printf '%s' "$success_output" | grep -Fq 'test all -O2 --ghc-options=-optl-fuse-ld=lld --test-options=--skip Unit'; then
  printf 'Integration and E2E gate did not invoke the expected Cabal command. Output:\n%s\n' "$success_output" >&2
  exit 1
fi

printf '%s\n' 'Integration and E2E runtime fixture checks passed.'
