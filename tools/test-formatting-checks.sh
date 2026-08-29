#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
formatting_script="$repo_root/.github/scripts/formatting-checks.sh"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

mkdir -p "$fixture_root/bin" "$fixture_root/empty" "$fixture_root/only-cabal/packages/core" "$fixture_root/only-haskell/packages/core" "$fixture_root/complete/packages/core" "$fixture_root/complete/examples/example" "$fixture_root/crlf/packages/core"

printf '%s\n' '#!/usr/bin/env bash' 'if [ "${1:-}" = "--help" ]; then' '  printf "%s\\n" "Usage: cabal-gild [OPTIONS] [FILE ...]"' 'fi' 'printf "%s\\n" "$*" >>"$FORMAT_TOOL_LOG"' >"$fixture_root/bin/cabal-gild"
printf '%s\n' '#!/usr/bin/env bash' 'printf "%s\\n" "$*" >>"$FORMAT_TOOL_LOG"' 'exit 0' >"$fixture_root/bin/hlint"
printf '%s\n' '#!/usr/bin/env bash' 'printf "%s\\n" "$*" >>"$FORMAT_TOOL_LOG"' 'exit 0' >"$fixture_root/bin/ormolu"
chmod +x "$fixture_root/bin/cabal-gild" "$fixture_root/bin/hlint" "$fixture_root/bin/ormolu"

printf '%s\n' 'cabal-version: 3.0' 'name: fixture' 'version: 0' >"$fixture_root/only-cabal/packages/core/fixture.cabal"
printf '%s\n' 'module Fixture where' 'fixture = ()' >"$fixture_root/only-haskell/packages/core/Fixture.hs"
printf '%s\n' 'cabal-version: 3.0' 'name: fixture' 'version: 0' >"$fixture_root/complete/packages/core/fixture.cabal"
printf '%s\n' 'module Fixture where' 'fixture = ()' >"$fixture_root/complete/packages/core/Fixture.hs"
printf '%s\n' 'cabal-version: 3.0' 'name: fixture-example' 'version: 0' >"$fixture_root/complete/examples/example/fixture-example.cabal"
printf '%s\n' 'module FixtureExample where' 'fixtureExample = ()' >"$fixture_root/complete/examples/example/FixtureExample.hs"
printf '%s\n' 'cabal-version: 3.0' 'name: fixture-crlf' 'version: 0' >"$fixture_root/crlf/packages/core/fixture-crlf.cabal"
printf 'module FixtureCrlf where\r\nfixtureCrlf = ()\r\n' >"$fixture_root/crlf/packages/core/FixtureCrlf.hs"

for fixture_name in empty only-cabal only-haskell complete crlf; do
  printf '%s\n' 'packages:' '  packages/core/' >"$fixture_root/$fixture_name/cabal.project"
done
printf '%s\n' 'packages:' '  packages/core/' '  examples/example/' >"$fixture_root/complete/cabal.project"

expect_failure() {
  local description="$1"
  local fixture_path="$2"
  local expected_message="$3"
  local output

  if output="$(FORMAT_TOOL_LOG="$fixture_root/tool.log" PATH="$fixture_root/bin:$PATH" "$formatting_script" --format-target-fixture "$fixture_path" 2>&1)"; then
    printf 'Formatting check unexpectedly accepted %s.\n' "$description" >&2
    exit 1
  fi
  if ! printf '%s' "$output" | grep -Fq "$expected_message"; then
    printf 'Formatting check did not explain %s.\n' "$description" >&2
    exit 1
  fi
}

expect_failure 'an empty formatting target' "$fixture_root/empty" 'Cabal project package directory does not exist:'
expect_failure 'a target with no Haskell files' "$fixture_root/only-cabal" 'No Haskell files were found for formatting checks.'
expect_failure 'a target with no Cabal files' "$fixture_root/only-haskell" 'No Cabal files were found for formatting checks.'
expect_failure 'a target with CRLF Haskell input' "$fixture_root/crlf" 'CR byte found; use LF line endings.'
if ! LC_ALL=C grep -q $'\r' "$fixture_root/crlf/packages/core/FixtureCrlf.hs"; then
  printf '%s\n' 'Formatting check unexpectedly changed CRLF fixture input.' >&2
  exit 1
fi

fixture_tool_log="$fixture_root/tool.log"
FORMAT_TOOL_LOG="$fixture_tool_log" PATH="$fixture_root/bin:$PATH" "$formatting_script" --format-target-fixture "$fixture_root/complete" >/dev/null
grep -Fq "$fixture_root/complete/packages/core/fixture.cabal" "$fixture_tool_log"
grep -Fq "$fixture_root/complete/examples/example/fixture-example.cabal" "$fixture_tool_log"
grep -Fq "$fixture_root/complete/examples/example/FixtureExample.hs" "$fixture_tool_log"

printf '%s\n' 'Formatting-check fixture checks passed.'
