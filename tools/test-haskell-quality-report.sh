#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

mkdir -p "$fixture_root/bin" "$fixture_root/tools" "$fixture_root/packages/core/src" "$fixture_root/packages/core/test" "$fixture_root/packages/harch-web/src" "$fixture_root/packages/harch-web/test" "$fixture_root/packages/test-core/src" "$fixture_root/packages/test-core/test" "$fixture_root/packages/web-api/src" "$fixture_root/packages/web-api/test" "$fixture_root/packages/hspec-expectations-match/src" "$fixture_root/examples"
git -C "$fixture_root" init --quiet
cp "$repo_root/tools/haskell-quality-report.sh" "$fixture_root/tools/haskell-quality-report.sh"

printf '%s\n' 'Either Left Right' 'Custom Rejected Accepted' >"$fixture_root/tools/haskell-quality-monads.conf"
printf '%s\n' 'module Fixture where' 'forward value = case value of Left problem -> Left problem; Right result -> Right result' 'forwardCustom value = case value of Rejected problem -> Rejected problem; Accepted result -> Accepted result' >"$fixture_root/packages/core/src/Fixture.hs"
printf '%s\n' 'module Spec where' 'spec = pure ()' 'helper value = if value then 1 else 2' >"$fixture_root/packages/core/test/Spec.hs"
printf '%s\n' 'module Vendor where' 'vendored value = case value of Left problem -> Left problem; Right result -> Right result' >"$fixture_root/packages/hspec-expectations-match/src/Vendor.hs"

printf '%s\n' '#!/usr/bin/env bash' 'printf "%s\\n" "$*" >>"$QUALITY_ARGON_CALLS"' 'printf "%s\\n" "fixture/Production.hs" "  1:1 productionHotspot - 12" "  2:1 spec - 120" "  3:1 helperHotspot - 9"' >"$fixture_root/bin/argon"
printf '%s\n' '#!/usr/bin/env bash' 'exit 0' >"$fixture_root/bin/hlint"
chmod +x "$fixture_root/bin/argon" "$fixture_root/bin/hlint" "$fixture_root/tools/haskell-quality-report.sh"

argon_calls="$fixture_root/argon-calls.txt"
report_output="$(cd "$fixture_root" && QUALITY_ARGON_CALLS="$argon_calls" PATH="$fixture_root/bin:$PATH" tools/haskell-quality-report.sh)"

printf '%s' "$report_output" | rg -q 'productionHotspot - 12'
printf '%s' "$report_output" | rg -q 'helperHotspot - 9'
if printf '%s' "$report_output" | rg -q 'spec - 120'; then
  printf '%s\n' 'quality report did not exclude top-level Hspec spec' >&2
  exit 1
fi
printf '%s' "$report_output" | rg -q 'Either \(Left/Right\)'
printf '%s' "$report_output" | rg -q 'Custom \(Rejected/Accepted\)'
if printf '%s' "$report_output" | rg -q 'Vendor.hs'; then
  printf '%s\n' 'quality report did not exclude vendored code' >&2
  exit 1
fi

test "$(rg -c -- '--min 11' "$argon_calls")" -eq 2
test "$(rg -c -- '--min 8' "$argon_calls")" -eq 2

printf '%s\n' 'Haskell quality report fixture checks passed.'
