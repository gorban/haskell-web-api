#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
checker="$repo_root/tools/check-ci-workflow-policy.sh"
workflow="$repo_root/.github/workflows/ci.yml"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

expect_rejection() {
  local description="$1"
  local fixture="$2"

  if "$checker" "$fixture" >/dev/null 2>&1; then
    printf '%s\n' "CI workflow policy unexpectedly accepted $description." >&2
    exit 1
  fi
}

"$checker" "$workflow"

mutable_action_fixture="$fixture_root/mutable-action.yml"
cp "$workflow" "$mutable_action_fixture"
sed -i -E '0,/(uses: actions\/checkout@)[^ #]+/s//\1v6/' "$mutable_action_fixture"
expect_rejection 'a mutable action reference' "$mutable_action_fixture"

broad_permission_fixture="$fixture_root/broad-permission.yml"
cp "$workflow" "$broad_permission_fixture"
sed -i '0,/      contents: read/s//      contents: write/' "$broad_permission_fixture"
expect_rejection 'a broad build permission' "$broad_permission_fixture"

for frozen_input in "'cabal.project.freeze'" 'test -s cabal.project.freeze' 'cabal build all --dry-run' 'cp cabal.project.freeze' 'git archive --format=tar.gz'; do
  frozen_fixture="$fixture_root/missing-frozen-input.yml"
  awk -v omitted="$frozen_input" 'index($0, omitted) == 0' "$workflow" > "$frozen_fixture"
  expect_rejection "a missing freeze-policy input: $frozen_input" "$frozen_fixture"
done

tls_compatibility_fixture="$fixture_root/missing-tls-compatibility.yml"
awk 'index($0, "tools/test-tls-compatibility-stack.sh") == 0' "$workflow" > "$tls_compatibility_fixture"
expect_rejection 'a missing released TLS compatibility verification' "$tls_compatibility_fixture"

cache_hit_fixture="$fixture_root/skip-frozen-on-hit.yml"
sed "s/if: steps.cabal_cache.outcome == 'success'$/if: steps.cabal_cache.outcome == 'success' \&\& steps.cabal_cache.outputs.cache-hit != 'true'/" "$workflow" > "$cache_hit_fixture"
expect_rejection 'skipping frozen resolution on cache hits' "$cache_hit_fixture"

ungated_packages_fixture="$fixture_root/ungated-source-packages.yml"
sed "s/if: steps.run_tests_skip_unit.outcome == 'success' \&\& steps.vscode_ormolu_formatter_checks.outcome == 'success'/if: always()/" "$workflow" > "$ungated_packages_fixture"
expect_rejection 'preparing source packages before required gates pass' "$ungated_packages_fixture"

staged_changes_fixture="$fixture_root/staged-source-changes.yml"
sed 's/git diff --exit-code HEAD/git diff --exit-code/' "$workflow" > "$staged_changes_fixture"
expect_rejection 'excluding staged changes from release provenance verification' "$staged_changes_fixture"

printf '%s\n' 'CI workflow policy fixture checks passed.'
