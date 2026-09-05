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

printf '%s\n' 'CI workflow policy fixture checks passed.'
