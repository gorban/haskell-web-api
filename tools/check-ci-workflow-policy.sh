#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workflow_path="${1:-$repo_root/.github/workflows/ci.yml}"

fail() {
  printf '%s\n' "CI workflow policy failure: $1" >&2
  exit 1
}

job_body() {
  awk -v requested_job="$1" '
    $0 == "  " requested_job ":" { found = 1; next }
    found && /^  [[:alnum:]_-]+:$/ { exit }
    found { print }
  ' "$workflow_path"
}

permissions_body() {
  awk '
    /^    permissions:$/ { found = 1; next }
    found && /^    [[:alnum:]_-]+:/ { exit }
    found { print }
  '
}

[[ -f "$workflow_path" ]] || fail "workflow does not exist: $workflow_path"

if rg -q '^permissions:' "$workflow_path"; then
  fail "workflow-level permissions are forbidden; grant permissions to the owning job only"
fi

if [[ "$(rg '^[[:space:]]+[[:alnum:]_-]+: write$' "$workflow_path")" != $'      pages: write\n      id-token: write' ]]; then
  fail "Pages write and OIDC write permissions must exist exactly once, in publish-coverage"
fi

while IFS= read -r uses_line; do
  action_reference="$(sed -E 's/^[[:space:]]*uses:[[:space:]]*//' <<<"$uses_line")"
  if [[ ! "$action_reference" =~ ^[^@[:space:]]+@[0-9a-f]{40}([[:space:]]+\#.*)?$ ]]; then
    fail "external actions must use a full immutable commit SHA: $action_reference"
  fi
done < <(rg '^[[:space:]]*uses:' "$workflow_path")

build_job="$(job_body build-and-test)"
[[ -n "$build_job" ]] || fail "build-and-test job is missing"
[[ "$(permissions_body <<<"$build_job")" == '      contents: read' ]] || fail "build-and-test must have only contents: read"
if rg -q 'pages:|id-token:|configure-pages|upload-pages-artifact|deploy-pages' <<<"$build_job"; then
  fail "build-and-test must not hold or use Pages deployment authority"
fi

publish_job="$(job_body publish-coverage)"
[[ -n "$publish_job" ]] || fail "publish-coverage job is missing"
rg -Fq "if: github.ref == 'refs/heads/main'" <<<"$publish_job" || fail "publish-coverage must run only on main"
rg -Fq 'needs: build-and-test' <<<"$publish_job" || fail "publish-coverage must depend on build-and-test"
[[ "$(permissions_body <<<"$publish_job")" == $'      contents: read\n      pages: write\n      id-token: write' ]] || fail "publish-coverage must own only read, Pages write, and OIDC permissions"
if rg -q '^[[:space:]]+run:' <<<"$publish_job"; then
  fail "publish-coverage must only publish the tested artifact, not run repository code"
fi
rg -Fq 'actions/download-artifact@' <<<"$publish_job" || fail "publish-coverage must download the build artifact"
rg -Fq 'actions/configure-pages@' <<<"$publish_job" || fail "publish-coverage must configure Pages"
rg -Fq 'actions/upload-pages-artifact@' <<<"$publish_job" || fail "publish-coverage must create a Pages artifact"
rg -Fq 'actions/deploy-pages@' <<<"$publish_job" || fail "publish-coverage must deploy Pages"

printf '%s\n' 'CI workflow permission and action-pin policy passed.'
