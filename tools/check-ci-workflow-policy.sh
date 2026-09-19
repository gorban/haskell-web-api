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

step_body() {
  awk -v requested_step="$1" '
    $0 == "      - name: " requested_step { found = 1; next }
    found && /^      - name:/ { exit }
    found { print }
  ' "$workflow_path"
}

[[ -f "$workflow_path" ]] || fail "workflow does not exist: $workflow_path"

if grep -q '^permissions:' "$workflow_path"; then
  fail "workflow-level permissions are forbidden; grant permissions to the owning job only"
fi

if [[ "$(grep -E '^[[:space:]]+[[:alnum:]_-]+: write$' "$workflow_path")" != $'      pages: write\n      id-token: write' ]]; then
  fail "Pages write and OIDC write permissions must exist exactly once, in publish-coverage"
fi

while IFS= read -r uses_line; do
  action_reference="$(sed -E 's/^[[:space:]]*uses:[[:space:]]*//' <<<"$uses_line")"
  if [[ ! "$action_reference" =~ ^[^@[:space:]]+@[0-9a-f]{40}([[:space:]]+\#.*)?$ ]]; then
    fail "external actions must use a full immutable commit SHA: $action_reference"
  fi
done < <(grep -E '^[[:space:]]*uses:' "$workflow_path")

build_job="$(job_body build-and-test)"
[[ -n "$build_job" ]] || fail "build-and-test job is missing"
grep -Fq "hashFiles('**/*.cabal', 'cabal.project', 'cabal.project.freeze')" <<<"$build_job" || fail "Cabal cache key must include the reviewed freeze file"
grep -Fq 'test -s cabal.project.freeze' <<<"$build_job" || fail "CI must require the freeze file before resolution"
grep -Fq 'cabal build all --dry-run' <<<"$build_job" || fail "CI must resolve the frozen plan before building"
grep -Fq 'tools/test-tls-compatibility-stack.sh' <<<"$build_job" || fail "CI must verify the released TLS compatibility stack"
grep -Fq 'cp cabal.project.freeze dist-newstyle/release/' <<<"$build_job" || fail "source artifacts must carry dependency provenance"
grep -Fq 'git archive --format=tar.gz --output=dist-newstyle/release/repository.tar.gz HEAD' <<<"$build_job" || fail "source artifacts must include the complete frozen repository"
if grep -Fq 'cabal install --ignore-project hspec-discover' <<<"$build_job"; then
  fail "test build tools must come from the frozen project plan"
fi
frozen_step="$(step_body 'Resolve frozen dependencies')"
grep -Fxq "        if: steps.cabal_cache.outcome == 'success'" <<<"$frozen_step" || fail "frozen resolution must run on cache hits as well as misses"
for frozen_command in 'test -s cabal.project.freeze' 'cabal update' 'cabal build all --dry-run'; do
  grep -Fxq "          $frozen_command" <<<"$frozen_step" || fail "frozen resolution step is missing: $frozen_command"
done
tls_compatibility_step="$(step_body 'Verify released TLS compatibility stack')"
grep -Fxq "        if: steps.cabal_check.outcome == 'success'" <<<"$tls_compatibility_step" || fail "TLS compatibility verification must wait for manifest checks"
grep -Fxq '          tools/test-tls-compatibility-stack.sh' <<<"$tls_compatibility_step" || fail "TLS compatibility step must run the isolated released-source suites"
source_step="$(step_body 'Prepare tested source packages')"
grep -Fxq "        if: steps.run_tests_skip_unit.outcome == 'success' && steps.vscode_ormolu_formatter_checks.outcome == 'success'" <<<"$source_step" || fail "source packages must wait for browser and formatting gates"
grep -Fxq '          git diff --exit-code HEAD' <<<"$source_step" || fail "source packages must match the archived commit, including staged changes"
[[ "$(permissions_body <<<"$build_job")" == '      contents: read' ]] || fail "build-and-test must have only contents: read"
if grep -Eq 'pages:|id-token:|configure-pages|upload-pages-artifact|deploy-pages' <<<"$build_job"; then
  fail "build-and-test must not hold or use Pages deployment authority"
fi

publish_job="$(job_body publish-coverage)"
[[ -n "$publish_job" ]] || fail "publish-coverage job is missing"
grep -Fq "if: github.ref == 'refs/heads/main'" <<<"$publish_job" || fail "publish-coverage must run only on main"
grep -Fq 'needs: build-and-test' <<<"$publish_job" || fail "publish-coverage must depend on build-and-test"
[[ "$(permissions_body <<<"$publish_job")" == $'      contents: read\n      pages: write\n      id-token: write' ]] || fail "publish-coverage must own only read, Pages write, and OIDC permissions"
if grep -Eq '^[[:space:]]+run:' <<<"$publish_job"; then
  fail "publish-coverage must only publish the tested artifact, not run repository code"
fi
grep -Fq 'actions/download-artifact@' <<<"$publish_job" || fail "publish-coverage must download the build artifact"
grep -Fq 'actions/configure-pages@' <<<"$publish_job" || fail "publish-coverage must configure Pages"
grep -Fq 'actions/upload-pages-artifact@' <<<"$publish_job" || fail "publish-coverage must create a Pages artifact"
grep -Fq 'actions/deploy-pages@' <<<"$publish_job" || fail "publish-coverage must deploy Pages"

printf '%s\n' 'CI workflow permission and action-pin policy passed.'
