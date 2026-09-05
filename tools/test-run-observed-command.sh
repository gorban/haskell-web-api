#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
runner="$repo_root/tools/run-observed-command.sh"
optimized_gate="$repo_root/tools/run-optimized-build-check.sh"
ci_workflow="$repo_root/.github/workflows/ci.yml"
formatter_installer="$repo_root/.github/scripts/install-formatting-tools.sh"

if ! grep -Fq 'tools/test-run-observed-command.sh' "$optimized_gate"; then
  printf '%s\n' 'Optimized build gate does not run observed-command fixture checks.' >&2
  exit 1
fi

if ! grep -Fq 'tools/seed-test-database.sh' "$ci_workflow"; then
  printf '%s\n' 'CI does not invoke the bounded database-seed wrapper.' >&2
  exit 1
fi

if ! grep -Fq 'tools/run-observed-command.sh' "$formatter_installer"; then
  printf '%s\n' 'Formatter installer does not invoke the observed-command wrapper.' >&2
  exit 1
fi

if ! grep -Fq "'Install cabal-gild' 5m" "$formatter_installer" \
  || ! grep -Fq "'Download and install HLint' 5m" "$formatter_installer" \
  || ! grep -Fq "'Download and install Ormolu' 20m" "$formatter_installer"; then
  printf '%s\n' 'Formatter installer does not keep its per-operation timeout policy.' >&2
  exit 1
fi

expect_failure() {
  local expected_status="$1"
  shift

  set +e
  failure_output="$("$@" 2>&1)"
  actual_status=$?
  set -e

  if [ "$actual_status" -ne "$expected_status" ]; then
    printf 'Expected exit status %s, got %s. Output:\n%s\n' "$expected_status" "$actual_status" "$failure_output" >&2
    exit 1
  fi
}

success_output="$($runner --label 'fixture success' --timeout 2s -- bash -c 'printf "stdout evidence\\n"; printf "stderr evidence\\n" >&2')"
printf '%s' "$success_output" | grep -Fq 'fixture success started'
printf '%s' "$success_output" | grep -Fq 'stdout evidence'
printf '%s' "$success_output" | grep -Fq 'stderr evidence'
printf '%s' "$success_output" | grep -Fq 'completed successfully'

expect_failure 23 "$runner" --label 'fixture failure' --timeout 2s -- bash -c 'printf "failure evidence\\n" >&2; exit 23'
printf '%s' "$failure_output" | grep -Fq 'failure evidence'
printf '%s' "$failure_output" | grep -Fq 'failed with exit status 23'
printf '%s' "$failure_output" | grep -Fq 'complete command output is above'

expect_failure 124 "$runner" --label 'fixture timeout' --timeout 1s -- bash -c 'printf "timeout evidence\\n"; sleep 3'
printf '%s' "$failure_output" | grep -Fq 'timeout evidence'
printf '%s' "$failure_output" | grep -Fq 'fixture timeout exceeded its 1s timeout'
printf '%s' "$failure_output" | grep -Fq 'complete command output is above'

printf '%s\n' 'Observed-command fixture checks passed.'
