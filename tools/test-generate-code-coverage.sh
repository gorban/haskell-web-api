#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
coverage_script="$repo_root/generate-code-coverage.sh"

expect_failure() {
  local description="$1"
  local project_fraction="$2"
  local aggregate_percentage="$3"

  if "$coverage_script" --coverage-gate-fixture "$project_fraction" "$aggregate_percentage"; then
    printf 'coverage gate unexpectedly accepted %s\n' "$description" >&2
    exit 1
  fi
}

expect_success() {
  local description="$1"
  local project_fraction="$2"
  local aggregate_percentage="$3"

  if ! "$coverage_script" --coverage-gate-fixture "$project_fraction" "$aggregate_percentage"; then
    printf 'coverage gate unexpectedly rejected %s\n' "$description" >&2
    exit 1
  fi
}

expect_failure 'a project report below 100%' '99/100' '100'
expect_failure 'an aggregate report below 100%' '100/100' '99'
expect_failure 'both project and aggregate reports below 100%' '99/100' '99'
expect_success 'complete project and aggregate reports' '100/100' '100'

printf '%s\n' 'Coverage gate fixture checks passed.'
