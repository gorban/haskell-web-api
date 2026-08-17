#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
coverage_script="$repo_root/generate-code-coverage.sh"
coverage_wrapper="$repo_root/tools/run-code-coverage-check.sh"
ci_workflow="$repo_root/.github/workflows/ci.yml"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

if ! grep -Fq '"$repo_root/tools/test-generate-code-coverage.sh"' "$coverage_wrapper"; then
  printf '%s\n' 'Coverage wrapper does not run the coverage gate fixture checks.' >&2
  exit 1
fi

if ! grep -Fq 'tools/run-code-coverage-check.sh' "$ci_workflow"; then
  printf '%s\n' 'CI does not invoke the shared coverage wrapper.' >&2
  exit 1
fi

if grep -Fq './generate-code-coverage.sh' "$ci_workflow"; then
  printf '%s\n' 'CI still contains a separate coverage gate implementation.' >&2
  exit 1
fi

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

write_report_fixture() {
  local fixture_name="$1"
  local report_contents="$2"
  local report_path="$fixture_root/$fixture_name.html"

  printf '%s\n' "$report_contents" > "$report_path"
  printf '%s\n' "$report_path"
}

expect_report_failure() {
  local description="$1"
  local report_path="$2"

  if "$coverage_script" --coverage-report-fixture "$report_path"; then
    printf 'coverage report gate unexpectedly accepted %s\n' "$description" >&2
    exit 1
  fi
}

expect_report_success() {
  local description="$1"
  local report_path="$2"

  if ! "$coverage_script" --coverage-report-fixture "$report_path"; then
    printf 'coverage report gate unexpectedly rejected %s\n' "$description" >&2
    exit 1
  fi
}

expect_failure 'a project report below 100%' '99/100' '100'
expect_failure 'an aggregate report below 100%' '100/100' '99'
expect_failure 'both project and aggregate reports below 100%' '99/100' '99'
expect_success 'complete project and aggregate reports' '100/100' '100'
expect_success 'a project with no HPC counters and a complete aggregate report' '0/0' '100'

complete_report="$(write_report_fixture complete '<tr>Program Coverage Total 100/100 100/100 100/100</tr>')"
incomplete_report="$(write_report_fixture incomplete '<tr>Program Coverage Total 100/100 100/100</tr>')"
partial_report="$(write_report_fixture partial '<tr>Program Coverage Total 100/100 99/100 100/100</tr>')"
zero_total_report="$(write_report_fixture zero-total '<tr>Program Coverage Total 0/0 0/0 0/0</tr>')"
mixed_zero_total_report="$(write_report_fixture mixed-zero-total '<tr>Program Coverage Total 100/100 0/0 100/100</tr>')"
invalid_zero_total_report="$(write_report_fixture invalid-zero-total '<tr>Program Coverage Total 100/100 100/100 1/0</tr>')"

expect_report_success 'a complete three-category HPC report' "$complete_report"
expect_report_failure 'an HPC report missing a category' "$incomplete_report"
expect_report_failure 'an HPC report with incomplete coverage' "$partial_report"
expect_report_success 'a purely declarative HPC report with no counters' "$zero_total_report"
expect_report_success 'an HPC report with a valid empty alternative category' "$mixed_zero_total_report"
expect_report_failure 'an HPC report with a nonzero numerator and zero total' "$invalid_zero_total_report"

printf '%s\n' 'Coverage gate fixture checks passed.'
