#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
fixture_directory="$(mktemp -d)"
trap 'rm -rf "$fixture_directory"' EXIT

expect_success() {
  local description="$1"
  local fixture="$2"
  local expected_output="${3:-}"
  local diagnostic_output

  if diagnostic_output="$("$diagnostic_gate" "$fixture" 2>&1)"; then
    if [ -n "$expected_output" ] && [ "$diagnostic_output" != "$expected_output" ]; then
      printf 'FAIL: diagnostic gate output for %s did not match the expected summary\n' "$description" >&2
      printf 'expected:\n%s\nactual:\n%s\n' "$expected_output" "$diagnostic_output" >&2
      exit 1
    fi
    printf 'PASS: accepts %s\n' "$description"
  else
    printf 'FAIL: diagnostic gate unexpectedly rejected %s\n' "$description" >&2
    printf '%s\n' "$diagnostic_output" >&2
    exit 1
  fi
}

expect_failure() {
  local description="$1"
  local fixture="$2"
  local diagnostic_output

  if diagnostic_output="$("$diagnostic_gate" "$fixture" 2>&1)"; then
    printf 'FAIL: diagnostic gate unexpectedly accepted %s\n' "$description" >&2
    printf '%s\n' "$diagnostic_output" >&2
    exit 1
  else
    printf 'PASS: rejects %s\n' "$description"
  fi
}

clean_fixture="$fixture_directory/clean.log"
dynamic_link_warning_fixture="$fixture_directory/dynamic-link-warning.log"
hpc_deprecation_fixture="$fixture_directory/hpc-deprecation.log"
hpc_deprecation_repeated_fixture="$fixture_directory/hpc-deprecation-repeated.log"
compiler_warning_fixture="$fixture_directory/compiler-warning.log"
linker_warning_fixture="$fixture_directory/linker-warning.log"
near_match_linker_warning_fixture="$fixture_directory/near-match-linker-warning.log"
wrong_linker_path_fixture="$fixture_directory/wrong-linker-path.log"
unrecognized_deprecation_fixture="$fixture_directory/unrecognized-deprecation.log"
uppercase_warning_fixture="$fixture_directory/uppercase-warning.log"

: > "$clean_fixture"
printf '%s\n' "/usr/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost_closure' are not defined" > "$dynamic_link_warning_fixture"
printf '%s\n' \
  'Deprecation warning:' \
  'I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.' \
  'GHC 9.14 will cease looking for an existing tix file by default.' \
  'If you positively want to add hpc info to the current tix file, use the RTS option --read-tix-file=yes.' \
  'More information can be found in the accepted GHC proposal 612.' > "$hpc_deprecation_fixture"
for _ in $(seq 1 41); do
  cat "$hpc_deprecation_fixture"
done > "$hpc_deprecation_repeated_fixture"
printf '%s\n' 'src/App.hs:12:3: warning: unused binding' > "$compiler_warning_fixture"
printf '%s\n' '/usr/bin/ld.bfd: warning: libmissing.so, needed by app, not found' > "$linker_warning_fixture"
printf '%s\n' "/usr/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost' are not defined" > "$near_match_linker_warning_fixture"
printf '%s\n' "/usr/local/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost_closure' are not defined" > "$wrong_linker_path_fixture"
printf '%s\n' 'Deprecation warning:' 'An unrecognised deprecation must not be ignored.' > "$unrecognized_deprecation_fixture"
printf '%s\n' '/usr/bin/ld.bfd: WARNING: libmissing.so, needed by app, not found' > "$uppercase_warning_fixture"

expect_success 'a clean build log' "$clean_fixture"
expect_success \
  'the documented GHC HPC deprecation warning' \
  "$hpc_deprecation_fixture" \
  $'x1 Documented external GHC HPC deprecation warning: I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.\nNo actionable build warnings found.'
expect_success \
  'repeated documented GHC HPC deprecation warnings with one useful summary' \
  "$hpc_deprecation_repeated_fixture" \
  $'x41 Documented external GHC HPC deprecation warning: I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.\nNo actionable build warnings found.'
expect_failure 'a resolvable compiler warning' "$compiler_warning_fixture"
expect_failure 'an arbitrary linker warning' "$linker_warning_fixture"
expect_failure 'the former GHC dynamic-link warning' "$dynamic_link_warning_fixture"
expect_failure 'a closure warning without the generated closure suffix' "$near_match_linker_warning_fixture"
expect_failure 'a dynamic-link warning from an unapproved linker path' "$wrong_linker_path_fixture"
expect_failure 'an unrecognised deprecation warning' "$unrecognized_deprecation_fixture"
expect_failure 'an all-caps WARNING linker diagnostic' "$uppercase_warning_fixture"

printf '%s\n' 'Build diagnostic gate fixture checks passed.'
