#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
fixture_directory="$(mktemp -d)"
trap 'rm -rf "$fixture_directory"' EXIT

expect_success() {
  local description="$1"
  local fixture="$2"

  if ! "$diagnostic_gate" "$fixture"; then
    printf 'diagnostic gate unexpectedly rejected %s\n' "$description" >&2
    exit 1
  fi
}

expect_failure() {
  local description="$1"
  local fixture="$2"

  if "$diagnostic_gate" "$fixture"; then
    printf 'diagnostic gate unexpectedly accepted %s\n' "$description" >&2
    exit 1
  fi
}

clean_fixture="$fixture_directory/clean.log"
external_fixture="$fixture_directory/external.log"
hpc_deprecation_fixture="$fixture_directory/hpc-deprecation.log"
compiler_warning_fixture="$fixture_directory/compiler-warning.log"
linker_warning_fixture="$fixture_directory/linker-warning.log"
near_match_linker_warning_fixture="$fixture_directory/near-match-linker-warning.log"
wrong_linker_path_fixture="$fixture_directory/wrong-linker-path.log"
unrecognized_deprecation_fixture="$fixture_directory/unrecognized-deprecation.log"

: > "$clean_fixture"
printf '%s\n' "/usr/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost_closure' are not defined" > "$external_fixture"
printf '%s\n' \
  'Deprecation warning:' \
  'I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.' \
  'GHC 9.14 will cease looking for an existing tix file by default.' \
  'If you positively want to add hpc info to the current tix file, use the RTS option --read-tix-file=yes.' \
  'More information can be found in the accepted GHC proposal 612.' > "$hpc_deprecation_fixture"
printf '%s\n' 'src/App.hs:12:3: warning: unused binding' > "$compiler_warning_fixture"
printf '%s\n' '/usr/bin/ld.bfd: warning: libmissing.so, needed by app, not found' > "$linker_warning_fixture"
printf '%s\n' "/usr/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost' are not defined" > "$near_match_linker_warning_fixture"
printf '%s\n' "/usr/local/bin/ld.bfd: warning: type and size of dynamic symbol \`harchzmwebzm0zi1zi2zi0zminplace_HarchWebziEmail_smtpServerHost_closure' are not defined" > "$wrong_linker_path_fixture"
printf '%s\n' 'Deprecation warning:' 'An unrecognised deprecation must not be ignored.' > "$unrecognized_deprecation_fixture"

expect_success 'a clean build log' "$clean_fixture"
expect_success 'the documented GHC dynamic-link warning' "$external_fixture"
expect_success 'the documented GHC HPC deprecation warning' "$hpc_deprecation_fixture"
expect_failure 'a resolvable compiler warning' "$compiler_warning_fixture"
expect_failure 'an arbitrary linker warning' "$linker_warning_fixture"
expect_failure 'a closure warning without the generated closure suffix' "$near_match_linker_warning_fixture"
expect_failure 'a dynamic-link warning from an unapproved linker path' "$wrong_linker_path_fixture"
expect_failure 'an unrecognised deprecation warning' "$unrecognized_deprecation_fixture"

printf '%s\n' 'Build diagnostic gate fixture checks passed.'
