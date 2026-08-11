#!/usr/bin/env bash

set -euo pipefail

if [ "$#" != 1 ]; then
  printf 'usage: %s <build-log>\n' "$0" >&2
  exit 2
fi

build_log="$1"

if [ ! -f "$build_log" ]; then
  printf 'build log does not exist: %s\n' "$build_log" >&2
  exit 2
fi

is_documented_external_warning() {
  local line="$1"

  [[ "$line" =~ ^/usr/bin/ld(\.bfd)?:\ warning:\ type\ and\ size\ of\ dynamic\ symbol\ \`[[:alnum:]_]+_closure\'\ are\ not\ defined$ ]]
}

is_documented_hpc_deprecation_warning() {
  local index="$1"
  local -n log_lines="$2"

  [ "${log_lines[index]}" = 'Deprecation warning:' ] \
    && [ "${log_lines[index + 1]:-}" = 'I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.' ] \
    && [ "${log_lines[index + 2]:-}" = 'GHC 9.14 will cease looking for an existing tix file by default.' ] \
    && [ "${log_lines[index + 3]:-}" = 'If you positively want to add hpc info to the current tix file, use the RTS option --read-tix-file=yes.' ] \
    && [ "${log_lines[index + 4]:-}" = 'More information can be found in the accepted GHC proposal 612.' ]
}

diagnostic_failure=false
mapfile -t lines < "$build_log"
for ((index = 0; index < ${#lines[@]}; index += 1)); do
  line="${lines[index]}"
  if is_documented_hpc_deprecation_warning "$index" lines; then
    printf 'Documented external GHC HPC deprecation warning: %s\n' "$line" >&2
    index=$((index + 4))
    continue
  fi

  case "$line" in
    *warning:* | *Warning:*)
      if is_documented_external_warning "$line"; then
        printf 'Documented external GHC dynamic-link warning: %s\n' "$line" >&2
      else
        printf 'Actionable build warning: %s\n' "$line" >&2
        diagnostic_failure=true
      fi
      ;;
  esac
done

if "$diagnostic_failure"; then
  printf '%s\n' 'Actionable build warnings found.' >&2
  exit 1
fi

printf '%s\n' 'No actionable build warnings found.'
