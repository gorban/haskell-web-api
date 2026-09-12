#!/usr/bin/env bash

set -euo pipefail

tls_compatibility_stack_packages=()
while [[ "${1:-}" == --allow-ghc-9-14-tls-compatibility-stack=* ]]; do
  tls_compatibility_stack_package="${1#*=}"
  case "$tls_compatibility_stack_package" in
    cborg-0.2.10.0 | serialise-0.2.6.1)
      tls_compatibility_stack_packages+=("$tls_compatibility_stack_package")
      shift
      ;;
    *)
      printf 'unsupported TLS compatibility-stack package: %s\n' "$tls_compatibility_stack_package" >&2
      exit 2
      ;;
  esac
done

if [ "$#" != 1 ]; then
  printf 'usage: %s [--allow-ghc-9-14-tls-compatibility-stack=cborg-0.2.10.0|serialise-0.2.6.1] <build-log>\n' "$0" >&2
  exit 2
fi

build_log="$1"

if [ ! -f "$build_log" ]; then
  printf 'build log does not exist: %s\n' "$build_log" >&2
  exit 2
fi

is_documented_hpc_deprecation_warning() {
  local index="$1"
  local -n log_lines="$2"

  [ "${log_lines[index]}" = 'Deprecation warning:' ] \
    && [ "${log_lines[index + 1]:-}" = 'I am reading in the existing tix file, and will add hpc info from this run to the existing data in that file.' ] \
    && [ "${log_lines[index + 2]:-}" = 'GHC 9.14 will cease looking for an existing tix file by default.' ] \
    && [ "${log_lines[index + 3]:-}" = 'If you positively want to add hpc info to the current tix file, use the RTS option --read-tix-file=yes.' ] \
    && [ "${log_lines[index + 4]:-}" = 'More information can be found in the accepted GHC proposal 612.' ]
}

is_documented_tls_compatibility_warning() {
  local line="$1"
  local package_name

  for package_name in "${tls_compatibility_stack_packages[@]}"; do
    case "$package_name:$line" in
      'cborg-0.2.10.0:src/Codec/CBOR/ByteArray/Sliced.hs:44:1: warning: [GHC-66111] [-Wunused-imports]' | \
        'cborg-0.2.10.0:src/Codec/CBOR/ByteArray.hs:39:1: warning: [GHC-66111] [-Wunused-imports]' | \
        'cborg-0.2.10.0:src/Codec/CBOR/Read.hs:90:23: warning: [GHC-90584] [-Wderiving-typeable]' | \
        'serialise-0.2.6.1:src/Codec/Serialise/Internal/GeneralisedUTF8.hs:78:15: warning: [GHC-68441] [-Wdeprecations]' | \
        'serialise-0.2.6.1:src/Codec/Serialise/Class.hs:1370:1: warning: [GHC-53633] [-Woverlapping-patterns]')
        return 0
        ;;
    esac
  done

  return 1
}

diagnostic_failure=false
hpc_deprecation_count=0
hpc_deprecation_summary=''
tls_compatibility_warning_count=0
mapfile -t lines < "$build_log"
for ((index = 0; index < ${#lines[@]}; index += 1)); do
  line="${lines[index]}"
  if is_documented_hpc_deprecation_warning "$index" lines; then
    hpc_deprecation_count=$((hpc_deprecation_count + 1))
    if [ -z "$hpc_deprecation_summary" ]; then
      hpc_deprecation_summary="${lines[index + 1]}"
    fi
    index=$((index + 4))
    continue
  fi

  case "$line" in
    *[Ww][Aa][Rr][Nn][Ii][Nn][Gg]:*)
      if is_documented_tls_compatibility_warning "$line"; then
        tls_compatibility_warning_count=$((tls_compatibility_warning_count + 1))
      else
        printf 'Actionable build warning: %s\n' "$line" >&2
        diagnostic_failure=true
      fi
      ;;
  esac
done

if [ "$hpc_deprecation_count" -gt 0 ]; then
  printf 'x%d Documented external GHC HPC deprecation warning: %s\n' "$hpc_deprecation_count" "$hpc_deprecation_summary" >&2
fi

if [ "$tls_compatibility_warning_count" -gt 0 ]; then
  printf 'x%d Exact GHC 9.14 TLS compatibility-stack warning(s) accepted.\n' "$tls_compatibility_warning_count" >&2
fi

if "$diagnostic_failure"; then
  printf '%s\n' 'Actionable build warnings found.' >&2
  exit 1
fi

printf '%s\n' 'No actionable build warnings found.'
