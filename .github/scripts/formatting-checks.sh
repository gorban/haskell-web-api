#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

format_root="$repo_root"
case "${1:-}" in
  '') ;;
  --format-target-fixture)
    if [ "$#" != 2 ]; then
      printf 'usage: %s --format-target-fixture <directory>\n' "$0" >&2
      exit 2
    fi
    format_root="$2"
    ;;
  *)
    printf 'usage: %s [--format-target-fixture <directory>]\n' "$0" >&2
    exit 2
    ;;
esac

if [ "$format_root" = "$repo_root" ]; then
  "$repo_root/tools/test-formatting-checks.sh"
fi

required_commands=(cabal-gild hlint ormolu)
missing_commands=()

for command_name in "${required_commands[@]}"; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    missing_commands+=("$command_name")
  fi
done

if [ "${#missing_commands[@]}" -ne 0 ]; then
  printf 'Missing required formatting tools: %s\n' "${missing_commands[*]}" >&2
  printf 'Install them using the instructions in SETUP.md before committing.\n' >&2
  exit 1
fi

if ! cabal-gild --help 2>&1 | grep -q '^Usage: cabal-gild \[OPTIONS\] \[FILE \.\.\.\]$'; then
  printf '%s\n' 'cabal-gild with positional FILE arguments is required for formatting checks.' >&2
  printf '%s\n' 'Run .github/scripts/install-formatting-tools.sh to install the pinned version used by CI.' >&2
  exit 1
fi

format_ok=0

project_file="$format_root/cabal.project"
if [ ! -f "$project_file" ]; then
  printf 'Formatting target root has no cabal.project file: %s\n' "$format_root" >&2
  exit 1
fi

mapfile -t package_directories < <(
  awk '
    /^packages:[[:space:]]*$/ { collecting = 1; next }
    collecting && /^[[:space:]]+/ { print $1; next }
    collecting { exit }
  ' "$project_file"
)

if [ "${#package_directories[@]}" -eq 0 ]; then
  printf 'No package directories were found in cabal.project: %s\n' "$project_file" >&2
  exit 1
fi

cabal_files=()
haskell_files=()
for package_directory in "${package_directories[@]}"; do
  case "$package_directory" in
    packages/hspec-expectations-match/) continue ;;
  esac

  package_root="$format_root/$package_directory"
  if [ ! -d "$package_root" ]; then
    printf 'Cabal project package directory does not exist: %s\n' "$package_root" >&2
    exit 1
  fi

  while IFS= read -r cabal_file; do
    cabal_files+=("$cabal_file")
  done < <(find "$package_root" -name '*.cabal' -type f -print | sort)

  while IFS= read -r haskell_file; do
    haskell_files+=("$haskell_file")
  done < <(find "$package_root" -name '*.hs' -type f -print | sort)
done

if [ "${#cabal_files[@]}" -eq 0 ]; then
  printf '%s\n' 'No Cabal files were found for formatting checks.' >&2
  exit 1
fi

if [ "${#haskell_files[@]}" -eq 0 ]; then
  printf '%s\n' 'No Haskell files were found for formatting checks.' >&2
  exit 1
fi

for cabal_file in "${cabal_files[@]}"; do
  output="$(cabal-gild "$cabal_file" --mode check 2>&1)" || {
    printf '%s: %s\n\n' "$cabal_file" "$output"
    format_ok=1
  }
done

for haskell_file in "${haskell_files[@]}"; do
  if LC_ALL=C grep -q $'\r' "$haskell_file"; then
    printf '%s: CR byte found; use LF line endings.\n' "$haskell_file"
    format_ok=1
    continue
  fi
  output="$(hlint --language=ImportQualifiedPost "$haskell_file" 2>&1)" || {
    printf '%s\n' "$output"
    format_ok=1
    continue
  }
  output="$(ormolu -m check --ghc-opt=-XImportQualifiedPost "$haskell_file" 2>&1)" || {
    printf '%s\n' "$output"
    format_ok=1
  }
done

if [ "$format_ok" -ne 0 ]; then
  echo 'Found formatting issues in project-owned package files'
  exit 1
fi

echo 'No formatting issues in project-owned package files'
