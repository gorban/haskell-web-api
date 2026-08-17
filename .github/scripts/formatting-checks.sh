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

required_commands=(cabal-gild hlint ormolu dos2unix)
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

if [ ! -d "$format_root/packages" ]; then
  printf 'Formatting target root has no packages directory: %s\n' "$format_root" >&2
  exit 1
fi

mapfile -t cabal_files < <(find "$format_root/packages" -name '*.cabal' -type f ! -path "$format_root/packages/hspec-expectations-match/*" -print | sort)
mapfile -t haskell_files < <(find "$format_root/packages" -name '*.hs' -type f ! -path "$format_root/packages/hspec-expectations-match/*" -print | sort)

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
  dos2unix -q "$haskell_file"
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
  echo 'Found formatting issues in packages/ files'
  exit 1
fi

echo 'No formatting issues in packages/ files'
