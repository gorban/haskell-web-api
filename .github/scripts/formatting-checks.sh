#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

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

while IFS= read -r cabal_file; do
  output="$(cabal-gild "$cabal_file" --mode check 2>&1)" || {
    printf '%s: %s\n\n' "$cabal_file" "$output"
    format_ok=1
  }
done < <(find packages -name '*.cabal' -type f | grep -v '^packages/hspec-expectations-match')

while IFS= read -r haskell_file; do
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
done < <(find packages -name '*.hs' -type f | grep -v '^packages/hspec-expectations-match')

if [ "$format_ok" -ne 0 ]; then
  echo 'Found formatting issues in packages/ files'
  exit 1
fi

echo 'No formatting issues in packages/ files'
