#!/usr/bin/env bash

set -euo pipefail

argon_commit="28ca07453e3c28c0b52c1025f96420f214c354c2"
argon_ghc="${ARGON_GHC:-ghc-9.10.3}"

for command_name in cabal curl "$argon_ghc"; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    printf 'Missing required command: %s\n' "$command_name" >&2
    exit 1
  fi
done

quality_temp_dir="$(mktemp -d)"
trap 'rm -rf "$quality_temp_dir"' EXIT

archive_path="$quality_temp_dir/argon.tar.gz"
source_directory="$quality_temp_dir/argon-$argon_commit"

curl -fsSL "https://github.com/rubik/argon/archive/$argon_commit.tar.gz" -o "$archive_path"
tar -xzf "$archive_path" -C "$quality_temp_dir"

(
  cd "$source_directory"
  LANG=C.UTF-8 cabal install exe:argon \
    --with-compiler="$argon_ghc" \
    --install-method=copy \
    --overwrite-policy=always
)

printf 'Installed Argon from commit %s using %s.\n' "$argon_commit" "$argon_ghc"
