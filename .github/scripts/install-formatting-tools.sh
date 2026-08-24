#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
observed_runner="$repo_root/tools/run-observed-command.sh"

run_observed() {
  "$observed_runner" --timeout 10m --label "$1" -- "${@:2}"
}

run_observed 'Install cabal-gild' env LANG=C.UTF-8 cabal install cabal-gild-1.8.4.1 --install-method=copy --overwrite-policy=always --ignore-project

temp_dir="$(mktemp -d)"
trap 'rm -rf "$temp_dir"' EXIT

run_observed 'Download and install HLint' bash -c '
  set -euo pipefail
  curl -fsSL https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-linux.tar.gz | tar -xzf - -C "$1"
  install -Dm755 "$1/hlint-3.10/hlint" "$HOME/.cabal/bin/hlint"
' -- "$temp_dir"

run_observed 'Download and install Ormolu' bash -c '
  set -euo pipefail
  curl -fsSL https://github.com/tweag/ormolu/archive/2164be7c68086e647c2c001a12a2f6f51c214ff0.tar.gz | tar -xzf - -C "$1"
  cd "$1/ormolu-2164be7c68086e647c2c001a12a2f6f51c214ff0"
  LANG=C.UTF-8 cabal install exe:ormolu --install-method=copy --overwrite-policy=always
' -- "$temp_dir"
