#!/usr/bin/env bash

set -euo pipefail

LANG=C.UTF-8 cabal install cabal-gild --install-method=copy --overwrite-policy=always --ignore-project

temp_dir="$(mktemp -d)"
trap 'rm -rf "$temp_dir"' EXIT

curl -fsSL https://github.com/ndmitchell/hlint/releases/download/v3.10/hlint-3.10-x86_64-linux.tar.gz | tar -xzf - -C "$temp_dir"
install -Dm755 "$temp_dir/hlint-3.10/hlint" "$HOME/.cabal/bin/hlint"

curl -fsSL https://github.com/tweag/ormolu/archive/2164be7c68086e647c2c001a12a2f6f51c214ff0.tar.gz | tar -xzf - -C "$temp_dir"
cd "$temp_dir/ormolu-2164be7c68086e647c2c001a12a2f6f51c214ff0"
LANG=C.UTF-8 cabal install exe:ormolu --install-method=copy --overwrite-policy=always
