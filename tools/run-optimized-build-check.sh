#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
build_log="$(mktemp)"
trap 'rm -f "$build_log"' EXIT

"$repo_root/tools/test-check-build-diagnostics.sh"

set +e
cabal build all -O2 --ghc-options=-Werror >"$build_log" 2>&1
build_exit=$?
set -e

cat "$build_log"
"$repo_root/tools/check-build-diagnostics.sh" "$build_log"

if [ "$build_exit" != 0 ]; then
  exit "$build_exit"
fi
