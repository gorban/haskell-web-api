#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
build_log="$(mktemp)"
trap 'rm -f "$build_log"' EXIT

"$repo_root/tools/test-check-build-diagnostics.sh"

if ! command -v ld.lld >/dev/null; then
  printf '%s\n' 'LLVM lld is required for the optimized diagnostic gate; install an ld.lld executable before running this check.' >&2
  exit 2
fi

set +e
cabal build all -O2 --ghc-options="-Werror -optl-fuse-ld=lld" >"$build_log" 2>&1
build_exit=$?
set -e

cat "$build_log"
"$repo_root/tools/check-build-diagnostics.sh" "$build_log"

if [ "$build_exit" != 0 ]; then
  exit "$build_exit"
fi
