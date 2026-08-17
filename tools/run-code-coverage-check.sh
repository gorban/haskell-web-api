#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
coverage_log="$(mktemp)"
trap 'rm -f "$coverage_log"' EXIT

# This wrapper is the single coverage boundary for local development and CI.
# Keep its predicate self-test here so either caller proves the gate can reject
# malformed or incomplete coverage before spending time on the real rebuild.
"$repo_root/tools/test-generate-code-coverage.sh"

set +e
"$repo_root/generate-code-coverage.sh" >"$coverage_log" 2>&1
coverage_exit=$?
set -e

cat "$coverage_log"
"$repo_root/tools/check-build-diagnostics.sh" "$coverage_log"

if [ "$coverage_exit" != 0 ]; then
  exit "$coverage_exit"
fi
