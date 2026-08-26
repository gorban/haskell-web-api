#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
core_test_support_files=(
  src/TestCore/Prelude.hs
  src/TestCore/CustomAssertions.hs
)
package_directories=(
  examples/custom-api
  examples/custom-db-adapter
  examples/localization
  examples/multipart-upload
  examples/two-pages
  packages/core
  packages/harch-web
  packages/test-core
  packages/test-spec-preprocessor
  packages/web-api
)
simple_setup_packages=(
  packages/core
  packages/harch-web
  packages/test-core
)

for support_file in "${core_test_support_files[@]}"; do
  source_file="$repo_root/packages/test-core/$support_file"
  snapshot_file="$repo_root/packages/core/test-core-src/$support_file"
  if ! cmp -s "$source_file" "$snapshot_file"; then
    printf 'Core test-support snapshot differs from test-core source: %s\n' "$support_file" >&2
    exit 1
  fi
done

for package_directory in "${simple_setup_packages[@]}"; do
  manifest=$(find "$repo_root/$package_directory" -maxdepth 1 -name '*.cabal' -type f -print -quit)
  if ! grep -q '^build-type: Simple$' "$manifest"; then
    printf 'Package must use Cabal Simple build type after Custom-setup removal: %s\n' "$package_directory" >&2
    exit 1
  fi
done

web_api_manifest="$repo_root/packages/web-api/haskell-web-api.cabal"
if ! grep -Fxq 'build-type: Hooks' "$web_api_manifest" \
  || ! grep -Fxq '    Cabal-hooks ==3.16.*,' "$web_api_manifest" \
  || [ ! -f "$repo_root/packages/web-api/Setup.hs" ] \
  || [ ! -f "$repo_root/packages/web-api/SetupHooks.hs" ]; then
  printf '%s\n' 'web-api must use its checked-in Cabal Hooks setup implementation.' >&2
  exit 1
fi

for package_directory in "${package_directories[@]}"; do
  printf 'Checking package manifest: %s\n' "$package_directory"
  if ! check_output="$(
    cd "$repo_root/$package_directory"
    cabal check 2>&1
  )"; then
    printf '%s\n' "$check_output" >&2
    exit 1
  fi
  printf '%s\n' "$check_output"
  if grep -q '^Warning:' <<<"$check_output"; then
    printf 'Package manifest emitted an unapproved cabal check warning: %s\n' "$package_directory" >&2
    exit 1
  fi
done

printf '%s\n' 'All project-owned package manifests passed cabal check with no warnings.'
