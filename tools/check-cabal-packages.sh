#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
core_test_support_files=(
  app/SpecPreprocessor.hs
  src/TestCore/SpecPreprocessor.hs
  src/TestCore/Prelude.hs
  src/TestCore/CustomAssertions.hs
)
package_directories=(
  examples/custom-api
  examples/custom-db-adapter
  examples/multipart-upload
  examples/two-pages
  packages/core
  packages/harch-web
  packages/test-core
  packages/web-api
)

for support_file in "${core_test_support_files[@]}"; do
  source_file="$repo_root/packages/test-core/$support_file"
  snapshot_file="$repo_root/packages/core/test-core-src/$support_file"
  if ! cmp -s "$source_file" "$snapshot_file"; then
    printf 'Core test-support snapshot differs from test-core source: %s\n' "$support_file" >&2
    exit 1
  fi
done

for package_directory in "${package_directories[@]}"; do
  printf 'Checking package manifest: %s\n' "$package_directory"
  (
    cd "$repo_root/$package_directory"
    cabal check
  )
done

printf '%s\n' 'All project-owned package manifests passed cabal check.'
