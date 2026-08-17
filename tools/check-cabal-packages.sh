#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
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

for package_directory in "${package_directories[@]}"; do
  printf 'Checking package manifest: %s\n' "$package_directory"
  (
    cd "$repo_root/$package_directory"
    cabal check
  )
done

printf '%s\n' 'All project-owned package manifests passed cabal check.'
