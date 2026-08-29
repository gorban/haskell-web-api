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

check_test_module_metadata() {
  local package_directory="$1"
  local component_name="$2"
  shift 2
  local manifest="$repo_root/$package_directory/$(basename "$package_directory").cabal"

  case "$package_directory" in
    packages/web-api) manifest="$repo_root/$package_directory/haskell-web-api.cabal" ;;
    packages/harch-web) manifest="$repo_root/$package_directory/harch-web.cabal" ;;
    examples/multipart-upload) manifest="$repo_root/$package_directory/multipart-upload-example.cabal" ;;
    examples/two-pages) manifest="$repo_root/$package_directory/two-pages-example.cabal" ;;
  esac

  local expected_modules
  expected_modules="$(
    cd "$repo_root/$package_directory"
    for source_directory in "$@"; do
      while IFS= read -r source_path; do
        source_path=${source_path#"$source_directory"/}
        source_path=${source_path%.hs}
        printf '%s\n' "${source_path//\//.}"
      done < <(rg --files "$source_directory" -g '*.hs' | rg -v '/Main\.hs$')
    done | LC_ALL=C sort -u
  )"
  local declared_modules
  declared_modules="$(
    awk -v component_name="$component_name" '
      $0 == "test-suite " component_name { in_component = 1; next }
      in_other_modules && $0 ~ /^    [[:alnum:]_.]+$/ { print $1; next }
      in_other_modules { exit }
      in_component && $0 != "" && $0 !~ /^[[:space:]]/ { exit }
      in_component && $0 == "  other-modules:" { in_other_modules = 1; next }
    ' "$manifest" | LC_ALL=C sort -u
  )"

  if ! diff -u <(printf '%s\n' "$expected_modules") <(printf '%s\n' "$declared_modules"); then
    printf 'Test component module metadata differs from source tree: %s:%s\n' "$package_directory" "$component_name" >&2
    exit 1
  fi
}

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

# The SPEC preprocessor injects test module headers, but Cabal still owns the
# complete home-module graph used by hspec-discover. Keep it in manifests and
# fail the manifest gate on either a missing or stale entry.
check_test_module_metadata packages/harch-web harch-web-tests test
check_test_module_metadata packages/web-api haskell-web-api-tests test
check_test_module_metadata packages/test-core test-core-tests test
check_test_module_metadata packages/core core-tests test test-core-src/src
check_test_module_metadata examples/multipart-upload multipart-upload-example-tests test
check_test_module_metadata examples/two-pages two-pages-example-tests test

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
