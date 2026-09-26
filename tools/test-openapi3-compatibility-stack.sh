#!/usr/bin/env bash

# Rebuild the public OpenAPI data-model releases selected by AHI-4E in an
# isolated store. openapi3-3.2.5 and insert-ordered-containers-0.3.0 have
# stale Aeson upper bounds, so the frozen plan permits only their two exact
# Aeson edges. openapi3 additionally emits one GHC-9.14 -Wx-partial header;
# keep it visible and admit only that exact source location through the shared
# diagnostic gate. No source is patched, no test case is removed, and cleanup
# ensures neither package can become a local runtime dependency by accident.
#
# Track https://github.com/biocad/openapi3/pull/120 and
# https://github.com/erikd/insert-ordered-containers/issues/10. Remove this
# verifier's bound and warning allowances only after public releases widen the
# bounds, fix the partial warning, and pass these unmodified suites with
# -Werror and the repository's complete gates.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
temporary_directory="$(mktemp -d)"
isolated_cabal_directory="$temporary_directory/cabal"
openapi_directory="$temporary_directory/openapi3-3.2.5"
ordered_containers_directory="$temporary_directory/insert-ordered-containers-0.3.0"
build_log="$temporary_directory/build.log"
openapi_warning_log="$temporary_directory/openapi3-warnings.log"

cleanup() {
  local exit_status="$?"
  set +e
  if [ -d "$openapi_directory" ]; then
    (cd "$openapi_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/build")
  fi
  rm -rf "$temporary_directory"
  exit "$exit_status"
}
trap cleanup EXIT

if [ "$(ghc --numeric-version)" != '9.14.1' ]; then
  printf 'OpenAPI compatibility source tests require GHC 9.14.1; found %s\n' "$(ghc --numeric-version)" >&2
  exit 2
fi

mkdir -p "$isolated_cabal_directory"
cat > "$isolated_cabal_directory/config" <<'CONFIG'
repository hackage.haskell.org
  url: https://hackage.haskell.org/
  secure: True
CONFIG

CABAL_DIR="$isolated_cabal_directory" cabal update
CABAL_DIR="$isolated_cabal_directory" cabal unpack openapi3-3.2.5 insert-ordered-containers-0.3.0 --destdir="$temporary_directory"

grep -Fxq 'version:             3.2.5' "$openapi_directory/openapi3.cabal"
grep -Fxq 'version:            0.3.0' "$ordered_containers_directory/insert-ordered-containers.cabal"

cat > "$temporary_directory/cabal.project" <<EOF_PROJECT
packages:
  $openapi_directory
  $ordered_containers_directory
EOF_PROJECT
cat "$repo_root/cabal.project.freeze" >> "$temporary_directory/cabal.project"
cat >> "$temporary_directory/cabal.project" <<'EOF_PROJECT'
allow-newer:
  openapi3:aeson,
  insert-ordered-containers:aeson

package openapi3
  ghc-options: -Wno-error=x-partial
EOF_PROJECT

set +e
(
  cd "$temporary_directory"
  CABAL_DIR="$isolated_cabal_directory" cabal test openapi3 insert-ordered-containers \
    --test-show-details=direct \
    --ghc-options=-Werror
) >"$build_log" 2>&1
build_exit="$?"
set -e

cat "$build_log"

# A cold upstream build also compiles independent transitive Hackage packages.
# Their diagnostics are neither OpenAPI diagnostics nor an allowance for this
# repository. Attribute only headers from the unpacked OpenAPI source to its
# exact-header classifier, normalizing the absolute path that doctests print.
# Any new OpenAPI library or test warning remains fatal in that classifier.
awk -v source_prefix="$openapi_directory/" '
  index($0, source_prefix) == 1 && /warning:/ {
    print substr($0, length(source_prefix) + 1)
  }
' "$build_log" > "$openapi_warning_log"
"$diagnostic_gate" --allow-ghc-9-14-openapi3-partial=openapi3-3.2.5 "$openapi_warning_log"

if [ "$build_exit" != 0 ]; then
  exit "$build_exit"
fi

cabal build all --dry-run
printf '%s\n' 'Released OpenAPI compatibility stack passed.'
