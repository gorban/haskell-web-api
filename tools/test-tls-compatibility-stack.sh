#!/usr/bin/env bash

# Exercises the two released source packages admitted by cabal.project's
# bounded base-4.22 exceptions.  This deliberately uses an isolated CABAL_DIR:
# the temporary Serialise test-source patch cannot supply, or leave behind, a
# runtime dependency for this repository's frozen Hackage plan.
# Track https://github.com/well-typed/cborg/pull/385 plus the fixes on master.
# Once fixed public releases are pinned, remove obsolete bound/flag overrides,
# the duplicate-orphan patch and warning allowances after both upstream suites
# pass unpatched with -Werror against the runtime dependency plan. Keep the
# suite checks and application regressions; run the full gates before retirement.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
temporary_directory="$(mktemp -d)"
isolated_cabal_directory="$temporary_directory/cabal"
cborg_directory="$temporary_directory/cborg-0.2.10.0"
serialise_directory="$temporary_directory/serialise-0.2.6.1"

cleanup() {
  local exit_status="$?"
  set +e
  if [ -d "$cborg_directory" ]; then
    (cd "$cborg_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/cborg-build")
  fi
  if [ -d "$serialise_directory" ]; then
    (cd "$serialise_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/serialise-build")
  fi
  rm -rf "$temporary_directory"
  exit "$exit_status"
}
trap cleanup EXIT

if [ "$(ghc --numeric-version)" != '9.14.1' ]; then
  printf 'TLS compatibility source tests require GHC 9.14.1; found %s\n' "$(ghc --numeric-version)" >&2
  exit 2
fi

mkdir -p "$isolated_cabal_directory"
cat > "$isolated_cabal_directory/config" <<'EOF'
repository hackage.haskell.org
  url: https://hackage.haskell.org/
  secure: True
EOF

CABAL_DIR="$isolated_cabal_directory" cabal update
CABAL_DIR="$isolated_cabal_directory" cabal unpack cborg-0.2.10.0 serialise-0.2.6.1 --destdir="$temporary_directory"

cat > "$cborg_directory/cabal.project.local" <<'EOF'
allow-newer:
  cborg:base,
  cborg:bytestring,
  cborg:containers

package cborg
  tests: True
EOF

cat > "$serialise_directory/cabal.project.local" <<'EOF'
constraints:
  serialise -newtime15

allow-newer:
  serialise:base,
  serialise:array,
  serialise:bytestring,
  serialise:containers,
  serialise:ghc-prim,
  serialise:time,
  cborg:base,
  cborg:bytestring,
  cborg:containers,
  quickcheck-instances:base

package serialise
  tests: True
EOF

# quickcheck-instances supplies this Vector instance starting with 0.3.32.
# Remove only Serialise's duplicate orphan in this temporary test tree; the
# primitive-vector test cases remain. Retire this patch with a fixed public
# release (https://github.com/well-typed/cborg/pull/385 and master), not the tests.
# The released library source and repository runtime plan remain unmodified.
perl -0pi -e '
  s/import qualified Data\.Vector\.Primitive      as Vector\.Primitive\n//;
  s/instance \(Vector\.Primitive\.Prim a, Arbitrary a\n         \) => Arbitrary \(Vector\.Primitive\.Vector a\) where\n    arbitrary = Vector\.Primitive\.fromList <\$> arbitrary\n\n//;
' "$serialise_directory/tests/Tests/Orphanage.hs"
if grep -Fq 'Vector.Primitive' "$serialise_directory/tests/Tests/Orphanage.hs"; then
  printf '%s\n' 'Serialise test-only duplicate Vector orphan patch did not apply.' >&2
  exit 1
fi

run_package_tests() {
  local package_directory="$1"
  local package_name="$2"
  local build_directory="$3"
  local build_log="$temporary_directory/$package_name.log"
  local package_warning_log="$temporary_directory/$package_name-warnings.log"

  if ! (
    cd "$package_directory"
    CABAL_DIR="$isolated_cabal_directory" cabal test test:tests --builddir="$build_directory"
  ) 2>&1 | tee "$build_log"; then
    printf 'Released %s tests failed.\n' "$package_name" >&2
    exit 1
  fi

  case "$package_name" in
    cborg-0.2.10.0)
      grep -E '^src/Codec/CBOR/.*warning:' "$build_log" > "$package_warning_log" || :
      ;;
    serialise-0.2.6.1)
      grep -E '^src/Codec/Serialise/.*warning:' "$build_log" > "$package_warning_log" || :
      ;;
  esac
  "$diagnostic_gate" --allow-ghc-9-14-tls-compatibility-stack="$package_name" "$package_warning_log"
}

run_package_tests "$cborg_directory" 'cborg-0.2.10.0' "$temporary_directory/cborg-build"
run_package_tests "$serialise_directory" 'serialise-0.2.6.1' "$temporary_directory/serialise-build"

runtime_plan_directory="$temporary_directory/runtime-plan"
cabal build harch-web:lib:harch-web --dry-run --builddir="$runtime_plan_directory"
jq --exit-status '
  any(
    .["install-plan"][];
    .["pkg-name"] == "serialise"
      and .["pkg-src"].type == "repo-tar"
  )
' "$runtime_plan_directory/cache/plan.json" >/dev/null

printf '%s\n' 'Released TLS compatibility-stack suites passed; temporary source builds were isolated from the runtime plan.'
