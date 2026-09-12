#!/usr/bin/env bash

# Exercises the released source packages reached by the frozen TLS plan.  This
# deliberately uses an isolated CABAL_DIR: the temporary Serialise and Primitive
# test-source patches cannot supply, or leave behind, a runtime dependency for
# this repository's frozen Hackage plan.
# Track https://github.com/well-typed/cborg/pull/385 plus the fixes on master.
# Once fixed public releases are pinned, remove obsolete bound/flag overrides,
# the duplicate-orphan patch and warning allowances after both upstream suites
# pass unpatched with -Werror against the runtime dependency plan. Keep the
# suite checks and application regressions; run the full gates before retirement.
# Primitive-0.9.1.0's released test suite enables deprecated TypeInType under
# GHC 9.14, while three compatibility wrappers emit deprecations. Apply only
# https://github.com/haskell/primitive/issues/447's test pragma replacement and
# https://github.com/haskell/primitive/pull/434's three module-local warning
# settings in the disposable test source. After a public release includes both,
# refresh the freeze entry and run unpatched test-qc with -Werror plus the full
# gates; retain all test cases.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
temporary_directory="$(mktemp -d)"
isolated_cabal_directory="$temporary_directory/cabal"
cborg_directory="$temporary_directory/cborg-0.2.10.0"
serialise_directory="$temporary_directory/serialise-0.2.6.1"
primitive_directory="$temporary_directory/primitive-0.9.1.0"

cleanup() {
  local exit_status="$?"
  set +e
  if [ -d "$cborg_directory" ]; then
    (cd "$cborg_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/cborg-build")
  fi
  if [ -d "$serialise_directory" ]; then
    (cd "$serialise_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/serialise-build")
  fi
  if [ -d "$primitive_directory" ]; then
    (cd "$primitive_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/primitive-build")
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
CABAL_DIR="$isolated_cabal_directory" cabal unpack cborg-0.2.10.0 serialise-0.2.6.1 primitive-0.9.1.0 --destdir="$temporary_directory"

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

cat > "$primitive_directory/cabal.project.local" <<'EOF'
package primitive
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

# Primitive-0.9.1.0 has no released GHC 9.14 strict-suite update.  Restrict the
# temporary patch to its test pragma and the three deprecated compatibility
# wrappers changed by upstream #434; no test case, runtime source, or dependency
# constraint is removed.  The isolated store is deleted below, and the final
# dry-run proves the repository still resolves the public Hackage tarball.
perl -0pi -e '
  s/\{\-# LANGUAGE TypeInType #\-\}/\{\-# LANGUAGE DataKinds #\-\}\n\{\-# LANGUAGE PolyKinds #\-\}/;
' "$primitive_directory/test/Main.hs"
perl -0pi -e '
  s/(\{\-# LANGUAGE RankNTypes #\-\}\n)/$1\n\{\-# OPTIONS_GHC -Wno-deprecations #\-\}\n/;
' "$primitive_directory/Data/Primitive/ByteArray.hs"
perl -0pi -e '
  s/(\{\-# LANGUAGE RoleAnnotations #\-\}\n)/$1\n\{\-# OPTIONS_GHC -Wno-deprecations #\-\}\n/;
' "$primitive_directory/Data/Primitive/PrimArray.hs"
perl -0pi -e '
  s/(\{\-# LANGUAGE TemplateHaskellQuotes #\-\}\n)/$1\n\{\-# OPTIONS_GHC -Wno-deprecations #\-\}\n/;
' "$primitive_directory/Data/Primitive/SmallArray.hs"
if grep -Fq 'TypeInType' "$primitive_directory/test/Main.hs"; then
  printf '%s\n' 'Primitive test-only TypeInType patch did not apply.' >&2
  exit 1
fi
if [ "$(grep -Rl --include='*.hs' 'OPTIONS_GHC -Wno-deprecations' "$primitive_directory" | sort | wc -l)" -ne 3 ]; then
  printf '%s\n' 'Primitive test-only deprecation patch changed an unexpected number of modules.' >&2
  exit 1
fi

run_package_tests() {
  local package_directory="$1"
  local package_name="$2"
  local build_directory="$3"
  local test_target="$4"
  local build_log="$temporary_directory/$package_name.log"
  local package_warning_log="$temporary_directory/$package_name-warnings.log"

  if ! (
    cd "$package_directory"
    if [ "$package_name" = 'primitive-0.9.1.0' ]; then
      CABAL_DIR="$isolated_cabal_directory" cabal test "$test_target" --builddir="$build_directory" --ghc-options=-Werror
    else
      CABAL_DIR="$isolated_cabal_directory" cabal test "$test_target" --builddir="$build_directory"
    fi
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
    primitive-0.9.1.0)
      grep -E '^(Control/Monad/Primitive|Data/Primitive)/(ByteArray|PrimArray|SmallArray)\.hs:.*warning:' "$build_log" > "$package_warning_log" || :
      ;;
  esac
  if [ "$package_name" = 'primitive-0.9.1.0' ]; then
    "$diagnostic_gate" "$package_warning_log"
  else
    "$diagnostic_gate" --allow-ghc-9-14-tls-compatibility-stack="$package_name" "$package_warning_log"
  fi
}

run_package_tests "$cborg_directory" 'cborg-0.2.10.0' "$temporary_directory/cborg-build" test:tests
run_package_tests "$serialise_directory" 'serialise-0.2.6.1' "$temporary_directory/serialise-build" test:tests
run_package_tests "$primitive_directory" 'primitive-0.9.1.0' "$temporary_directory/primitive-build" test:test-qc

runtime_plan_directory="$temporary_directory/runtime-plan"
cabal build harch-web:lib:harch-web --dry-run --builddir="$runtime_plan_directory"
jq --exit-status '
  .["install-plan"] as $packages
  | all(
      ["cborg", "serialise", "primitive"][];
      . as $package_name
      | any(
          $packages[];
          .["pkg-name"] == $package_name
            and .["pkg-src"].type == "repo-tar"
        )
    )
' "$runtime_plan_directory/cache/plan.json" >/dev/null

printf '%s\n' 'Released TLS compatibility-stack suites passed; temporary source builds were isolated from the runtime plan.'
