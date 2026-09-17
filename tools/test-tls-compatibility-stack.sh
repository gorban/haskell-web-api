#!/usr/bin/env bash

# Exercises the released source packages reached by the frozen TLS plan.  This
# deliberately uses an isolated CABAL_DIR: the temporary Serialise, Primitive,
# and HTTP2 test-source patches cannot supply, or leave behind, a runtime
# dependency for this repository's frozen Hackage plan.
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
# HTTP2-5.4.4 is also tested only in this disposable source tree with the
# pending #176 lifecycle fix and time-manager-0.2.4. Its full upstream suite
# runs with -Werror, then the root dry-run proves runtime still uses the frozen
# public HTTP2/time-manager tarballs. Retire this test patch after a public
# HTTP2 release contains the fix and passes the same unpatched suite.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
diagnostic_gate="$repo_root/tools/check-build-diagnostics.sh"
temporary_directory="$(mktemp -d)"
isolated_cabal_directory="$temporary_directory/cabal"
cborg_directory="$temporary_directory/cborg-0.2.10.0"
serialise_directory="$temporary_directory/serialise-0.2.6.1"
primitive_directory="$temporary_directory/primitive-0.9.1.0"
http2_directory="$temporary_directory/http2-5.4.4"

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
  if [ -d "$http2_directory" ]; then
    (cd "$http2_directory" && CABAL_DIR="$isolated_cabal_directory" cabal clean --builddir="$temporary_directory/http2-build")
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
CABAL_DIR="$isolated_cabal_directory" cabal unpack cborg-0.2.10.0 serialise-0.2.6.1 primitive-0.9.1.0 http2-5.4.4 --destdir="$temporary_directory"

# A Hackage cabal-file revision can silently switch a package's line endings
# to CRLF (observed on http2-5.4.4). Every patch below is a perl substitution
# anchored on a bare "\n", so CRLF input makes it match nothing instead of
# failing loudly; only the later exact-count verification catches it, as a
# constraint-count mismatch that does not explain the real cause. Normalize
# every unpacked source file to LF immediately after unpacking so the patches
# below apply the same way regardless of how upstream revised its line endings.
find "$cborg_directory" "$serialise_directory" "$primitive_directory" "$http2_directory" -type f \( -name '*.cabal' -o -name '*.hs' \) -print0 |
  xargs -0 sed -i 's/\r$//'

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

cat > "$http2_directory/cabal.project.local" <<'EOF'
constraints: time-manager ==0.2.4

package http2
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

# HTTP2-5.4.4 requires time-manager-0.3.x, where killManager is a deprecated
# no-op.  In a disposable source tree only, apply the pending #176 fix: retain
# the released 0.2.4 manager lifecycle and bracket the receiver handle so #169's
# changed withHandle result does not leak into HTTP2.  The lifecycle regression
# below is added to the upstream spec suite and no test case is removed.  This
# is executable review evidence for the proposed public fix, not a source
# dependency: cleanup deletes this tree and the final dry-run requires the
# frozen runtime to use Hackage tarballs.
perl -0pi -e '
  s/time-manager >=0\.3\.0 && <0\.4,/time-manager >=0.2.4 \&\& <0.3,/g;
  s/(        HTTP2\.ClientSpec\n)/$1        HTTP2.ConfigSpec\n/;
  s/(        random,\n)/$1        time-manager >=0.2.4 \&\& <0.3,\n/;
' "$http2_directory/http2.cabal"
perl -0pi -e '
  s/import qualified System\.ThreadManager as T\n/import qualified System.ThreadManager as T\nimport qualified System.TimeManager as Timeout\n/;
  s/T\.withHandle \(threadManager ctx\) \(E\.throwTo tid ConnectionIsTimeout\) loop2/E.bracket\n                    (Timeout.register confTimeoutManager (E.throwTo tid ConnectionIsTimeout))\n                    Timeout.cancel\n                    loop2/;
' "$http2_directory/Network/HTTP2/H2/Receiver.hs"
cat > "$http2_directory/test/HTTP2/ConfigSpec.hs" <<'EOF'
module HTTP2.ConfigSpec (spec) where

import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Network.HTTP2.Server (allocSimpleConfig', confTimeoutManager, freeSimpleConfig)
import qualified Network.Socket as Socket
import qualified System.TimeManager as TimeManager
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "simple configuration timeout ownership (#175)" $ do
  it "runs callbacks while the configuration is live" $
    withPair $ \socket ->
      bracket (allocSimpleConfig' socket 4096 10000) freeSimpleConfig $ \configuration -> do
        fired <- newEmptyMVar
        void $ TimeManager.withHandle (confTimeoutManager configuration) (putMVar fired ()) $ \_ ->
          timeout 2000000 (takeMVar fired) `shouldReturn` Just ()

  it "cancels callbacks when their handle scope ends before release" $
    withPair $ \socket -> do
      fired <- newEmptyMVar
      void $ bracket (allocSimpleConfig' socket 4096 1000000) freeSimpleConfig $ \configuration ->
        TimeManager.withHandle (confTimeoutManager configuration) (putMVar fired ()) $ \_ -> pure ()
      timeout 3000000 (takeMVar fired) `shouldReturn` Nothing

  it "cancels registered callbacks when freeSimpleConfig releases the configuration" $
    withPair $ \socket -> do
      fired <- newEmptyMVar
      handle <- bracket (allocSimpleConfig' socket 4096 1000000) freeSimpleConfig $ \configuration ->
        TimeManager.register (confTimeoutManager configuration) (putMVar fired ())
      (timeout 3000000 (takeMVar fired) `shouldReturn` Nothing)
        `finally` TimeManager.cancel handle

withPair :: (Socket.Socket -> IO a) -> IO a
withPair action = Socket.withSocketsDo $
  bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \listener -> do
    Socket.bind listener (Socket.SockAddrInet 0 (Socket.tupleToHostAddress (127, 0, 0, 1)))
    Socket.listen listener 1
    port <- Socket.socketPort listener
    bracket (Socket.socket Socket.AF_INET Socket.Stream Socket.defaultProtocol) Socket.close $ \client -> do
      Socket.connect client (Socket.SockAddrInet port (Socket.tupleToHostAddress (127, 0, 0, 1)))
      bracket (fst <$> Socket.accept listener) Socket.close action
EOF
if [ "$(grep -Fxc '        time-manager >=0.2.4 && <0.3,' "$http2_directory/http2.cabal")" -ne 2 ]; then
  printf '%s\n' 'HTTP2 test-only time-manager bound patch changed an unexpected number of constraints.' >&2
  exit 1
fi
if ! grep -Fq 'HTTP2.ConfigSpec' "$http2_directory/http2.cabal" \
  || ! grep -Fq 'Timeout.register confTimeoutManager' "$http2_directory/Network/HTTP2/H2/Receiver.hs"; then
  printf '%s\n' 'HTTP2 lifecycle test-only patch did not apply.' >&2
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
    if [ "$package_name" = 'primitive-0.9.1.0' ] || [ "$package_name" = 'http2-5.4.4' ]; then
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
    http2-5.4.4)
      grep -E '^(Network/HTTP2|test/(HTTP2|HPACK))/.*warning:' "$build_log" > "$package_warning_log" || :
      ;;
  esac
  if [ "$package_name" = 'cborg-0.2.10.0' ] || [ "$package_name" = 'serialise-0.2.6.1' ]; then
    "$diagnostic_gate" --allow-ghc-9-14-tls-compatibility-stack="$package_name" "$package_warning_log"
  else
    "$diagnostic_gate" "$package_warning_log"
  fi
}

run_package_tests "$cborg_directory" 'cborg-0.2.10.0' "$temporary_directory/cborg-build" test:tests
run_package_tests "$serialise_directory" 'serialise-0.2.6.1' "$temporary_directory/serialise-build" test:tests
run_package_tests "$primitive_directory" 'primitive-0.9.1.0' "$temporary_directory/primitive-build" test:test-qc
run_package_tests "$http2_directory" 'http2-5.4.4' "$temporary_directory/http2-build" all

runtime_plan_directory="$temporary_directory/runtime-plan"
cabal build harch-web:lib:harch-web --dry-run --builddir="$runtime_plan_directory"
jq --exit-status '
  .["install-plan"] as $packages
  | all(
      ["cborg", "serialise", "primitive", "http2", "time-manager"][];
      . as $package_name
      | any(
          $packages[];
          .["pkg-name"] == $package_name
            and .["pkg-src"].type == "repo-tar"
        )
    )
' "$runtime_plan_directory/cache/plan.json" >/dev/null

printf '%s\n' 'Released TLS compatibility-stack suites passed; temporary source builds were isolated from the runtime plan.'
