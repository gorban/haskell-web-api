#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
metadata_check="$repo_root/tools/check-executable-file-metadata.sh"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

expected_executables=(
  .github/hooks/pre-commit
  .github/scripts/formatting-checks.sh
  .github/scripts/install-formatting-tools.sh
  examples/reverse-proxy/generate-local-tls.sh
  generate-code-coverage.sh
  tools/check-build-diagnostics.sh
  tools/check-cabal-packages.sh
  tools/check-executable-file-metadata.sh
  tools/check-vscode-ormolu-formatter.sh
  tools/haskell-quality-report.sh
  tools/install-git-hooks.sh
  tools/install-haskell-quality-tools.sh
  tools/install-vscode-ormolu-formatter.sh
  tools/package-vscode-ormolu-formatter.sh
  tools/run-code-coverage-check.sh
  tools/run-observed-command.sh
  tools/run-optimized-build-check.sh
  tools/seed-test-database.sh
  tools/test-check-build-diagnostics.sh
  tools/test-check-executable-file-metadata.sh
  tools/test-formatting-checks.sh
  tools/test-generate-code-coverage.sh
  tools/test-haskell-quality-report.sh
  tools/test-install-git-hooks.sh
  tools/test-run-observed-command.sh
)

git -C "$fixture_root" init --quiet
git -C "$fixture_root" config user.email 'fixture@example.invalid'
git -C "$fixture_root" config user.name fixture

for executable_file in "${expected_executables[@]}"; do
  mkdir -p "$fixture_root/$(dirname "$executable_file")"
  printf '%s\n' '#!/usr/bin/env bash' >"$fixture_root/$executable_file"
  chmod +x "$fixture_root/$executable_file"
done

printf '%s\n' '# documentation' >"$fixture_root/README.md"
git -C "$fixture_root" add .

expect_success() {
  if (cd "$fixture_root" && "$metadata_check") >/dev/null; then
    return
  fi

  printf '%s\n' 'Metadata check rejected the allowed executable policy.' >&2
  exit 1
}

expect_failure() {
  local description="$1"
  local expected_output="$2"
  local check_output

  if check_output="$(cd "$fixture_root" && "$metadata_check" 2>&1)"; then
    printf 'Metadata check accepted %s.\n' "$description" >&2
    exit 1
  fi
  if ! printf '%s' "$check_output" | grep -Fq "$expected_output"; then
    printf 'Metadata check did not report %s.\n' "$description" >&2
    printf '%s\n' "$check_output" >&2
    exit 1
  fi
}

expect_success

chmod +x "$fixture_root/README.md"
git -C "$fixture_root" add README.md
expect_failure 'an unexpected executable mode' 'Unexpected executable-file mode: README.md'

git -C "$fixture_root" update-index --chmod=-x -- README.md tools/check-cabal-packages.sh
expect_failure 'a required executable mode removed' 'Expected executable is not mode 100755: tools/check-cabal-packages.sh (found 100644)'

printf '%s\n' 'Executable-file metadata fixture checks passed.'
