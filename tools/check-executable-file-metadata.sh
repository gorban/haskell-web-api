#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

# These are the repository's supported direct-entry scripts. Keeping the list
# explicit makes an accidental executable bit a reviewable policy change.
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

failed=0

for executable_file in "${expected_executables[@]}"; do
  if ! index_entry="$(git ls-files -s -- "$executable_file")" || [ -z "$index_entry" ]; then
    printf 'Expected executable is not tracked: %s\n' "$executable_file" >&2
    failed=1
    continue
  fi

  index_mode="${index_entry%% *}"
  if [ "$index_mode" != '100755' ]; then
    printf 'Expected executable is not mode 100755: %s (found %s)\n' "$executable_file" "$index_mode" >&2
    failed=1
  fi

  if [ "$(head -c 2 -- "$executable_file")" != '#!' ]; then
    printf 'Expected executable has no shebang: %s\n' "$executable_file" >&2
    failed=1
  fi
done

while IFS=$'\t' read -r index_entry tracked_file; do
  index_mode="${index_entry%% *}"
  if [ "$index_mode" != '100755' ]; then
    continue
  fi

  allowed=0
  for executable_file in "${expected_executables[@]}"; do
    if [ "$tracked_file" = "$executable_file" ]; then
      allowed=1
      break
    fi
  done

  if [ "$allowed" -eq 0 ]; then
    printf 'Unexpected executable-file mode: %s\n' "$tracked_file" >&2
    failed=1
  fi
done < <(git ls-files -s)

if [ "$failed" -ne 0 ]; then
  exit 1
fi

printf '%s\n' 'Executable-file metadata matches the supported direct-entry policy.'
