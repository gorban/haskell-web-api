#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
hook_source="$repo_root/.github/hooks/pre-commit"
configured_hooks_path="$(git config --get core.hooksPath || true)"

if [ -n "$configured_hooks_path" ]; then
  printf '%s\n' 'Refusing to bypass the configured core.hooksPath; install the project hook through that hook manager instead.' >&2
  exit 1
fi

hook_target="$(git rev-parse --git-path hooks/pre-commit)"
if [ -e "$hook_target" ] || [ -L "$hook_target" ]; then
  if ! cmp -s "$hook_source" "$hook_target"; then
    printf 'Refusing to overwrite a different pre-commit hook: %s\n' "$hook_target" >&2
    exit 1
  fi
fi

install -Dm755 "$hook_source" "$hook_target"
printf 'Installed the project pre-commit hook at %s\n' "$hook_target"
