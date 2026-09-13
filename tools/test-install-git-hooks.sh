#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
fixture_root="$(mktemp -d)"
trap 'rm -rf "$fixture_root"' EXIT

mkdir -p "$fixture_root/.github/hooks" "$fixture_root/tools"
cp "$repo_root/.github/hooks/pre-commit" "$fixture_root/.github/hooks/pre-commit"
cp "$repo_root/tools/install-git-hooks.sh" "$fixture_root/tools/install-git-hooks.sh"
chmod +x "$fixture_root/tools/install-git-hooks.sh"
git -C "$fixture_root" init --quiet

initial_install_output="$(cd "$fixture_root" && tools/install-git-hooks.sh)"
printf '%s' "$initial_install_output" | grep -Fq 'Installed the project pre-commit hook'

hook_target="$fixture_root/.git/hooks/pre-commit"
cmp -s "$fixture_root/.github/hooks/pre-commit" "$hook_target"
test -x "$hook_target"

repeat_install_output="$(cd "$fixture_root" && tools/install-git-hooks.sh)"
printf '%s' "$repeat_install_output" | grep -Fq 'Installed the project pre-commit hook'

printf '%s\n' '# another hook' >"$hook_target"
if conflicting_hook_output="$(cd "$fixture_root" && tools/install-git-hooks.sh 2>&1)"; then
  printf '%s\n' 'Hook installer overwrote a different pre-commit hook.' >&2
  exit 1
fi
if ! printf '%s' "$conflicting_hook_output" | grep -Fq 'Refusing to overwrite a different pre-commit hook'; then
  printf '%s\n' 'Hook installer did not explain the conflicting pre-commit hook.' >&2
  exit 1
fi
grep -Fxq '# another hook' "$hook_target"

git -C "$fixture_root" config core.hooksPath "$fixture_root/custom-hooks"
if custom_hooks_output="$(cd "$fixture_root" && tools/install-git-hooks.sh 2>&1)"; then
  printf '%s\n' 'Hook installer bypassed core.hooksPath.' >&2
  exit 1
fi
if ! printf '%s' "$custom_hooks_output" | grep -Fq 'Refusing to bypass the configured core.hooksPath'; then
  printf '%s\n' 'Hook installer did not explain the configured core.hooksPath.' >&2
  exit 1
fi

printf '%s\n' 'Git hook installer fixture checks passed.'
