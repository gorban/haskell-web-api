#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
extension_dir="$repo_root/tools/vscode-ormolu-formatter"
output_dir="$(mktemp -d)"
trap 'rm -rf "$output_dir"' EXIT

if [ -n "${VSCODE_CWD:-}" ] && [ -x "$VSCODE_CWD/bin/remote-cli/code" ]; then
  code_cli="$VSCODE_CWD/bin/remote-cli/code"
else
  code_cli="$(find "$HOME/.vscode-server/bin" -path '*/bin/remote-cli/code' -type f -perm -u+x -print 2>/dev/null | sort | tail -n1)"
fi

if [ -z "${code_cli:-}" ]; then
  echo 'Could not find the attached VS Code server CLI. Run this from an integrated terminal in Dev Containers.' >&2
  exit 1
fi

vsix="$output_dir/haskell-web-api-ormolu-formatter.vsix"
cd "$extension_dir"
npx --yes @vscode/vsce@3.9.2 package --allow-missing-repository --out "$vsix"
"$code_cli" --install-extension "$vsix" --force

echo 'Installed haskell-web-api Ormolu Formatter in the attached VS Code server.'
