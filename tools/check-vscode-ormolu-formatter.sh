#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
extension_dir="$repo_root/tools/vscode-ormolu-formatter"
output_dir="$(mktemp -d)"
trap 'rm -rf "$output_dir"' EXIT

if ! command -v ormolu >/dev/null 2>&1; then
  echo 'Ormolu is required for the VS Code formatter integration check.' >&2
  exit 1
fi

actual_format="$(printf 'module Example where\n\nimport Data.Text qualified as Text\n\nanswer::Int\nanswer=42\n' | ormolu --ghc-opt=-XImportQualifiedPost --stdin-input-file Example.hs)"
expected_format="$(printf 'module Example where\n\nimport Data.Text qualified as Text\n\nanswer :: Int\nanswer = 42')"
if [ "$actual_format" != "$expected_format" ]; then
  echo 'The installed Ormolu executable did not produce the expected formatting.' >&2
  exit 1
fi

npm ci --prefix "$extension_dir" --ignore-scripts
npm run lint --prefix "$extension_dir"
npm run check --prefix "$extension_dir"

(
  cd "$extension_dir"
  npx --yes @vscode/vsce@3.9.2 package \
    --allow-missing-repository \
    --out "$output_dir/haskell-web-api-ormolu-formatter.vsix"
)

unzip -Z1 "$output_dir/haskell-web-api-ormolu-formatter.vsix" | grep -qx 'extension/extension.js'
unzip -Z1 "$output_dir/haskell-web-api-ormolu-formatter.vsix" | grep -qx 'extension/ormolu.js'
unzip -Z1 "$output_dir/haskell-web-api-ormolu-formatter.vsix" | grep -qx 'extension/package.json'
if unzip -Z1 "$output_dir/haskell-web-api-ormolu-formatter.vsix" | grep -q '^extension/test/'; then
  echo 'The VSIX unexpectedly includes test sources.' >&2
  exit 1
fi

echo 'VS Code Ormolu formatter checks passed.'
