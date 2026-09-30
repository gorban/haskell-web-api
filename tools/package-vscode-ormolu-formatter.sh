#!/usr/bin/env bash

set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "usage: $0 OUTPUT_VSIX" >&2
  exit 2
fi

repo_root="$(git rev-parse --show-toplevel)"
extension_dir="$repo_root/tools/vscode-ormolu-formatter"
output_path="$(cd "$(dirname "$1")" && pwd)/$(basename "$1")"
package_dir="$(mktemp -d)"
trap 'rm -rf "$package_dir"' EXIT

for source_file in package.json package-lock.json extension.js ormolu.js .vscodeignore; do
  cp "$extension_dir/$source_file" "$package_dir/$source_file"
done
cp "$repo_root/LICENSE" "$package_dir/LICENSE"

(
  cd "$package_dir"
  npx --yes @vscode/vsce@3.9.2 package \
    --allow-missing-repository \
    --out "$output_path"
)
