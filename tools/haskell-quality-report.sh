#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

required_commands=(argon hlint rg)
missing_commands=()

for command_name in "${required_commands[@]}"; do
  if ! command -v "$command_name" >/dev/null 2>&1; then
    missing_commands+=("$command_name")
  fi
done

if [ "${#missing_commands[@]}" -ne 0 ]; then
  printf 'Missing required quality tools: %s\n' "${missing_commands[*]}" >&2
  printf '%s\n' 'Run tools/install-haskell-quality-tools.sh and the formatting-tool installer documented in SETUP.md.' >&2
  exit 1
fi

production_paths=(
  packages/core/src
  packages/harch-web/src
  packages/test-core/src
  packages/web-api/src
  examples
)
test_paths=(
  packages/core/test
  packages/harch-web/test
  packages/test-core/test
  packages/web-api/test
)

print_argon_report() {
  local title="$1"
  local minimum="$2"
  shift 2

  printf '\n%s\n\n' "$title"
  argon --no-color --min "$minimum" "$@" \
    | rg -v '^\s+[0-9]+:[0-9]+ spec - ' \
    || true
}

print_argon_report 'Production priority hotspots (complexity > 10)' 11 "${production_paths[@]}"
print_argon_report 'Production review candidates (complexity >= 8)' 8 "${production_paths[@]}"
print_argon_report 'Test helper priority hotspots (top-level spec excluded)' 11 "${test_paths[@]}"
print_argon_report 'Test helper review candidates (top-level spec excluded)' 8 "${test_paths[@]}"

printf '\nHLint advisory report\n\n'
hlint "${production_paths[@]}" "${test_paths[@]}" || true

all_paths=("${production_paths[@]}" "${test_paths[@]}")
pattern_config="tools/haskell-quality-monads.conf"

printf '\nConstructor-forwarding review candidates\n'
while read -r label failure_constructor success_constructor; do
  case "$label" in
    ''|'#'*) continue ;;
  esac

  printf '\n%s (%s/%s)\n' "$label" "$failure_constructor" "$success_constructor"
  rg -n --glob '*.hs' --glob '!packages/hspec-expectations-match/**' \
    --pcre2 \
    "(?:${failure_constructor}|${success_constructor})[^\\n]*->\\s*(?:pure\\s+|return\\s+)?\\(?\\s*(?:${failure_constructor}|${success_constructor})\\b" \
    "${all_paths[@]}" \
    || true
done < "$pattern_config"

printf '\nTransformer-result case review candidates\n\n'
rg -n -U --glob '*.hs' --glob '!packages/hspec-expectations-match/**' \
  --pcre2 '<-\s*(?:runExceptT|runMaybeT)\b[^\n]*\n\s*case\b' \
  "${all_paths[@]}" \
  || true

printf '\nQuality report complete. Findings are advisory and require human review.\n'
