#!/usr/bin/env bash

# Verify the Docker dependency-cache layer against cabal.project without needing
# an image build.  Docker cannot discover Cabal's local package graph after a
# partial copy, so every project package and each Custom Setup input must be
# declared before `cabal build all --only-dependencies`.
set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
dockerfile="$repo_root/Dockerfile"
project_file="$repo_root/cabal.project"

if [[ ! -f "$dockerfile" || ! -f "$project_file" ]]; then
  printf '%s\n' 'Dockerfile and cabal.project must be present at the repository root.' >&2
  exit 1
fi

mapfile -t package_directories < <(
  awk '
    /^packages:[[:space:]]*$/ { collecting = 1; next }
    collecting && /^[[:space:]]+/ { print $1; next }
    collecting { exit }
  ' "$project_file"
)

if [[ "${#package_directories[@]}" -eq 0 ]]; then
  printf '%s\n' 'No project package directories found in cabal.project.' >&2
  exit 1
fi

dependency_line="$(grep -n 'cabal build all --only-dependencies' "$dockerfile" | head -n 1 | cut -d: -f1)"
if [[ -z "$dependency_line" ]]; then
  printf '%s\n' 'Dockerfile has no Cabal dependency-cache command.' >&2
  exit 1
fi

missing_inputs=()
if ! sed -n "1,${dependency_line}p" "$dockerfile" | grep -Fxq 'COPY cabal.project cabal.project.freeze ./'; then
  missing_inputs+=('cabal.project.freeze (before dependency resolution)')
fi
for package_directory in "${package_directories[@]}"; do
  mapfile -t cabal_files < <(find "$repo_root/$package_directory" -maxdepth 1 -name '*.cabal' -type f -printf '%P\n' | sort)
  if [[ "${#cabal_files[@]}" -ne 1 ]]; then
    printf 'Expected exactly one Cabal file in %s, found %s.\n' "$package_directory" "${#cabal_files[@]}" >&2
    exit 1
  fi
  cabal_file="$package_directory${cabal_files[0]}"
  if ! sed -n "1,${dependency_line}p" "$dockerfile" | grep -Fq "COPY $cabal_file $package_directory"; then
    missing_inputs+=("$cabal_file")
  fi
done

for setup_input in examples/two-pages/Setup.hs examples/two-pages/SetupHooks.hs packages/web-api/Setup.hs packages/web-api/SetupHooks.hs; do
  setup_directory="${setup_input%/*}/"
  if ! sed -n "1,${dependency_line}p" "$dockerfile" | grep -Fq "$setup_input"; then
    missing_inputs+=("$setup_input")
  elif ! sed -n "1,${dependency_line}p" "$dockerfile" | grep -F "$setup_input" | grep -Fq "$setup_directory"; then
    missing_inputs+=("$setup_input (wrong Docker destination)")
  fi
done

if [[ "${#missing_inputs[@]}" -ne 0 ]]; then
  printf 'Docker dependency-cache layer omits required project inputs:\n' >&2
  printf '  %s\n' "${missing_inputs[@]}" >&2
  exit 1
fi

for required_builder_package in libicu-dev lld npm; do
  grep -Fq "    $required_builder_package \\" "$dockerfile" || {
    printf 'Docker builder is missing required package %s.\n' "$required_builder_package" >&2
    exit 1
  }
done

for required_builder_locale in 'ENV LANG=C.UTF-8' 'ENV LC_ALL=C.UTF-8'; do
  grep -Fxq "$required_builder_locale" "$dockerfile" || {
    printf 'Docker builder must declare UTF-8 locale setting %s.\n' "$required_builder_locale" >&2
    exit 1
  }
done

for required_test_stage_setting in "'SETUP_AUTOSTART_DATABASE=false'" "'SETUP_AUTOSTART_JAEGER=false'"; do
  grep -Fq "$required_test_stage_setting" "$dockerfile" \
    && grep -Fq '> packages/web-api/.env.local' "$dockerfile" || {
    printf 'Docker test stage must write hermetic prerequisite setting %s.\n' "$required_test_stage_setting" >&2
    exit 1
  }
done

for required_runtime_package in libcap2-bin libicu72 libpq5 libstdc++6; do
  runtime_occurrences="$(grep -Fc "    $required_runtime_package \\" "$dockerfile")"
  if [[ "$runtime_occurrences" -ne 2 ]]; then
    printf 'Both Debian runtime stages must install %s (found %s).\n' "$required_runtime_package" "$runtime_occurrences" >&2
    exit 1
  fi
done

if [[ "$(grep -Fc 'addgroup --gid 1000 app' "$dockerfile")" -ne 2 ]]; then
  printf '%s\n' 'Both Debian runtime stages must create the app group with addgroup --gid 1000.' >&2
  exit 1
fi

if grep -Eq '^FROM alpine:' "$dockerfile"; then
  printf '%s\n' 'Runtime stages must share the builder Debian ABI; Alpine is unsupported.' >&2
  exit 1
fi

if [[ "$(grep -Ec '^COPY --from=(build-and-test|release-build) --chown=app:app /app/cabal.project.freeze /app/cabal.project.freeze$' "$dockerfile")" -ne 2 ]]; then
  printf '%s\n' 'Both runtime images must retain their build freeze file.' >&2
  exit 1
fi

if grep -Eq '^cabal install .*hspec-discover' "$dockerfile"; then
  printf '%s\n' 'Docker test tools must come from the frozen project plan.' >&2
  exit 1
fi

printf '%s\n' 'Docker dependency-cache and runtime ABI policy checks passed.'
