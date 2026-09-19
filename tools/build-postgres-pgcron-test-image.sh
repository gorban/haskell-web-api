#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
image='localhost/haskell-web-api/postgres-pgcron:17-1.6.7'
container_runtime="${WEB_API_CONTAINER_RUNTIME:-}"

if [[ -z "$container_runtime" ]]; then
  for candidate in docker podman; do
    if command -v "$candidate" >/dev/null 2>&1; then
      container_runtime="$candidate"
      break
    fi
  done
fi

if [[ -z "$container_runtime" ]]; then
  printf '%s\n' 'A podman or docker runtime is required to build the PostgreSQL pg_cron test image.' >&2
  exit 1
fi

exec "$container_runtime" build \
  --tag "$image" \
  --file "$repo_root/tools/postgres-pgcron.Dockerfile" \
  "$repo_root"
