#!/usr/bin/env bash

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"

exec "$repo_root/tools/run-observed-command.sh" \
  --label 'Seed PostgreSQL test database' \
  --timeout 10m \
  -- cabal run haskell-web-api-db -- migrate-and-seed
