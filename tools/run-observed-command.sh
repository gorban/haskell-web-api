#!/usr/bin/env bash

set -euo pipefail

usage() {
  printf '%s\n' 'Usage: run-observed-command.sh --label LABEL --timeout DURATION -- COMMAND [ARGUMENT ...]' >&2
  exit 2
}

label=''
timeout_duration=''

while [ "$#" -gt 0 ]; do
  case "$1" in
    --label)
      [ "$#" -ge 2 ] || usage
      label="$2"
      shift 2
      ;;
    --timeout)
      [ "$#" -ge 2 ] || usage
      timeout_duration="$2"
      shift 2
      ;;
    --)
      shift
      break
      ;;
    *)
      usage
      ;;
  esac
done

[ -n "$label" ] && [ -n "$timeout_duration" ] && [ "$#" -gt 0 ] || usage

started_at="$(date -u '+%Y-%m-%dT%H:%M:%SZ')"
started_seconds="$(date +%s)"
command_log="$(mktemp)"
trap 'rm -f "$command_log"' EXIT

printf '==> %s started at %s (timeout: %s)\n' "$label" "$started_at" "$timeout_duration"

set +e
timeout --foreground --kill-after=30s "$timeout_duration" "$@" 2>&1 | tee "$command_log"
pipeline_statuses=("${PIPESTATUS[@]}")
set -e

command_status="${pipeline_statuses[0]}"
tee_status="${pipeline_statuses[1]}"
elapsed_seconds="$(( $(date +%s) - started_seconds ))"

if [ "$tee_status" -ne 0 ]; then
  printf 'ERROR: %s diagnostics could not be captured after %ss (tee exited %s).\n' "$label" "$elapsed_seconds" "$tee_status" >&2
  exit "$tee_status"
fi

if [ "$command_status" -eq 0 ]; then
  printf '<== %s completed successfully in %ss\n' "$label" "$elapsed_seconds"
  exit 0
fi

if [ "$command_status" -eq 124 ]; then
  printf 'ERROR: %s exceeded its %s timeout after %ss; the complete command output is above.\n' "$label" "$timeout_duration" "$elapsed_seconds" >&2
else
  printf 'ERROR: %s failed with exit status %s after %ss; the complete command output is above.\n' "$label" "$command_status" "$elapsed_seconds" >&2
fi

exit "$command_status"
