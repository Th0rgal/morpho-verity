#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -lt 5 ]]; then
  echo "Usage: $0 <timeout_env_var> <default_timeout_sec> <description> -- <command> [args...]" >&2
  exit 2
fi

timeout_env_var="$1"
default_timeout_sec="$2"
description="$3"
shift 3

if [[ "${1}" != "--" ]]; then
  echo "ERROR: expected '--' before command arguments" >&2
  exit 2
fi
shift

if [[ "$#" -eq 0 ]]; then
  echo "ERROR: missing command to execute" >&2
  exit 2
fi

if [[ ! "${timeout_env_var}" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
  echo "ERROR: invalid timeout environment variable name: ${timeout_env_var}" >&2
  exit 2
fi

if [[ ! "${default_timeout_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: default timeout must be a non-negative integer (got: ${default_timeout_sec})" >&2
  exit 2
fi

timeout_sec="${!timeout_env_var:-${default_timeout_sec}}"
if [[ ! "${timeout_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: ${timeout_env_var} must be a non-negative integer (got: ${timeout_sec})" >&2
  exit 2
fi

kill_after_sec="${MORPHO_TIMEOUT_KILL_AFTER_SEC:-30}"
if [[ ! "${kill_after_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be a non-negative integer (got: ${kill_after_sec})" >&2
  exit 2
fi
if [[ "${kill_after_sec}" -eq 0 ]]; then
  echo "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be greater than zero to keep hard-kill timeout semantics" >&2
  exit 2
fi

if [[ "${timeout_sec}" -eq 0 ]]; then
  "$@"
  exit $?
fi

if ! command -v timeout >/dev/null 2>&1; then
  echo "ERROR: timeout command is required when ${timeout_env_var} is greater than zero" >&2
  exit 2
fi

start_epoch="$(date +%s)"
if timeout --kill-after="${kill_after_sec}s" "${timeout_sec}" "$@"; then
  exit 0
else
  status=$?
fi
end_epoch="$(date +%s)"
elapsed_sec="$((end_epoch - start_epoch))"

if [[ "${status}" -eq 124 || "${status}" -eq 137 ]]; then
  echo "ERROR: ${description} timed out after ${timeout_sec}s" >&2
  echo "ERROR: timeout env=${timeout_env_var} kill-after=${kill_after_sec}s elapsed=${elapsed_sec}s exit=${status}" >&2
  echo "ERROR: remediate by increasing ${timeout_env_var} or reducing work in this stage" >&2
fi
exit "${status}"
