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

if [[ "${timeout_sec}" -eq 0 ]]; then
  "$@"
  exit $?
fi

if ! command -v timeout >/dev/null 2>&1; then
  echo "ERROR: timeout command is required when ${timeout_env_var} is greater than zero" >&2
  exit 2
fi

if timeout "${timeout_sec}" "$@"; then
  exit 0
else
  status=$?
fi

if [[ "${status}" -eq 124 ]]; then
  echo "ERROR: ${description} timed out after ${timeout_sec}s" >&2
fi
exit "${status}"
