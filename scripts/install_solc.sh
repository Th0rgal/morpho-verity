#!/usr/bin/env bash
set -euo pipefail

retry() {
  local attempts="$1"
  shift
  local n=1
  local rc=0
  while true; do
    if "$@"; then
      return 0
    fi
    rc=$?
    if [[ "${n}" -ge "${attempts}" ]]; then
      return "${rc}"
    fi
    local sleep_sec=$((2 ** n))
    echo "WARN: command failed (attempt ${n}/${attempts}, rc=${rc}); retrying in ${sleep_sec}s: $*" >&2
    sleep "${sleep_sec}"
    n=$((n + 1))
  done
}

SOLC_VERSION="${1:-0.8.28}"

has_solc_version() {
  solc-select versions \
    | tr '[:space:]' '\n' \
    | sed '/^$/d; s/^\*//g' \
    | grep -Fxq "${SOLC_VERSION}"
}

if ! command -v solc-select >/dev/null 2>&1; then
  retry 4 pip3 install solc-select
fi

if ! has_solc_version; then
  retry 4 solc-select install "${SOLC_VERSION}"
fi

retry 4 solc-select use "${SOLC_VERSION}"
solc --version
