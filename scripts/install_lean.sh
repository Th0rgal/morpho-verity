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

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TOOLCHAIN_FILE="${ROOT_DIR}/lean-toolchain"
REQUESTED_TOOLCHAIN="${1:-}"

if [[ -n "${REQUESTED_TOOLCHAIN}" ]]; then
  TOOLCHAIN="${REQUESTED_TOOLCHAIN}"
elif [[ -f "${TOOLCHAIN_FILE}" ]]; then
  TOOLCHAIN="$(head -n 1 "${TOOLCHAIN_FILE}" | tr -d '[:space:]')"
else
  echo "ERROR: Lean toolchain not provided and ${TOOLCHAIN_FILE} is missing."
  exit 1
fi

if [[ -z "${TOOLCHAIN}" ]]; then
  echo "ERROR: Lean toolchain is empty."
  exit 1
fi

if ! command -v elan >/dev/null 2>&1; then
  if ! command -v curl >/dev/null 2>&1; then
    echo "ERROR: elan is missing and curl is unavailable; cannot install elan" >&2
    exit 2
  fi
  echo "Installing elan..."
  retry 4 bash -lc 'curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y'
fi

ELAN_BIN_DIR="${HOME}/.elan/bin"
if [[ ":${PATH}:" != *":${ELAN_BIN_DIR}:"* ]]; then
  export PATH="${ELAN_BIN_DIR}:${PATH}"
fi
if [[ -n "${GITHUB_PATH:-}" ]]; then
  echo "${ELAN_BIN_DIR}" >> "${GITHUB_PATH}"
fi

if ! command -v elan >/dev/null 2>&1; then
  echo "ERROR: elan is still unavailable after installation; ensure ~/.elan/bin is on PATH" >&2
  exit 2
fi

if ! elan toolchain list | grep -Fq "${TOOLCHAIN}"; then
  echo "Installing Lean toolchain ${TOOLCHAIN}..."
  retry 4 elan toolchain install "${TOOLCHAIN}"
fi

if ! command -v lean >/dev/null 2>&1; then
  echo "ERROR: lean binary is unavailable after toolchain setup" >&2
  exit 2
fi

echo "Lean toolchain ready: ${TOOLCHAIN}"
lean --version
