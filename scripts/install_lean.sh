#!/usr/bin/env bash
set -euo pipefail

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
  echo "Installing elan..."
  curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y
fi

ELAN_BIN_DIR="${HOME}/.elan/bin"
if [[ ":${PATH}:" != *":${ELAN_BIN_DIR}:"* ]]; then
  export PATH="${ELAN_BIN_DIR}:${PATH}"
fi

if ! elan toolchain list | grep -Fq "${TOOLCHAIN}"; then
  echo "Installing Lean toolchain ${TOOLCHAIN}..."
  elan toolchain install "${TOOLCHAIN}"
fi

echo "Lean toolchain ready: ${TOOLCHAIN}"
lean --version
