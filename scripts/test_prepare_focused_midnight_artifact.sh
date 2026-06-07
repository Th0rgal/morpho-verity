#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="${ROOT_DIR}/scripts/prepare_focused_midnight_artifact.sh"

if [[ ! -x "${SCRIPT}" ]]; then
  echo "missing executable script: ${SCRIPT}"
  exit 1
fi

if ! grep -Fq "MidnightRCF.bin.raw" "${SCRIPT}"; then
  echo "expected focused artifact script to emit MidnightRCF.bin.raw"
  exit 1
fi

if ! grep -Fq "must not be copied to" "${SCRIPT}"; then
  echo "expected focused artifact script to document that it is not full Midnight parity"
  exit 1
fi

echo "prepare-focused-midnight-artifact script check: OK"
