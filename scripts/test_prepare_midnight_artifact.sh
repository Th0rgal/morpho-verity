#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT="${ROOT_DIR}/scripts/prepare_midnight_artifact.sh"

if [[ ! -x "${SCRIPT}" ]]; then
  echo "missing executable script: ${SCRIPT}"
  exit 1
fi

if ! grep -Fq -- "--artifact full" "${SCRIPT}"; then
  echo "expected Midnight artifact script to compile the full Midnight surface"
  exit 1
fi

if ! grep -Fq "Midnight.bin.raw" "${SCRIPT}"; then
  echo "expected Midnight artifact script to emit Midnight.bin.raw"
  exit 1
fi

if grep -Fq "MidnightRCF.bin.raw" "${SCRIPT}"; then
  echo "Midnight artifact script must not emit the focused MidnightRCF artifact"
  exit 1
fi

if grep -Fq "patch_midnight_sstore2_yul.py" "${SCRIPT}"; then
  echo "Midnight artifact script must not run a Midnight-specific Yul patch"
  exit 1
fi

echo "prepare-midnight-artifact script check: OK"
