#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_WITH_TIMEOUT="${ROOT_DIR}/scripts/run_with_timeout.sh"
OUT_DIR="${MORPHO_VERITY_PARITY_OUT_DIR:-}"
TMP_DIR=""

cleanup() {
  if [[ -n "${TMP_DIR}" ]]; then
    rm -rf "${TMP_DIR}"
  fi
}

trap cleanup EXIT

if [[ -n "${OUT_DIR}" ]]; then
  mkdir -p "${OUT_DIR}/edsl"
  rm -rf "${OUT_DIR}/edsl"
  mkdir -p "${OUT_DIR}/edsl"
  TARGET_OUT="${OUT_DIR}/edsl"
else
  TMP_DIR="$(mktemp -d)"
  TARGET_OUT="${TMP_DIR}/edsl"
fi

echo "Preparing Morpho compiler artifact (edsl-only)..."
MORPHO_VERITY_OUT_DIR="${TARGET_OUT}" \
  MORPHO_VERITY_ARTIFACT_MODE="edsl" \
  "${RUN_WITH_TIMEOUT}" MORPHO_VERITY_PREP_TIMEOUT_SEC 900 \
    "Prepare edsl artifact" -- \
    "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

for artifact in "${TARGET_OUT}/Morpho.yul" "${TARGET_OUT}/Morpho.bin" "${TARGET_OUT}/Morpho.abi.json"; do
  if [[ ! -s "${artifact}" ]]; then
    echo "ERROR: missing or empty artifact: ${artifact}"
    exit 1
  fi
done

echo "Artifact preparation passed: Morpho.yul, Morpho.bin, and Morpho.abi.json are present."
