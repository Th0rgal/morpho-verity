#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
TMP_DIR="$(mktemp -d)"
trap 'rm -rf "${TMP_DIR}"' EXIT

MODEL_OUT="${TMP_DIR}/model"
EDSL_OUT="${TMP_DIR}/edsl"

echo "Checking Morpho compiler input-mode parity (model vs edsl)..."

MORPHO_VERITY_OUT_DIR="${MODEL_OUT}" \
MORPHO_VERITY_INPUT_MODE="model" \
  "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

MORPHO_VERITY_SKIP_BUILD="1" \
MORPHO_VERITY_OUT_DIR="${EDSL_OUT}" \
MORPHO_VERITY_INPUT_MODE="edsl" \
  "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

MODEL_YUL="${MODEL_OUT}/Morpho.yul"
EDSL_YUL="${EDSL_OUT}/Morpho.yul"
MODEL_BIN="${MODEL_OUT}/Morpho.bin"
EDSL_BIN="${EDSL_OUT}/Morpho.bin"
MODEL_ABI="${MODEL_OUT}/Morpho.abi.json"
EDSL_ABI="${EDSL_OUT}/Morpho.abi.json"

for artifact in "${MODEL_YUL}" "${EDSL_YUL}" "${MODEL_BIN}" "${EDSL_BIN}" "${MODEL_ABI}" "${EDSL_ABI}"; do
  if [[ ! -s "${artifact}" ]]; then
    echo "ERROR: missing or empty artifact: ${artifact}"
    exit 1
  fi
done

if ! cmp -s "${MODEL_YUL}" "${EDSL_YUL}"; then
  echo "ERROR: model vs edsl Yul artifacts differ"
  diff -u "${MODEL_YUL}" "${EDSL_YUL}" | head -n 200 || true
  exit 1
fi

if ! cmp -s "${MODEL_BIN}" "${EDSL_BIN}"; then
  echo "ERROR: model vs edsl bytecode artifacts differ"
  exit 1
fi

if ! cmp -s "${MODEL_ABI}" "${EDSL_ABI}"; then
  echo "ERROR: model vs edsl ABI artifacts differ"
  exit 1
fi

echo "Input-mode parity passed: model and edsl Yul/bytecode/ABI artifacts are identical."
