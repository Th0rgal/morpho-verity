#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PARITY_OUT_DIR="${MORPHO_VERITY_PARITY_OUT_DIR:-}"
TMP_DIR=""
ABI_MODEL_CANONICAL=""
ABI_EDSL_CANONICAL=""

cleanup() {
  if [[ -n "${TMP_DIR}" ]]; then
    rm -rf "${TMP_DIR}"
  fi
  if [[ -n "${ABI_MODEL_CANONICAL}" ]]; then
    rm -f "${ABI_MODEL_CANONICAL}"
  fi
  if [[ -n "${ABI_EDSL_CANONICAL}" ]]; then
    rm -f "${ABI_EDSL_CANONICAL}"
  fi
}

trap cleanup EXIT

if [[ -n "${PARITY_OUT_DIR}" ]]; then
  mkdir -p "${PARITY_OUT_DIR}"
  MODEL_OUT="${PARITY_OUT_DIR}/model"
  EDSL_OUT="${PARITY_OUT_DIR}/edsl"
  rm -rf "${MODEL_OUT}" "${EDSL_OUT}"
  mkdir -p "${MODEL_OUT}" "${EDSL_OUT}"
else
  TMP_DIR="$(mktemp -d)"
  MODEL_OUT="${TMP_DIR}/model"
  EDSL_OUT="${TMP_DIR}/edsl"
fi

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

ABI_MODEL_CANONICAL="$(mktemp)"
ABI_EDSL_CANONICAL="$(mktemp)"
python3 - "${MODEL_ABI}" "${ABI_MODEL_CANONICAL}" <<'PY'
import json
import sys

with open(sys.argv[1], "r", encoding="utf-8") as src, open(
    sys.argv[2], "w", encoding="utf-8"
) as dst:
    json.dump(json.load(src), dst, sort_keys=True, separators=(",", ":"))
    dst.write("\n")
PY
python3 - "${EDSL_ABI}" "${ABI_EDSL_CANONICAL}" <<'PY'
import json
import sys

with open(sys.argv[1], "r", encoding="utf-8") as src, open(
    sys.argv[2], "w", encoding="utf-8"
) as dst:
    json.dump(json.load(src), dst, sort_keys=True, separators=(",", ":"))
    dst.write("\n")
PY

if ! cmp -s "${ABI_MODEL_CANONICAL}" "${ABI_EDSL_CANONICAL}"; then
  echo "ERROR: model vs edsl ABI artifacts differ semantically"
  exit 1
fi

echo "Input-mode parity passed: model and edsl Yul/bytecode match byte-for-byte and ABI matches semantically."
