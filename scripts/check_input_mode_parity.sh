#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
PARITY_OUT_DIR="${MORPHO_VERITY_PARITY_OUT_DIR:-}"
PREP_TIMEOUT_SEC="${MORPHO_VERITY_PREP_TIMEOUT_SEC:-900}"
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

run_prepare() {
  local mode="$1"
  local out_dir="$2"
  local skip_build="${3:-0}"

  if [[ ! "${PREP_TIMEOUT_SEC}" =~ ^[0-9]+$ ]]; then
    echo "ERROR: MORPHO_VERITY_PREP_TIMEOUT_SEC must be a non-negative integer (got: ${PREP_TIMEOUT_SEC})"
    exit 1
  fi

  if [[ "${PREP_TIMEOUT_SEC}" -gt 0 ]] && ! command -v timeout >/dev/null 2>&1; then
    echo "ERROR: timeout command is required when MORPHO_VERITY_PREP_TIMEOUT_SEC is greater than zero"
    exit 1
  fi

  local -a prepare_cmd=("${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh")
  local -a maybe_timeout=()
  if [[ "${PREP_TIMEOUT_SEC}" -gt 0 ]] && command -v timeout >/dev/null 2>&1; then
    maybe_timeout=(timeout "${PREP_TIMEOUT_SEC}")
  fi

  if [[ "${skip_build}" == "1" ]]; then
    MORPHO_VERITY_SKIP_BUILD="1" \
    MORPHO_VERITY_OUT_DIR="${out_dir}" \
    MORPHO_VERITY_INPUT_MODE="${mode}" \
      "${maybe_timeout[@]}" "${prepare_cmd[@]}"
  else
    MORPHO_VERITY_OUT_DIR="${out_dir}" \
    MORPHO_VERITY_INPUT_MODE="${mode}" \
      "${maybe_timeout[@]}" "${prepare_cmd[@]}"
  fi
}

canonicalize_abi() {
  local abi_path="$1"
  local out_path="$2"
  local label="$3"

  if ! command -v python3 >/dev/null 2>&1; then
    echo "ERROR: python3 is required to canonicalize ABI artifacts for parity checks"
    exit 1
  fi

  if ! python3 - "${abi_path}" "${out_path}" <<'PY'
import json
import sys

try:
    with open(sys.argv[1], "r", encoding="utf-8") as src:
        data = json.load(src)
except json.JSONDecodeError as exc:
    print(f"invalid JSON: {exc}", file=sys.stderr)
    sys.exit(2)

with open(sys.argv[2], "w", encoding="utf-8") as dst:
    json.dump(data, dst, sort_keys=True, separators=(",", ":"))
    dst.write("\n")
PY
  then
    echo "ERROR: failed to canonicalize ${label} ABI artifact: ${abi_path}"
    exit 1
  fi
}

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

set +e
run_prepare "model" "${MODEL_OUT}" "0"
model_status=$?
set -e
if [[ "${model_status}" -eq 124 ]]; then
  echo "ERROR: timed out while preparing model artifact after ${PREP_TIMEOUT_SEC}s"
  exit 1
fi
if [[ "${model_status}" -ne 0 ]]; then
  exit "${model_status}"
fi

set +e
run_prepare "edsl" "${EDSL_OUT}" "1"
edsl_status=$?
set -e
if [[ "${edsl_status}" -eq 124 ]]; then
  echo "ERROR: timed out while preparing edsl artifact after ${PREP_TIMEOUT_SEC}s"
  exit 1
fi
if [[ "${edsl_status}" -ne 0 ]]; then
  exit "${edsl_status}"
fi

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
canonicalize_abi "${MODEL_ABI}" "${ABI_MODEL_CANONICAL}" "model"
canonicalize_abi "${EDSL_ABI}" "${ABI_EDSL_CANONICAL}" "edsl"

if ! cmp -s "${ABI_MODEL_CANONICAL}" "${ABI_EDSL_CANONICAL}"; then
  echo "ERROR: model vs edsl ABI artifacts differ semantically"
  exit 1
fi

echo "Input-mode parity passed: model and edsl Yul/bytecode match byte-for-byte and ABI matches semantically."
