#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${MORPHO_VERITY_OUT_DIR:-${ROOT_DIR}/compiler/yul}"
MORPHO_YUL="${OUT_DIR}/Morpho.yul"
MORPHO_BIN="${OUT_DIR}/Morpho.bin"
MORPHO_ABI="${OUT_DIR}/Morpho.abi.json"
HASH_LIB="${ROOT_DIR}/compiler/external-libs/MarketParamsHash.yul"
TARGET_JSON="${ROOT_DIR}/config/parity-target.json"

default_pack=""
if [[ -f "${TARGET_JSON}" ]]; then
  default_pack="$(python3 -c 'import json,sys; d=json.load(open(sys.argv[1])); print(d.get("verity",{}).get("parityPackId",""))' "${TARGET_JSON}")"
fi
PARITY_PACK="${MORPHO_VERITY_PARITY_PACK:-${default_pack}}"
INPUT_MODE="${MORPHO_VERITY_INPUT_MODE:-edsl}"
SKIP_BUILD="${MORPHO_VERITY_SKIP_BUILD:-0}"
SKIP_SOLC="${MORPHO_VERITY_SKIP_SOLC:-0}"

mkdir -p "${OUT_DIR}"

if [[ ! -f "${HASH_LIB}" ]]; then
  echo "ERROR: missing hash library: ${HASH_LIB}"
  exit 1
fi

if [[ "${SKIP_BUILD}" != "1" ]]; then
  echo "Building Morpho Verity compiler target..."
  (cd "${ROOT_DIR}" && lake build morpho-verity-compiler)
fi

echo "Running Morpho Verity compiler..."
if [[ "${INPUT_MODE}" != "model" && "${INPUT_MODE}" != "edsl" ]]; then
  echo "ERROR: MORPHO_VERITY_INPUT_MODE must be 'model' or 'edsl' (got: ${INPUT_MODE})"
  exit 1
fi
compiler_args=(--output "${OUT_DIR}" --abi-output "${OUT_DIR}" --input "${INPUT_MODE}" --link "${HASH_LIB}" --verbose)
if [[ -n "${PARITY_PACK}" ]]; then
  compiler_args+=(--parity-pack "${PARITY_PACK}")
  echo "Using Verity parity pack: ${PARITY_PACK}"
fi
echo "Using input mode: ${INPUT_MODE}"
(cd "${ROOT_DIR}" && lake exe morpho-verity-compiler "${compiler_args[@]}")

if [[ ! -s "${MORPHO_YUL}" ]]; then
  cat <<EOF
ERROR: ${MORPHO_YUL} was not generated.

The Morpho compiler target did not emit the expected artifact.
Check `lake exe morpho-verity-compiler --help` and build logs above.
EOF
  exit 1
fi

if [[ "${SKIP_SOLC}" != "1" ]]; then
  echo "Compiling Yul to EVM init bytecode..."
  solc --strict-assembly --bin "${MORPHO_YUL}" \
    | awk '/Binary representation:/{getline; print; exit}' \
    > "${MORPHO_BIN}"
fi

if [[ "${SKIP_SOLC}" != "1" && ! -s "${MORPHO_BIN}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_BIN}"
  exit 1
fi
if [[ ! -s "${MORPHO_ABI}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_ABI}"
  exit 1
fi

echo "Generated Verity artifact: ${MORPHO_YUL}"
if [[ "${SKIP_SOLC}" != "1" ]]; then
  echo "Generated Verity bytecode: ${MORPHO_BIN}"
else
  echo "Skipped bytecode generation (MORPHO_VERITY_SKIP_SOLC=1)"
fi
echo "Generated Verity ABI: ${MORPHO_ABI}"
