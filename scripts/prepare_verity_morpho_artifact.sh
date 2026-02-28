#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${MORPHO_VERITY_OUT_DIR:-${ROOT_DIR}/compiler/yul}"
MORPHO_YUL="${OUT_DIR}/Morpho.yul"
MORPHO_BIN="${OUT_DIR}/Morpho.bin"
MORPHO_ABI="${OUT_DIR}/Morpho.abi.json"
HASH_LIB="${ROOT_DIR}/compiler/external-libs/MarketParamsHash.yul"
TARGET_JSON="${ROOT_DIR}/config/parity-target.json"

require_command() {
  local cmd="$1"
  local message="$2"
  if ! command -v "${cmd}" >/dev/null 2>&1; then
    echo "ERROR: ${message}"
    exit 2
  fi
}

validate_toggle() {
  local name="$1"
  local value="$2"
  if [[ "${value}" != "0" && "${value}" != "1" ]]; then
    echo "ERROR: ${name} must be '0' or '1' (got: ${value})"
    exit 2
  fi
}

INPUT_MODE="${MORPHO_VERITY_INPUT_MODE:-edsl}"
SKIP_BUILD="${MORPHO_VERITY_SKIP_BUILD:-0}"
SKIP_SOLC="${MORPHO_VERITY_SKIP_SOLC:-0}"

validate_toggle "MORPHO_VERITY_SKIP_BUILD" "${SKIP_BUILD}"
validate_toggle "MORPHO_VERITY_SKIP_SOLC" "${SKIP_SOLC}"

if [[ "${INPUT_MODE}" != "model" && "${INPUT_MODE}" != "edsl" ]]; then
  echo "ERROR: MORPHO_VERITY_INPUT_MODE must be 'model' or 'edsl' (got: ${INPUT_MODE})"
  exit 1
fi

default_pack=""
if [[ -f "${TARGET_JSON}" ]]; then
  require_command "python3" "python3 is required to read parity pack from ${TARGET_JSON}"
  if ! default_pack="$(python3 - "${TARGET_JSON}" <<'PY' 2>/dev/null
import json
import sys

data = json.load(open(sys.argv[1], "r", encoding="utf-8"))
parity_pack = data.get("verity", {}).get("parityPackId")
if not isinstance(parity_pack, str) or not parity_pack:
    raise ValueError("missing required verity.parityPackId")
print(parity_pack)
PY
)"; then
    echo "ERROR: failed to read required verity.parityPackId from ${TARGET_JSON}"
    exit 2
  fi
fi
PARITY_PACK="${MORPHO_VERITY_PARITY_PACK:-${default_pack}}"

mkdir -p "${OUT_DIR}"

if [[ ! -f "${HASH_LIB}" ]]; then
  echo "ERROR: missing hash library: ${HASH_LIB}"
  exit 1
fi

require_command "lake" "lake is required to build and compile Morpho artifacts"

if [[ "${SKIP_BUILD}" != "1" ]]; then
  echo "Building Morpho Verity compiler target..."
  (cd "${ROOT_DIR}" && lake build morpho-verity-compiler)
fi

echo "Running Morpho Verity compiler..."
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
  require_command "solc" "solc is required unless MORPHO_VERITY_SKIP_SOLC=1"
  require_command "awk" "awk is required to extract bytecode from solc output"
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
