#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${ROOT_DIR}/compiler/yul"
MORPHO_YUL="${OUT_DIR}/Morpho.yul"
MORPHO_BIN="${OUT_DIR}/Morpho.bin"
HASH_LIB="${ROOT_DIR}/compiler/external-libs/MarketParamsHash.yul"

mkdir -p "${OUT_DIR}"

if [[ -f "${MORPHO_YUL}" ]]; then
  echo "Found existing Verity artifact: ${MORPHO_YUL}"
else
  if [[ ! -f "${HASH_LIB}" ]]; then
    echo "ERROR: missing hash library: ${HASH_LIB}"
    exit 1
  fi

  echo "Building Morpho Verity compiler target..."
  (cd "${ROOT_DIR}" && lake build morpho-verity-compiler)

  echo "Running Morpho Verity compiler..."
  (cd "${ROOT_DIR}" && lake exe morpho-verity-compiler --output "${OUT_DIR}" --link "${HASH_LIB}" --verbose)
fi

if [[ ! -f "${MORPHO_YUL}" ]]; then
  cat <<'EOF'
ERROR: compiler/yul/Morpho.yul was not generated.

The Morpho compiler target did not emit the expected artifact.
Check `lake exe morpho-verity-compiler --help` and build logs above.
EOF
  exit 1
fi

echo "Compiling Yul to EVM init bytecode..."
solc --strict-assembly --bin "${MORPHO_YUL}" \
  | awk '/Binary representation:/{getline; print; exit}' \
  > "${MORPHO_BIN}"

if [[ ! -s "${MORPHO_BIN}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_BIN}"
  exit 1
fi

echo "Generated Verity artifact: ${MORPHO_YUL}"
echo "Generated Verity bytecode: ${MORPHO_BIN}"
