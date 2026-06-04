#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${MORPHO_MIDNIGHT_FOCUSED_OUT_DIR:-${ROOT_DIR}/artifacts/midnight-focused}"
YUL="${OUT_DIR}/MidnightRCF.yul"
ABI="${OUT_DIR}/MidnightRCF.abi.json"
BIN="${OUT_DIR}/MidnightRCF.bin"
BIN_RAW="${OUT_DIR}/MidnightRCF.bin.raw"
MANIFEST="${OUT_DIR}/MidnightRCF.artifact-manifest.env"
UNIQUIFY_YUL_SHADOWS="${ROOT_DIR}/scripts/uniquify_yul_shadows.py"

compute_input_digest() {
  python3 "${ROOT_DIR}/scripts/focused_midnight_digest.py" "${ROOT_DIR}"
}

if ! command -v lake >/dev/null 2>&1; then
  echo "ERROR: lake is required to build the focused Midnight artifact."
  exit 2
fi
if ! command -v solc >/dev/null 2>&1; then
  echo "ERROR: solc is required to compile the focused Midnight artifact."
  exit 2
fi
if ! command -v awk >/dev/null 2>&1; then
  echo "ERROR: awk is required to extract solc binary output."
  exit 2
fi
if ! command -v python3 >/dev/null 2>&1; then
  echo "ERROR: python3 is required to convert solc hex bytecode into raw deployment bytes."
  exit 2
fi

mkdir -p "${OUT_DIR}"
INPUT_DIGEST="$(compute_input_digest)"

(
  cd "${ROOT_DIR}"
  lake build midnight-verity-compiler
  lake exe midnight-verity-compiler --output "${OUT_DIR}" --abi-output "${OUT_DIR}"
)

if [[ ! -s "${YUL}" || ! -s "${ABI}" ]]; then
  echo "ERROR: focused Midnight compiler did not emit ${YUL} and ${ABI}."
  exit 1
fi

python3 "${UNIQUIFY_YUL_SHADOWS}" --input "${YUL}" --output "${YUL}"

solc --strict-assembly --bin "${YUL}" \
  | awk '/Binary representation:/{getline; print; exit}' \
  > "${BIN}"
python3 - "${BIN}" "${BIN_RAW}" <<'PY'
import pathlib
import sys

hex_path = pathlib.Path(sys.argv[1])
raw_path = pathlib.Path(sys.argv[2])
raw_path.write_bytes(bytes.fromhex(hex_path.read_text(encoding="utf-8").strip()))
PY

if [[ ! -s "${BIN_RAW}" ]]; then
  echo "ERROR: solc did not emit focused Midnight bytecode."
  exit 1
fi

cat > "${OUT_DIR}/README.md" <<EOF
# Focused MidnightRCF Artifact

This directory contains executable Yul/bytecode for the focused Verity proof
model in morpho-midnight-verity/Midnight/Contract.lean.

It is not a complete IMidnight implementation and must not be copied to
artifacts/midnight/Midnight.bin.raw for full upstream test parity.
EOF

cat > "${MANIFEST}" <<EOF
input_digest=${INPUT_DIGEST}
artifact_scope=focused-midnight-rcf
contract_name=MidnightRCF
complete_imidnight_artifact=0
EOF

echo "Focused MidnightRCF artifact ready: ${BIN_RAW}"
