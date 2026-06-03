#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${MORPHO_MIDNIGHT_OUT_DIR:-${ROOT_DIR}/artifacts/midnight}"
YUL="${OUT_DIR}/Midnight.yul"
ABI="${OUT_DIR}/Midnight.abi.json"
BIN="${OUT_DIR}/Midnight.bin"
BIN_RAW="${OUT_DIR}/Midnight.bin.raw"
MANIFEST="${OUT_DIR}/Midnight.artifact-manifest.env"
UNIQUIFY_YUL_SHADOWS="${ROOT_DIR}/scripts/uniquify_yul_shadows.py"
PATCH_MIDNIGHT_SSTORE2="${ROOT_DIR}/scripts/patch_midnight_sstore2_yul.py"

compute_input_digest() {
  local -a files=(
    "${ROOT_DIR}/lean-toolchain"
    "${ROOT_DIR}/lake-manifest.json"
    "${ROOT_DIR}/lakefile.lean"
    "${ROOT_DIR}/morpho-midnight-verity/Midnight.lean"
    "${ROOT_DIR}/morpho-midnight-verity/Midnight/Contract.lean"
    "${ROOT_DIR}/morpho-midnight-verity/Midnight/Compiler/ArtifactConfig.lean"
    "${ROOT_DIR}/morpho-midnight-verity/Midnight/Compiler/Main.lean"
    "${ROOT_DIR}/morpho-midnight-verity/MidnightCompiler.lean"
    "${ROOT_DIR}/scripts/prepare_midnight_artifact.sh"
    "${ROOT_DIR}/scripts/uniquify_yul_shadows.py"
    "${ROOT_DIR}/scripts/patch_midnight_sstore2_yul.py"
  )

  {
    for path in "${files[@]}"; do
      if [[ ! -f "${path}" ]]; then
        echo "ERROR: missing Midnight artifact input: ${path}" >&2
        return 2
      fi
      sha256sum "${path}"
    done
  } | sha256sum | awk '{print $1}'
}

if ! command -v lake >/dev/null 2>&1; then
  echo "ERROR: lake is required to build the Midnight artifact."
  exit 2
fi
if ! command -v solc >/dev/null 2>&1; then
  echo "ERROR: solc is required to compile the Midnight artifact."
  exit 2
fi
if ! command -v awk >/dev/null 2>&1; then
  echo "ERROR: awk is required to extract solc binary output."
  exit 2
fi
if ! command -v sha256sum >/dev/null 2>&1; then
  echo "ERROR: sha256sum is required to write the Midnight artifact manifest."
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
  lake exe midnight-verity-compiler --artifact full --output "${OUT_DIR}" --abi-output "${OUT_DIR}"
)

if [[ ! -s "${YUL}" || ! -s "${ABI}" ]]; then
  echo "ERROR: Midnight compiler did not emit ${YUL} and ${ABI}."
  exit 1
fi

python3 "${UNIQUIFY_YUL_SHADOWS}" --input "${YUL}" --output "${YUL}"
python3 "${PATCH_MIDNIGHT_SSTORE2}" --input "${YUL}" --output "${YUL}"

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
  echo "ERROR: solc did not emit Midnight bytecode."
  exit 1
fi

cat > "${MANIFEST}" <<EOF
input_digest=${INPUT_DIGEST}
artifact_scope=midnight-full-imidnight
contract_name=Midnight
complete_imidnight_artifact=1
parity_ready=1
EOF

echo "Midnight full artifact ready: ${BIN_RAW}"
