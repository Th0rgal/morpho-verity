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
SOLC_0_8_34="${ROOT_DIR}/.cache/solc-0.8.34+commit.80d5c536"

compute_input_digest() {
  python3 "${ROOT_DIR}/scripts/full_midnight_digest.py" "${ROOT_DIR}"
}

if ! command -v lake >/dev/null 2>&1; then
  echo "ERROR: lake is required to build the Midnight artifact."
  exit 2
fi
if ! command -v node >/dev/null 2>&1; then
  echo "ERROR: node is required to import the pinned Midnight Sol-C AST."
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

(
  cd "${ROOT_DIR}"
  node scripts/import_midnight_full.mjs
  node scripts/import_midnight_full.mjs --check
  node scripts/audit_midnight_memory.mjs
)
if [[ ! -x "${SOLC_0_8_34}" ]]; then
  echo "ERROR: pinned solc was not materialized at ${SOLC_0_8_34}." >&2
  exit 2
fi
INPUT_DIGEST="$(compute_input_digest)"

(
  cd "${ROOT_DIR}"
  # Execute the same compiler Main via Lean after kernel-checking its modules.
  # Compiling thousands of generated data constants to a native C object adds
  # substantial cost without adding a model-validation or semantic check.
  lake build +Midnight.Compiler.Main:olean
  lake env lean -s 65536 --run morpho-midnight-verity/MidnightCompiler.lean --artifact full --output "${OUT_DIR}" --abi-output "${OUT_DIR}"
)

if [[ ! -s "${YUL}" || ! -s "${ABI}" ]]; then
  echo "ERROR: Midnight compiler did not emit ${YUL} and ${ABI}."
  exit 1
fi

python3 "${UNIQUIFY_YUL_SHADOWS}" --input "${YUL}" --output "${YUL}"

# Explicit, recorded consumer-owned memory/stack policy; no default optimizer
# inlining. This does not certify standard-network deployment size or proofs.
python3 "${ROOT_DIR}/scripts/compile_midnight_yul.py" --input "${YUL}" --output-dir "${OUT_DIR}"

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
