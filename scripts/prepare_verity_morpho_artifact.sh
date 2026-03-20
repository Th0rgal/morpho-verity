#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
OUT_DIR="${MORPHO_VERITY_OUT_DIR:-${ROOT_DIR}/artifacts/yul}"
MORPHO_YUL="${OUT_DIR}/Morpho.yul"
MORPHO_REWRITTEN_YUL="${OUT_DIR}/Morpho.rewritten.yul"
MORPHO_BIN="${OUT_DIR}/Morpho.bin"
MORPHO_ABI="${OUT_DIR}/Morpho.abi.json"
MANIFEST_FILE="${OUT_DIR}/Morpho.artifact-manifest.env"
TIMINGS_LOG="${OUT_DIR}/Morpho.stage-times.log"
REWRITE_REPORT="${OUT_DIR}/Morpho.rewrite-report.json"
HASH_LIB="${ROOT_DIR}/artifacts/inputs/MarketParamsHash.yul"
BORROW_RATE_LIB="${ROOT_DIR}/artifacts/inputs/BorrowRate.yul"
ORACLE_PRICE_LIB="${ROOT_DIR}/artifacts/inputs/OraclePrice.yul"
COLLATERAL_PRICE_LIB="${ROOT_DIR}/artifacts/inputs/CollateralPrice.yul"
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

stage_log() {
  local stage="$1"
  local status="$2"
  local elapsed="$3"
  local detail="${4:-}"
  local line="stage=${stage} status=${status} elapsed_sec=${elapsed}"
  if [[ -n "${detail}" ]]; then
    line="${line} detail=${detail}"
  fi
  echo "${line}" | tee -a "${TIMINGS_LOG}"
}

run_stage() {
  local stage="$1"
  shift
  local start_epoch end_epoch elapsed
  start_epoch="$(date +%s)"
  echo "==> [stage:start] ${stage}"
  "$@"
  end_epoch="$(date +%s)"
  elapsed="$((end_epoch - start_epoch))"
  echo "==> [stage:done] ${stage} (${elapsed}s)"
  stage_log "${stage}" "ok" "${elapsed}"
}

artifacts_ready() {
  if [[ ! -s "${MORPHO_YUL}" || ! -s "${MORPHO_REWRITTEN_YUL}" || ! -s "${MORPHO_ABI}" ]]; then
    return 1
  fi
  if [[ ! -s "${REWRITE_REPORT}" ]]; then
    return 1
  fi
  if [[ "${SKIP_SOLC}" != "1" && ! -s "${MORPHO_BIN}" ]]; then
    return 1
  fi
  return 0
}

compute_input_digest() {
  local path
  local -a files=()

  if ! command -v sha256sum >/dev/null 2>&1; then
    echo ""
    return 0
  fi

  for path in \
    "${ROOT_DIR}/lean-toolchain" \
    "${ROOT_DIR}/lake-manifest.json" \
    "${ROOT_DIR}/lakefile.lean" \
    "${ROOT_DIR}/Morpho.lean" \
    "${ROOT_DIR}/MorphoCompiler.lean" \
    "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh" \
    "${ROOT_DIR}/scripts/apply_yul_rewrite_pipeline.py" \
    "${ROOT_DIR}/scripts/check_yul_rewrite_proof_obligations.py" \
    "${ROOT_DIR}/config/parity-target.json" \
    "${ROOT_DIR}/config/yul-rewrite-pipeline.json" \
    "${ROOT_DIR}/config/yul-rewrite-proof-obligations.json" \
    "${ROOT_DIR}/artifacts/inputs/MarketParamsHash.yul" \
    "${ROOT_DIR}/artifacts/inputs/BorrowRate.yul" \
    "${ROOT_DIR}/artifacts/inputs/OraclePrice.yul" \
    "${ROOT_DIR}/artifacts/inputs/CollateralPrice.yul" \
    "${ROOT_DIR}/Morpho"; do
    if [[ -f "${path}" ]]; then
      files+=("${path}")
    elif [[ -d "${path}" ]]; then
      while IFS= read -r -d '' file; do
        files+=("${file}")
      done < <(find "${path}" -type f -print0 | sort -z)
    fi
  done

  if [[ "${#files[@]}" -eq 0 ]]; then
    echo ""
    return 0
  fi

  local digest_line=""
  digest_line="$({
    for path in "${files[@]}"; do
      sha256sum "${path}"
    done
    printf 'artifact_mode=%s\n' "${ARTIFACT_MODE}"
    printf 'skip_solc=%s\n' "${SKIP_SOLC}"
    printf 'parity_pack=%s\n' "${PARITY_PACK}"
  } | sha256sum)"
  printf '%s\n' "${digest_line%% *}"
}

manifest_matches() {
  local digest="$1"
  if [[ -z "${digest}" || ! -f "${MANIFEST_FILE}" ]]; then
    return 1
  fi

  local recorded_digest="" recorded_mode="" recorded_skip_solc="" recorded_pack=""
  while IFS='=' read -r key value; do
    case "${key}" in
      input_digest) recorded_digest="${value}" ;;
      artifact_mode) recorded_mode="${value}" ;;
      skip_solc) recorded_skip_solc="${value}" ;;
      parity_pack) recorded_pack="${value}" ;;
    esac
  done < "${MANIFEST_FILE}"

  [[ "${recorded_digest}" == "${digest}" ]] \
    && [[ "${recorded_mode}" == "${ARTIFACT_MODE}" ]] \
    && [[ "${recorded_skip_solc}" == "${SKIP_SOLC}" ]] \
    && [[ "${recorded_pack}" == "${PARITY_PACK}" ]]
}

write_manifest() {
  local digest="$1"
  cat > "${MANIFEST_FILE}" <<EOF
input_digest=${digest}
artifact_mode=${ARTIFACT_MODE}
skip_solc=${SKIP_SOLC}
parity_pack=${PARITY_PACK}
EOF
}

run_lake_build() {
  (
    cd "${ROOT_DIR}"
    lake build morpho-verity-compiler
  )
}

run_lake_exe() {
  (
    cd "${ROOT_DIR}"
    lake exe morpho-verity-compiler "${compiler_args[@]}"
  )
}

run_uniquify_yul_shadows() {
  python3 "${ROOT_DIR}/scripts/uniquify_yul_shadows.py" \
    --input "${MORPHO_YUL}" \
    --output "${MORPHO_YUL}"
}

run_solc_bin() {
  solc --strict-assembly --bin "${MORPHO_YUL}" \
    | awk '/Binary representation:/{getline; print; exit}' \
    > "${MORPHO_BIN}"
}

run_rewrite_yul() {
  python3 "${ROOT_DIR}/scripts/apply_yul_rewrite_pipeline.py" \
    --input "${MORPHO_YUL}" \
    --output "${MORPHO_REWRITTEN_YUL}" \
    --json-out "${REWRITE_REPORT}"
}

SKIP_BUILD="${MORPHO_VERITY_SKIP_BUILD:-0}"
SKIP_SOLC="${MORPHO_VERITY_SKIP_SOLC:-0}"
ARTIFACT_MODE="${MORPHO_VERITY_ARTIFACT_MODE:-}"
LEGACY_INPUT_MODE="${MORPHO_VERITY_INPUT_MODE:-}"

validate_toggle "MORPHO_VERITY_SKIP_BUILD" "${SKIP_BUILD}"
validate_toggle "MORPHO_VERITY_SKIP_SOLC" "${SKIP_SOLC}"

if [[ -n "${ARTIFACT_MODE}" && -n "${LEGACY_INPUT_MODE}" && "${ARTIFACT_MODE}" != "${LEGACY_INPUT_MODE}" ]]; then
  echo "ERROR: MORPHO_VERITY_ARTIFACT_MODE and MORPHO_VERITY_INPUT_MODE disagree (${ARTIFACT_MODE} vs ${LEGACY_INPUT_MODE})"
  exit 2
fi

if [[ -z "${ARTIFACT_MODE}" ]]; then
  ARTIFACT_MODE="${LEGACY_INPUT_MODE:-edsl}"
fi

if [[ "${ARTIFACT_MODE}" != "edsl" ]]; then
  echo "ERROR: MORPHO_VERITY_ARTIFACT_MODE only supports 'edsl' (got: ${ARTIFACT_MODE})"
  exit 2
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
: > "${TIMINGS_LOG}"

for lib_file in "${HASH_LIB}" "${BORROW_RATE_LIB}" "${ORACLE_PRICE_LIB}" "${COLLATERAL_PRICE_LIB}"; do
  if [[ ! -f "${lib_file}" ]]; then
    echo "ERROR: missing linked library: ${lib_file}"
    exit 2
  fi
done

require_command "lake" "lake is required to build and compile Morpho artifacts"

input_digest="$(compute_input_digest)"
if manifest_matches "${input_digest}" && artifacts_ready; then
  echo "Reusing existing Verity artifact from ${OUT_DIR} (manifest matched)."
  stage_log "reuse-artifact" "ok" "0" "manifest-matched"
  exit 0
fi

if [[ "${SKIP_BUILD}" != "1" ]]; then
  run_stage "lake-build" run_lake_build
fi

compiler_args=(--output "${OUT_DIR}" --abi-output "${OUT_DIR}" --link "${HASH_LIB}" --link "${BORROW_RATE_LIB}" --link "${ORACLE_PRICE_LIB}" --link "${COLLATERAL_PRICE_LIB}" --verbose)
if [[ -n "${PARITY_PACK}" ]]; then
  compiler_args+=(--parity-pack "${PARITY_PACK}")
  echo "Using Verity parity pack: ${PARITY_PACK}"
fi
echo "Using artifact mode: ${ARTIFACT_MODE}"
run_stage "lake-exe" run_lake_exe

if [[ ! -s "${MORPHO_YUL}" ]]; then
  cat <<EOF
ERROR: ${MORPHO_YUL} was not generated.

The Morpho compiler target did not emit the expected artifact.
Check `lake exe morpho-verity-compiler --help` and build logs above.
EOF
  exit 1
fi

require_command "python3" "python3 is required to run the Yul rewrite pipeline"
run_stage "uniquify-yul-shadows" run_uniquify_yul_shadows
run_stage "rewrite-yul" run_rewrite_yul

if [[ "${SKIP_SOLC}" != "1" ]]; then
  require_command "solc" "solc is required unless MORPHO_VERITY_SKIP_SOLC=1"
  require_command "awk" "awk is required to extract bytecode from solc output"
  run_stage "solc-bin" run_solc_bin
fi

if [[ "${SKIP_SOLC}" != "1" && ! -s "${MORPHO_BIN}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_BIN}"
  exit 1
fi
if [[ ! -s "${MORPHO_ABI}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_ABI}"
  exit 1
fi
if [[ ! -s "${MORPHO_REWRITTEN_YUL}" ]]; then
  echo "ERROR: failed to generate ${MORPHO_REWRITTEN_YUL}"
  exit 1
fi

write_manifest "${input_digest}"
echo "Generated Verity artifact: ${MORPHO_YUL}"
echo "Generated rewritten Verity artifact: ${MORPHO_REWRITTEN_YUL}"
if [[ "${SKIP_SOLC}" != "1" ]]; then
  echo "Generated Verity bytecode: ${MORPHO_BIN}"
else
  echo "Skipped bytecode generation (MORPHO_VERITY_SKIP_SOLC=1)"
fi
echo "Generated Verity ABI: ${MORPHO_ABI}"
echo "Rewrite pipeline report: ${REWRITE_REPORT}"
echo "Stage timing log: ${TIMINGS_LOG}"
