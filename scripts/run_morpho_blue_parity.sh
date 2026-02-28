#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_WITH_TIMEOUT="${ROOT_DIR}/scripts/run_with_timeout.sh"
LOG_DIR="${ROOT_DIR}/out/parity"
PARITY_OUT_DIR="$(mktemp -d)"
SKIP_PARITY_PREFLIGHT="${MORPHO_VERITY_SKIP_PARITY_PREFLIGHT:-0}"
ALLOW_LOCAL_SKIP="${MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP:-0}"
SUITE_TIMEOUT_SEC="${MORPHO_BLUE_SUITE_TIMEOUT_SEC:-0}"
trap 'rm -rf "${PARITY_OUT_DIR}"' EXIT
mkdir -p "${LOG_DIR}"

if [[ ! "${SUITE_TIMEOUT_SEC}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: MORPHO_BLUE_SUITE_TIMEOUT_SEC must be a non-negative integer (got: ${SUITE_TIMEOUT_SEC})"
  exit 1
fi

if [[ "${SUITE_TIMEOUT_SEC}" -gt 0 ]] && ! command -v timeout >/dev/null 2>&1; then
  echo "ERROR: timeout command is required when MORPHO_BLUE_SUITE_TIMEOUT_SEC is greater than zero"
  exit 1
fi

if [[ "${SKIP_PARITY_PREFLIGHT}" == "1" ]]; then
  if [[ "${CI:-}" != "true" && "${ALLOW_LOCAL_SKIP}" != "1" ]]; then
    echo "Refusing to skip parity preflight outside CI."
    echo "Set MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1 only for explicit local debugging."
    exit 1
  fi
  echo "Skipping input-mode parity preflight (MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1)."
  MORPHO_VERITY_OUT_DIR="${PARITY_OUT_DIR}/edsl" \
  MORPHO_VERITY_INPUT_MODE="edsl" \
    "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"
else
  # Fast fail-closed guard before the long differential suite.
  MORPHO_VERITY_PARITY_OUT_DIR="${PARITY_OUT_DIR}" \
    "${ROOT_DIR}/scripts/check_input_mode_parity.sh"
fi

# Reuse the verified EDSL artifact already produced by parity checking.
mkdir -p "${ROOT_DIR}/compiler/yul"
cp "${PARITY_OUT_DIR}/edsl/Morpho.yul" "${ROOT_DIR}/compiler/yul/Morpho.yul"
cp "${PARITY_OUT_DIR}/edsl/Morpho.bin" "${ROOT_DIR}/compiler/yul/Morpho.bin"
cp "${PARITY_OUT_DIR}/edsl/Morpho.abi.json" "${ROOT_DIR}/compiler/yul/Morpho.abi.json"

if [[ "${MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP:-0}" == "1" ]]; then
  echo "Exiting after artifact preparation (MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1)."
  exit 0
fi

run_suite() {
  local impl="$1"
  local log_file="${LOG_DIR}/morpho_blue_${impl}.log"
  local status=0
  local foundry_profile=""

  if grep -Eq "^\[profile\.difftest\]" "${ROOT_DIR}/morpho-blue/foundry.toml"; then
    foundry_profile="difftest"
  fi

  echo "==> Running Morpho Blue suite with MORPHO_IMPL=${impl}"
  set +e
  (
    cd "${ROOT_DIR}/morpho-blue"
    if [[ -n "${foundry_profile}" ]]; then
      FOUNDRY_PROFILE="${foundry_profile}" MORPHO_IMPL="${impl}" \
        "${RUN_WITH_TIMEOUT}" MORPHO_BLUE_SUITE_TIMEOUT_SEC 0 \
        "Morpho Blue suite (${impl})" -- \
        forge test -vvv --no-match-path 'test/tmp_yul_deploy.t.sol'
    else
      MORPHO_IMPL="${impl}" \
        "${RUN_WITH_TIMEOUT}" MORPHO_BLUE_SUITE_TIMEOUT_SEC 0 \
        "Morpho Blue suite (${impl})" -- \
        forge test -vvv --no-match-path 'test/tmp_yul_deploy.t.sol'
    fi
  ) | tee "${log_file}"
  status=${PIPESTATUS[0]}
  set -e

  local summary
  summary="$(grep -En "Ran .* total tests|Ran .*\(.*total tests\)" "${log_file}" | tail -n 1 | cut -d: -f2- || true)"
  if [[ -n "${summary}" ]]; then
    echo "    ${impl} summary:${summary}"
  fi

  echo "    ${impl} exit code: ${status}"
  echo "    Log: ${log_file}"
  if [[ "${status}" -eq 124 ]]; then
    echo "    ${impl} timed out after ${SUITE_TIMEOUT_SEC}s"
  fi
  return "${status}"
}

extract_total_test_count() {
  local log_file="$1"
  local total=0
  local counts

  # Prefer Forge's explicit "(N total tests)" summary when present.
  total="$(grep -Eo "[0-9]+ total tests" "${log_file}" | tail -n 1 | grep -Eo "[0-9]+" || true)"
  if [[ "${total}" -gt 0 ]]; then
    echo "${total}"
    return
  fi

  counts="$(grep -Eo "Ran [0-9]+ tests? for " "${log_file}" | grep -Eo "[0-9]+" || true)"
  if [[ -z "${counts}" ]]; then
    echo 0
    return
  fi

  while IFS= read -r n; do
    [[ -n "${n}" ]] || continue
    total=$((total + n))
  done <<< "${counts}"
  echo "${total}"
}

solidity_status=0
verity_status=0

run_suite solidity || solidity_status=$?
run_suite verity || verity_status=$?

if [[ "${solidity_status}" -ne 0 || "${verity_status}" -ne 0 ]]; then
  if [[ "${solidity_status}" -eq 124 || "${verity_status}" -eq 124 ]]; then
    echo "==> Differential suite FAILED: timeout (MORPHO_BLUE_SUITE_TIMEOUT_SEC=${SUITE_TIMEOUT_SEC})"
  fi
  echo "==> Differential suite FAILED"
  exit 1
fi

solidity_count="$(extract_total_test_count "${LOG_DIR}/morpho_blue_solidity.log")"
verity_count="$(extract_total_test_count "${LOG_DIR}/morpho_blue_verity.log")"

echo "==> Total executed tests: solidity=${solidity_count}, verity=${verity_count}"
if [[ "${solidity_count}" -eq 0 || "${verity_count}" -eq 0 ]]; then
  echo "==> Unable to parse test counts from parity logs"
  exit 1
fi

if [[ "${solidity_count}" -ne "${verity_count}" ]]; then
  echo "==> Differential suite FAILED: test-count mismatch"
  exit 1
fi

echo "==> Differential suite finished"
