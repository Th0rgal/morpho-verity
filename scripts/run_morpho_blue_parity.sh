#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="${ROOT_DIR}/out/parity"
mkdir -p "${LOG_DIR}"

# Fast fail-closed guard before the long differential suite.
"${ROOT_DIR}/scripts/check_input_mode_parity.sh"

# Build latest Verity artifact before running differential tests.
# Reuse the compiler build already produced during parity checking.
MORPHO_VERITY_SKIP_BUILD=1 "${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

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
        forge test -vvv --no-match-path 'test/tmp_yul_deploy.t.sol'
    else
      MORPHO_IMPL="${impl}" forge test -vvv --no-match-path 'test/tmp_yul_deploy.t.sol'
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
