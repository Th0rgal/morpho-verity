#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
LOG_DIR="${ROOT_DIR}/out/parity"
mkdir -p "${LOG_DIR}"

# Build latest Verity artifact before running differential tests.
"${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

run_suite() {
  local impl="$1"
  local log_file="${LOG_DIR}/morpho_blue_${impl}.log"
  local status=0
  local foundry_profile=""

  if rg -q "^\[profile\.difftest\]" "${ROOT_DIR}/morpho-blue/foundry.toml"; then
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
  summary="$(rg -n "Ran .* total tests|Ran .*\(.*total tests\)" "${log_file}" -S | tail -n 1 | cut -d: -f2- || true)"
  if [[ -n "${summary}" ]]; then
    echo "    ${impl} summary:${summary}"
  fi

  echo "    ${impl} exit code: ${status}"
  echo "    Log: ${log_file}"
  return "${status}"
}

solidity_status=0
verity_status=0

run_suite solidity || solidity_status=$?
run_suite verity || verity_status=$?

if [[ "${solidity_status}" -ne 0 || "${verity_status}" -ne 0 ]]; then
  echo "==> Differential suite FAILED"
  exit 1
fi

echo "==> Differential suite finished"
