#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
RUN_WITH_TIMEOUT="${ROOT_DIR}/scripts/run_with_timeout.sh"
PATCH_MORPHO_MIDNIGHT_HARNESS="${ROOT_DIR}/scripts/patch_morpho_midnight_harness.py"
INSTALL_SOLC="${ROOT_DIR}/scripts/install_solc.sh"
LOG_DIR="${ROOT_DIR}/out/parity"
SUITE_TIMEOUT_SEC="${MORPHO_MIDNIGHT_SUITE_TIMEOUT_SEC:-0}"
MODE="${MORPHO_MIDNIGHT_PARITY_MODE:-both}"
ARTIFACT_RAW="${MORPHO_MIDNIGHT_ARTIFACT_RAW:-${ROOT_DIR}/artifacts/midnight/Midnight.bin.raw}"
SOLC_VERSION="${MORPHO_MIDNIGHT_SOLC_VERSION:-0.8.34}"

mkdir -p "${LOG_DIR}"

if ! [[ "${SUITE_TIMEOUT_SEC}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: MORPHO_MIDNIGHT_SUITE_TIMEOUT_SEC must be a non-negative integer (got: ${SUITE_TIMEOUT_SEC})"
  exit 2
fi

case "${MODE}" in
  both | solidity | verity) ;;
  *)
    echo "ERROR: MORPHO_MIDNIGHT_PARITY_MODE must be both, solidity, or verity (got: ${MODE})"
    exit 2
    ;;
esac

require_midnight_repo() {
  if [[ ! -f "${ROOT_DIR}/morpho-midnight/test/BaseTest.sol" ]]; then
    echo "ERROR: missing morpho-midnight submodule. Run: git submodule update --init --recursive"
    exit 2
  fi
}

ensure_solc_version() {
  if command -v solc >/dev/null 2>&1 && solc --version | grep -F "Version: ${SOLC_VERSION}" >/dev/null; then
    return
  fi

  if [[ ! -x "${INSTALL_SOLC}" ]]; then
    echo "ERROR: Midnight requires solc ${SOLC_VERSION}, and ${INSTALL_SOLC} is not executable."
    exit 2
  fi

  "${RUN_WITH_TIMEOUT}" MORPHO_SOLC_INSTALL_TIMEOUT_SEC 600 \
    "Install Midnight solc ${SOLC_VERSION}" -- \
    "${INSTALL_SOLC}" "${SOLC_VERSION}"
}

require_midnight_impl_wiring() {
  local base_test="${ROOT_DIR}/morpho-midnight/test/BaseTest.sol"
  local selector_pattern='vm\.env(String|Or)[[:space:]]*\([^)]*"MIDNIGHT_IMPL"'
  python3 "${PATCH_MORPHO_MIDNIGHT_HARNESS}"
  if ! grep -Eq "${selector_pattern}" "${base_test}"; then
    cat <<EOF
ERROR: Morpho Midnight harness does not consume MIDNIGHT_IMPL.

Expected ${base_test} to read MIDNIGHT_IMPL=solidity|verity via an explicit Foundry env lookup.
EOF
    exit 2
  fi

  local bypassing_deploys
  bypassing_deploys="$(grep -RFn "new Midnight(" "${ROOT_DIR}/morpho-midnight/test" | grep -Fv "/BaseTest.sol:" || true)"
  if [[ -n "${bypassing_deploys}" ]]; then
    cat <<EOF
ERROR: found Morpho Midnight tests that bypass the MIDNIGHT_IMPL deployment selector:
${bypassing_deploys}

Route test deployments through the shared harness before claiming Solidity/Verity parity.
EOF
    exit 2
  fi
}

require_verity_artifact() {
  if [[ ! -s "${ARTIFACT_RAW}" ]]; then
    cat <<EOF
ERROR: missing Midnight Verity deployment artifact: ${ARTIFACT_RAW}

The current Midnight Lean package is a focused proof model. To run the original
Midnight tests against a compiled Verity implementation, provide a complete
Midnight creation bytecode artifact at artifacts/midnight/Midnight.bin.raw or
set MORPHO_MIDNIGHT_ARTIFACT_RAW to that file.
EOF
    exit 2
  fi
}

run_suite() {
  local impl="$1"
  local log_file="${LOG_DIR}/morpho_midnight_${impl}.log"
  local status=0
  local foundry_profile=""
  local solc_bin
  solc_bin="$(command -v solc)"

  if grep -Eq "^\[profile\.difftest\]" "${ROOT_DIR}/morpho-midnight/foundry.toml"; then
    foundry_profile="difftest"
  fi

  echo "==> Running Morpho Midnight suite with MIDNIGHT_IMPL=${impl}"
  set +e
  (
    cd "${ROOT_DIR}/morpho-midnight"
    if [[ -n "${foundry_profile}" ]]; then
      FOUNDRY_PROFILE="${foundry_profile}" MIDNIGHT_IMPL="${impl}" \
        "${RUN_WITH_TIMEOUT}" MORPHO_MIDNIGHT_SUITE_TIMEOUT_SEC 0 \
        "Morpho Midnight suite (${impl})" -- \
        forge test -vvv --use "${solc_bin}"
    else
      MIDNIGHT_IMPL="${impl}" \
        "${RUN_WITH_TIMEOUT}" MORPHO_MIDNIGHT_SUITE_TIMEOUT_SEC 0 \
        "Morpho Midnight suite (${impl})" -- \
        forge test -vvv --use "${solc_bin}"
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

require_midnight_repo
require_midnight_impl_wiring
ensure_solc_version

if [[ "${MODE}" == "verity" || "${MODE}" == "both" ]]; then
  require_verity_artifact
  mkdir -p "${ROOT_DIR}/artifacts/midnight"
  if [[ "${ARTIFACT_RAW}" != "${ROOT_DIR}/artifacts/midnight/Midnight.bin.raw" ]]; then
    cp "${ARTIFACT_RAW}" "${ROOT_DIR}/artifacts/midnight/Midnight.bin.raw"
  fi
fi

solidity_status=0
verity_status=0

if [[ "${MODE}" == "solidity" || "${MODE}" == "both" ]]; then
  run_suite solidity || solidity_status=$?
fi
if [[ "${MODE}" == "verity" || "${MODE}" == "both" ]]; then
  run_suite verity || verity_status=$?
fi

if [[ "${solidity_status}" -ne 0 || "${verity_status}" -ne 0 ]]; then
  echo "==> Morpho Midnight parity FAILED"
  exit 1
fi

echo "==> Morpho Midnight parity completed"
