#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/run_with_timeout.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

test_invalid_timeout_value_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  MORPHO_TEST_TIMEOUT_SEC="abc" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 10 "test command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for invalid timeout, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_TEST_TIMEOUT_SEC must be a non-negative integer (got: abc)" "${output_file}"
}

test_timeout_failure_reports_diagnostic() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  MORPHO_TEST_TIMEOUT_SEC="1" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "sleeping command" -- bash -lc 'sleep 2' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 124 ]]; then
    echo "ASSERTION FAILED: expected timeout exit code 124, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: sleeping command timed out after 1s" "${output_file}"
}

test_non_timeout_failure_preserves_exit_code() {
  set +e
  MORPHO_TEST_TIMEOUT_SEC="5" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 1 "failing command" -- bash -lc 'exit 19'
  status=$?
  set -e

  if [[ "${status}" -ne 19 ]]; then
    echo "ASSERTION FAILED: expected non-timeout exit code 19, got ${status}"
    exit 1
  fi
}

test_zero_timeout_disables_timeout_and_preserves_exit_code() {
  set +e
  MORPHO_TEST_TIMEOUT_SEC="0" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 1 "exit command" -- bash -lc 'exit 7'
  status=$?
  set -e

  if [[ "${status}" -ne 7 ]]; then
    echo "ASSERTION FAILED: expected exit code 7 when timeout disabled, got ${status}"
    exit 1
  fi
}

test_timeout_command_missing_fails_closed() {
  local fake_root fake_bin output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s "$(command -v bash)" "${fake_bin}/bash"

  set +e
  PATH="${fake_bin}" \
    MORPHO_TEST_TIMEOUT_SEC="1" \
    bash "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "missing-timeout command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 when timeout command is missing, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: timeout command is required when MORPHO_TEST_TIMEOUT_SEC is greater than zero" "${output_file}"
}

test_success_passthrough() {
  "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "success command" -- bash -lc 'exit 0'
}

test_invalid_timeout_value_fails_closed
test_timeout_failure_reports_diagnostic
test_non_timeout_failure_preserves_exit_code
test_zero_timeout_disables_timeout_and_preserves_exit_code
test_timeout_command_missing_fails_closed
test_success_passthrough

echo "run_with_timeout.sh tests passed"
