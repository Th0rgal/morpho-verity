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

test_usage_fails_closed_on_missing_args() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 10 "desc" -- >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for missing args, got ${status}"
    exit 1
  fi
  assert_contains "Usage: " "${output_file}"
}

test_invalid_timeout_env_var_name_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  "${SCRIPT_UNDER_TEST}" 1INVALID 10 "desc" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for invalid timeout env var name, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: invalid timeout environment variable name: 1INVALID" "${output_file}"
}

test_invalid_default_timeout_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC nope "desc" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for invalid default timeout, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: default timeout must be a non-negative integer (got: nope)" "${output_file}"
}

test_missing_separator_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 10 "desc" bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for missing '--' separator, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: expected '--' before command arguments" "${output_file}"
}

test_default_timeout_is_used_when_env_unset() {
  set +e
  env -u MORPHO_TEST_TIMEOUT_SEC \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 0 "default-zero timeout" -- bash -lc 'exit 9'
  status=$?
  set -e

  if [[ "${status}" -ne 9 ]]; then
    echo "ASSERTION FAILED: expected default timeout value to apply when env var unset, got ${status}"
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

test_invalid_kill_after_value_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  MORPHO_TIMEOUT_KILL_AFTER_SEC="bad" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 10 "test command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for invalid kill-after timeout, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be a non-negative integer (got: bad)" "${output_file}"
}

test_zero_kill_after_value_fails_closed() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  MORPHO_TIMEOUT_KILL_AFTER_SEC="0" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 10 "test command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 for zero kill-after timeout, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be greater than zero to keep hard-kill timeout semantics" "${output_file}"
}

test_timeout_failure_reports_diagnostic() {
  local output_file
  local expected_kill_after
  output_file="$(mktemp)"
  expected_kill_after="${MORPHO_TIMEOUT_KILL_AFTER_SEC:-30}"
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
  assert_contains "ERROR: timeout env=MORPHO_TEST_TIMEOUT_SEC kill-after=${expected_kill_after}s" "${output_file}"
  assert_contains "ERROR: remediate by increasing MORPHO_TEST_TIMEOUT_SEC or reducing work in this stage" "${output_file}"
}

test_timeout_kills_term_ignoring_processes() {
  local output_file
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  MORPHO_TEST_TIMEOUT_SEC="1" \
    MORPHO_TIMEOUT_KILL_AFTER_SEC="1" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "term-ignoring command" -- \
      bash -lc 'trap "" TERM; while true; do sleep 5; done' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 137 ]]; then
    echo "ASSERTION FAILED: expected kill-after timeout exit code 137, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: term-ignoring command timed out after 1s" "${output_file}"
  assert_contains "ERROR: timeout env=MORPHO_TEST_TIMEOUT_SEC kill-after=1s" "${output_file}"
  assert_contains "ERROR: remediate by increasing MORPHO_TEST_TIMEOUT_SEC or reducing work in this stage" "${output_file}"
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

test_usage_fails_closed_on_missing_args
test_invalid_timeout_env_var_name_fails_closed
test_invalid_default_timeout_fails_closed
test_missing_separator_fails_closed
test_default_timeout_is_used_when_env_unset
test_invalid_timeout_value_fails_closed
test_invalid_kill_after_value_fails_closed
test_zero_kill_after_value_fails_closed
test_timeout_failure_reports_diagnostic
test_timeout_kills_term_ignoring_processes
test_non_timeout_failure_preserves_exit_code
test_zero_timeout_disables_timeout_and_preserves_exit_code
test_timeout_command_missing_fails_closed
test_success_passthrough

echo "run_with_timeout.sh tests passed"
