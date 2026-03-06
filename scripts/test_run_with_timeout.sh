#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/run_with_timeout.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq -- "${needle}" "${haystack_file}"; then
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

test_timeout_kills_descendant_process_group() {
  local temp_dir output_file child_script marker
  temp_dir="$(mktemp -d)"
  output_file="$(mktemp)"
  marker="morpho-timeout-descendant-$$"
  child_script="${temp_dir}/spawn-descendant.sh"
  trap 'pkill -f "${marker}" >/dev/null 2>&1 || true; rm -rf "${temp_dir}" "${output_file}"' RETURN

  cat > "${child_script}" <<EOF
#!/usr/bin/env bash
set -euo pipefail
trap '' TERM
exec -a "${marker}" bash -lc 'trap "" TERM; while true; do sleep 5; done' &
while true; do sleep 5; done
EOF
  chmod +x "${child_script}"

  set +e
  MORPHO_TEST_TIMEOUT_SEC="1" \
    MORPHO_TIMEOUT_KILL_AFTER_SEC="1" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "descendant command" -- \
      "${child_script}" >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 137 ]]; then
    echo "ASSERTION FAILED: expected descendant timeout exit code 137, got ${status}"
    exit 1
  fi
  if pgrep -f "${marker}" >/dev/null 2>&1; then
    echo "ASSERTION FAILED: expected descendant process group to be terminated"
    exit 1
  fi
  assert_contains "ERROR: descendant command timed out after 1s" "${output_file}"
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

test_setsid_command_missing_fails_closed() {
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
    bash "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "missing-setsid command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2 when setsid command is missing, got ${status}"
    exit 1
  fi
  assert_contains "ERROR: setsid command is required when MORPHO_TEST_TIMEOUT_SEC is greater than zero" "${output_file}"
}

test_setsid_waits_for_command_exit() {
  local fake_root fake_bin output_file args_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  args_file="${fake_root}/setsid-args"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  cat > "${fake_bin}/setsid" <<EOF
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "\$@" > "${args_file}"
if [[ "\${1:-}" != "--wait" ]]; then
  echo "expected --wait" >&2
  exit 99
fi
shift
exec "\$@"
EOF
  chmod +x "${fake_bin}/setsid"

  set +e
  PATH="${fake_bin}:$PATH" \
    MORPHO_TEST_TIMEOUT_SEC="1" \
    "${SCRIPT_UNDER_TEST}" MORPHO_TEST_TIMEOUT_SEC 5 "setsid wait command" -- bash -lc 'exit 0' >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -ne 0 ]]; then
    echo "ASSERTION FAILED: expected wrapped command to succeed with fake setsid, got ${status}"
    exit 1
  fi
  assert_contains "--wait" "${args_file}"
}

test_setsid_fork_path_targets_session_leader_process_group() {
  local fake_root fake_bin output_file marker runner_status
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  marker="morpho-timeout-forked-$$"
  trap 'pkill -f "${marker}" >/dev/null 2>&1 || true; rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  cat > "${fake_bin}/setsid" <<EOF
#!/usr/bin/env bash
set -euo pipefail
if [[ "\${1:-}" != "--wait" ]]; then
  echo "expected --wait" >&2
  exit 99
fi
shift
exec /usr/bin/setsid --fork --wait "\$@"
EOF
  chmod +x "${fake_bin}/setsid"

  set +e
  PATH="${fake_bin}:$PATH" python3 - <<'PY' "${SCRIPT_UNDER_TEST}" "${output_file}" "${marker}"
import os
import subprocess
import sys

script, output_file, marker = sys.argv[1:]
env = os.environ.copy()
env["MORPHO_TEST_TIMEOUT_SEC"] = "1"
env["MORPHO_TIMEOUT_KILL_AFTER_SEC"] = "1"
cmd = [
    script,
    "MORPHO_TEST_TIMEOUT_SEC",
    "5",
    "forked-setsid command",
    "--",
    "bash",
    "-lc",
    f'trap "" TERM; exec -a "{marker}" bash -lc \'trap "" TERM; while true; do sleep 5; done\'',
]
with open(output_file, "w", encoding="utf-8") as out:
    try:
        proc = subprocess.run(cmd, stdout=out, stderr=subprocess.STDOUT, env=env, timeout=8)
    except subprocess.TimeoutExpired:
        sys.exit(124)
sys.exit(proc.returncode)
PY
  runner_status=$?
  set -e

  if [[ "${runner_status}" -ne 137 ]]; then
    echo "ASSERTION FAILED: expected forked setsid path to return 137, got ${runner_status}"
    cat "${output_file}" || true
    exit 1
  fi
  if pgrep -f "${marker}" >/dev/null 2>&1; then
    echo "ASSERTION FAILED: expected forked setsid session leader process group to be terminated"
    exit 1
  fi
  assert_contains "ERROR: forked-setsid command timed out after 1s" "${output_file}"
}

test_timeout_pipeline_does_not_leave_tee_waiting_on_watchdog_sleep() {
  local output_file runner_status
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  set +e
  python3 - <<'PY' "${SCRIPT_UNDER_TEST}" "${output_file}"
import os
import subprocess
import sys

script, output_file = sys.argv[1:]
cmd = (
    'set -o pipefail; '
    'MORPHO_TEST_TIMEOUT_SEC=1 '
    f'"{script}" MORPHO_TEST_TIMEOUT_SEC 5 pipeline-timeout -- '
    'bash -lc "sleep 2" | tee /tmp/morpho-timeout-pipeline.log'
)
env = os.environ.copy()
with open(output_file, "w", encoding="utf-8") as out:
    try:
        proc = subprocess.run(
            ["bash", "-lc", cmd],
            stdout=out,
            stderr=subprocess.STDOUT,
            env=env,
            timeout=8,
        )
    except subprocess.TimeoutExpired:
        sys.exit(124)
sys.exit(proc.returncode)
PY
  runner_status=$?
  set -e

  rm -f /tmp/morpho-timeout-pipeline.log

  if [[ "${runner_status}" -ne 124 ]]; then
    echo "ASSERTION FAILED: expected piped timeout to return 124, got ${runner_status}"
    cat "${output_file}" || true
    exit 1
  fi
  assert_contains "ERROR: pipeline-timeout timed out after 1s" "${output_file}"
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
test_timeout_kills_descendant_process_group
test_non_timeout_failure_preserves_exit_code
test_zero_timeout_disables_timeout_and_preserves_exit_code
test_setsid_command_missing_fails_closed
test_setsid_waits_for_command_exit
test_setsid_fork_path_targets_session_leader_process_group
test_timeout_pipeline_does_not_leave_tee_waiting_on_watchdog_sleep
test_success_passthrough

echo "run_with_timeout.sh tests passed"
