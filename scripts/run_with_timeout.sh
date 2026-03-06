#!/usr/bin/env bash
set -euo pipefail

if [[ "$#" -lt 5 ]]; then
  echo "Usage: $0 <timeout_env_var> <default_timeout_sec> <description> -- <command> [args...]" >&2
  exit 2
fi

timeout_env_var="$1"
default_timeout_sec="$2"
description="$3"
shift 3

if [[ "${1}" != "--" ]]; then
  echo "ERROR: expected '--' before command arguments" >&2
  exit 2
fi
shift

if [[ "$#" -eq 0 ]]; then
  echo "ERROR: missing command to execute" >&2
  exit 2
fi

if [[ ! "${timeout_env_var}" =~ ^[A-Za-z_][A-Za-z0-9_]*$ ]]; then
  echo "ERROR: invalid timeout environment variable name: ${timeout_env_var}" >&2
  exit 2
fi

if [[ ! "${default_timeout_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: default timeout must be a non-negative integer (got: ${default_timeout_sec})" >&2
  exit 2
fi

timeout_sec="${!timeout_env_var:-${default_timeout_sec}}"
if [[ ! "${timeout_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: ${timeout_env_var} must be a non-negative integer (got: ${timeout_sec})" >&2
  exit 2
fi

kill_after_sec="${MORPHO_TIMEOUT_KILL_AFTER_SEC:-30}"
if [[ ! "${kill_after_sec}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be a non-negative integer (got: ${kill_after_sec})" >&2
  exit 2
fi
if [[ "${kill_after_sec}" -eq 0 ]]; then
  echo "ERROR: MORPHO_TIMEOUT_KILL_AFTER_SEC must be greater than zero to keep hard-kill timeout semantics" >&2
  exit 2
fi

if [[ "${timeout_sec}" -eq 0 ]]; then
  "$@"
  exit $?
fi

if ! command -v setsid >/dev/null 2>&1; then
  echo "ERROR: setsid command is required when ${timeout_env_var} is greater than zero" >&2
  exit 2
fi

tmp_dir="$(mktemp -d)"
status_file="${tmp_dir}/timeout-status"
session_pid_file="${tmp_dir}/session-pid"
command_status_file="${tmp_dir}/command-status"
launcher_script="${tmp_dir}/launch-command.sh"
command_pid=""
session_pid=""
watchdog_pid=""
signal_pid=""
signal_target_mode=""

load_session_pid() {
  local loaded_session_pid=""

  if [[ -n "${session_pid}" || ! -s "${session_pid_file}" ]]; then
    return 0
  fi

  loaded_session_pid="$(tr -d '[:space:]' < "${session_pid_file}")"
  if [[ -z "${loaded_session_pid}" ]]; then
    return 0
  fi
  if [[ ! "${loaded_session_pid}" =~ ^[0-9]+$ ]]; then
    echo "ERROR: invalid session leader pid captured for ${description}: ${loaded_session_pid}" >&2
    return 1
  fi

  session_pid="${loaded_session_pid}"
  signal_pid="${session_pid}"
  signal_target_mode="group"
}

refresh_signal_target() {
  load_session_pid || return 1

  if [[ -n "${session_pid}" ]]; then
    signal_pid="${session_pid}"
    signal_target_mode="group"
  elif [[ -n "${command_pid}" ]]; then
    signal_pid="${command_pid}"
    signal_target_mode="pid"
  else
    signal_pid=""
    signal_target_mode=""
  fi
}

wait_for_session_target() {
  local attempts="${1:-20}"

  for _ in $(seq 1 "${attempts}"); do
    load_session_pid || return 1
    if [[ -n "${session_pid}" ]]; then
      return 0
    fi
    if [[ -z "${command_pid}" ]] || ! kill -0 "${command_pid}" >/dev/null 2>&1; then
      return 0
    fi
    sleep 0.05
  done

  load_session_pid
}

stop_watchdog() {
  if [[ -n "${watchdog_pid}" ]]; then
    kill "${watchdog_pid}" >/dev/null 2>&1 || true
    wait "${watchdog_pid}" >/dev/null 2>&1 || true
  fi
}

signal_target_is_alive() {
  refresh_signal_target || return 1
  if [[ -z "${signal_pid}" ]]; then
    return 1
  fi
  if [[ "${signal_target_mode}" == "group" ]]; then
    kill -0 "-${signal_pid}" >/dev/null 2>&1
  else
    kill -0 "${signal_pid}" >/dev/null 2>&1
  fi
}

send_signal_to_target() {
  local sig="$1"

  refresh_signal_target || return 1
  if [[ -z "${signal_pid}" ]]; then
    return 1
  fi

  if [[ "${signal_target_mode}" == "group" ]]; then
    kill "-${sig}" "-${signal_pid}" >/dev/null 2>&1 || true
  else
    kill "-${sig}" "${signal_pid}" >/dev/null 2>&1 || true
  fi
}

terminate_running_command() {
  local reason="${1:-}"
  local target_pid=""
  local target_mode=""

  refresh_signal_target || return 1
  if [[ -z "${signal_pid}" ]]; then
    return 1
  fi
  if ! signal_target_is_alive; then
    return 1
  fi
  target_pid="${signal_pid}"
  target_mode="${signal_target_mode}"

  if [[ -n "${reason}" ]]; then
    echo "WARNING: ${description} interrupted by ${reason}; terminating ${target_mode} ${target_pid}" >&2
  fi

  send_signal_to_target TERM
  sleep 1

  if signal_target_is_alive; then
    send_signal_to_target KILL
  fi
}

cleanup() {
  stop_watchdog
  terminate_running_command >/dev/null 2>&1 || true
  rm -rf "${tmp_dir}"
}

trap cleanup EXIT

handle_signal() {
  local sig="$1"
  local signal_number
  signal_number="$(kill -l "${sig}")"

  trap - EXIT TERM INT HUP
  stop_watchdog
  wait_for_session_target || true
  terminate_running_command "signal ${sig}" || true
  if [[ -n "${command_pid}" ]]; then
    wait "${command_pid}" >/dev/null 2>&1 || true
  fi
  rm -rf "${tmp_dir}"
  exit $((128 + signal_number))
}

trap 'handle_signal TERM' TERM
trap 'handle_signal INT' INT
trap 'handle_signal HUP' HUP

cat > "${launcher_script}" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail

session_pid_file="$1"
command_status_file="$2"
shift 2

set +e
setsid --wait bash -lc '
  set -euo pipefail
  session_pid_file="$1"
  shift
  printf "%s\n" "$$" > "${session_pid_file}"
  exec "$@"
' bash "${session_pid_file}" "$@"
status=$?
set -e
printf "%s\n" "${status}" > "${command_status_file}"
exit "${status}"
EOF
chmod +x "${launcher_script}"

start_epoch="$(date +%s)"
"${launcher_script}" "${session_pid_file}" "${command_status_file}" "$@" &
command_pid=$!

for _ in $(seq 1 50); do
  if [[ -s "${session_pid_file}" ]]; then
    load_session_pid || exit 1
    break
  fi
  if ! kill -0 "${command_pid}" >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

load_session_pid || exit 1

watchdog() {
  local sleep_pid=""
  exec </dev/null >/dev/null 2>&1

  cleanup_watchdog() {
    if [[ -n "${sleep_pid}" ]]; then
      kill "${sleep_pid}" >/dev/null 2>&1 || true
      wait "${sleep_pid}" >/dev/null 2>&1 || true
    fi
  }

  trap cleanup_watchdog EXIT TERM INT

  sleep "${timeout_sec}" &
  sleep_pid=$!
  wait "${sleep_pid}" >/dev/null 2>&1 || true
  sleep_pid=""

  if signal_target_is_alive; then
    printf 'timeout\n' > "${status_file}"
    send_signal_to_target TERM

    sleep "${kill_after_sec}" &
    sleep_pid=$!
    wait "${sleep_pid}" >/dev/null 2>&1 || true
    sleep_pid=""

    if signal_target_is_alive; then
      printf 'kill-after\n' > "${status_file}"
      send_signal_to_target KILL
    fi
  fi
}

watchdog &
watchdog_pid=$!

status=""
for _ in $(seq 1 $((timeout_sec * 20 + 200))); do
  if [[ -s "${command_status_file}" ]]; then
    status="$(tr -d '[:space:]' < "${command_status_file}")"
    break
  fi
  if ! kill -0 "${command_pid}" >/dev/null 2>&1; then
    break
  fi
  sleep 0.05
done

if [[ -z "${status}" ]]; then
  set +e
  wait "${command_pid}"
  status=$?
  set -e
else
  wait "${command_pid}" >/dev/null 2>&1 || true
fi

stop_watchdog

if signal_target_is_alive; then
  echo "WARNING: ${description} left descendant processes running; terminating ${signal_target_mode} ${signal_pid}" >&2
  send_signal_to_target TERM
  sleep 1
  if signal_target_is_alive; then
    send_signal_to_target KILL
  fi
fi

end_epoch="$(date +%s)"
elapsed_sec="$((end_epoch - start_epoch))"

if [[ -f "${status_file}" ]]; then
  case "$(cat "${status_file}")" in
    timeout) status=124 ;;
    kill-after) status=137 ;;
  esac
fi

if [[ "${status}" -eq 124 || "${status}" -eq 137 ]]; then
  echo "ERROR: ${description} timed out after ${timeout_sec}s" >&2
  echo "ERROR: timeout env=${timeout_env_var} kill-after=${kill_after_sec}s elapsed=${elapsed_sec}s exit=${status}" >&2
  echo "ERROR: remediate by increasing ${timeout_env_var} or reducing work in this stage" >&2
fi
exit "${status}"
