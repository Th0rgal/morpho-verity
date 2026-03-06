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

cleanup() {
  if [[ -n "${watchdog_pid}" ]]; then
    kill "${watchdog_pid}" >/dev/null 2>&1 || true
    wait "${watchdog_pid}" >/dev/null 2>&1 || true
  fi
  rm -rf "${tmp_dir}"
}

trap cleanup EXIT

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
    session_pid="$(tr -d '[:space:]' < "${session_pid_file}")"
    break
  fi
  if ! kill -0 "${command_pid}" >/dev/null 2>&1; then
    break
  fi
  sleep 0.1
done

if [[ -n "${session_pid}" && ! "${session_pid}" =~ ^[0-9]+$ ]]; then
  echo "ERROR: invalid session leader pid captured for ${description}: ${session_pid}" >&2
  exit 1
fi

signal_pid="${session_pid:-${command_pid}}"

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

  if kill -0 "${command_pid}" >/dev/null 2>&1; then
    printf 'timeout\n' > "${status_file}"
    kill -TERM "-${signal_pid}" >/dev/null 2>&1 || true

    sleep "${kill_after_sec}" &
    sleep_pid=$!
    wait "${sleep_pid}" >/dev/null 2>&1 || true
    sleep_pid=""

    if kill -0 "${command_pid}" >/dev/null 2>&1; then
      printf 'kill-after\n' > "${status_file}"
      kill -KILL "-${signal_pid}" >/dev/null 2>&1 || true
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

kill "${watchdog_pid}" >/dev/null 2>&1 || true
wait "${watchdog_pid}" >/dev/null 2>&1 || true

if kill -0 "-${signal_pid}" >/dev/null 2>&1; then
  echo "WARNING: ${description} left descendant processes running; terminating process group ${signal_pid}" >&2
  kill -TERM "-${signal_pid}" >/dev/null 2>&1 || true
  sleep 1
  if kill -0 "-${signal_pid}" >/dev/null 2>&1; then
    kill -KILL "-${signal_pid}" >/dev/null 2>&1 || true
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
