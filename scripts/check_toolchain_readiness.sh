#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_toolchain_readiness.sh [--require <lean|foundry|solc>]...

Checks that required toolchain binaries are available and executable.
When no --require flags are provided, defaults to: lean, foundry, solc.
USAGE
}

requirement_enabled() {
  local name="$1"
  shift
  local item
  for item in "$@"; do
    if [[ "${item}" == "${name}" ]]; then
      return 0
    fi
  done
  return 1
}

check_binary() {
  local label="$1"
  local bin="$2"
  if ! command -v "${bin}" >/dev/null 2>&1; then
    echo "ERROR: missing required ${label} binary: ${bin}" >&2
    return 1
  fi
  return 0
}

show_version() {
  local bin="$1"
  local fallback="$2"
  if ! "${bin}" --version 2>/dev/null; then
    if [[ -n "${fallback}" ]]; then
      "${bin}" "${fallback}" 2>/dev/null || true
    fi
  fi
}

requirements=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    --require)
      if [[ $# -lt 2 ]]; then
        echo "ERROR: missing value for --require" >&2
        usage >&2
        exit 2
      fi
      case "$2" in
        lean|foundry|solc)
          requirements+=("$2")
          ;;
        *)
          echo "ERROR: invalid --require value '$2' (expected lean, foundry, or solc)" >&2
          usage >&2
          exit 2
          ;;
      esac
      shift 2
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "ERROR: unknown argument: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
done

if [[ ${#requirements[@]} -eq 0 ]]; then
  requirements=(lean foundry solc)
fi

status=0

if requirement_enabled lean "${requirements[@]}"; then
  check_binary "Lean toolchain" lean || status=1
  check_binary "Lean toolchain" lake || status=1
fi

if requirement_enabled foundry "${requirements[@]}"; then
  check_binary "Foundry" forge || status=1
  check_binary "Foundry" anvil || status=1
fi

if requirement_enabled solc "${requirements[@]}"; then
  check_binary "solc toolchain" solc || status=1
  check_binary "solc toolchain" solc-select || status=1
fi

if [[ "${status}" -ne 0 ]]; then
  echo "ERROR: toolchain readiness check failed." >&2
  echo "DEBUG: PATH=${PATH}" >&2
  exit 127
fi

echo "Toolchain readiness check passed for requirements: ${requirements[*]}"
if requirement_enabled lean "${requirements[@]}"; then
  show_version lean ""
fi
if requirement_enabled foundry "${requirements[@]}"; then
  show_version forge ""
  show_version anvil ""
fi
if requirement_enabled solc "${requirements[@]}"; then
  show_version solc ""
  show_version solc-select "versions"
fi
