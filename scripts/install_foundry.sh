#!/usr/bin/env bash
set -euo pipefail

retry() {
  local attempts="$1"
  shift
  local n=1
  local rc=0
  while true; do
    if "$@"; then
      return 0
    fi
    rc=$?
    if [[ "${n}" -ge "${attempts}" ]]; then
      return "${rc}"
    fi
    local sleep_sec=$((2 ** n))
    echo "WARN: command failed (attempt ${n}/${attempts}, rc=${rc}); retrying in ${sleep_sec}s: $*" >&2
    sleep "${sleep_sec}"
    n=$((n + 1))
  done
}

if command -v forge >/dev/null 2>&1 && command -v anvil >/dev/null 2>&1; then
  echo "Foundry already available:"
  forge --version
  anvil --version
  exit 0
fi

export PATH="$HOME/.foundry/bin:$PATH"
if [[ -n "${GITHUB_PATH:-}" ]]; then
  echo "$HOME/.foundry/bin" >> "$GITHUB_PATH"
fi

# CI caches commonly restore binaries under ~/.foundry/bin without PATH wiring.
if command -v forge >/dev/null 2>&1 && command -v anvil >/dev/null 2>&1; then
  echo "Foundry available after PATH bootstrap:"
  forge --version
  anvil --version
  exit 0
fi

if ! command -v foundryup >/dev/null 2>&1; then
  retry 4 bash -lc 'curl -fsSL https://foundry.paradigm.xyz | bash'
fi

retry 4 foundryup

forge --version
anvil --version
