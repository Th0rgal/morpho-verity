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

resolve_tool_candidate() {
  local tool="$1"
  local candidate=""

  if candidate="$(command -v "${tool}" 2>/dev/null || true)" && [[ -n "${candidate}" && -x "${candidate}" ]]; then
    printf '%s\n' "${candidate}"
    return 0
  fi

  while IFS= read -r match; do
    if [[ -x "${match}" ]]; then
      printf '%s\n' "${match}"
      return 0
    fi
  done < <(find "${HOME}/.foundry" -type f \( -name "${tool}" -o -name "${tool}-*" \) 2>/dev/null || true)

  return 1
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

FOUNDRYUP_BIN="$HOME/.foundry/bin/foundryup"
if ! command -v foundryup >/dev/null 2>&1; then
  if [[ ! -x "${FOUNDRYUP_BIN}" ]]; then
    echo "WARN: foundryup missing after bootstrap; downloading standalone script..." >&2
    mkdir -p "$(dirname "${FOUNDRYUP_BIN}")"
    retry 4 curl -fsSL "https://raw.githubusercontent.com/foundry-rs/foundry/master/foundryup/foundryup" -o "${FOUNDRYUP_BIN}"
    chmod +x "${FOUNDRYUP_BIN}"
  fi
  retry 4 "${FOUNDRYUP_BIN}"
else
  retry 4 foundryup
fi

export PATH="$HOME/.foundry/bin:$PATH"
mkdir -p "${HOME}/.foundry/bin"
for tool in forge anvil cast chisel; do
  target="${HOME}/.foundry/bin/${tool}"
  if [[ -x "${target}" ]]; then
    continue
  fi
  if candidate="$(resolve_tool_candidate "${tool}" 2>/dev/null || true)" && [[ -n "${candidate}" ]]; then
    ln -sf "${candidate}" "${target}"
    chmod +x "${target}" || true
  fi
done
FORGE_BIN="$(resolve_tool_candidate forge 2>/dev/null || true)"
ANVIL_BIN="$(resolve_tool_candidate anvil 2>/dev/null || true)"

if [[ ! -x "${FORGE_BIN}" || ! -x "${ANVIL_BIN}" ]]; then
  echo "ERROR: expected forge/anvil binaries are missing after installation" >&2
  echo "DEBUG: ~/.foundry/bin contents:" >&2
  ls -la "${HOME}/.foundry/bin" >&2 || true
  echo "DEBUG: forge/anvil candidates under ~/.foundry:" >&2
  find "${HOME}/.foundry" -maxdepth 5 -type f \( -name "forge*" -o -name "anvil*" \) 2>/dev/null >&2 || true
  exit 127
fi

"${FORGE_BIN}" --version
"${ANVIL_BIN}" --version
