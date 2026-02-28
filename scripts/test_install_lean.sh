#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/install_lean.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

make_exe() {
  local path="$1"
  local content="$2"
  cat > "${path}" <<EOF_INNER
${content}
EOF_INNER
  chmod +x "${path}"
}

test_fast_path_with_existing_toolchain() {
  local fake_root fake_bin fake_elan_bin output_file toolchain
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_elan_bin="${fake_root}/.elan/bin"
  output_file="$(mktemp)"
  toolchain="leanprover/lean4:v4.22.0"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_elan_bin}"
  make_exe "${fake_bin}/elan" '#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  toolchain)
    case "${2:-}" in
      list)
        echo "leanprover/lean4:v4.22.0"
        ;;
      install)
        echo "ASSERTION FAILED: toolchain install should not run on fast path" >&2
        exit 99
        ;;
      *)
        exit 98
        ;;
    esac
    ;;
  *)
    exit 97
    ;;
esac'
  make_exe "${fake_bin}/lean" '#!/usr/bin/env bash
set -euo pipefail
echo "Lean (version 4.22.0)"'
  make_exe "${fake_bin}/curl" '#!/usr/bin/env bash
set -euo pipefail
echo "ASSERTION FAILED: curl should not run on fast path" >&2
exit 96'
  cp "${fake_bin}/elan" "${fake_elan_bin}/elan"
  cp "${fake_bin}/lean" "${fake_elan_bin}/lean"

  HOME="${fake_root}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" "${toolchain}" >"${output_file}" 2>&1

  assert_contains "Lean toolchain ready: ${toolchain}" "${output_file}"
}

test_retry_toolchain_install_then_succeed() {
  local fake_root fake_bin fake_elan_bin output_file state_file toolchain
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_elan_bin="${fake_root}/.elan/bin"
  output_file="$(mktemp)"
  state_file="$(mktemp)"
  toolchain="leanprover/lean4:v4.22.0"
  trap 'rm -rf "${fake_root}" "${output_file}" "${state_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_elan_bin}"
  make_exe "${fake_bin}/sleep" '#!/usr/bin/env bash
set -euo pipefail
# keep retries fast in tests'
  make_exe "${fake_bin}/elan" '#!/usr/bin/env bash
set -euo pipefail
state_file="${TEST_STATE_FILE:?}"
case "${1:-}" in
  toolchain)
    case "${2:-}" in
      list)
        if grep -q "^installed$" "${state_file}" 2>/dev/null; then
          echo "leanprover/lean4:v4.22.0"
        fi
        ;;
      install)
        if grep -q "^failed_once$" "${state_file}" 2>/dev/null; then
          echo "installed" > "${state_file}"
          exit 0
        fi
        echo "failed_once" > "${state_file}"
        exit 1
        ;;
      *)
        exit 95
        ;;
    esac
    ;;
  *)
    exit 94
    ;;
esac'
  make_exe "${fake_bin}/lean" '#!/usr/bin/env bash
set -euo pipefail
echo "Lean (version 4.22.0)"'
  cp "${fake_bin}/elan" "${fake_elan_bin}/elan"
  cp "${fake_bin}/lean" "${fake_elan_bin}/lean"

  HOME="${fake_root}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
  TEST_STATE_FILE="${state_file}" \
    "${SCRIPT_UNDER_TEST}" "${toolchain}" >"${output_file}" 2>&1

  assert_contains "Installing Lean toolchain ${toolchain}..." "${output_file}"
  assert_contains "WARN: command failed (attempt 1/4" "${output_file}"
  assert_contains "Lean toolchain ready: ${toolchain}" "${output_file}"
}

test_fail_closed_when_elan_and_curl_are_missing() {
  local fake_root fake_bin output_file toolchain rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  toolchain="leanprover/lean4:v4.22.0"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"

  rc=0
  if HOME="${fake_root}" PATH="${fake_bin}" "${SCRIPT_UNDER_TEST}" "${toolchain}" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected failure when both elan and curl are missing"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: elan is missing and curl is unavailable; cannot install elan" "${output_file}"
}

test_fail_closed_when_elan_still_missing_after_bootstrap() {
  local fake_root fake_bin output_file toolchain rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  toolchain="leanprover/lean4:v4.22.0"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  make_exe "${fake_bin}/sleep" '#!/usr/bin/env bash
set -euo pipefail
# keep retries fast in tests'
  make_exe "${fake_bin}/curl" '#!/usr/bin/env bash
set -euo pipefail
exit 0'

  rc=0
  if HOME="${fake_root}" PATH="${fake_bin}" "${SCRIPT_UNDER_TEST}" "${toolchain}" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected failure when elan remains missing after bootstrap"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: elan is still unavailable after installation; ensure ~/.elan/bin is on PATH" "${output_file}"
}

test_fast_path_with_existing_toolchain
test_retry_toolchain_install_then_succeed
test_fail_closed_when_elan_and_curl_are_missing
test_fail_closed_when_elan_still_missing_after_bootstrap

echo "install_lean.sh tests passed"
