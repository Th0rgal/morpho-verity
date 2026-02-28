#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/install_solc.sh"

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

test_fast_path_with_existing_solc_version() {
  local fake_root fake_bin output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/solc-select" '#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  versions)
    echo "0.8.28"
    ;;
  install)
    echo "ASSERTION FAILED: install should not be called on fast path" >&2
    exit 99
    ;;
  use)
    echo "Using ${2:-}" >&2
    ;;
  *)
    echo "unknown command: ${1:-}" >&2
    exit 98
    ;;
esac'
  make_exe "${fake_bin}/solc" '#!/usr/bin/env bash
set -euo pipefail
echo "solc, the solidity compiler commandline interface"
echo "Version: 0.8.28"'

  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" 0.8.28 >"${output_file}" 2>&1

  assert_contains "Using 0.8.28" "${output_file}"
  assert_contains "Version: 0.8.28" "${output_file}"
}

test_retry_install_then_succeed() {
  local fake_root fake_bin output_file state_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  state_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${state_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/sleep" '#!/usr/bin/env bash
set -euo pipefail
# keep retries fast in tests'
  make_exe "${fake_bin}/solc-select" '#!/usr/bin/env bash
set -euo pipefail
state_file="${TEST_STATE_FILE:?}"
command="${1:-}"
case "${command}" in
  versions)
    if grep -q "^installed$" "${state_file}" 2>/dev/null; then
      echo "0.8.28"
    else
      echo "0.8.27"
    fi
    ;;
  install)
    if grep -q "^install_failed_once$" "${state_file}" 2>/dev/null; then
      echo "installed" > "${state_file}"
      exit 0
    fi
    echo "install_failed_once" > "${state_file}"
    exit 1
    ;;
  use)
    exit 0
    ;;
  *)
    exit 97
    ;;
esac'
  make_exe "${fake_bin}/solc" '#!/usr/bin/env bash
set -euo pipefail
echo "Version: 0.8.28"'

  PATH="${fake_bin}:/usr/bin:/bin" \
  TEST_STATE_FILE="${state_file}" \
    "${SCRIPT_UNDER_TEST}" 0.8.28 >"${output_file}" 2>&1

  assert_contains "WARN: command failed (attempt 1/4" "${output_file}"
  assert_contains "Version: 0.8.28" "${output_file}"
}

test_retry_pip_install_when_solc_select_missing() {
  local fake_root fake_bin output_file state_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  state_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${state_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/sleep" '#!/usr/bin/env bash
set -euo pipefail
# keep retries fast in tests'
  make_exe "${fake_bin}/pip3" '#!/usr/bin/env bash
set -euo pipefail
state_file="${TEST_STATE_FILE:?}"
fake_bin="${TEST_FAKE_BIN:?}"
if grep -q "^pip_failed_once$" "${state_file}" 2>/dev/null; then
  cat > "${fake_bin}/solc-select" <<"EOF_SOLC_SELECT"
#!/usr/bin/env bash
set -euo pipefail
case "${1:-}" in
  versions)
    echo "0.8.28"
    ;;
  install)
    exit 0
    ;;
  use)
    exit 0
    ;;
  *)
    exit 96
    ;;
esac
EOF_SOLC_SELECT
  chmod +x "${fake_bin}/solc-select"
  exit 0
fi
echo "pip_failed_once" > "${state_file}"
exit 1'
  make_exe "${fake_bin}/solc" '#!/usr/bin/env bash
set -euo pipefail
echo "Version: 0.8.28"'

  PATH="${fake_bin}:/usr/bin:/bin" \
  TEST_STATE_FILE="${state_file}" \
  TEST_FAKE_BIN="${fake_bin}" \
    "${SCRIPT_UNDER_TEST}" 0.8.28 >"${output_file}" 2>&1

  assert_contains "WARN: command failed (attempt 1/4" "${output_file}"
  assert_contains "Version: 0.8.28" "${output_file}"
}

test_fast_path_with_existing_solc_version
test_retry_install_then_succeed
test_retry_pip_install_when_solc_select_missing

echo "install_solc.sh tests passed"
