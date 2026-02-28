#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/check_toolchain_readiness.sh"

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

test_passes_when_all_default_requirements_exist() {
  local fake_root fake_bin output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/lean" '#!/usr/bin/env bash
set -euo pipefail
echo "Lean (version 4.22.0)"'
  make_exe "${fake_bin}/lake" '#!/usr/bin/env bash
set -euo pipefail
echo "Lake version 5.0.0"'
  make_exe "${fake_bin}/forge" '#!/usr/bin/env bash
set -euo pipefail
echo "forge 1.4.0"'
  make_exe "${fake_bin}/anvil" '#!/usr/bin/env bash
set -euo pipefail
echo "anvil 1.4.0"'
  make_exe "${fake_bin}/solc" '#!/usr/bin/env bash
set -euo pipefail
echo "Version: 0.8.28"'
  make_exe "${fake_bin}/solc-select" '#!/usr/bin/env bash
set -euo pipefail
if [[ "${1:-}" == "versions" ]]; then
  echo "0.8.28"
else
  echo "solc-select"
fi'

  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "Toolchain readiness check passed for requirements: lean foundry solc" "${output_file}"
  assert_contains "forge 1.4.0" "${output_file}"
}

test_fail_closed_when_required_binary_missing() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/forge" '#!/usr/bin/env bash
set -euo pipefail
echo "forge 1.4.0"'

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" "${SCRIPT_UNDER_TEST}" --require foundry >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected readiness check to fail when anvil is missing"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 127 ]]; then
    echo "ASSERTION FAILED: expected exit code 127, got ${rc}"
    exit 1
  fi

  assert_contains "ERROR: missing required Foundry binary: anvil" "${output_file}"
  assert_contains "ERROR: toolchain readiness check failed." "${output_file}"
}

test_can_target_single_requirement() {
  local fake_root fake_bin output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/lean" '#!/usr/bin/env bash
set -euo pipefail
echo "Lean (version 4.22.0)"'
  make_exe "${fake_bin}/lake" '#!/usr/bin/env bash
set -euo pipefail
echo "Lake version 5.0.0"'

  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" --require lean >"${output_file}" 2>&1

  assert_contains "Toolchain readiness check passed for requirements: lean" "${output_file}"
}

test_rejects_invalid_requirement_name() {
  local output_file rc
  output_file="$(mktemp)"
  trap 'rm -f "${output_file}"' RETURN

  rc=0
  if "${SCRIPT_UNDER_TEST}" --require foo >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected invalid requirement to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi

  assert_contains "ERROR: invalid --require value 'foo'" "${output_file}"
}

test_passes_when_all_default_requirements_exist
test_fail_closed_when_required_binary_missing
test_can_target_single_requirement
test_rejects_invalid_requirement_name

echo "check_toolchain_readiness.sh tests passed"
