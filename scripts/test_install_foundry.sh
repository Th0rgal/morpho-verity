#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/install_foundry.sh"

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

test_fast_path_when_forge_and_anvil_exist() {
  local fake_root fake_bin output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  make_exe "${fake_bin}/forge" '#!/usr/bin/env bash
set -euo pipefail
echo "forge 1.0.0"'
  make_exe "${fake_bin}/anvil" '#!/usr/bin/env bash
set -euo pipefail
echo "anvil 1.0.0"'
  make_exe "${fake_bin}/foundryup" '#!/usr/bin/env bash
set -euo pipefail
echo "ASSERTION FAILED: foundryup should not be called in fast path" >&2
exit 99'

  PATH="${fake_bin}:${PATH}" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "Foundry already available:" "${output_file}"
  assert_contains "forge 1.0.0" "${output_file}"
  assert_contains "anvil 1.0.0" "${output_file}"
}

test_retry_foundryup_then_succeed() {
  local fake_root fake_bin fake_home output_file state_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_home="${fake_root}/home"
  output_file="$(mktemp)"
  state_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${state_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_home}/.foundry/bin"

  make_exe "${fake_bin}/sleep" '#!/usr/bin/env bash
set -euo pipefail
# no-op in tests to keep retries fast'

  make_exe "${fake_bin}/foundryup" '#!/usr/bin/env bash
set -euo pipefail
state_file="${TEST_STATE_FILE:?}"
home_bin="${HOME}/.foundry/bin"
if [[ ! -s "${state_file}" ]]; then
  echo "first-failure" > "${state_file}"
  exit 1
fi
cat > "${home_bin}/forge" <<"EOF_FORGE"
#!/usr/bin/env bash
set -euo pipefail
echo "forge via foundryup"
EOF_FORGE
cat > "${home_bin}/anvil" <<"EOF_ANVIL"
#!/usr/bin/env bash
set -euo pipefail
echo "anvil via foundryup"
EOF_ANVIL
chmod +x "${home_bin}/forge" "${home_bin}/anvil"
exit 0'

  HOME="${fake_home}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
  TEST_STATE_FILE="${state_file}" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "WARN: command failed (attempt 1/4" "${output_file}"
  assert_contains "forge via foundryup" "${output_file}"
  assert_contains "anvil via foundryup" "${output_file}"
}

test_fast_path_when_forge_and_anvil_exist
test_retry_foundryup_then_succeed

echo "install_foundry.sh tests passed"
