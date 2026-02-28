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

test_fast_path_after_path_bootstrap() {
  local fake_root fake_bin fake_home output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_home="${fake_root}/home"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_home}/.foundry/bin"
  make_exe "${fake_home}/.foundry/bin/forge" '#!/usr/bin/env bash
set -euo pipefail
echo "forge from home bin"'
  make_exe "${fake_home}/.foundry/bin/anvil" '#!/usr/bin/env bash
set -euo pipefail
echo "anvil from home bin"'
  make_exe "${fake_bin}/foundryup" '#!/usr/bin/env bash
set -euo pipefail
echo "ASSERTION FAILED: foundryup should not be called when cached binaries exist" >&2
exit 99'

  HOME="${fake_home}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "Foundry available after PATH bootstrap:" "${output_file}"
  assert_contains "forge from home bin" "${output_file}"
  assert_contains "anvil from home bin" "${output_file}"
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

test_links_version_suffixed_binaries() {
  local fake_root fake_bin fake_home output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_home="${fake_root}/home"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_home}/.foundry/releases/stable"
  make_exe "${fake_home}/.foundry/releases/stable/forge-1.5.1" '#!/usr/bin/env bash
set -euo pipefail
echo "forge from suffixed binary"'
  make_exe "${fake_home}/.foundry/releases/stable/anvil-1.5.1" '#!/usr/bin/env bash
set -euo pipefail
echo "anvil from suffixed binary"'
  make_exe "${fake_bin}/foundryup" '#!/usr/bin/env bash
set -euo pipefail
exit 0'

  HOME="${fake_home}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "forge from suffixed binary" "${output_file}"
  assert_contains "anvil from suffixed binary" "${output_file}"
}

test_archive_fallback_when_foundryup_emits_no_binaries() {
  local fake_root fake_bin fake_home output_file
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  fake_home="${fake_root}/home"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}" "${fake_home}/.foundry/bin"
  make_exe "${fake_bin}/foundryup" '#!/usr/bin/env bash
set -euo pipefail
echo "foundryup simulated success without binaries"
exit 0'
  make_exe "${fake_bin}/curl" '#!/usr/bin/env bash
set -euo pipefail
out=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -o)
      shift
      out="$1"
      ;;
  esac
  shift || true
done
: "${out:?missing -o output path}"
echo "fake archive payload" > "${out}"'
  make_exe "${fake_bin}/tar" '#!/usr/bin/env bash
set -euo pipefail
dest=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -C)
      shift
      dest="$1"
      ;;
  esac
  shift || true
done
: "${dest:?missing -C destination}"
mkdir -p "${dest}"
cat > "${dest}/forge" <<"EOF_FORGE"
#!/usr/bin/env bash
set -euo pipefail
echo "forge via archive fallback"
EOF_FORGE
cat > "${dest}/anvil" <<"EOF_ANVIL"
#!/usr/bin/env bash
set -euo pipefail
echo "anvil via archive fallback"
EOF_ANVIL
cat > "${dest}/cast" <<"EOF_CAST"
#!/usr/bin/env bash
set -euo pipefail
echo "cast via archive fallback"
EOF_CAST
cat > "${dest}/chisel" <<"EOF_CHISEL"
#!/usr/bin/env bash
set -euo pipefail
echo "chisel via archive fallback"
EOF_CHISEL
chmod +x "${dest}/forge" "${dest}/anvil" "${dest}/cast" "${dest}/chisel"'

  HOME="${fake_home}" \
  PATH="${fake_bin}:/usr/bin:/bin" \
    "${SCRIPT_UNDER_TEST}" >"${output_file}" 2>&1

  assert_contains "falling back to direct release archive install" "${output_file}"
  assert_contains "forge via archive fallback" "${output_file}"
  assert_contains "anvil via archive fallback" "${output_file}"
}

test_fast_path_when_forge_and_anvil_exist
test_fast_path_after_path_bootstrap
test_retry_foundryup_then_succeed
test_links_version_suffixed_binaries
test_archive_fallback_when_foundryup_emits_no_binaries

echo "install_foundry.sh tests passed"
