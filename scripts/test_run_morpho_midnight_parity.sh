#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/run_morpho_midnight_parity.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq -- "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

make_fake_repo() {
  local fake_root="$1"
  mkdir -p "${fake_root}/scripts" "${fake_root}/morpho-midnight/test" "${fake_root}/morpho-midnight/src"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/run_morpho_midnight_parity.sh"
  cp "${ROOT_DIR}/scripts/run_with_timeout.sh" "${fake_root}/scripts/run_with_timeout.sh"
  cp "${ROOT_DIR}/scripts/patch_morpho_midnight_harness.py" "${fake_root}/scripts/patch_morpho_midnight_harness.py"
  chmod +x "${fake_root}/scripts/run_morpho_midnight_parity.sh"
  chmod +x "${fake_root}/scripts/run_with_timeout.sh"
  chmod +x "${fake_root}/scripts/patch_morpho_midnight_harness.py"

  cat > "${fake_root}/morpho-midnight/foundry.toml" <<'EOF'
[profile.default]
evm_version = "osaka"
fs_permissions = [{ access = "read", path = "test/ticks_exact.json" }]
EOF

  cat > "${fake_root}/morpho-midnight/test/BaseTest.sol" <<'EOF'
contract BaseTest {
    Midnight internal midnight;

    function setUp() public virtual {
        midnight = new Midnight();
    }
}
EOF

  mkdir -p "${fake_root}/bin"
  cat > "${fake_root}/bin/forge" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo "MIDNIGHT_IMPL=${MIDNIGHT_IMPL:-unset}"
echo "Ran 1 tests for test/Fake.t.sol:FakeTest"
echo "Ran 1 tests (1 total tests)"
EOF
  chmod +x "${fake_root}/bin/forge"

  cat > "${fake_root}/bin/solc" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
echo "solc, the solidity compiler commandline interface"
echo "Version: 0.8.34+commit.test.Linux.g++"
EOF
  chmod +x "${fake_root}/bin/solc"
}

test_solidity_mode_runs_original_suite() {
  local tmpdir output_file
  tmpdir="$(mktemp -d)"
  output_file="${tmpdir}/out.log"
  make_fake_repo "${tmpdir}"

  (
    cd "${tmpdir}"
    PATH="${tmpdir}/bin:${PATH}" MORPHO_MIDNIGHT_PARITY_MODE=solidity \
      ./scripts/run_morpho_midnight_parity.sh
  ) >"${output_file}" 2>&1

  assert_contains "MIDNIGHT_IMPL=solidity" "${output_file}"
  assert_contains "Morpho Midnight parity completed" "${output_file}"
}

test_verity_mode_requires_artifact() {
  local tmpdir output_file status
  tmpdir="$(mktemp -d)"
  output_file="${tmpdir}/out.log"
  make_fake_repo "${tmpdir}"

  set +e
  (
    cd "${tmpdir}"
    PATH="${tmpdir}/bin:${PATH}" MORPHO_MIDNIGHT_PARITY_MODE=verity \
      ./scripts/run_morpho_midnight_parity.sh
  ) >"${output_file}" 2>&1
  status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing artifact to fail"
    exit 1
  fi
  assert_contains "missing Midnight Verity deployment artifact" "${output_file}"
}

test_verity_mode_runs_with_artifact() {
  local tmpdir output_file
  tmpdir="$(mktemp -d)"
  output_file="${tmpdir}/out.log"
  make_fake_repo "${tmpdir}"
  mkdir -p "${tmpdir}/artifacts/midnight"
  printf '\x00' > "${tmpdir}/artifacts/midnight/Midnight.bin.raw"

  (
    cd "${tmpdir}"
    PATH="${tmpdir}/bin:${PATH}" MORPHO_MIDNIGHT_PARITY_MODE=verity \
      ./scripts/run_morpho_midnight_parity.sh
  ) >"${output_file}" 2>&1

  assert_contains "MIDNIGHT_IMPL=verity" "${output_file}"
  assert_contains "Morpho Midnight parity completed" "${output_file}"
}

test_solidity_mode_runs_original_suite
test_verity_mode_requires_artifact
test_verity_mode_runs_with_artifact

echo "test_run_morpho_midnight_parity: OK"
