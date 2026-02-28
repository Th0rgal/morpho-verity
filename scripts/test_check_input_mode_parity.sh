#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/check_input_mode_parity.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

make_fake_repo() {
  local fake_root="$1"
  mkdir -p "${fake_root}/scripts"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/check_input_mode_parity.sh"
  cp "${ROOT_DIR}/scripts/run_with_timeout.sh" "${fake_root}/scripts/run_with_timeout.sh"
  chmod +x "${fake_root}/scripts/check_input_mode_parity.sh"
  chmod +x "${fake_root}/scripts/run_with_timeout.sh"

  cat > "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_OUT_DIR:?MORPHO_VERITY_OUT_DIR is required}"
mkdir -p "${out}"
if [[ "${FAKE_MISSING_ARTIFACTS:-0}" == "1" ]]; then
  exit 0
fi
printf '%s\n' "fake-yul-edsl" > "${out}/Morpho.yul"
printf '%s\n' "fake-bin-edsl" > "${out}/Morpho.bin"
printf '%s\n' "[]" > "${out}/Morpho.abi.json"
EOF
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
}

test_success_when_edsl_artifacts_exist() {
  local fake_root out_dir
  fake_root="$(mktemp -d)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" ./scripts/check_input_mode_parity.sh
  )

  [[ -s "${out_dir}/edsl/Morpho.yul" ]]
  [[ -s "${out_dir}/edsl/Morpho.bin" ]]
  [[ -s "${out_dir}/edsl/Morpho.abi.json" ]]
}

test_fail_closed_on_missing_artifact() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_MISSING_ARTIFACTS=1 ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing artifact to fail"
    exit 1
  fi
  assert_contains "ERROR: missing or empty artifact:" "${output_file}"
}

test_success_when_edsl_artifacts_exist
test_fail_closed_on_missing_artifact

echo "check_input_mode_parity.sh tests passed"
