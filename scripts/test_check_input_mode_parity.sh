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
  chmod +x "${fake_root}/scripts/check_input_mode_parity.sh"

  cat > "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_OUT_DIR:?MORPHO_VERITY_OUT_DIR is required}"
mkdir -p "${out}"
mode="${MORPHO_VERITY_INPUT_MODE:-unknown}"
sleep_secs="${FAKE_SLEEP_SECS:-0}"
if [[ "${sleep_secs}" != "0" ]]; then
  sleep "${sleep_secs}"
fi

if [[ "${mode}" == "model" ]]; then
  printf '%s\n' "shared-yul" > "${out}/Morpho.yul"
  printf '%s\n' "shared-bin" > "${out}/Morpho.bin"
  if [[ "${FAKE_ABI_FORMAT_DRIFT:-0}" == "1" ]]; then
    printf '%s\n' '[{"type":"function","name":"same"}]' > "${out}/Morpho.abi.json"
  else
    printf '%s\n' "[]" > "${out}/Morpho.abi.json"
  fi
  exit 0
fi

if [[ "${FAKE_MISMATCH_YUL:-0}" == "1" ]]; then
  printf '%s\n' "different-yul" > "${out}/Morpho.yul"
else
  printf '%s\n' "shared-yul" > "${out}/Morpho.yul"
fi
if [[ "${FAKE_MISMATCH_BIN:-0}" == "1" ]]; then
  printf '%s\n' "different-bin" > "${out}/Morpho.bin"
else
  printf '%s\n' "shared-bin" > "${out}/Morpho.bin"
fi
if [[ "${FAKE_MISSING_ABI:-0}" != "1" ]]; then
  if [[ "${FAKE_MISMATCH_ABI:-0}" == "1" ]]; then
    printf '%s\n' "[{\"type\":\"function\",\"name\":\"different\"}]" > "${out}/Morpho.abi.json"
  elif [[ "${FAKE_ABI_FORMAT_DRIFT:-0}" == "1" ]]; then
    printf '%s\n' '[ { "name":"same" , "type":"function" } ]' > "${out}/Morpho.abi.json"
  else
  printf '%s\n' "[]" > "${out}/Morpho.abi.json"
  fi
fi
EOF
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
}

test_success_when_model_and_edsl_artifacts_match() {
  local fake_root out_dir
  fake_root="$(mktemp -d)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  )

  [[ -s "${out_dir}/model/Morpho.yul" ]]
  [[ -s "${out_dir}/edsl/Morpho.yul" ]]
  [[ -s "${out_dir}/model/Morpho.bin" ]]
  [[ -s "${out_dir}/edsl/Morpho.bin" ]]
  [[ -s "${out_dir}/model/Morpho.abi.json" ]]
  [[ -s "${out_dir}/edsl/Morpho.abi.json" ]]
}

test_fail_closed_on_yul_mismatch() {
  local fake_root output_file out_dir
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_MISMATCH_YUL=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected Yul mismatch to fail"
    exit 1
  fi
  assert_contains "ERROR: model vs edsl Yul artifacts differ" "${output_file}"
}

test_fail_closed_on_bytecode_mismatch() {
  local fake_root output_file out_dir
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_MISMATCH_BIN=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected bytecode mismatch to fail"
    exit 1
  fi
  assert_contains "ERROR: model vs edsl bytecode artifacts differ" "${output_file}"
}

test_fail_closed_on_abi_mismatch() {
  local fake_root output_file out_dir
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_MISMATCH_ABI=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected ABI mismatch to fail"
    exit 1
  fi
  assert_contains "ERROR: model vs edsl ABI artifacts differ" "${output_file}"
}

test_fail_closed_on_missing_artifact() {
  local fake_root output_file out_dir
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_MISSING_ABI=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing artifact to fail"
    exit 1
  fi
  assert_contains "ERROR: missing or empty artifact" "${output_file}"
}

test_success_on_abi_formatting_drift() {
  local fake_root out_dir
  fake_root="$(mktemp -d)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    FAKE_ABI_FORMAT_DRIFT=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  )

  [[ -s "${out_dir}/model/Morpho.abi.json" ]]
  [[ -s "${out_dir}/edsl/Morpho.abi.json" ]]
}

test_fail_closed_on_prepare_timeout() {
  local fake_root output_file out_dir
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  out_dir="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${out_dir}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    FAKE_SLEEP_SECS=2 \
    MORPHO_VERITY_PREP_TIMEOUT_SEC=1 \
    MORPHO_VERITY_PARITY_OUT_DIR="${out_dir}" \
      ./scripts/check_input_mode_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected prepare timeout to fail"
    exit 1
  fi
  assert_contains "ERROR: timed out while preparing model artifact after 1s" "${output_file}"
}

test_success_when_model_and_edsl_artifacts_match
test_success_on_abi_formatting_drift
test_fail_closed_on_yul_mismatch
test_fail_closed_on_bytecode_mismatch
test_fail_closed_on_abi_mismatch
test_fail_closed_on_missing_artifact
test_fail_closed_on_prepare_timeout

echo "check_input_mode_parity.sh tests passed"
