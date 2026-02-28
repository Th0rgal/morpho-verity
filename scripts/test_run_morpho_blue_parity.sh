#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/run_morpho_blue_parity.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

build_path_without_timeout() {
  local out_dir="$1"
  mkdir -p "${out_dir}"
  for tool in bash cat cp cut dirname grep mkdir mktemp pwd rm tee; do
    ln -sf "$(command -v "${tool}")" "${out_dir}/${tool}"
  done
}

make_fake_repo() {
  local fake_root="$1"
  mkdir -p "${fake_root}/scripts" "${fake_root}/compiler/yul" "${fake_root}/morpho-blue"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/run_morpho_blue_parity.sh"
  cp "${ROOT_DIR}/scripts/run_with_timeout.sh" "${fake_root}/scripts/run_with_timeout.sh"
  chmod +x "${fake_root}/scripts/run_morpho_blue_parity.sh"
  chmod +x "${fake_root}/scripts/run_with_timeout.sh"

cat > "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_OUT_DIR:?MORPHO_VERITY_OUT_DIR is required}"
mkdir -p "${out}"
sleep_secs="${FAKE_PREP_SLEEP_SECS:-0}"
missing_artifact="${MORPHO_TEST_MISSING_PREP_ARTIFACT:-}"
if [[ "${sleep_secs}" != "0" ]]; then
  sleep "${sleep_secs}"
fi
if [[ "${missing_artifact}" != "yul" ]]; then
  printf '%s\n' "fake-yul-${MORPHO_VERITY_INPUT_MODE:-unknown}" > "${out}/Morpho.yul"
fi
if [[ "${missing_artifact}" != "bin" ]]; then
  printf '%s\n' "fake-bin-${MORPHO_VERITY_INPUT_MODE:-unknown}" > "${out}/Morpho.bin"
fi
if [[ "${missing_artifact}" != "abi" ]]; then
  printf '%s\n' "[]" > "${out}/Morpho.abi.json"
fi
EOF
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"

cat > "${fake_root}/scripts/check_input_mode_parity.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_PARITY_OUT_DIR:?MORPHO_VERITY_PARITY_OUT_DIR is required}"
sentinel="${MORPHO_TEST_PARITY_SENTINEL:-}"
missing_artifact="${MORPHO_TEST_MISSING_PARITY_ARTIFACT:-}"
sleep_secs="${MORPHO_TEST_PARITY_PRECHECK_SLEEP_SECS:-0}"
mkdir -p "${out}/model" "${out}/edsl"
if [[ "${sleep_secs}" != "0" ]]; then
  sleep "${sleep_secs}"
fi
if [[ "${missing_artifact}" != "yul" ]]; then
  printf '%s\n' "fake-yul-edsl" > "${out}/edsl/Morpho.yul"
fi
if [[ "${missing_artifact}" != "bin" ]]; then
  printf '%s\n' "fake-bin-edsl" > "${out}/edsl/Morpho.bin"
fi
if [[ "${missing_artifact}" != "abi" ]]; then
  printf '%s\n' "[]" > "${out}/edsl/Morpho.abi.json"
fi
printf '%s\n' "fake-yul-model" > "${out}/model/Morpho.yul"
printf '%s\n' "fake-bin-model" > "${out}/model/Morpho.bin"
printf '%s\n' "[]" > "${out}/model/Morpho.abi.json"
if [[ -n "${sentinel}" ]]; then
  printf '%s\n' "called" > "${sentinel}"
fi
EOF
  chmod +x "${fake_root}/scripts/check_input_mode_parity.sh"

  cat > "${fake_root}/morpho-blue/foundry.toml" <<'EOF'
[profile.default]
src = "src"
test = "test"
EOF

  mkdir -p "${fake_root}/bin"
  cat > "${fake_root}/bin/forge" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
sleep_sec="${MORPHO_TEST_FAKE_FORGE_SLEEP_SEC:-0}"
if [[ "${sleep_sec}" != "0" ]]; then
  sleep "${sleep_sec}"
fi
echo "Ran 1 tests for test/Fake.t.sol:FakeTest"
echo "Ran 1 tests (1 total tests)"
EOF
  chmod +x "${fake_root}/bin/forge"
}

test_fail_closed_when_timeout_missing_but_enabled() {
  local fake_root output_file restricted_path
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  restricted_path="$(mktemp -d)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${restricted_path}"' RETURN
  make_fake_repo "${fake_root}"
  build_path_without_timeout "${restricted_path}"

  set +e
  (
    cd "${fake_root}"
    PATH="${restricted_path}" \
    MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC=0 \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=1 \
      bash ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing timeout command to fail"
    exit 1
  fi
  assert_contains "ERROR: timeout command is required when MORPHO_BLUE_SUITE_TIMEOUT_SEC is greater than zero" "${output_file}"
}

test_fail_closed_on_invalid_parity_preflight_timeout_value() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC=oops \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid parity-preflight timeout value to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC must be a non-negative integer (got: oops)" "${output_file}"
}

test_fail_closed_on_invalid_skip_toggle() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=maybe \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid skip toggle to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_SKIP_PARITY_PREFLIGHT must be '0' or '1' (got: maybe)" "${output_file}"
}

test_fail_closed_on_invalid_local_skip_override_toggle() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=yes \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid local skip override toggle to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP must be '0' or '1' (got: yes)" "${output_file}"
}

test_fail_closed_on_invalid_exit_after_artifact_prep_toggle() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=true \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid exit-after-artifact-prep toggle to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP must be '0' or '1' (got: true)" "${output_file}"
}

test_fail_closed_on_invalid_suite_timeout_value() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    PATH="${fake_root}/bin:${PATH}" \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=oops \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid suite timeout value to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_BLUE_SUITE_TIMEOUT_SEC must be a non-negative integer (got: oops)" "${output_file}"
}

test_skip_refused_outside_ci() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    env -u CI \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected skip mode to fail outside CI"
    exit 1
  fi
  assert_contains "Refusing to skip parity preflight outside CI." "${output_file}"
}

test_skip_allowed_with_explicit_override() {
  local fake_root sentinel
  fake_root="$(mktemp -d)"
  sentinel="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${sentinel}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_TEST_PARITY_SENTINEL="${sentinel}" \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1 \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  )

  [[ -s "${fake_root}/compiler/yul/Morpho.yul" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.bin" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.abi.json" ]]
  [[ ! -s "${sentinel}" ]]
}

test_skip_allowed_in_ci_without_override() {
  local fake_root sentinel
  fake_root="$(mktemp -d)"
  sentinel="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${sentinel}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_TEST_PARITY_SENTINEL="${sentinel}" \
    CI=true \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  )

  [[ -s "${fake_root}/compiler/yul/Morpho.yul" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.bin" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.abi.json" ]]
  [[ ! -s "${sentinel}" ]]
}

test_default_mode_runs_parity_preflight() {
  local fake_root sentinel
  fake_root="$(mktemp -d)"
  sentinel="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${sentinel}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_TEST_PARITY_SENTINEL="${sentinel}" \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  )

  [[ -s "${fake_root}/compiler/yul/Morpho.yul" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.bin" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.abi.json" ]]
  [[ -s "${sentinel}" ]]
}

test_suite_timeout_fails_closed() {
  if ! command -v timeout >/dev/null 2>&1; then
    echo "Skipping timeout regression: 'timeout' command is unavailable."
    return
  fi

  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    PATH="${fake_root}/bin:${PATH}" \
    MORPHO_TEST_FAKE_FORGE_SLEEP_SEC=2 \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=1 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected timeout run to fail"
    exit 1
  fi

  assert_contains "timed out after 1s" "${output_file}"
  assert_contains "Differential suite FAILED: timeout" "${output_file}"
}

test_skip_mode_prep_timeout_fails_closed() {
  if ! command -v timeout >/dev/null 2>&1; then
    echo "Skipping prep-timeout regression: 'timeout' command is unavailable."
    return
  fi

  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    CI=true \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_PREP_TIMEOUT_SEC=1 \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=0 \
    FAKE_PREP_SLEEP_SECS=2 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected skip-mode prep timeout to fail"
    exit 1
  fi
  assert_contains "ERROR: Prepare edsl artifact timed out after 1s" "${output_file}"
}

test_parity_preflight_timeout_fails_closed() {
  if ! command -v timeout >/dev/null 2>&1; then
    echo "Skipping parity-preflight timeout regression: 'timeout' command is unavailable."
    return
  fi

  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_TEST_PARITY_PRECHECK_SLEEP_SECS=2 \
    MORPHO_VERITY_PARITY_PREFLIGHT_TIMEOUT_SEC=1 \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=0 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected parity-preflight timeout to fail"
    exit 1
  fi
  assert_contains "ERROR: Input-mode parity preflight timed out after 1s" "${output_file}"
}

test_fail_closed_on_invalid_prep_timeout_value_in_skip_mode() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    CI=true \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_PREP_TIMEOUT_SEC=oops \
    MORPHO_BLUE_SUITE_TIMEOUT_SEC=0 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected invalid prep timeout value to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_PREP_TIMEOUT_SEC must be a non-negative integer (got: oops)" "${output_file}"
}

test_fail_closed_when_parity_preflight_missing_required_artifact() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    MORPHO_TEST_MISSING_PARITY_ARTIFACT=bin \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing parity-preflight artifact to fail"
    exit 1
  fi
  assert_contains "ERROR: expected non-empty parity artifact:" "${output_file}"
  assert_contains "/edsl/Morpho.bin" "${output_file}"
}

test_fail_closed_when_skip_prep_missing_required_artifact() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN
  make_fake_repo "${fake_root}"

  set +e
  (
    cd "${fake_root}"
    CI=true \
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
    MORPHO_TEST_MISSING_PREP_ARTIFACT=abi \
      ./scripts/run_morpho_blue_parity.sh
  ) >"${output_file}" 2>&1
  local status=$?
  set -e

  if [[ "${status}" -eq 0 ]]; then
    echo "ASSERTION FAILED: expected missing skip-prep artifact to fail"
    exit 1
  fi
  assert_contains "ERROR: expected non-empty parity artifact:" "${output_file}"
  assert_contains "/edsl/Morpho.abi.json" "${output_file}"
}

test_skip_refused_outside_ci
test_fail_closed_on_invalid_skip_toggle
test_fail_closed_on_invalid_local_skip_override_toggle
test_fail_closed_on_invalid_exit_after_artifact_prep_toggle
test_fail_closed_on_invalid_suite_timeout_value
test_fail_closed_on_invalid_parity_preflight_timeout_value
test_skip_allowed_with_explicit_override
test_skip_allowed_in_ci_without_override
test_default_mode_runs_parity_preflight
test_fail_closed_when_timeout_missing_but_enabled
test_suite_timeout_fails_closed
test_skip_mode_prep_timeout_fails_closed
test_parity_preflight_timeout_fails_closed
test_fail_closed_on_invalid_prep_timeout_value_in_skip_mode
test_fail_closed_when_parity_preflight_missing_required_artifact
test_fail_closed_when_skip_prep_missing_required_artifact

echo "run_morpho_blue_parity.sh tests passed"
