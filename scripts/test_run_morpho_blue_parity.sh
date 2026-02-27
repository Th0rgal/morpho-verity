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

make_fake_repo() {
  local fake_root="$1"
  mkdir -p "${fake_root}/scripts" "${fake_root}/compiler/yul" "${fake_root}/morpho-blue"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/run_morpho_blue_parity.sh"
  chmod +x "${fake_root}/scripts/run_morpho_blue_parity.sh"

  cat > "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_OUT_DIR:?MORPHO_VERITY_OUT_DIR is required}"
mkdir -p "${out}"
printf '%s\n' "fake-yul-${MORPHO_VERITY_INPUT_MODE:-unknown}" > "${out}/Morpho.yul"
printf '%s\n' "fake-bin-${MORPHO_VERITY_INPUT_MODE:-unknown}" > "${out}/Morpho.bin"
printf '%s\n' "[]" > "${out}/Morpho.abi.json"
EOF
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"

  cat > "${fake_root}/scripts/check_input_mode_parity.sh" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
out="${MORPHO_VERITY_PARITY_OUT_DIR:?MORPHO_VERITY_PARITY_OUT_DIR is required}"
mkdir -p "${out}/model" "${out}/edsl"
printf '%s\n' "fake-yul-edsl" > "${out}/edsl/Morpho.yul"
printf '%s\n' "fake-bin-edsl" > "${out}/edsl/Morpho.bin"
printf '%s\n' "[]" > "${out}/edsl/Morpho.abi.json"
printf '%s\n' "fake-yul-model" > "${out}/model/Morpho.yul"
printf '%s\n' "fake-bin-model" > "${out}/model/Morpho.bin"
printf '%s\n' "[]" > "${out}/model/Morpho.abi.json"
printf '%s\n' "called" > "${out}/parity_invoked.txt"
EOF
  chmod +x "${fake_root}/scripts/check_input_mode_parity.sh"
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
  local fake_root
  fake_root="$(mktemp -d)"
  trap 'rm -rf "${fake_root}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_VERITY_SKIP_PARITY_PREFLIGHT=1 \
    MORPHO_VERITY_ALLOW_LOCAL_PARITY_PREFLIGHT_SKIP=1 \
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  )

  [[ -s "${fake_root}/compiler/yul/Morpho.yul" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.bin" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.abi.json" ]]
}

test_default_mode_runs_parity_preflight() {
  local fake_root
  fake_root="$(mktemp -d)"
  trap 'rm -rf "${fake_root}"' RETURN
  make_fake_repo "${fake_root}"

  (
    cd "${fake_root}"
    MORPHO_VERITY_EXIT_AFTER_ARTIFACT_PREP=1 \
      ./scripts/run_morpho_blue_parity.sh
  )

  [[ -s "${fake_root}/compiler/yul/Morpho.yul" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.bin" ]]
  [[ -s "${fake_root}/compiler/yul/Morpho.abi.json" ]]
}

test_skip_refused_outside_ci
test_skip_allowed_with_explicit_override
test_default_mode_runs_parity_preflight

echo "run_morpho_blue_parity.sh tests passed"
