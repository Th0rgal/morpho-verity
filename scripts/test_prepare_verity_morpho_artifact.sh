#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/prepare_verity_morpho_artifact.sh"

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

setup_fake_repo() {
  local fake_root="$1"
  local target_json_content="${2:-{\"verity\":{\"parityPackId\":\"test-pack\"}}}"
  mkdir -p "${fake_root}/scripts" "${fake_root}/config" "${fake_root}/compiler/external-libs"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
  printf '%s\n' "${target_json_content}" > "${fake_root}/config/parity-target.json"
  cat > "${fake_root}/compiler/external-libs/MarketParamsHash.yul" <<'EOF_LIB'
{
  function hashMarketParams() -> result { result := 0 }
}
EOF_LIB
}

install_fake_lake() {
  local fake_bin="$1"
  make_exe "${fake_bin}/lake" '#!/usr/bin/env bash
set -euo pipefail
if [[ "${1:-}" == "build" ]]; then
  exit 0
fi
if [[ "${1:-}" != "exe" ]]; then
  exit 97
fi
shift
if [[ "${1:-}" != "morpho-verity-compiler" ]]; then
  exit 96
fi
shift

out_dir=""
abi_out_dir=""
input_mode=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --output)
      out_dir="$2"
      shift 2
      ;;
    --abi-output)
      abi_out_dir="$2"
      shift 2
      ;;
    --input)
      input_mode="$2"
      shift 2
      ;;
    *)
      shift
      ;;
  esac
done

printf "// yul artifact (%s)\n" "${input_mode}" > "${out_dir}/Morpho.yul"
printf "%s\n" "[{\"type\":\"function\",\"name\":\"foo\",\"inputs\":[],\"outputs\":[]}]" > "${abi_out_dir}/Morpho.abi.json"
'
}

install_fake_python3() {
  local fake_bin="$1"
  make_exe "${fake_bin}/python3" '#!/usr/bin/env bash
set -euo pipefail
exec /usr/bin/python3 "$@"'
}

install_fake_solc() {
  local fake_bin="$1"
  make_exe "${fake_bin}/solc" '#!/usr/bin/env bash
set -euo pipefail
echo "Binary representation:"
echo "6000"'
}

install_fake_awk() {
  local fake_bin="$1"
  make_exe "${fake_bin}/awk" '#!/usr/bin/env bash
set -euo pipefail
exec /usr/bin/awk "$@"'
}

test_fail_closed_on_invalid_skip_build_toggle() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_BUILD=oops "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected invalid MORPHO_VERITY_SKIP_BUILD to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_SKIP_BUILD must be '0' or '1' (got: oops)" "${output_file}"
}

test_fail_closed_on_invalid_skip_solc_toggle() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_SOLC=nope "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected invalid MORPHO_VERITY_SKIP_SOLC to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_SKIP_SOLC must be '0' or '1' (got: nope)" "${output_file}"
}

test_fail_closed_when_python3_missing_for_parity_target_read() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  ln -s "$(command -v dirname)" "${fake_bin}/dirname"
  setup_fake_repo "${fake_root}"

  rc=0
  if PATH="${fake_bin}" "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing python3 to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: python3 is required to read parity pack from ${fake_root}/config/parity-target.json" "${output_file}"
}

test_fail_closed_on_invalid_parity_target_json() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}" '{"verity":'
  install_fake_python3 "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected malformed parity target JSON to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: failed to read parity pack from ${fake_root}/config/parity-target.json" "${output_file}"
}

test_fail_closed_when_lake_missing() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_BUILD=1 MORPHO_VERITY_SKIP_SOLC=1 "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing lake to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: lake is required to build and compile Morpho artifacts" "${output_file}"
}

test_fail_closed_when_solc_missing_and_not_skipped() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"
  install_fake_awk "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_BUILD=1 "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing solc to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: solc is required unless MORPHO_VERITY_SKIP_SOLC=1" "${output_file}"
}

test_fail_closed_when_awk_missing_and_not_skipped() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  ln -s "$(command -v dirname)" "${fake_bin}/dirname"
  ln -s "$(command -v mkdir)" "${fake_bin}/mkdir"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"
  install_fake_solc "${fake_bin}"

  rc=0
  if PATH="${fake_bin}" MORPHO_VERITY_SKIP_BUILD=1 "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing awk to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: awk is required to extract bytecode from solc output" "${output_file}"
}

test_success_when_solc_is_skipped() {
  local fake_root fake_bin output_file out_dir
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  out_dir="${fake_root}/out"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"

  PATH="${fake_bin}:/usr/bin:/bin" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  if [[ ! -s "${out_dir}/Morpho.yul" ]]; then
    echo "ASSERTION FAILED: expected Morpho.yul output"
    exit 1
  fi
  if [[ ! -s "${out_dir}/Morpho.abi.json" ]]; then
    echo "ASSERTION FAILED: expected Morpho.abi.json output"
    exit 1
  fi
  assert_contains "Skipped bytecode generation (MORPHO_VERITY_SKIP_SOLC=1)" "${output_file}"
}

test_fail_closed_on_invalid_skip_build_toggle
test_fail_closed_on_invalid_skip_solc_toggle
test_fail_closed_when_python3_missing_for_parity_target_read
test_fail_closed_on_invalid_parity_target_json
test_fail_closed_when_lake_missing
test_fail_closed_when_solc_missing_and_not_skipped
test_fail_closed_when_awk_missing_and_not_skipped
test_success_when_solc_is_skipped

echo "prepare_verity_morpho_artifact.sh tests passed"
