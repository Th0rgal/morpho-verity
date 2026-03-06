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
  mkdir -p "${fake_root}/scripts" "${fake_root}/config" "${fake_root}/artifacts/inputs"
  printf '%s\n' 'import Morpho.Morpho' > "${fake_root}/Morpho.lean"
  printf '%s\n' 'import Morpho.Compiler.Main' > "${fake_root}/MorphoCompiler.lean"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
  cp "${ROOT_DIR}/scripts/apply_yul_rewrite_pipeline.py" "${fake_root}/scripts/apply_yul_rewrite_pipeline.py"
  cp "${ROOT_DIR}/scripts/check_yul_rewrite_proof_obligations.py" "${fake_root}/scripts/check_yul_rewrite_proof_obligations.py"
  chmod +x "${fake_root}/scripts/prepare_verity_morpho_artifact.sh"
  chmod +x "${fake_root}/scripts/apply_yul_rewrite_pipeline.py"
  printf '%s\n' "${target_json_content}" > "${fake_root}/config/parity-target.json"
  cp "${ROOT_DIR}/config/yul-rewrite-pipeline.json" "${fake_root}/config/yul-rewrite-pipeline.json"
  cp "${ROOT_DIR}/config/yul-rewrite-proof-obligations.json" "${fake_root}/config/yul-rewrite-proof-obligations.json"
  cat > "${fake_root}/artifacts/inputs/MarketParamsHash.yul" <<'EOF_LIB'
{
  function hashMarketParams() -> result { result := 0 }
}
EOF_LIB
}

install_fake_lake() {
  local fake_bin="$1"
  make_exe "${fake_bin}/lake" '#!/usr/bin/env bash
set -euo pipefail
if [[ -n "${FAKE_LAKE_LOG:-}" ]]; then
  printf "%s\n" "$*" >> "${FAKE_LAKE_LOG}"
fi
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
    *)
      shift
      ;;
  esac
done

printf "// yul artifact (edsl)\n" > "${out_dir}/Morpho.yul"
printf "%s\n" "[{\"type\":\"function\",\"name\":\"foo\",\"inputs\":[],\"outputs\":[]}]" > "${abi_out_dir}/Morpho.abi.json"
'
}

install_fake_lake_without_yul() {
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
    *)
      shift
      ;;
  esac
done

mkdir -p "${out_dir}" "${abi_out_dir}"
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

test_fail_closed_on_invalid_artifact_mode() {
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
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_ARTIFACT_MODE=invalid "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected invalid MORPHO_VERITY_ARTIFACT_MODE to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_ARTIFACT_MODE only supports 'edsl' (got: invalid)" "${output_file}"
}

test_fail_closed_when_hash_library_missing() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  rm -f "${fake_root}/artifacts/inputs/MarketParamsHash.yul"
  install_fake_python3 "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_BUILD=1 MORPHO_VERITY_SKIP_SOLC=1 "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing hash library to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: missing hash library: ${fake_root}/artifacts/inputs/MarketParamsHash.yul" "${output_file}"
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

test_fail_closed_when_lake_exe_omits_yul_artifact() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_lake_without_yul "${fake_bin}"
  install_fake_python3 "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" MORPHO_VERITY_SKIP_SOLC=1 "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing Morpho.yul to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 1 ]]; then
    echo "ASSERTION FAILED: expected exit code 1, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: ${fake_root}/artifacts/yul/Morpho.yul was not generated." "${output_file}"
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
  assert_contains "ERROR: failed to read required verity.parityPackId from ${fake_root}/config/parity-target.json" "${output_file}"
}

test_fail_closed_when_parity_pack_missing_in_target_json() {
  local fake_root fake_bin output_file rc
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}" '{"verity":{}}'
  install_fake_python3 "${fake_bin}"

  rc=0
  if PATH="${fake_bin}:/usr/bin:/bin" "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected missing verity.parityPackId to fail"
    exit 1
  else
    rc=$?
  fi

  if [[ "${rc}" -ne 2 ]]; then
    echo "ASSERTION FAILED: expected exit code 2, got ${rc}"
    exit 1
  fi
  assert_contains "ERROR: failed to read required verity.parityPackId from ${fake_root}/config/parity-target.json" "${output_file}"
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
  ln -s "$(command -v date)" "${fake_bin}/date"
  ln -s "$(command -v find)" "${fake_bin}/find"
  ln -s "$(command -v mkdir)" "${fake_bin}/mkdir"
  ln -s "$(command -v sha256sum)" "${fake_bin}/sha256sum"
  ln -s "$(command -v sort)" "${fake_bin}/sort"
  ln -s "$(command -v tee)" "${fake_bin}/tee"
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
  if [[ ! -s "${out_dir}/Morpho.artifact-manifest.env" ]]; then
    echo "ASSERTION FAILED: expected Morpho.artifact-manifest.env output"
    exit 1
  fi
  if [[ ! -s "${out_dir}/Morpho.stage-times.log" ]]; then
    echo "ASSERTION FAILED: expected Morpho.stage-times.log output"
    exit 1
  fi
  assert_contains "Skipped bytecode generation (MORPHO_VERITY_SKIP_SOLC=1)" "${output_file}"
  assert_contains "stage=lake-exe status=ok" "${out_dir}/Morpho.stage-times.log"
}

test_legacy_input_mode_alias_remains_compatible() {
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
  MORPHO_VERITY_INPUT_MODE=edsl \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  [[ -s "${out_dir}/Morpho.yul" ]]
  [[ -s "${out_dir}/Morpho.abi.json" ]]
  assert_contains "Using artifact mode: edsl" "${output_file}"
}

test_fail_closed_on_non_edsl_artifact_mode() {
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

  if PATH="${fake_bin}:/usr/bin:/bin" \
    MORPHO_VERITY_SKIP_BUILD=1 \
    MORPHO_VERITY_SKIP_SOLC=1 \
    MORPHO_VERITY_ARTIFACT_MODE=model \
    MORPHO_VERITY_OUT_DIR="${out_dir}" \
      "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1; then
    echo "ASSERTION FAILED: expected non-edsl artifact mode to fail"
    exit 1
  fi
  assert_contains "ERROR: MORPHO_VERITY_ARTIFACT_MODE only supports 'edsl' (got: model)" "${output_file}"
}

test_reuses_existing_artifact_when_manifest_matches() {
  local fake_root fake_bin output_file out_dir lake_log
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  out_dir="${fake_root}/out"
  lake_log="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${lake_log}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"

  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  : > "${lake_log}"
  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  if [[ -s "${lake_log}" ]]; then
    echo "ASSERTION FAILED: expected manifest reuse to skip lake invocations"
    exit 1
  fi
  assert_contains "Reusing existing Verity artifact from ${out_dir} (manifest matched)." "${output_file}"
  assert_contains "stage=reuse-artifact status=ok" "${out_dir}/Morpho.stage-times.log"
}

test_rebuilds_when_rewrite_outputs_are_missing() {
  local fake_root fake_bin output_file out_dir lake_log
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  out_dir="${fake_root}/out"
  lake_log="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${lake_log}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"

  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  rm -f "${out_dir}/Morpho.rewritten.yul" "${out_dir}/Morpho.rewrite-report.json"
  : > "${lake_log}"
  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  if [[ ! -s "${lake_log}" ]]; then
    echo "ASSERTION FAILED: expected missing rewrite outputs to invalidate manifest reuse"
    exit 1
  fi
  if [[ ! -s "${out_dir}/Morpho.rewritten.yul" ]]; then
    echo "ASSERTION FAILED: expected Morpho.rewritten.yul to be regenerated"
    exit 1
  fi
  if [[ ! -s "${out_dir}/Morpho.rewrite-report.json" ]]; then
    echo "ASSERTION FAILED: expected Morpho.rewrite-report.json to be regenerated"
    exit 1
  fi
}

test_rebuilds_when_top_level_lean_entrypoint_changes() {
  local fake_root fake_bin output_file out_dir lake_log
  fake_root="$(mktemp -d)"
  fake_bin="${fake_root}/bin"
  output_file="$(mktemp)"
  out_dir="${fake_root}/out"
  lake_log="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}" "${lake_log}"' RETURN

  mkdir -p "${fake_bin}"
  ln -s /bin/bash "${fake_bin}/bash"
  setup_fake_repo "${fake_root}"
  install_fake_python3 "${fake_bin}"
  install_fake_lake "${fake_bin}"

  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  : > "${lake_log}"
  printf '%s\n' 'import Morpho.Compiler.Generated' > "${fake_root}/MorphoCompiler.lean"

  PATH="${fake_bin}:/usr/bin:/bin" \
  FAKE_LAKE_LOG="${lake_log}" \
  MORPHO_VERITY_SKIP_BUILD=1 \
  MORPHO_VERITY_SKIP_SOLC=1 \
  MORPHO_VERITY_OUT_DIR="${out_dir}" \
    "${fake_root}/scripts/prepare_verity_morpho_artifact.sh" >"${output_file}" 2>&1

  if [[ ! -s "${lake_log}" ]]; then
    echo "ASSERTION FAILED: expected top-level Lean entrypoint changes to invalidate manifest reuse"
    exit 1
  fi
}

test_fail_closed_on_invalid_skip_build_toggle
test_fail_closed_on_invalid_skip_solc_toggle
test_fail_closed_on_invalid_artifact_mode
test_fail_closed_when_hash_library_missing
test_fail_closed_when_python3_missing_for_parity_target_read
test_fail_closed_on_invalid_parity_target_json
test_fail_closed_when_parity_pack_missing_in_target_json
test_fail_closed_when_lake_missing
test_fail_closed_when_solc_missing_and_not_skipped
test_fail_closed_when_awk_missing_and_not_skipped
test_success_when_solc_is_skipped
test_legacy_input_mode_alias_remains_compatible
test_fail_closed_on_non_edsl_artifact_mode
test_reuses_existing_artifact_when_manifest_matches
test_rebuilds_when_rewrite_outputs_are_missing
test_rebuilds_when_top_level_lean_entrypoint_changes

echo "prepare_verity_morpho_artifact.sh tests passed"
