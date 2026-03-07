#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/clear_stale_verity_lake_state.sh"

assert_contains() {
  local needle="$1"
  local haystack_file="$2"
  if ! grep -Fq "${needle}" "${haystack_file}"; then
    echo "ASSERTION FAILED: expected to find '${needle}' in ${haystack_file}"
    exit 1
  fi
}

make_root() {
  local fake_root="$1"
  mkdir -p "${fake_root}/scripts" "${fake_root}/.lake/packages" "${fake_root}/.lake/build"
  cp "${SCRIPT_UNDER_TEST}" "${fake_root}/scripts/clear_stale_verity_lake_state.sh"
  chmod +x "${fake_root}/scripts/clear_stale_verity_lake_state.sh"
}

write_manifest() {
  local fake_root="$1"
  local rev="$2"
  cat > "${fake_root}/lake-manifest.json" <<EOF
{"packages":[{"name":"verity","rev":"${rev}"}]}
EOF
}

init_verity_repo() {
  local fake_root="$1"
  local verity_dir="${fake_root}/.lake/packages/verity"
  mkdir -p "${verity_dir}"
  git init "${verity_dir}" >/dev/null 2>&1
  git -C "${verity_dir}" config user.email "test@example.com"
  git -C "${verity_dir}" config user.name "Test User"
  echo "pin" > "${verity_dir}/PIN"
  git -C "${verity_dir}" add PIN
  git -C "${verity_dir}" commit -m "pin" >/dev/null 2>&1
  git -C "${verity_dir}" rev-parse HEAD
}

test_no_cached_verity_package_is_noop() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  make_root "${fake_root}"
  write_manifest "${fake_root}" "abc123"

  "${fake_root}/scripts/clear_stale_verity_lake_state.sh" "${fake_root}" >"${output_file}" 2>&1

  assert_contains "No cached verity Lake package present" "${output_file}"
  [[ -d "${fake_root}/.lake/packages" ]]
  [[ -d "${fake_root}/.lake/build" ]]
}

test_matching_cached_rev_keeps_lake_state() {
  local fake_root output_file rev
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  make_root "${fake_root}"
  rev="$(init_verity_repo "${fake_root}")"
  write_manifest "${fake_root}" "${rev}"

  "${fake_root}/scripts/clear_stale_verity_lake_state.sh" "${fake_root}" >"${output_file}" 2>&1

  assert_contains "already matches" "${output_file}"
  [[ -d "${fake_root}/.lake/packages/verity" ]]
  [[ -d "${fake_root}/.lake/build" ]]
}

test_mismatched_cached_rev_clears_lake_state() {
  local fake_root output_file
  fake_root="$(mktemp -d)"
  output_file="$(mktemp)"
  trap 'rm -rf "${fake_root}" "${output_file}"' RETURN

  make_root "${fake_root}"
  init_verity_repo "${fake_root}" >/dev/null
  write_manifest "${fake_root}" "ffffffffffffffffffffffffffffffffffffffff"

  "${fake_root}/scripts/clear_stale_verity_lake_state.sh" "${fake_root}" >"${output_file}" 2>&1

  assert_contains "clearing stale Lake state" "${output_file}"
  [[ ! -d "${fake_root}/.lake/packages" ]]
  [[ ! -d "${fake_root}/.lake/build" ]]
}

test_no_cached_verity_package_is_noop
test_matching_cached_rev_keeps_lake_state
test_mismatched_cached_rev_clears_lake_state

echo "clear_stale_verity_lake_state.sh tests passed"
