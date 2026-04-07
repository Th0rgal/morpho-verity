#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SCRIPT_UNDER_TEST="${ROOT_DIR}/scripts/ci_local_persistence.sh"

assert_eq() {
  local label="$1" expected="$2" actual="$3"
  if [[ "${expected}" != "${actual}" ]]; then
    echo "ASSERTION FAILED (${label}): expected '${expected}', got '${actual}'"
    exit 1
  fi
}

assert_file_exists() {
  local path="$1"
  if [[ ! -f "${path}" ]]; then
    echo "ASSERTION FAILED: expected file to exist: ${path}"
    exit 1
  fi
}

assert_dir_exists() {
  local path="$1"
  if [[ ! -d "${path}" ]]; then
    echo "ASSERTION FAILED: expected directory to exist: ${path}"
    exit 1
  fi
}

assert_symlink() {
  local path="$1"
  if [[ ! -L "${path}" ]]; then
    echo "ASSERTION FAILED: expected symlink at: ${path}"
    exit 1
  fi
}

test_mount_creates_symlink() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local root="${tmpdir}/root"
  local target="${tmpdir}/workspace/.lake/packages"
  mkdir -p "${root}"
  mkdir -p "$(dirname "${target}")"

  "${SCRIPT_UNDER_TEST}" mount \
    --root "${root}" \
    --key "test-key-abc" \
    --path "${target}"

  assert_symlink "${target}"
  assert_dir_exists "${root}/test-key-abc"

  local resolved
  resolved="$(readlink "${target}")"
  assert_eq "symlink target" "${root}/test-key-abc" "${resolved}"
}

test_mount_with_fallback_key() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local root="${tmpdir}/root"
  local target="${tmpdir}/workspace/.elan"
  mkdir -p "${root}/fallback-key"
  echo "cached-data" > "${root}/fallback-key/sentinel.txt"
  mkdir -p "$(dirname "${target}")"

  "${SCRIPT_UNDER_TEST}" mount \
    --root "${root}" \
    --key "new-key" \
    --path "${target}" \
    --fallback-key "fallback-key"

  assert_symlink "${target}"
  assert_file_exists "${root}/new-key/sentinel.txt"
}

test_mount_no_fallback_copy_when_primary_has_data() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local root="${tmpdir}/root"
  local target="${tmpdir}/workspace/data"
  mkdir -p "${root}/primary"
  echo "primary-data" > "${root}/primary/file.txt"
  mkdir -p "${root}/fallback"
  echo "fallback-data" > "${root}/fallback/file.txt"
  mkdir -p "$(dirname "${target}")"

  "${SCRIPT_UNDER_TEST}" mount \
    --root "${root}" \
    --key "primary" \
    --path "${target}" \
    --fallback-key "fallback"

  local content
  content="$(cat "${target}/file.txt")"
  assert_eq "primary not overwritten" "primary-data" "${content}"
}

test_publish_single_file() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local artifact_root="${tmpdir}/artifacts"
  mkdir -p "${artifact_root}"

  local staging="${tmpdir}/staging"
  mkdir -p "${staging}"
  echo "binary-content" > "${staging}/my-binary"

  ARTIFACT_ROOT="${artifact_root}" \
    "${SCRIPT_UNDER_TEST}" publish \
    --run-id "12345" \
    --name "test-artifact" \
    --path "${staging}/my-binary"

  assert_file_exists "${artifact_root}/12345/test-artifact/my-binary"
  assert_file_exists "${artifact_root}/12345/test-artifact/.ready"
}

test_publish_directory() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local artifact_root="${tmpdir}/artifacts"
  mkdir -p "${artifact_root}"

  local staging="${tmpdir}/staging"
  mkdir -p "${staging}/subdir"
  echo "file-a" > "${staging}/a.txt"
  echo "file-b" > "${staging}/subdir/b.txt"

  ARTIFACT_ROOT="${artifact_root}" \
    "${SCRIPT_UNDER_TEST}" publish \
    --run-id "67890" \
    --name "dir-artifact" \
    --path "${staging}"

  assert_file_exists "${artifact_root}/67890/dir-artifact/a.txt"
  assert_file_exists "${artifact_root}/67890/dir-artifact/subdir/b.txt"
  assert_file_exists "${artifact_root}/67890/dir-artifact/.ready"
}

test_publish_clears_stale_files() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local artifact_root="${tmpdir}/artifacts"
  mkdir -p "${artifact_root}/99/name"
  echo "stale" > "${artifact_root}/99/name/old-file.txt"
  touch "${artifact_root}/99/name/.ready"

  local staging="${tmpdir}/staging"
  mkdir -p "${staging}"
  echo "fresh" > "${staging}/new-file.txt"

  ARTIFACT_ROOT="${artifact_root}" \
    "${SCRIPT_UNDER_TEST}" publish \
    --run-id "99" \
    --name "name" \
    --path "${staging}"

  assert_file_exists "${artifact_root}/99/name/new-file.txt"
  if [[ -f "${artifact_root}/99/name/old-file.txt" ]]; then
    echo "ASSERTION FAILED: stale file should have been removed"
    exit 1
  fi
}

test_cleanup_removes_old_artifacts() {
  local tmpdir
  tmpdir="$(mktemp -d)"
  trap 'rm -rf "${tmpdir}"' RETURN

  local artifact_root="${tmpdir}/artifacts"
  mkdir -p "${artifact_root}/old-run"
  touch -d "2 days ago" "${artifact_root}/old-run"
  mkdir -p "${artifact_root}/recent-run"

  ARTIFACT_ROOT="${artifact_root}" \
    "${SCRIPT_UNDER_TEST}" cleanup --max-age-hours 24

  if [[ -d "${artifact_root}/old-run" ]]; then
    echo "ASSERTION FAILED: old artifact directory should have been removed"
    exit 1
  fi
  assert_dir_exists "${artifact_root}/recent-run"
}

test_mount_creates_symlink
test_mount_with_fallback_key
test_mount_no_fallback_copy_when_primary_has_data
test_publish_single_file
test_publish_directory
test_publish_clears_stale_files
test_cleanup_removes_old_artifacts

echo "ci_local_persistence.sh tests passed"
