#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)}"
MANIFEST_PATH="${ROOT_DIR}/lake-manifest.json"
LAKE_PACKAGES_DIR="${ROOT_DIR}/.lake/packages"
LAKE_BUILD_DIR="${ROOT_DIR}/.lake/build"
VERITY_DIR="${LAKE_PACKAGES_DIR}/verity"

if [[ ! -f "${MANIFEST_PATH}" ]]; then
  echo "ERROR: missing lake manifest at ${MANIFEST_PATH}" >&2
  exit 1
fi

if [[ ! -d "${VERITY_DIR}" ]]; then
  echo "No cached verity Lake package present; nothing to clear."
  exit 0
fi

WANTED_REV="$(python3 - "${MANIFEST_PATH}" <<'PY'
import json
import pathlib
import sys

manifest_path = pathlib.Path(sys.argv[1])
data = json.loads(manifest_path.read_text(encoding="utf-8"))
for pkg in data.get("packages", []):
  if isinstance(pkg, dict) and pkg.get("name") == "verity":
    rev = pkg.get("rev")
    if not isinstance(rev, str) or not rev:
      raise SystemExit("missing verity rev in manifest")
    print(rev)
    raise SystemExit(0)
raise SystemExit("missing verity package entry in manifest")
PY
)"
CACHED_REV="$(git -C "${VERITY_DIR}" rev-parse HEAD 2>/dev/null || echo "none")"

if [[ "${CACHED_REV}" != "${WANTED_REV}" ]]; then
  echo "Verity pin changed (${CACHED_REV} -> ${WANTED_REV}), clearing stale Lake state"
  rm -rf "${LAKE_PACKAGES_DIR}" "${LAKE_BUILD_DIR}"
  exit 0
fi

echo "Cached verity Lake package already matches ${WANTED_REV}."
