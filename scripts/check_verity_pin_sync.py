#!/usr/bin/env python3
"""Fail-closed sync check for the pinned Verity dependency revision."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
LAKEFILE_PATH = ROOT / "lakefile.lean"
MANIFEST_PATH = ROOT / "lake-manifest.json"


REQUIRE_VERITY_RE = re.compile(
  r'require\s+verity\s+from\s+git\s+"(?P<url>[^"]+)"\s+@\s+"(?P<rev>[^"]+)"',
  re.MULTILINE,
)


class VerityPinSyncError(RuntimeError):
  pass


def fail(msg: str) -> None:
  print(f"verity-pin-sync check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def parse_lakefile_verity(lakefile_path: pathlib.Path) -> tuple[str, str]:
  try:
    text = lakefile_path.read_text(encoding="utf-8")
  except (OSError, UnicodeDecodeError) as exc:
    raise VerityPinSyncError(f"failed to read lakefile {lakefile_path}: {exc}") from exc
  match = REQUIRE_VERITY_RE.search(text)
  if match is None:
    raise VerityPinSyncError(f"missing `require verity from git` stanza in {lakefile_path}")
  return match.group("url"), match.group("rev")


def parse_manifest_verity(manifest_path: pathlib.Path) -> tuple[str, str, str]:
  try:
    data: Any = json.loads(manifest_path.read_text(encoding="utf-8"))
  except (OSError, UnicodeDecodeError) as exc:
    raise VerityPinSyncError(f"failed to read manifest {manifest_path}: {exc}") from exc
  except json.JSONDecodeError as exc:
    raise VerityPinSyncError(f"failed to parse JSON manifest {manifest_path}: {exc}") from exc
  if not isinstance(data, dict):
    raise VerityPinSyncError(f"manifest root must be a JSON object in {manifest_path}")
  packages = data.get("packages")
  if not isinstance(packages, list):
    raise VerityPinSyncError(f"missing `packages` list in {manifest_path}")
  for index, pkg in enumerate(packages):
    if not isinstance(pkg, dict):
      raise VerityPinSyncError(f"packages[{index}] is not an object in {manifest_path}")
    if isinstance(pkg, dict) and pkg.get("name") == "verity":
      url = pkg.get("url")
      rev = pkg.get("rev")
      input_rev = pkg.get("inputRev")
      if not isinstance(url, str) or not isinstance(rev, str) or not isinstance(input_rev, str):
        raise VerityPinSyncError(f"incomplete verity package metadata in {manifest_path}")
      return url, rev, input_rev
  raise VerityPinSyncError(f"missing verity package entry in {manifest_path}")


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate lakefile.lean and lake-manifest.json agree on the Verity pin"
  )
  parser.add_argument(
    "--lakefile",
    type=pathlib.Path,
    default=LAKEFILE_PATH,
    help="Path to lakefile.lean",
  )
  parser.add_argument(
    "--manifest",
    type=pathlib.Path,
    default=MANIFEST_PATH,
    help="Path to lake-manifest.json",
  )
  args = parser.parse_args()
  lakefile_path = args.lakefile.resolve()
  manifest_path = args.manifest.resolve()

  try:
    lakefile_url, lakefile_rev = parse_lakefile_verity(lakefile_path)
    manifest_url, manifest_rev, manifest_input_rev = parse_manifest_verity(manifest_path)

    if lakefile_url != manifest_url:
      raise VerityPinSyncError(
        "lakefile/manifest verity URL mismatch: "
        f"{lakefile_url} != {manifest_url}"
      )
    if lakefile_rev != manifest_input_rev:
      raise VerityPinSyncError(
        "lakefile/manifest verity inputRev mismatch: "
        f"{lakefile_rev} != {manifest_input_rev}"
      )
    if not manifest_rev.startswith(lakefile_rev):
      raise VerityPinSyncError(
        "lakefile/manifest verity rev mismatch: "
        f"{manifest_rev} does not start with {lakefile_rev}"
      )
  except VerityPinSyncError as exc:
    fail(str(exc))

  print(
    "verity-pin-sync: "
    f"lakefile_rev={lakefile_rev} manifest_rev={manifest_rev}"
  )
  print("verity-pin-sync check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
