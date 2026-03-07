#!/usr/bin/env python3
"""Fail-closed sync check for the documented Verity pin provenance."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys

from check_verity_pin_sync import parse_lakefile_verity, parse_manifest_verity


def fail(msg: str) -> None:
  print(f"verity-pin-provenance check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def load_provenance(path: pathlib.Path) -> dict[str, object]:
  data = json.loads(path.read_text(encoding="utf-8"))
  if not isinstance(data, dict):
    fail(f"provenance file must be a JSON object: {path}")
  return data


def require_str(data: dict[str, object], key: str, path: pathlib.Path) -> str:
  value = data.get(key)
  if not isinstance(value, str) or not value:
    fail(f"missing non-empty string `{key}` in {path}")
  return value


def require_divergences(data: dict[str, object], path: pathlib.Path) -> list[dict[str, object]]:
  value = data.get("remainingDivergences")
  if not isinstance(value, list) or not value:
    fail(f"missing non-empty `remainingDivergences` list in {path}")
  result: list[dict[str, object]] = []
  for i, item in enumerate(value):
    if not isinstance(item, dict):
      fail(f"`remainingDivergences[{i}]` must be an object in {path}")
    area = item.get("area")
    summary = item.get("summary")
    files = item.get("files")
    if not isinstance(area, str) or not area:
      fail(f"`remainingDivergences[{i}].area` must be a non-empty string in {path}")
    if not isinstance(summary, str) or not summary:
      fail(f"`remainingDivergences[{i}].summary` must be a non-empty string in {path}")
    if not isinstance(files, list) or not files or not all(isinstance(f, str) and f for f in files):
      fail(f"`remainingDivergences[{i}].files` must be a non-empty string list in {path}")
    result.append(item)
  return result


def require_doc_mentions(doc_text: str, needle: str, doc_path: pathlib.Path) -> None:
  normalized_doc = re.sub(r"\s+", " ", doc_text.replace("`", "")).strip()
  normalized_needle = re.sub(r"\s+", " ", needle.replace("`", "")).strip()
  if normalized_needle not in normalized_doc:
    fail(f"documentation {doc_path} missing expected text: {needle}")


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the documented Verity pin provenance against lake metadata"
  )
  parser.add_argument(
    "--lakefile",
    type=pathlib.Path,
    default=pathlib.Path("lakefile.lean"),
    help="Path to lakefile.lean",
  )
  parser.add_argument(
    "--manifest",
    type=pathlib.Path,
    default=pathlib.Path("lake-manifest.json"),
    help="Path to lake-manifest.json",
  )
  parser.add_argument(
    "--provenance",
    type=pathlib.Path,
    default=pathlib.Path("config/verity-pin-provenance.json"),
    help="Path to the machine-readable provenance file",
  )
  parser.add_argument(
    "--doc",
    type=pathlib.Path,
    default=pathlib.Path("docs/VERITY_PIN.md"),
    help="Path to the human-readable provenance document",
  )
  parser.add_argument(
    "--readme",
    type=pathlib.Path,
    default=pathlib.Path("README.md"),
    help="Path to README",
  )
  args = parser.parse_args()

  lakefile_url, lakefile_rev = parse_lakefile_verity(args.lakefile)
  manifest_url, manifest_rev, manifest_input_rev = parse_manifest_verity(args.manifest)
  provenance = load_provenance(args.provenance)

  upstream_repo = require_str(provenance, "upstreamRepo", args.provenance)
  input_rev = require_str(provenance, "inputRev", args.provenance)
  full_rev = require_str(provenance, "fullRev", args.provenance)
  tracked_issue = require_str(provenance, "trackedIssue", args.provenance)
  why_pinned = require_str(provenance, "whyPinned", args.provenance)
  divergences = require_divergences(provenance, args.provenance)

  if upstream_repo != lakefile_url or upstream_repo != manifest_url:
    fail(
      "provenance upstreamRepo does not match lake metadata: "
      f"{upstream_repo} / {lakefile_url} / {manifest_url}"
    )
  if input_rev != lakefile_rev or input_rev != manifest_input_rev:
    fail(
      "provenance inputRev does not match lake metadata: "
      f"{input_rev} / {lakefile_rev} / {manifest_input_rev}"
    )
  if full_rev != manifest_rev:
    fail(f"provenance fullRev does not match manifest rev: {full_rev} != {manifest_rev}")
  if not full_rev.startswith(input_rev):
    fail(f"provenance fullRev does not start with inputRev: {full_rev} / {input_rev}")

  repo_root = args.provenance.resolve().parent.parent
  for item in divergences:
    for raw_path in item["files"]:
      file_path = repo_root / raw_path
      if not file_path.exists():
        fail(f"documented divergence file does not exist: {raw_path}")

  doc_text = args.doc.read_text(encoding="utf-8")
  require_doc_mentions(doc_text, upstream_repo, args.doc)
  require_doc_mentions(doc_text, input_rev, args.doc)
  require_doc_mentions(doc_text, full_rev, args.doc)
  require_doc_mentions(doc_text, tracked_issue, args.doc)
  require_doc_mentions(doc_text, why_pinned, args.doc)
  for item in divergences:
    require_doc_mentions(doc_text, item["area"], args.doc)
    require_doc_mentions(doc_text, item["summary"], args.doc)
    for raw_path in item["files"]:
      require_doc_mentions(doc_text, raw_path, args.doc)

  readme_text = args.readme.read_text(encoding="utf-8")
  require_doc_mentions(readme_text, "docs/VERITY_PIN.md", args.readme)

  print(
    "verity-pin-provenance: "
    f"input_rev={input_rev} full_rev={full_rev} divergences={len(divergences)}"
  )
  print("verity-pin-provenance check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
