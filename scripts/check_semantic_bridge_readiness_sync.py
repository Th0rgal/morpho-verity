#!/usr/bin/env python3
"""Fail-closed sync check for SemanticBridgeReadiness.lean vs config tracker."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
READINESS_PATH = ROOT / "Morpho" / "Proofs" / "SemanticBridgeReadiness.lean"

OBLIGATIONS_RE = re.compile(
  r"def\s+obligations\s*:\s*List\s+SemanticBridgeObligation\s*:=\s*\[(?P<body>.*?)]",
  re.DOTALL,
)
ENTRY_BLOCK_RE = re.compile(r"\{(?P<body>.*?)}", re.DOTALL)
FIELD_PATTERNS = {
  "id": re.compile(r'id\s*:=\s*"([^"]+)"'),
  "hypothesis": re.compile(r'hypothesis\s*:=\s*"([^"]+)"'),
  "operation": re.compile(r'operation\s*:=\s*"([^"]+)"'),
  "status": re.compile(r"status\s*:=\s*\.(\w+)"),
  "macroMigrated": re.compile(r"macroMigrated\s*:=\s*(true|false)"),
}

STATUS_MAP = {
  "assumed": "assumed",
  "inProgress": "in_progress",
  "discharged": "discharged",
}


class SemanticBridgeReadinessSyncError(RuntimeError):
  pass


def load_config(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def parse_readiness_entries(path: pathlib.Path) -> list[dict[str, Any]]:
  text = path.read_text(encoding="utf-8")
  obligations_match = OBLIGATIONS_RE.search(text)
  if obligations_match is None:
    raise SemanticBridgeReadinessSyncError(
      "failed to locate obligations list in SemanticBridgeReadiness.lean"
    )

  entries: list[dict[str, Any]] = []
  for match in ENTRY_BLOCK_RE.finditer(obligations_match.group("body")):
    entry_body = match.group("body")
    values: dict[str, str] = {}
    for field, pattern in FIELD_PATTERNS.items():
      field_match = pattern.search(entry_body)
      if field_match is None:
        raise SemanticBridgeReadinessSyncError(
          f"failed to parse field '{field}' from SemanticBridgeReadiness.lean entry"
        )
      values[field] = field_match.group(1)
    entries.append(
      {
        "id": values["id"],
        "hypothesis": values["hypothesis"],
        "operation": values["operation"],
        "status": STATUS_MAP[values["status"]],
        "macroMigrated": values["macroMigrated"] == "true",
      }
    )
  if not entries:
    raise SemanticBridgeReadinessSyncError(
      "failed to parse obligation entries from SemanticBridgeReadiness.lean"
    )
  return entries


def build_config_projection(config: dict[str, Any]) -> list[dict[str, Any]]:
  obligations = config.get("obligations")
  if not isinstance(obligations, list):
    raise SemanticBridgeReadinessSyncError("config missing 'obligations' array")

  projection: list[dict[str, Any]] = []
  seen_ids: set[str] = set()
  for i, obligation in enumerate(obligations):
    if not isinstance(obligation, dict):
      raise SemanticBridgeReadinessSyncError(f"obligations[{i}] is not an object")
    for field in ("id", "hypothesis", "operation", "status", "macroMigrated"):
      if field not in obligation:
        raise SemanticBridgeReadinessSyncError(
          f"obligations[{i}] missing required field '{field}'"
        )
    obligation_id = obligation["id"]
    if not isinstance(obligation_id, str) or not obligation_id:
      raise SemanticBridgeReadinessSyncError(
        f"obligations[{i}] missing non-empty string field 'id'"
      )
    if obligation_id in seen_ids:
      raise SemanticBridgeReadinessSyncError(
        f"config contains duplicate obligation id '{obligation_id}'"
      )
    seen_ids.add(obligation_id)
    projection.append(
      {
        "id": obligation_id,
        "hypothesis": obligation["hypothesis"],
        "operation": obligation["operation"],
        "status": obligation["status"],
        "macroMigrated": obligation["macroMigrated"],
      }
    )
  return projection


def compare_entries(
  config_entries: list[dict[str, Any]],
  readiness_entries: list[dict[str, Any]],
) -> None:
  seen_readiness_ids: set[str] = set()
  duplicate_readiness_ids: set[str] = set()
  for entry in readiness_entries:
    obligation_id = entry["id"]
    if obligation_id in seen_readiness_ids:
      duplicate_readiness_ids.add(obligation_id)
    seen_readiness_ids.add(obligation_id)
  if duplicate_readiness_ids:
    raise SemanticBridgeReadinessSyncError(
      "SemanticBridgeReadiness.lean contains duplicate obligation ids: "
      + ", ".join(sorted(duplicate_readiness_ids))
    )

  config_by_id = {entry["id"]: entry for entry in config_entries}
  readiness_by_id = {entry["id"]: entry for entry in readiness_entries}

  missing = sorted(config_by_id.keys() - readiness_by_id.keys())
  if missing:
    raise SemanticBridgeReadinessSyncError(
      "SemanticBridgeReadiness.lean missing config obligations: " + ", ".join(missing)
    )

  extra = sorted(readiness_by_id.keys() - config_by_id.keys())
  if extra:
    raise SemanticBridgeReadinessSyncError(
      "SemanticBridgeReadiness.lean contains obligations not in config: " + ", ".join(extra)
    )

  for obligation_id in sorted(config_by_id):
    config_entry = config_by_id[obligation_id]
    readiness_entry = readiness_by_id[obligation_id]
    for field in ("hypothesis", "operation", "status", "macroMigrated"):
      if readiness_entry[field] != config_entry[field]:
        raise SemanticBridgeReadinessSyncError(
          f"obligation '{obligation_id}' field '{field}' drift: "
          f"config={config_entry[field]!r} readiness={readiness_entry[field]!r}"
        )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate SemanticBridgeReadiness.lean stays in sync with the JSON obligation tracker"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--readiness", type=pathlib.Path, default=READINESS_PATH)
  args = parser.parse_args()

  config_entries = build_config_projection(load_config(args.config))
  readiness_entries = parse_readiness_entries(args.readiness)
  compare_entries(config_entries, readiness_entries)

  print("semantic-bridge-readiness-sync check: OK")
  print(f"obligations: {len(config_entries)}")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SemanticBridgeReadinessSyncError as e:
    print(f"semantic-bridge-readiness-sync check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"semantic-bridge-readiness-sync check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
