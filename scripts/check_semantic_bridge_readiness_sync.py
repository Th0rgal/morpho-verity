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
NAMESPACE_HEADER = "namespace Morpho.Proofs.SemanticBridgeReadiness"
NAMESPACE_FOOTER = "end Morpho.Proofs.SemanticBridgeReadiness"

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
VALID_CONFIG_STATUSES = set(STATUS_MAP.values())


class SemanticBridgeReadinessSyncError(RuntimeError):
  pass


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise SemanticBridgeReadinessSyncError(f"failed to read {context} {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise SemanticBridgeReadinessSyncError(
      f"failed to decode {context} {path} as UTF-8: {exc}"
    ) from exc


def normalize_readiness_status(raw_status: str) -> str:
  if raw_status not in STATUS_MAP:
    raise SemanticBridgeReadinessSyncError(
      "failed to parse supported status from SemanticBridgeReadiness.lean entry"
    )
  return STATUS_MAP[raw_status]


def require_non_empty_string(value: Any, *, field: str, context: str) -> str:
  if not isinstance(value, str) or not value:
    raise SemanticBridgeReadinessSyncError(f"{context} missing non-empty string field '{field}'")
  return value


def require_boolean(value: Any, *, field: str, context: str) -> bool:
  if not isinstance(value, bool):
    raise SemanticBridgeReadinessSyncError(f"{context} missing boolean field '{field}'")
  return value


def require_config_status(value: Any, *, context: str) -> str:
  status = require_non_empty_string(value, field="status", context=context)
  if status not in VALID_CONFIG_STATUSES:
    raise SemanticBridgeReadinessSyncError(
      f"{context} has unsupported status {status!r}"
    )
  return status


def load_config(path: pathlib.Path) -> dict[str, Any]:
  try:
    data = json.loads(read_text(path, context="config"))
  except json.JSONDecodeError as exc:
    raise SemanticBridgeReadinessSyncError(
      f"failed to parse JSON config {path}: {exc}"
    ) from exc
  if not isinstance(data, dict):
    raise SemanticBridgeReadinessSyncError(f"config root must be an object in {path}")
  return data


def extract_namespace_blocks(text: str) -> list[tuple[re.Match[str], re.Match[str], str]]:
  namespace_matches = list(
    re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_HEADER)}\r?$", text)
  )
  if not namespace_matches:
    raise SemanticBridgeReadinessSyncError(
      "failed to locate SemanticBridgeReadiness namespace boundary in SemanticBridgeReadiness.lean"
    )

  end_matches = list(re.finditer(rf"(?m)^\s*{re.escape(NAMESPACE_FOOTER)}\r?$", text))
  if not end_matches:
    raise SemanticBridgeReadinessSyncError(
      "failed to locate SemanticBridgeReadiness namespace end boundary in SemanticBridgeReadiness.lean"
    )
  if end_matches[0].start() < namespace_matches[0].start():
    raise SemanticBridgeReadinessSyncError(
      "found unmatched SemanticBridgeReadiness namespace footer before first namespace block"
    )

  blocks: list[tuple[re.Match[str], re.Match[str], str]] = []
  end_index = 0
  previous_end = -1
  for namespace_match in namespace_matches:
    if namespace_match.start() < previous_end:
      raise SemanticBridgeReadinessSyncError(
        "SemanticBridgeReadiness namespace blocks overlap in SemanticBridgeReadiness.lean"
      )
    while end_index < len(end_matches) and end_matches[end_index].start() <= namespace_match.end():
      end_index += 1
    if end_index >= len(end_matches):
      raise SemanticBridgeReadinessSyncError(
        "failed to locate SemanticBridgeReadiness namespace end boundary in SemanticBridgeReadiness.lean"
      )
    end_match = end_matches[end_index]
    namespace_body = text[namespace_match.end() : end_match.start()]
    if not namespace_body.strip():
      raise SemanticBridgeReadinessSyncError("SemanticBridgeReadiness namespace body is empty")
    blocks.append((namespace_match, end_match, namespace_body))
    previous_end = end_match.end()
    end_index += 1

  if end_index != len(end_matches):
    raise SemanticBridgeReadinessSyncError(
      "found unmatched SemanticBridgeReadiness namespace footer after namespace blocks"
    )
  return blocks


def extract_obligations_body(text: str) -> str:
  namespace_blocks = extract_namespace_blocks(text)
  _, _, primary_namespace_body = namespace_blocks[0]
  obligations_matches = list(OBLIGATIONS_RE.finditer(primary_namespace_body))
  if not obligations_matches:
    raise SemanticBridgeReadinessSyncError(
      "primary SemanticBridgeReadiness namespace block is missing tracked obligations"
    )
  if len(obligations_matches) > 1:
    raise SemanticBridgeReadinessSyncError(
      "primary SemanticBridgeReadiness namespace block contains multiple tracked obligations definitions"
    )

  duplicate_obligation_blocks = [
    match.group(0).strip()
    for _, _, namespace_body in namespace_blocks[1:]
    for match in OBLIGATIONS_RE.finditer(namespace_body)
  ]
  if duplicate_obligation_blocks:
    raise SemanticBridgeReadinessSyncError(
      "found multiple SemanticBridgeReadiness namespace blocks with tracked obligations"
    )

  return obligations_matches[0].group("body")


def parse_readiness_entries(path: pathlib.Path) -> list[dict[str, Any]]:
  text = read_text(path, context="readiness file")
  obligations_body = extract_obligations_body(text)

  entries: list[dict[str, Any]] = []
  for match in ENTRY_BLOCK_RE.finditer(obligations_body):
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
        "status": normalize_readiness_status(values["status"]),
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
    context = f"obligations[{i}]"
    obligation_id = require_non_empty_string(obligation["id"], field="id", context=context)
    if obligation_id in seen_ids:
      raise SemanticBridgeReadinessSyncError(
        f"config contains duplicate obligation id '{obligation_id}'"
      )
    seen_ids.add(obligation_id)
    projection.append(
      {
        "id": obligation_id,
        "hypothesis": require_non_empty_string(
          obligation["hypothesis"], field="hypothesis", context=context
        ),
        "operation": require_non_empty_string(
          obligation["operation"], field="operation", context=context
        ),
        "status": require_config_status(obligation["status"], context=context),
        "macroMigrated": require_boolean(
          obligation["macroMigrated"], field="macroMigrated", context=context
        ),
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
