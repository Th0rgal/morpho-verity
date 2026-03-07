#!/usr/bin/env python3
"""Fail-closed sync check for the remaining macro blocker table in EQUIVALENCE_OBLIGATIONS.md."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
DOC_PATH = ROOT / "docs" / "EQUIVALENCE_OBLIGATIONS.md"
SECTION_HEADING = "## Macro Migration Blockers"
NEXT_SECTION_HEADING = "## Primitive Coverage & Discharge Readiness"
SECTION_MARKER = "**Remaining blockers**:"
TABLE_HEADER = "| Blocker | Operations affected | Count |"
TABLE_SEPARATOR = "|---------|-------------------|:-----:|"
BLOCKER_ORDER = [
  "internalCall",
  "erc20",
  "structMember2",
  "callbacks",
  "externalWithReturn",
  "mappingStruct",
  "memoryOps",
  "precompileAccess",
  "tupleDestructuring",
  "externalCall",
  "blockTimestampValue",
]
BLOCKER_LABELS = {
  "internalCall": "Internal function calls (`Stmt.internalCall`)",
  "erc20": "ERC20 module (`ERC20.safeTransfer/From`)",
  "structMember2": "2D struct mapping read/write (`structMember2`)",
  "callbacks": "External callbacks (`Callbacks.callback`)",
  "externalWithReturn": "External contract calls (`Calls.withReturn`)",
  "mappingStruct": "`.mappingStruct` storage field type declarations",
  "memoryOps": "Memory management (`mstore/mload`)",
  "precompileAccess": "Precompile access (`ecrecover`)",
  "tupleDestructuring": "Tuple destructuring in macro bodies",
  "externalCall": "Pure-expression external calls (`externalCall`)",
  "blockTimestampValue": "Usable `blockTimestamp` values for `setMappingWord`",
}


class MacroMigrationBlockersDocError(RuntimeError):
  pass


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise MacroMigrationBlockersDocError(f"failed to read {context} {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise MacroMigrationBlockersDocError(
      f"failed to decode UTF-8 in {context} {path}: {exc}"
    ) from exc


def require_unique_heading_line(text: str, heading: str) -> re.Match[str]:
  matches = list(re.finditer(rf"(?m)^{re.escape(heading)}\r?$", text))
  if not matches:
    raise MacroMigrationBlockersDocError(f"missing `{heading}` section in EQUIVALENCE_OBLIGATIONS.md")
  if len(matches) > 1:
    raise MacroMigrationBlockersDocError(
      f"found multiple `{heading}` section markers in EQUIVALENCE_OBLIGATIONS.md"
    )
  return matches[0]


def extract_macro_migration_section(doc_text: str) -> str:
  start_match = require_unique_heading_line(doc_text, SECTION_HEADING)
  end_match = require_unique_heading_line(doc_text, NEXT_SECTION_HEADING)
  if end_match.start() <= start_match.end():
    raise MacroMigrationBlockersDocError(
      f"`{SECTION_HEADING}` has invalid boundary ordering in EQUIVALENCE_OBLIGATIONS.md"
    )
  section = doc_text[start_match.end() : end_match.start()]
  if not section.strip():
    raise MacroMigrationBlockersDocError(
      f"`{SECTION_HEADING}` section is empty in EQUIVALENCE_OBLIGATIONS.md"
    )
  return section


def load_config(path: pathlib.Path) -> dict[str, Any]:
  try:
    data = json.loads(read_text(path, context="config file"))
  except json.JSONDecodeError as exc:
    raise MacroMigrationBlockersDocError(f"failed to parse JSON in {path}: {exc}") from exc
  if not isinstance(data, dict):
    raise MacroMigrationBlockersDocError(f"config root must be a JSON object: {path}")
  return data


def build_blocker_report(config: dict[str, Any]) -> list[dict[str, Any]]:
  obligations = config.get("obligations")
  if not isinstance(obligations, list):
    raise MacroMigrationBlockersDocError("config missing 'obligations' array")

  operations_by_blocker: dict[str, list[str]] = {}
  seen_operations: set[str] = set()
  for i, item in enumerate(obligations):
    if not isinstance(item, dict):
      raise MacroMigrationBlockersDocError(f"obligations[{i}] is not an object")
    operation = item.get("operation")
    if not isinstance(operation, str) or not operation:
      raise MacroMigrationBlockersDocError(f"obligations[{i}] missing non-empty 'operation'")
    if operation in seen_operations:
      raise MacroMigrationBlockersDocError(f"duplicate obligation operation '{operation}'")
    seen_operations.add(operation)
    if item.get("macroMigrated") is not False:
      continue
    blockers = item.get("macroSurfaceBlockers")
    if not isinstance(blockers, list) or not blockers or not all(isinstance(b, str) and b for b in blockers):
      raise MacroMigrationBlockersDocError(
        f"obligation '{operation}' missing non-empty string-list 'macroSurfaceBlockers'"
      )
    if len(blockers) != len(set(blockers)):
      raise MacroMigrationBlockersDocError(
        f"obligation '{operation}' has duplicate entries in 'macroSurfaceBlockers'"
      )
    for blocker in blockers:
      if blocker not in BLOCKER_LABELS:
        raise MacroMigrationBlockersDocError(
          f"obligation '{operation}' references unknown blocker '{blocker}'"
        )
      operations_by_blocker.setdefault(blocker, []).append(operation)

  unexpected = sorted(set(operations_by_blocker) - set(BLOCKER_ORDER))
  if unexpected:
    raise MacroMigrationBlockersDocError(
      f"blocker order missing entries for: {', '.join(unexpected)}"
    )

  report: list[dict[str, Any]] = []
  for blocker in BLOCKER_ORDER:
    operations = operations_by_blocker.get(blocker)
    if not operations:
      continue
    sorted_operations = sorted(operations)
    report.append({
      "blocker": blocker,
      "label": BLOCKER_LABELS[blocker],
      "operations": sorted_operations,
      "count": len(sorted_operations),
    })
  return report


def format_row(entry: dict[str, Any]) -> str:
  operations = ", ".join(f"`{operation}`" for operation in entry["operations"])
  return f"| {entry['label']} | {operations} | {entry['count']} |"


def expected_table_lines(report: list[dict[str, Any]]) -> list[str]:
  return [TABLE_HEADER, TABLE_SEPARATOR, *[format_row(entry) for entry in report]]


def extract_table(doc_text: str) -> list[str]:
  lines = extract_macro_migration_section(doc_text).splitlines()
  try:
    marker_index = lines.index(SECTION_MARKER)
  except ValueError as exc:
    raise MacroMigrationBlockersDocError(
      f"missing `{SECTION_MARKER}` section followed by markdown table"
    ) from exc

  table_lines: list[str] = []
  for line in lines[marker_index + 1 :]:
    stripped = line.rstrip()
    if not stripped:
      if table_lines:
        break
      continue
    if not stripped.startswith("|"):
      if table_lines:
        break
      continue
    table_lines.append(stripped)

  if len(table_lines) < 2:
    raise MacroMigrationBlockersDocError(
      f"`{SECTION_MARKER}` table must include markdown header and separator rows"
    )
  if table_lines[0] != TABLE_HEADER or table_lines[1] != TABLE_SEPARATOR:
    raise MacroMigrationBlockersDocError(
      f"`{SECTION_MARKER}` table must start with the expected markdown header and separator"
    )
  return table_lines


def validate_doc_table(doc_text: str, report: list[dict[str, Any]]) -> None:
  actual = extract_table(doc_text)
  expected = expected_table_lines(report)
  if actual != expected:
    raise MacroMigrationBlockersDocError(
      "remaining blocker table does not match derived macro blocker inventory"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the remaining macro blocker table in EQUIVALENCE_OBLIGATIONS.md"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--doc", type=pathlib.Path, default=DOC_PATH)
  args = parser.parse_args()

  config = load_config(args.config)
  report = build_blocker_report(config)
  doc_text = read_text(args.doc, context="document file")
  validate_doc_table(doc_text, report)

  print("macro-migration-blockers-doc check: OK")
  print(f"blocker families: {len(report)}")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except MacroMigrationBlockersDocError as e:
    print(f"macro-migration-blockers-doc check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
