#!/usr/bin/env python3
"""Fail-closed sync check for docs/SOLIDITY_CORRESPONDENCE.md."""

from __future__ import annotations

import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
DOC_PATH = ROOT / "docs" / "SOLIDITY_CORRESPONDENCE.md"
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
REQUIRED_GATE_REFS = (
  "scripts/check_macro_migration_slice.py",
  "scripts/check_morpho_event_surface.py",
  "scripts/check_spec_correspondence.py",
  "config/semantic-bridge-obligations.json",
)
TABLE_ROW_RE = re.compile(
  r"^\|\s*`([^`]+)`\s*\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|\s*([^|]+?)\s*\|\s*$",
  re.MULTILINE,
)


class SolidityCorrespondenceDocError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise SolidityCorrespondenceDocError(f"failed to read {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise SolidityCorrespondenceDocError(f"{path} is not valid UTF-8: {exc}") from exc


def load_json(path: pathlib.Path) -> dict[str, Any]:
  try:
    with path.open("r", encoding="utf-8") as f:
      data = json.load(f)
  except json.JSONDecodeError as exc:
    raise SolidityCorrespondenceDocError(f"failed to parse JSON {path}: {exc}") from exc
  except (OSError, UnicodeDecodeError) as exc:
    raise SolidityCorrespondenceDocError(f"failed to read JSON {path}: {exc}") from exc
  if not isinstance(data, dict):
    raise SolidityCorrespondenceDocError(f"JSON root must be an object in {path}")
  return data


def extract_table_rows(doc_text: str) -> dict[str, dict[str, str]]:
  rows: dict[str, dict[str, str]] = {}
  for match in TABLE_ROW_RE.finditer(doc_text):
    surface = match.group(1)
    name = surface.split("(", 1)[0]
    rows[name] = {
      "surface": surface,
      "macro_status": match.group(2).strip(),
      "proof_status": match.group(3).strip(),
      "notes": match.group(4).strip(),
    }
  return rows


def expected_proof_status(obligation: dict[str, Any]) -> str:
  status = obligation.get("status")
  blocked_by = obligation.get("blockedBy")
  if status == "in_progress" and isinstance(blocked_by, str) and "Link 1 proven" in blocked_by:
    return "Link 1 proven"
  return "Assumed boundary"


def iter_macro_migrated_obligations(config: dict[str, Any]) -> list[dict[str, Any]]:
  obligations = config.get("obligations")
  if not isinstance(obligations, list):
    raise SolidityCorrespondenceDocError("semantic bridge config missing `obligations` list")
  migrated: list[dict[str, Any]] = []
  for index, obligation in enumerate(obligations):
    if not isinstance(obligation, dict):
      raise SolidityCorrespondenceDocError(f"obligation[{index}] is not an object")
    if obligation.get("macroMigrated") is True:
      migrated.append(obligation)
  return migrated


def validate_obligation_rows(config: dict[str, Any], rows: dict[str, dict[str, str]]) -> None:
  for obligation in iter_macro_migrated_obligations(config):
    operation = obligation.get("operation")
    if not isinstance(operation, str) or not operation:
      raise SolidityCorrespondenceDocError("macro-migrated obligation missing operation")
    alias = obligation.get("macroAlias")
    doc_name = alias if isinstance(alias, str) and alias else operation
    row = rows.get(doc_name)
    if row is None:
      raise SolidityCorrespondenceDocError(
        f"SOLIDITY_CORRESPONDENCE.md missing row for macro-migrated operation `{doc_name}`"
      )
    if row["macro_status"] != "Translated":
      raise SolidityCorrespondenceDocError(
        f"`{doc_name}` macro status drift: expected `Translated`, found `{row['macro_status']}`"
      )
    expected = expected_proof_status(obligation)
    if row["proof_status"] != expected:
      raise SolidityCorrespondenceDocError(
        f"`{doc_name}` proof status drift: expected `{expected}`, found `{row['proof_status']}`"
      )


def validate_gate_refs(doc_text: str) -> None:
  missing = [ref for ref in REQUIRED_GATE_REFS if ref not in doc_text]
  if missing:
    raise SolidityCorrespondenceDocError(
      "SOLIDITY_CORRESPONDENCE.md missing cross-cutting gate refs: "
      + ", ".join(missing)
    )


def main() -> int:
  doc_text = read_text(DOC_PATH)
  validate_obligation_rows(load_json(CONFIG_PATH), extract_table_rows(doc_text))
  validate_gate_refs(doc_text)
  print("solidity-correspondence-doc check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except SolidityCorrespondenceDocError as exc:
    print(f"solidity-correspondence-doc check failed: {exc}", file=sys.stderr)
    raise SystemExit(1)
