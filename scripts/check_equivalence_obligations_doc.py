#!/usr/bin/env python3
"""Fail-closed sync check for the open-issue summary in EQUIVALENCE_OBLIGATIONS.md."""

from __future__ import annotations

import argparse
import pathlib
import sys
from typing import Any

from check_issue_blocker_clusters import load_config, validate_issue_clusters


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
DOC_PATH = ROOT / "docs" / "EQUIVALENCE_OBLIGATIONS.md"
SECTION_HEADING = "### Open issue blocker summary"
TABLE_HEADER = "| Issue | Operations | Blocker families | Coverage counts |"
TABLE_SEPARATOR = "|-------|------------|------------------|-----------------|"


class EquivalenceObligationsDocError(RuntimeError):
  pass


def format_cluster_row(cluster: dict[str, Any]) -> str:
  issue = f"`#{cluster['issue']}`"
  operations = ", ".join(f"`{operation}`" for operation in cluster["operations"])
  blockers = ", ".join(f"`{blocker}`" for blocker in cluster["macroSurfaceBlockers"])
  counts = ", ".join(
    f"{name}\u00d7{count}" for name, count in cluster["blockerCoverageCounts"].items()
  )
  return f"| {issue} | {operations} | {blockers} | {counts} |"


def expected_table_lines(clusters: list[dict[str, Any]]) -> list[str]:
  return [TABLE_HEADER, TABLE_SEPARATOR, *[format_cluster_row(cluster) for cluster in clusters]]


def extract_issue_summary_table(doc_text: str) -> list[str]:
  lines = doc_text.splitlines()
  try:
    heading_index = lines.index(SECTION_HEADING)
  except ValueError as exc:
    raise EquivalenceObligationsDocError(
      f"missing `{SECTION_HEADING}` section followed by markdown table"
    ) from exc

  table_lines: list[str] = []
  for line in lines[heading_index + 1 :]:
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

  lines = table_lines
  if len(lines) < 3:
    raise EquivalenceObligationsDocError(
      f"`{SECTION_HEADING}` table must include header, separator, and at least one data row"
    )
  return lines


def validate_issue_summary_table(doc_text: str, clusters: list[dict[str, Any]]) -> None:
  actual_lines = extract_issue_summary_table(doc_text)
  expected_lines = expected_table_lines(clusters)
  if actual_lines != expected_lines:
    raise EquivalenceObligationsDocError(
      "open issue blocker summary table does not match derived issue-cluster report"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the open-issue blocker summary in EQUIVALENCE_OBLIGATIONS.md"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--doc", type=pathlib.Path, default=DOC_PATH)
  args = parser.parse_args()

  config = load_config(args.config)
  clusters = validate_issue_clusters(config)
  doc_text = args.doc.read_text(encoding="utf-8")

  validate_issue_summary_table(doc_text, clusters)

  print("equivalence-obligations-doc check: OK")
  print(f"issue clusters: {len(clusters)}")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except EquivalenceObligationsDocError as e:
    print(f"equivalence-obligations-doc check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"equivalence-obligations-doc check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
