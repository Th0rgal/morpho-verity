#!/usr/bin/env python3
"""Fail-closed sync check for the blocker-cluster summary in EQUIVALENCE_OBLIGATIONS.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from typing import Any

from check_issue_blocker_clusters import load_config, validate_issue_clusters
from check_semantic_bridge_readiness_summary import derive_summary


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
DOC_PATH = ROOT / "docs" / "EQUIVALENCE_OBLIGATIONS.md"
SECTION_HEADING = "### Blocker cluster summary"
TABLE_HEADER = "| Cluster | Operations | Blocker families | Coverage counts |"
TABLE_SEPARATOR = "|-------|------------|------------------|-----------------|"
STATUS_HEADING = "## Status"
MACRO_STATUS_PREFIX = (
  "**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in\n"
  "`MacroSlice.lean`, which is the current macro-generated contract surface."
)
LINK1_SUMMARY_RE = re.compile(
  r"(?P<count>\d+)/(?P<total>\d+)\s+obligations have Link 1 "
  r"\(stable `Morpho\.\*` wrapper API ↔ EDSL\) proven:\s*(?P<ops>.*?)\.\s+The proofs are in\s+"
  r"`Morpho/Proofs/SemanticBridgeDischarge\.lean`\.",
  re.DOTALL,
)
MACRO_MIGRATED_RE = re.compile(
  r"(?P<count>\d+)/(?P<total>\d+)\s+operations\s+are\s+macro-migrated; the remaining "
  r"(?P<pending>\d+)\s+are blocked on upstream macro",
  re.DOTALL,
)


class EquivalenceObligationsDocError(RuntimeError):
  pass


def require_match(pattern: re.Pattern[str], text: str, description: str) -> re.Match[str]:
  match = pattern.search(text)
  if match is None:
    raise EquivalenceObligationsDocError(
      f"missing `{description}` in EQUIVALENCE_OBLIGATIONS.md"
    )
  return match


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


def parse_operation_list(raw_ops: str) -> list[str]:
  operations = [item.strip() for item in raw_ops.replace("\n", " ").split(",")]
  normalized = [item for item in operations if item and item.lower() != "none"]
  if len(normalized) != len(set(normalized)):
    raise EquivalenceObligationsDocError(
      "equivalence obligations status operation list contains duplicate operations"
    )
  return normalized


def validate_status_summary(doc_text: str, summary: dict[str, object]) -> None:
  if STATUS_HEADING not in doc_text:
    raise EquivalenceObligationsDocError("missing `## Status` section in EQUIVALENCE_OBLIGATIONS.md")
  if MACRO_STATUS_PREFIX not in doc_text:
    raise EquivalenceObligationsDocError(
      "equivalence obligations macro migration intro drift: "
      f"expected `{MACRO_STATUS_PREFIX}`"
    )

  link1_match = require_match(LINK1_SUMMARY_RE, doc_text, "Link 1 status summary")
  actual_count = int(link1_match.group("count"))
  actual_total = int(link1_match.group("total"))
  expected_count = int(summary["link1_count"])
  expected_total = int(summary["total"])
  if actual_count != expected_count or actual_total != expected_total:
    raise EquivalenceObligationsDocError(
      "equivalence obligations Link 1 status summary drift: "
      f"expected {expected_count}/{expected_total}, found {actual_count}/{actual_total}"
    )

  actual_operations = parse_operation_list(link1_match.group("ops"))
  expected_operations = list(summary["link1_operations"])
  if not expected_operations and actual_operations:
    raise EquivalenceObligationsDocError(
      "equivalence obligations Link 1 operation list drift: expected no operations; found "
      + ", ".join(actual_operations)
    )
  if set(actual_operations) != set(expected_operations):
    raise EquivalenceObligationsDocError(
      "equivalence obligations Link 1 operation list drift: expected "
      + ", ".join(expected_operations)
      + "; found "
      + ", ".join(actual_operations)
    )

  macro_match = require_match(MACRO_MIGRATED_RE, doc_text, "macro-migrated status summary")
  actual_migrated = int(macro_match.group("count"))
  actual_migrated_total = int(macro_match.group("total"))
  actual_pending = int(macro_match.group("pending"))
  expected_migrated = int(summary["macro_migrated_count"])
  expected_pending = int(summary["macro_pending_count"])
  if (
    actual_migrated != expected_migrated
    or actual_migrated_total != expected_total
    or actual_pending != expected_pending
  ):
    raise EquivalenceObligationsDocError(
      "equivalence obligations macro migration summary drift: expected "
      f"{expected_migrated}/{expected_total} with {expected_pending} remaining, found "
      f"{actual_migrated}/{actual_migrated_total} with {actual_pending} remaining"
    )


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
  if len(lines) < 2:
    raise EquivalenceObligationsDocError(
      f"`{SECTION_HEADING}` table must include markdown header and separator rows"
    )
  if lines[0] != TABLE_HEADER or lines[1] != TABLE_SEPARATOR:
    raise EquivalenceObligationsDocError(
      f"`{SECTION_HEADING}` table must start with the expected markdown header and separator"
    )
  return lines


def validate_issue_summary_table(doc_text: str, clusters: list[dict[str, Any]]) -> None:
  actual_lines = extract_issue_summary_table(doc_text)
  expected_lines = expected_table_lines(clusters)
  if actual_lines != expected_lines:
    raise EquivalenceObligationsDocError(
      "blocker cluster summary table does not match derived issue-cluster report"
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate the blocker-cluster summary in EQUIVALENCE_OBLIGATIONS.md"
  )
  parser.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
  parser.add_argument("--doc", type=pathlib.Path, default=DOC_PATH)
  args = parser.parse_args()

  config = load_config(args.config)
  clusters = validate_issue_clusters(config)
  summary = derive_summary(config)
  doc_text = args.doc.read_text(encoding="utf-8")

  validate_status_summary(doc_text, summary)
  validate_issue_summary_table(doc_text, clusters)

  print("equivalence-obligations-doc check: OK")
  print(
    f"issue clusters: {len(clusters)}; "
    f"link1={summary['link1_count']}/{summary['total']}; "
    f"macro_migrated={summary['macro_migrated_count']}/{summary['total']}"
  )
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
