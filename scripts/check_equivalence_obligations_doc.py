#!/usr/bin/env python3
"""Fail-closed sync check for the blocker-cluster summary in EQUIVALENCE_OBLIGATIONS.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys
from typing import Any

from check_issue_blocker_clusters import IssueClusterError, load_config, validate_issue_clusters
from check_semantic_bridge_readiness_summary import derive_summary


ROOT = pathlib.Path(__file__).resolve().parent.parent
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
DOC_PATH = ROOT / "docs" / "EQUIVALENCE_OBLIGATIONS.md"
SECTION_HEADING = "### Blocker cluster summary"
TABLE_HEADER = "| Cluster | Operations | Blocker families | Coverage counts |"
TABLE_SEPARATOR = "|-------|------------|------------------|-----------------|"
STATUS_HEADING = "## Status"
SCOPE_HEADING = "## Scope"
OBLIGATION_TABLE_HEADING = "## Obligation Table"
DISCHARGE_PATH_HEADING = "## Semantic Bridge Discharge Path"
MACRO_STATUS_PREFIX = (
  "**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in\n"
  "`MacroSlice.lean`, which is the current macro-generated contract surface."
)
UPSTREAM_BRIDGE_PREFIX = (
  "- **Links 2+3** (EDSL ↔ EVMYulLean): delegated to Verity's compiler\n"
  "  framework."
)
UPSTREAM_BRIDGE_STATUS_RE = re.compile(
  r"- \*\*Links 2\+3\*\* \(EDSL ↔ EVMYulLean\): delegated to Verity's compiler\s+"
  r"framework\.",
  re.DOTALL,
)
LINK1_SUMMARY_RE = re.compile(
  r"(?P<count>\d+)/(?P<total>\d+)\s+obligations have Link 1 "
  r"\(stable `Morpho\.\*` wrapper API ↔ EDSL\) proven:\s*(?P<ops>.*?)\.\s+The proofs are in\s+"
  r"`Morpho/Proofs/SemanticBridgeDischarge\.lean`\.",
  re.DOTALL,
)
MACRO_MIGRATED_RE = re.compile(
  r"(?P<count>\d+)/(?P<total>\d+)\s+operations\s+are\s*\n?\s*macro-migrated"
  r"(?:; the remaining (?P<pending>\d+)\s+are blocked on upstream macro"
  r"|\s*\()",
  re.DOTALL,
)


class EquivalenceObligationsDocError(RuntimeError):
  pass


def load_tracker_config(path: pathlib.Path) -> dict[str, Any]:
  try:
    return load_config(path)
  except IssueClusterError as exc:
    raise EquivalenceObligationsDocError(str(exc)) from exc
  except OSError as exc:
    raise EquivalenceObligationsDocError(f"failed to read config {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise EquivalenceObligationsDocError(f"config {path} is not valid UTF-8: {exc}") from exc


def read_doc_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise EquivalenceObligationsDocError(f"failed to read document {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise EquivalenceObligationsDocError(
      f"document {path} is not valid UTF-8: {exc}"
    ) from exc


def normalize_text(text: str) -> str:
  return re.sub(r"\s+", " ", text.replace("`", "")).strip()


def require_unique_match(
  pattern: re.Pattern[str], text: str, description: str
) -> re.Match[str]:
  matches = list(pattern.finditer(text))
  if not matches:
    raise EquivalenceObligationsDocError(
      f"missing `{description}` in EQUIVALENCE_OBLIGATIONS.md"
    )
  if len(matches) > 1:
    raise EquivalenceObligationsDocError(
      f"found multiple `{description}` matches in EQUIVALENCE_OBLIGATIONS.md"
  )
  return matches[0]


def find_unique_heading_line(text: str, heading: str, description: str) -> re.Match[str]:
  return require_unique_match(
    re.compile(rf"(?m)^{re.escape(heading)}$"),
    text,
    description,
  )


def extract_heading_section(doc_text: str, start_heading: str, end_heading: str) -> str:
  try:
    start_match = find_unique_heading_line(doc_text, start_heading, f"{start_heading} heading")
  except EquivalenceObligationsDocError as exc:
    raise EquivalenceObligationsDocError(
      f"missing `{start_heading}` section in EQUIVALENCE_OBLIGATIONS.md"
    ) from exc
  try:
    end_match = find_unique_heading_line(doc_text, end_heading, f"{end_heading} heading")
  except EquivalenceObligationsDocError as exc:
    raise EquivalenceObligationsDocError(
      f"missing `{end_heading}` boundary after `{start_heading}` in EQUIVALENCE_OBLIGATIONS.md"
    ) from exc
  start = start_match.end() + 1
  end = end_match.start()
  if end <= start:
    raise EquivalenceObligationsDocError(
      f"`{start_heading}` has invalid boundary ordering in EQUIVALENCE_OBLIGATIONS.md"
    )
  section = doc_text[start:end]
  if not section.strip():
    raise EquivalenceObligationsDocError(
      f"`{start_heading}` section is empty in EQUIVALENCE_OBLIGATIONS.md"
    )
  return section


def extract_macro_status_block(doc_text: str) -> str:
  table_section = extract_heading_section(doc_text, OBLIGATION_TABLE_HEADING, DISCHARGE_PATH_HEADING)
  try:
    end = find_unique_heading_line(
      table_section,
      SECTION_HEADING,
      f"{SECTION_HEADING} heading",
    ).start()
  except EquivalenceObligationsDocError as exc:
    raise EquivalenceObligationsDocError(
      "missing `### Blocker cluster summary` boundary after macro status block in EQUIVALENCE_OBLIGATIONS.md"
    ) from exc
  block = table_section[:end]
  if not block.strip():
    raise EquivalenceObligationsDocError(
      "macro status block is empty in EQUIVALENCE_OBLIGATIONS.md"
    )
  return block


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
  none_count = sum(1 for item in operations if item.lower() == "none")
  normalized = [item for item in operations if item and item.lower() != "none"]
  if none_count > 1:
    raise EquivalenceObligationsDocError(
      "equivalence obligations status operation list repeats the none sentinel"
    )
  if none_count and normalized:
    raise EquivalenceObligationsDocError(
      "equivalence obligations status operation list mixes the none sentinel with named operations"
    )
  if len(normalized) != len(set(normalized)):
    raise EquivalenceObligationsDocError(
      "equivalence obligations status operation list contains duplicate operations"
    )
  return normalized


def validate_status_summary(doc_text: str, summary: dict[str, object]) -> None:
  status_section = extract_heading_section(doc_text, STATUS_HEADING, SCOPE_HEADING)
  macro_block = extract_macro_status_block(doc_text)
  discharge_path_section = extract_heading_section(
    doc_text, DISCHARGE_PATH_HEADING, "## Spec Correspondence"
  )

  if normalize_text(MACRO_STATUS_PREFIX) not in normalize_text(macro_block):
    raise EquivalenceObligationsDocError(
      "equivalence obligations macro migration intro drift: "
      f"expected `{MACRO_STATUS_PREFIX}`"
    )
  try:
    require_unique_match(
      UPSTREAM_BRIDGE_STATUS_RE,
      discharge_path_section,
      "upstream bridge status summary",
    )
  except EquivalenceObligationsDocError as exc:
    raise EquivalenceObligationsDocError(
      "equivalence obligations upstream bridge status drift: "
      f"expected `{UPSTREAM_BRIDGE_PREFIX}`"
    ) from exc

  link1_match = require_unique_match(LINK1_SUMMARY_RE, status_section, "Link 1 status summary")
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

  macro_match = require_unique_match(
    MACRO_MIGRATED_RE,
    macro_block,
    "macro-migrated status summary",
  )
  actual_migrated = int(macro_match.group("count"))
  actual_migrated_total = int(macro_match.group("total"))
  pending_str = macro_match.group("pending")
  actual_pending = int(pending_str) if pending_str is not None else 0
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
  try:
    table_section = extract_heading_section(
      doc_text,
      OBLIGATION_TABLE_HEADING,
      DISCHARGE_PATH_HEADING,
    )
    heading_match = find_unique_heading_line(
      table_section,
      SECTION_HEADING,
      f"{SECTION_HEADING} heading",
    )
  except EquivalenceObligationsDocError as exc:
    raise EquivalenceObligationsDocError(
      f"missing `{SECTION_HEADING}` section followed by markdown table"
    ) from exc

  lines = table_section[heading_match.end() :].splitlines()
  table_lines: list[str] = []
  for line in lines:
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
  config_path = args.config.resolve()
  doc_path = args.doc.resolve()

  config = load_tracker_config(config_path)
  clusters = validate_issue_clusters(config)
  summary = derive_summary(config)
  doc_text = read_doc_text(doc_path)

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
