#!/usr/bin/env python3
"""Fail-closed sync check for semantic-bridge status bullets in RELEASE_CRITERIA.md."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_named_step_runs


ROOT = pathlib.Path(__file__).resolve().parent.parent
DOC_PATH = ROOT / "docs" / "RELEASE_CRITERIA.md"
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"

PARTIALLY_ENFORCED_HEADING = "Partially enforced:"
NOT_YET_ENFORCED_HEADING = "Not yet enforced:"

EXPECTED_PARTIALLY_ENFORCED_ITEMS = [
  "README semantic-bridge proved-vs-assumed summary drift gate is enforced in CI (`scripts/check_readme_semantic_bridge_summary.py`).",
  "Proof-facing semantic-bridge readiness registry and summary drift gates are enforced in CI (`scripts/check_semantic_bridge_readiness_sync.py`, `scripts/check_semantic_bridge_readiness_summary.py`).",
  "Machine-tracked `equivalence-obligations` status summary and blocker-cluster drift gates are enforced in CI (`scripts/check_equivalence_obligations_doc.py`, `scripts/check_issue_blocker_clusters.py`).",
  "Machine-tracked `semantic-bridge-obligations` status is enforced in CI (`scripts/check_semantic_bridge_obligations.py`); until all 18 obligations are discharged via the upstream verity hybrid migration path (verity#1060 / verity#1065), Solidity equivalence status remains \"Conditional\" rather than \"Proved\".",
  "Release-criteria semantic-bridge/equivalence status drift gate is enforced in CI (`scripts/check_release_criteria_status.py`).",
]

FORBIDDEN_NOT_YET_ENFORCED_ITEMS = [
  "machine-tracked `equivalence-obligations` status in CI output.",
  "`semantic-bridge-obligations` status in CI output (`config/semantic-bridge-obligations.json`). Once all 18 obligations are discharged via the upstream verity hybrid migration path (verity#1060 / verity#1065), Solidity equivalence status upgrades from \"Conditional\" to \"Proved\".",
]

EXPECTED_WORKFLOW_STEPS = [
  "Validate semantic bridge obligations",
  "Validate semantic bridge readiness sync",
  "Validate semantic bridge readiness summary",
  "Validate README semantic bridge summary",
  "Validate issue blocker clusters",
  "Validate equivalence obligations doc sync",
  "Validate release criteria status sync",
]
EXPECTED_WORKFLOW_RUN_LINES = {
  "Validate semantic bridge obligations":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate semantic bridge obligations" -- python3 scripts/check_semantic_bridge_obligations.py',
  "Validate semantic bridge readiness sync":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate semantic bridge readiness sync" -- python3 scripts/check_semantic_bridge_readiness_sync.py',
  "Validate semantic bridge readiness summary":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate semantic bridge readiness summary" -- python3 scripts/check_semantic_bridge_readiness_summary.py',
  "Validate README semantic bridge summary":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate README semantic bridge summary" -- python3 scripts/check_readme_semantic_bridge_summary.py',
  "Validate issue blocker clusters":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate issue blocker clusters" -- python3 scripts/check_issue_blocker_clusters.py',
  "Validate equivalence obligations doc sync":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate equivalence obligations doc sync" -- python3 scripts/check_equivalence_obligations_doc.py',
  "Validate release criteria status sync":
    './scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "Validate release criteria status sync" -- python3 scripts/check_release_criteria_status.py',
}

TRACKED_STATUS_ITEM_MARKERS = (
  "semantic-bridge",
  "`equivalence-obligations`",
  "`semantic-bridge-obligations`",
)


class ReleaseCriteriaStatusError(RuntimeError):
  pass


def extract_section(text: str, heading: str, next_heading: str) -> str:
  try:
    start = text.index(heading)
  except ValueError as exc:
    raise ReleaseCriteriaStatusError(f"missing `{heading}` section in RELEASE_CRITERIA.md") from exc

  try:
    end = text.index(next_heading, start)
  except ValueError as exc:
    raise ReleaseCriteriaStatusError(
      f"missing `{next_heading}` section after `{heading}` in RELEASE_CRITERIA.md"
    ) from exc

  return text[start:end]


def parse_numbered_items(section: str) -> list[str]:
  items: list[str] = []
  for line in section.splitlines():
    match = re.match(r"\s*\d+\.\s+(.*\S)\s*$", line)
    if match is not None:
      items.append(match.group(1))
  return items


def filter_tracked_items(items: list[str]) -> list[str]:
  return [
    item for item in items if any(marker in item for marker in TRACKED_STATUS_ITEM_MARKERS)
  ]

def validate_doc_status(text: str) -> None:
  partially_enforced = parse_numbered_items(
    extract_section(text, PARTIALLY_ENFORCED_HEADING, NOT_YET_ENFORCED_HEADING)
  )
  tracked_items = filter_tracked_items(partially_enforced)
  missing_items = [item for item in EXPECTED_PARTIALLY_ENFORCED_ITEMS if item not in tracked_items]
  if missing_items:
    raise ReleaseCriteriaStatusError(
      "RELEASE_CRITERIA.md is missing expected partially-enforced status items: "
      + "; ".join(missing_items)
    )
  unexpected_items = [item for item in tracked_items if item not in EXPECTED_PARTIALLY_ENFORCED_ITEMS]
  if unexpected_items:
    raise ReleaseCriteriaStatusError(
      "RELEASE_CRITERIA.md has unexpected semantic-bridge/equivalence partially-enforced items: "
      + "; ".join(unexpected_items)
    )
  duplicate_items = {item for item in tracked_items if tracked_items.count(item) > 1}
  if duplicate_items:
    raise ReleaseCriteriaStatusError(
      "RELEASE_CRITERIA.md has duplicate semantic-bridge/equivalence partially-enforced items: "
      + "; ".join(sorted(duplicate_items))
    )

  not_yet_enforced = parse_numbered_items(
    extract_section(text, NOT_YET_ENFORCED_HEADING, "## Required Gates (Target State)")
  )
  stale_items = [
    item for item in FORBIDDEN_NOT_YET_ENFORCED_ITEMS if item in not_yet_enforced
  ]
  if stale_items:
    raise ReleaseCriteriaStatusError(
      "RELEASE_CRITERIA.md still lists already-enforced gates as not yet enforced: "
      + "; ".join(stale_items)
    )


def validate_workflow(text: str) -> None:
  step_counts, step_runs = extract_named_step_runs(text)
  missing_steps = [step for step in EXPECTED_WORKFLOW_STEPS if step not in step_runs]
  if missing_steps:
    raise ReleaseCriteriaStatusError(
      "verify.yml is missing workflow steps referenced by RELEASE_CRITERIA.md: "
      + ", ".join(missing_steps)
    )
  duplicate_steps = [
    step for step in EXPECTED_WORKFLOW_STEPS if step_counts.get(step, 0) > 1
  ]
  if duplicate_steps:
    raise ReleaseCriteriaStatusError(
      "verify.yml duplicates workflow steps referenced by RELEASE_CRITERIA.md: "
      + ", ".join(sorted(duplicate_steps))
    )
  wrong_run_steps = [
    step for step in EXPECTED_WORKFLOW_STEPS
    if step_runs.get(step) != [EXPECTED_WORKFLOW_RUN_LINES[step]]
  ]
  if wrong_run_steps:
    raise ReleaseCriteriaStatusError(
      "verify.yml has drifted workflow commands referenced by RELEASE_CRITERIA.md: "
      + ", ".join(wrong_run_steps)
    )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate semantic-bridge/equivalence release-criteria status bullets"
  )
  parser.add_argument("--doc", type=pathlib.Path, default=DOC_PATH)
  parser.add_argument("--workflow", type=pathlib.Path, default=WORKFLOW_PATH)
  args = parser.parse_args()

  doc_text = args.doc.read_text(encoding="utf-8")
  workflow_text = args.workflow.read_text(encoding="utf-8")
  validate_doc_status(doc_text)
  validate_workflow(workflow_text)

  print("release-criteria-status check: OK")
  print(
    f"validated {len(EXPECTED_PARTIALLY_ENFORCED_ITEMS)} semantic-bridge/equivalence status bullets"
  )
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except ReleaseCriteriaStatusError as e:
    print(f"release-criteria-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
  except FileNotFoundError as e:
    print(f"release-criteria-status check failed: {e}", file=sys.stderr)
    raise SystemExit(1)
