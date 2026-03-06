#!/usr/bin/env python3
"""Fail-closed sync check for check scripts vs workflow references."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

WORKFLOW_CHECK_REF_RE = re.compile(r"\bscripts/(check_[A-Za-z0-9_]+\.py)\b")


def fail(msg: str) -> None:
  print(f"ci-check-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def collect_repo_check_scripts(scripts_dir: pathlib.Path) -> set[str]:
  checks: set[str] = set()
  for path in scripts_dir.glob("check_*.py"):
    if path.is_file():
      checks.add(path.name)
  return checks


def collect_workflow_check_scripts(workflow_text: str) -> set[str]:
  return {match.group(1) for match in WORKFLOW_CHECK_REF_RE.finditer(workflow_text)}


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate check scripts remain covered by verify workflow"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=pathlib.Path(".github/workflows/verify.yml"),
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=pathlib.Path("scripts"),
    help="Path to scripts directory",
  )
  args = parser.parse_args()

  workflow_text = args.workflow.read_text(encoding="utf-8")
  repo_checks = collect_repo_check_scripts(args.scripts_dir)
  workflow_checks = collect_workflow_check_scripts(workflow_text)

  missing_from_workflow = sorted(repo_checks - workflow_checks)
  if missing_from_workflow:
    fail("repo check scripts missing from workflow: " + ", ".join(missing_from_workflow))

  stale_workflow_refs = sorted(workflow_checks - repo_checks)
  if stale_workflow_refs:
    fail("workflow references missing check scripts: " + ", ".join(stale_workflow_refs))

  print(
    "ci-check-coverage: "
    f"repo_checks={len(repo_checks)} workflow_checks={len(workflow_checks)}"
  )
  print("ci-check-coverage check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
