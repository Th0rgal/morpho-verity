#!/usr/bin/env python3
"""Fail-closed sync check for check scripts vs workflow references."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

from workflow_run_parser import extract_workflow_run_text

ROOT = pathlib.Path(__file__).resolve().parent.parent
WORKFLOW_PATH = ROOT / ".github" / "workflows" / "verify.yml"
SCRIPTS_DIR = ROOT / "scripts"

WORKFLOW_CHECK_REF_RE = re.compile(r"\bscripts/(check_[A-Za-z0-9_]+\.(?:py|sh))\b")


class CiCheckCoverageError(RuntimeError):
  """Raised when the CI check coverage checker cannot complete safely."""


def fail(msg: str) -> None:
  print(f"ci-check-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def read_text(path: pathlib.Path, *, context: str) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise CiCheckCoverageError(f"failed to read {context} {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise CiCheckCoverageError(
      f"failed to decode {context} {path} as UTF-8: {exc}"
    ) from exc


def collect_repo_check_scripts(scripts_dir: pathlib.Path) -> set[str]:
  checks: set[str] = set()
  for pattern in ("check_*.py", "check_*.sh"):
    for path in scripts_dir.glob(pattern):
      if path.is_file():
        checks.add(path.name)
  return checks


def collect_workflow_check_scripts(workflow_text: str) -> set[str]:
  try:
    run_text = extract_workflow_run_text(workflow_text)
  except ValueError as exc:
    raise CiCheckCoverageError(f"failed to parse workflow run commands: {exc}") from exc
  return {match.group(1) for match in WORKFLOW_CHECK_REF_RE.finditer(run_text)}


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate check scripts remain covered by verify workflow"
  )
  parser.add_argument(
    "--workflow",
    type=pathlib.Path,
    default=WORKFLOW_PATH,
    help="Path to workflow yaml",
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=SCRIPTS_DIR,
    help="Path to scripts directory",
  )
  args = parser.parse_args()
  workflow_path = args.workflow.resolve()
  scripts_dir = args.scripts_dir.resolve()

  try:
    workflow_text = read_text(workflow_path, context="workflow")
    repo_checks = collect_repo_check_scripts(scripts_dir)
    workflow_checks = collect_workflow_check_scripts(workflow_text)
  except CiCheckCoverageError as exc:
    fail(str(exc))

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
