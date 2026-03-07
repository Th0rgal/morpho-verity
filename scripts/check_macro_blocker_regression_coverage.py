#!/usr/bin/env python3
"""Fail-closed sync check for open-issue macro blockers vs compile regressions."""

from __future__ import annotations

import argparse
import json
import pathlib
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
OBLIGATIONS_PATH = ROOT / "config" / "semantic-bridge-obligations.json"

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from test_check_macro_migration_blockers import ISSUE_FRONTEND_REGRESSION_CASES  # noqa: E402


class RegressionCoverageError(RuntimeError):
  pass


def load_obligations(path: pathlib.Path) -> list[dict[str, Any]]:
  with path.open("r", encoding="utf-8") as f:
    raw = json.load(f)
  obligations = raw.get("obligations")
  if not isinstance(obligations, list):
    raise RegressionCoverageError(f"missing `obligations` list in {path}")
  result: list[dict[str, Any]] = []
  for i, item in enumerate(obligations):
    if not isinstance(item, dict):
      raise RegressionCoverageError(f"obligation[{i}] is not an object in {path}")
    result.append(item)
  return result


def build_required_issue_blockers(obligations: list[dict[str, Any]]) -> dict[int, set[str]]:
  required: dict[int, set[str]] = {}
  for item in obligations:
    issue = item.get("issue")
    if issue not in ISSUE_FRONTEND_REGRESSION_CASES:
      continue
    if item.get("macroMigrated") is not False:
      continue
    blockers = item.get("macroSurfaceBlockers")
    operation = item.get("operation", "<unknown>")
    if not isinstance(blockers, list) or not blockers or not all(isinstance(x, str) and x for x in blockers):
      raise RegressionCoverageError(
        f"obligation '{operation}' missing non-empty string-list 'macroSurfaceBlockers'"
      )
    required.setdefault(issue, set()).update(blockers)
  return required


def build_regression_case_coverage() -> dict[int, dict[str, set[str]]]:
  coverage: dict[int, dict[str, set[str]]] = {}
  for issue, cases in ISSUE_FRONTEND_REGRESSION_CASES.items():
    issue_coverage: dict[str, set[str]] = {}
    seen_names: set[str] = set()
    for case in cases:
      name = case.get("name")
      blocker = case.get("blocker")
      if not isinstance(name, str) or not name:
        raise RegressionCoverageError(f"issue {issue} has regression case with invalid name: {case!r}")
      if name in seen_names:
        raise RegressionCoverageError(f"issue {issue} duplicates regression case name '{name}'")
      seen_names.add(name)
      if not isinstance(blocker, str) or not blocker:
        raise RegressionCoverageError(f"issue {issue} regression case '{name}' missing blocker")
      issue_coverage.setdefault(blocker, set()).add(name)
    coverage[issue] = issue_coverage
  return coverage


def validate_issue_blocker_regression_coverage(
  required: dict[int, set[str]],
  covered: dict[int, dict[str, set[str]]],
) -> None:
  for issue, blockers in sorted(required.items()):
    covered_blockers = set(covered.get(issue, {}))
    missing = sorted(blockers - covered_blockers)
    if missing:
      raise RegressionCoverageError(
        f"issue #{issue} blocker regressions missing coverage for: {', '.join(missing)}"
      )


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate open issue macroSurfaceBlockers stay covered by compile regressions"
  )
  parser.add_argument(
    "--obligations",
    type=pathlib.Path,
    default=OBLIGATIONS_PATH,
    help="Path to semantic bridge obligations config",
  )
  args = parser.parse_args()

  required = build_required_issue_blockers(load_obligations(args.obligations))
  covered = build_regression_case_coverage()
  validate_issue_blocker_regression_coverage(required, covered)

  for issue in sorted(required):
    blockers = ", ".join(sorted(required[issue]))
    print(f"macro-blocker-regression-coverage: issue #{issue} blockers={blockers}")
  print("macro-blocker-regression-coverage check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
