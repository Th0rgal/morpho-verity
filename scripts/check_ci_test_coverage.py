#!/usr/bin/env python3
"""Fail-closed sync check for script unit tests vs workflow coverage."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

WORKFLOW_TEST_REF_RE = re.compile(r"\bscripts/(test_[A-Za-z0-9_]+\.(?:py|sh))\b")
WORKFLOW_PY_DISCOVER_RE = re.compile(
  r"python3\s+-m\s+unittest\s+discover[^\n]*-s\s+scripts[^\n]*-p\s+['\"]test_\*\.py['\"]"
)
WORKFLOW_SH_GLOB_RE = re.compile(r"\bscripts/test_\*\.sh\b")


def fail(msg: str) -> None:
  print(f"ci-test-coverage check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def collect_repo_script_tests(scripts_dir: pathlib.Path) -> set[str]:
  return collect_repo_python_script_tests(scripts_dir) | collect_repo_shell_script_tests(scripts_dir)


def collect_repo_python_script_tests(scripts_dir: pathlib.Path) -> set[str]:
  tests: set[str] = set()
  for path in scripts_dir.glob("test_*.py"):
    if path.is_file():
      tests.add(path.name)
  return tests


def collect_repo_shell_script_tests(scripts_dir: pathlib.Path) -> set[str]:
  tests: set[str] = set()
  for path in scripts_dir.glob("test_*.sh"):
    if path.is_file():
      tests.add(path.name)
  return tests


def collect_workflow_script_tests(workflow_text: str) -> set[str]:
  return {match.group(1) for match in WORKFLOW_TEST_REF_RE.finditer(workflow_text)}


def has_workflow_python_discovery(workflow_text: str) -> bool:
  return bool(WORKFLOW_PY_DISCOVER_RE.search(workflow_text))


def has_workflow_shell_glob(workflow_text: str) -> bool:
  return bool(WORKFLOW_SH_GLOB_RE.search(workflow_text))


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate script unit tests remain covered by verify workflow"
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
  repo_py_tests = collect_repo_python_script_tests(args.scripts_dir)
  repo_sh_tests = collect_repo_shell_script_tests(args.scripts_dir)
  repo_tests = repo_py_tests | repo_sh_tests
  workflow_explicit_tests = collect_workflow_script_tests(workflow_text)

  workflow_tests = set(workflow_explicit_tests)
  if has_workflow_python_discovery(workflow_text):
    workflow_tests.update(repo_py_tests)
  if has_workflow_shell_glob(workflow_text):
    workflow_tests.update(repo_sh_tests)

  missing_from_workflow = sorted(repo_tests - workflow_tests)
  if missing_from_workflow:
    fail("repo script tests missing from workflow: " + ", ".join(missing_from_workflow))

  stale_workflow_refs = sorted(workflow_explicit_tests - repo_tests)
  if stale_workflow_refs:
    fail("workflow references missing script tests: " + ", ".join(stale_workflow_refs))

  print(
    "ci-test-coverage: "
    f"repo_tests={len(repo_tests)} workflow_tests={len(workflow_tests)}"
  )
  print("ci-test-coverage check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
