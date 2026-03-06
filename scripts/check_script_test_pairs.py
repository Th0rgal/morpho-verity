#!/usr/bin/env python3
"""Fail-closed check that every operational script has a matching unit test."""

from __future__ import annotations

import argparse
import pathlib
import sys


def fail(msg: str) -> None:
  print(f"script-test-pairs check failed: {msg}", file=sys.stderr)
  raise SystemExit(1)


def collect_repo_scripts(scripts_dir: pathlib.Path) -> set[str]:
  scripts: set[str] = set()
  for path in scripts_dir.glob("*"):
    if not path.is_file():
      continue
    if path.suffix not in (".py", ".sh"):
      continue
    if path.name.startswith("test_"):
      continue
    scripts.add(path.name)
  return scripts


def collect_repo_script_tests(scripts_dir: pathlib.Path) -> set[str]:
  tests: set[str] = set()
  for path in scripts_dir.glob("test_*"):
    if path.is_file() and path.suffix in (".py", ".sh"):
      tests.add(path.name)
  return tests


def expected_tests_for_script(script_name: str) -> set[str]:
  stem = pathlib.Path(script_name).stem
  return {f"test_{stem}.py", f"test_{stem}.sh"}


def source_candidates_for_test(test_name: str) -> set[str]:
  if not test_name.startswith("test_"):
    return set()
  stem = pathlib.Path(test_name).stem
  source_stem = stem[len("test_") :]
  return {f"{source_stem}.py", f"{source_stem}.sh"}


def find_missing_tests(repo_scripts: set[str], repo_tests: set[str]) -> list[str]:
  missing: list[str] = []
  for script_name in sorted(repo_scripts):
    if expected_tests_for_script(script_name).isdisjoint(repo_tests):
      missing.append(script_name)
  return missing


def find_stale_tests(repo_scripts: set[str], repo_tests: set[str]) -> list[str]:
  stale: list[str] = []
  for test_name in sorted(repo_tests):
    if source_candidates_for_test(test_name).isdisjoint(repo_scripts):
      stale.append(test_name)
  return stale


def main() -> int:
  parser = argparse.ArgumentParser(
    description="Validate every script has a paired script unit test and vice versa"
  )
  parser.add_argument(
    "--scripts-dir",
    type=pathlib.Path,
    default=pathlib.Path("scripts"),
    help="Path to scripts directory",
  )
  args = parser.parse_args()

  repo_scripts = collect_repo_scripts(args.scripts_dir)
  repo_tests = collect_repo_script_tests(args.scripts_dir)

  missing_tests = find_missing_tests(repo_scripts, repo_tests)
  if missing_tests:
    fail("scripts missing matching tests: " + ", ".join(missing_tests))

  stale_tests = find_stale_tests(repo_scripts, repo_tests)
  if stale_tests:
    fail("tests missing matching scripts: " + ", ".join(stale_tests))

  print(
    "script-test-pairs: "
    f"scripts={len(repo_scripts)} tests={len(repo_tests)}"
  )
  print("script-test-pairs check: OK")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
