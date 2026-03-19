#!/usr/bin/env python3
"""Unit tests for macro blocker regression coverage checker."""

from __future__ import annotations

import json
import pathlib
import shutil
import subprocess
import tempfile
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_blocker_regression_coverage import (
  RegressionCoverageError,
  build_regression_case_coverage,
  build_required_issue_blockers,
  load_obligations,
  validate_issue_blocker_regression_coverage,
)


class RequiredIssueBlockersTests(unittest.TestCase):
  def test_build_required_issue_blockers_returns_empty_when_no_issues_tracked(self) -> None:
    """All issue clusters are now macro-migrated, so build_required_issue_blockers
    should return an empty dict regardless of input obligations."""
    obligations = [
      {
        "issue": 123,
        "operation": "supply",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["erc20", "memoryOps"],
      },
      {
        "issue": 124,
        "operation": "liquidate",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20"],
      },
    ]
    self.assertEqual(build_required_issue_blockers(obligations), {})


class LoadObligationsTests(unittest.TestCase):
  def write_config(self, payload: str) -> pathlib.Path:
    tmp = tempfile.NamedTemporaryFile("w", encoding="utf-8", delete=False)
    self.addCleanup(pathlib.Path(tmp.name).unlink, missing_ok=True)
    with tmp:
      tmp.write(payload)
    return pathlib.Path(tmp.name)

  def test_load_obligations_rejects_malformed_json(self) -> None:
    path = self.write_config("{not json")
    with self.assertRaisesRegex(RegressionCoverageError, "failed to parse JSON config"):
      load_obligations(path)

  def test_load_obligations_rejects_non_object_root(self) -> None:
    path = self.write_config("[]")
    with self.assertRaisesRegex(RegressionCoverageError, "config root must be an object"):
      load_obligations(path)

  def test_load_obligations_rejects_missing_operation(self) -> None:
    path = self.write_config('{"obligations":[{"issue":123,"macroMigrated":false}]}')
    with self.assertRaisesRegex(RegressionCoverageError, "obligation\\[0\\] missing non-empty 'operation'"):
      load_obligations(path)

  def test_load_obligations_rejects_non_boolean_macro_migrated(self) -> None:
    path = self.write_config('{"obligations":[{"issue":123,"operation":"supply","macroMigrated":"no"}]}')
    with self.assertRaisesRegex(RegressionCoverageError, "obligation\\[0\\] missing boolean 'macroMigrated'"):
      load_obligations(path)

  def test_load_obligations_rejects_non_integer_tracked_issue(self) -> None:
    path = self.write_config('{"obligations":[{"issue":"123","operation":"supply","macroMigrated":false}]}')
    with self.assertRaisesRegex(RegressionCoverageError, "obligation\\[0\\] has non-integer 'issue'"):
      load_obligations(path)

  def test_load_obligations_rejects_boolean_tracked_issue(self) -> None:
    path = self.write_config('{"obligations":[{"issue":true,"operation":"supply","macroMigrated":false}]}')
    with self.assertRaisesRegex(RegressionCoverageError, "obligation\\[0\\] has non-integer 'issue'"):
      load_obligations(path)

  def test_load_obligations_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "semantic-bridge-obligations.json"
      path.write_bytes(b"\xff")

      with self.assertRaisesRegex(RegressionCoverageError, "failed to decode JSON config"):
        load_obligations(path)


class RegressionCaseCoverageTests(unittest.TestCase):
  def test_build_regression_case_coverage_returns_empty_when_no_issues_tracked(self) -> None:
    coverage = build_regression_case_coverage()
    self.assertEqual(coverage, {})

  def test_validate_issue_blocker_regression_coverage_rejects_missing_family(self) -> None:
    required = {123: {"callbacks", "erc20", "memoryOps"}}
    covered = {123: {"callbacks": {"callback"}, "erc20": {"erc20_transfer"}}}
    with self.assertRaisesRegex(
      RegressionCoverageError,
      "issue #123 blocker regressions missing coverage for: memoryOps",
    ):
      validate_issue_blocker_regression_coverage(required, covered)

  def test_validate_issue_blocker_regression_coverage_rejects_stale_family(self) -> None:
    required = {124: {"callbacks", "erc20"}}
    covered = {
      124: {
        "callbacks": {"callback"},
        "erc20": {"erc20_transfer"},
        "memoryOps": {"memory_ops"},
      }
    }
    with self.assertRaisesRegex(
      RegressionCoverageError,
      "issue #124 blocker regressions contain stale coverage for: memoryOps",
    ):
      validate_issue_blocker_regression_coverage(required, covered)

  def test_validate_issue_blocker_regression_coverage_rejects_stale_issue_entry(self) -> None:
    required = {123: {"callbacks"}}
    covered = {123: {"callbacks": {"callback"}}, 124: {"erc20": {"erc20_transfer"}}}
    with self.assertRaisesRegex(
      RegressionCoverageError,
      r"regression coverage has stale issue entries for: #124",
    ):
      validate_issue_blocker_regression_coverage(required, covered)

  def test_validate_issue_blocker_regression_coverage_accepts_matching_families(self) -> None:
    required = {124: {"callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"}}
    covered = {
      124: {
        "callbacks": {"callback"},
        "erc20": {"erc20_transfer", "erc20_transfer_from"},
        "externalWithReturn": {"calls_with_return"},
        "internalCall": {"internal_call"},
        "memoryOps": {"memory_ops"},
        "structMember2": {"struct_member2_read", "struct_member2_write"},
      }
    }
    validate_issue_blocker_regression_coverage(required, covered)


class CliTests(unittest.TestCase):
  def test_cli_does_not_depend_on_test_module_being_present(self) -> None:
    script_dir = pathlib.Path(__file__).resolve().parent
    checker = script_dir / "check_macro_blocker_regression_coverage.py"
    shared_cases = script_dir / "macro_blocker_regression_cases.py"
    obligations = script_dir.parent / "config" / "semantic-bridge-obligations.json"
    with tempfile.TemporaryDirectory() as d:
      sandbox = pathlib.Path(d)
      shutil.copy2(checker, sandbox / checker.name)
      shutil.copy2(shared_cases, sandbox / shared_cases.name)
      proc = subprocess.run(
        [sys.executable, str(sandbox / checker.name), "--obligations", str(obligations)],
        capture_output=True,
        text=True,
        cwd=sandbox,
        check=False,
      )

    self.assertEqual(proc.returncode, 0)
    self.assertIn("macro-blocker-regression-coverage check: OK", proc.stdout)
    self.assertEqual(proc.stderr, "")

  def test_cli_reports_invalid_json_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      obligations = pathlib.Path(d) / "semantic-bridge-obligations.json"
      obligations.write_text("{\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_macro_blocker_regression_coverage.py"),
          "--obligations",
          str(obligations),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertNotEqual(proc.returncode, 0)
    self.assertIn("macro-blocker-regression-coverage check failed:", proc.stderr)
    self.assertIn("failed to parse JSON config", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_invalid_utf8_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      obligations = pathlib.Path(d) / "semantic-bridge-obligations.json"
      obligations.write_bytes(b"\xff")

      proc = subprocess.run(
        [
          sys.executable,
          str(pathlib.Path(__file__).resolve().parent / "check_macro_blocker_regression_coverage.py"),
          "--obligations",
          str(obligations),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertNotEqual(proc.returncode, 0)
    self.assertIn("macro-blocker-regression-coverage check failed:", proc.stderr)
    self.assertIn("failed to decode JSON config", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_resolves_relative_external_obligations_from_another_cwd(self) -> None:
    script_dir = pathlib.Path(__file__).resolve().parent
    checker = script_dir / "check_macro_blocker_regression_coverage.py"

    with tempfile.TemporaryDirectory() as d:
      workspace = pathlib.Path(d)
      external = workspace / "external"
      caller = workspace / "caller"
      external.mkdir()
      caller.mkdir()
      obligations = external / "semantic-bridge-obligations.json"
      obligations.write_text(
        json.dumps(
          {
            "obligations": [
              {
                "issue": 123,
                "operation": "supply",
                "macroMigrated": False,
                "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
              },
              {
                "issue": 124,
                "operation": "liquidate",
                "macroMigrated": False,
                "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
              },
            ]
          }
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(checker),
          "--obligations",
          str(pathlib.Path("..") / "external" / "semantic-bridge-obligations.json"),
        ],
        cwd=caller,
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 0)
    self.assertIn("macro-blocker-regression-coverage check: OK", proc.stdout)
    self.assertEqual(proc.stderr, "")


if __name__ == "__main__":
  unittest.main()
