#!/usr/bin/env python3
"""Unit tests for macro blocker regression coverage checker."""

from __future__ import annotations

import pathlib
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
  def test_build_required_issue_blockers_filters_to_open_issue_clusters(self) -> None:
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
      {
        "issue": 118,
        "operation": "createMarket",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["mappingStruct"],
      },
      {
        "issue": 123,
        "operation": "borrow",
        "macroMigrated": True,
        "macroSurfaceBlockers": ["externalWithReturn"],
      },
    ]

    self.assertEqual(
      build_required_issue_blockers(obligations),
      {
        123: {"erc20", "memoryOps"},
        124: {"callbacks", "erc20"},
      },
    )

  def test_build_required_issue_blockers_rejects_missing_blockers(self) -> None:
    obligations = [{"issue": 123, "operation": "supply", "macroMigrated": False}]
    with self.assertRaisesRegex(
      RegressionCoverageError,
      "obligation 'supply' missing non-empty string-list 'macroSurfaceBlockers'",
    ):
      build_required_issue_blockers(obligations)

  def test_build_required_issue_blockers_rejects_duplicate_blockers(self) -> None:
    obligations = [
      {
        "issue": 123,
        "operation": "supply",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["erc20", "erc20"],
      }
    ]
    with self.assertRaisesRegex(
      RegressionCoverageError,
      "obligation 'supply' contains duplicate macro blocker entries",
    ):
      build_required_issue_blockers(obligations)


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


class RegressionCaseCoverageTests(unittest.TestCase):
  def test_build_regression_case_coverage_tracks_case_names_by_blocker(self) -> None:
    coverage = build_regression_case_coverage()
    self.assertIn("calls_with_return", coverage[123]["externalWithReturn"])
    self.assertIn("memory_ops", coverage[124]["memoryOps"])

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


if __name__ == "__main__":
  unittest.main()
