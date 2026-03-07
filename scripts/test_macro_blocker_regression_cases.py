#!/usr/bin/env python3
"""Unit tests for shared macro blocker regression case catalog."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from macro_blocker_regression_cases import (  # noqa: E402
  COLLATERAL_FRONTEND_REGRESSION_CASES,
  CORE_FLOW_FRONTEND_REGRESSION_CASES,
  ISSUE_FRONTEND_REGRESSION_CASES,
)


class SharedRegressionCaseTests(unittest.TestCase):
  def test_issue_mapping_points_to_shared_case_catalogs(self) -> None:
    self.assertIs(ISSUE_FRONTEND_REGRESSION_CASES[123], CORE_FLOW_FRONTEND_REGRESSION_CASES)
    self.assertIs(ISSUE_FRONTEND_REGRESSION_CASES[124], COLLATERAL_FRONTEND_REGRESSION_CASES)

  def test_case_catalogs_cover_expected_blocker_families(self) -> None:
    self.assertEqual(
      {case["blocker"] for case in CORE_FLOW_FRONTEND_REGRESSION_CASES},
      {"callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"},
    )
    self.assertEqual(
      {case["blocker"] for case in COLLATERAL_FRONTEND_REGRESSION_CASES},
      {"callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"},
    )


if __name__ == "__main__":
  unittest.main()
