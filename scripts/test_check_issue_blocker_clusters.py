#!/usr/bin/env python3
"""Unit tests for issue-cluster blocker checker."""

from __future__ import annotations

import pathlib
import sys
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_issue_blocker_clusters import (  # noqa: E402
  IssueClusterError,
  derive_cluster_blockers,
  load_obligations_by_operation,
  validate_issue_clusters,
)


def make_config() -> dict:
  return {
    "obligations": [
      {
        "operation": "supply",
        "macroSurfaceBlockers": ["callbacks", "erc20", "internalCall"],
      },
      {
        "operation": "withdraw",
        "macroSurfaceBlockers": ["erc20", "internalCall"],
      },
      {
        "operation": "liquidate",
        "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn"],
      },
    ],
    "issueClusters": [
      {
        "issue": 123,
        "title": "Core flows",
        "operations": ["supply", "withdraw"],
        "macroSurfaceBlockers": ["callbacks", "erc20", "internalCall"],
        "blockerCoverageCounts": {
          "callbacks": 1,
          "erc20": 2,
          "internalCall": 2,
        },
      },
      {
        "issue": 124,
        "title": "Collateral flows",
        "operations": ["liquidate"],
        "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn"],
        "blockerCoverageCounts": {
          "callbacks": 1,
          "erc20": 1,
          "externalWithReturn": 1,
        },
      },
    ],
  }


class LoadObligationsTests(unittest.TestCase):
  def test_loads_by_operation(self) -> None:
    by_operation = load_obligations_by_operation(make_config())
    self.assertEqual(sorted(by_operation), ["liquidate", "supply", "withdraw"])

  def test_rejects_duplicate_operations(self) -> None:
    config = make_config()
    config["obligations"].append({
      "operation": "supply",
      "macroSurfaceBlockers": [],
    })
    with self.assertRaisesRegex(IssueClusterError, "duplicate obligation operation"):
      load_obligations_by_operation(config)


class DeriveClusterBlockersTests(unittest.TestCase):
  def test_derives_sorted_union_and_counts(self) -> None:
    by_operation = load_obligations_by_operation(make_config())
    blockers, counts = derive_cluster_blockers(["supply", "withdraw"], by_operation)
    self.assertEqual(blockers, ["callbacks", "erc20", "internalCall"])
    self.assertEqual(counts, {"callbacks": 1, "erc20": 2, "internalCall": 2})


class ValidateIssueClustersTests(unittest.TestCase):
  def test_validate_issue_clusters_passes(self) -> None:
    reports = validate_issue_clusters(make_config())
    self.assertEqual([item["issue"] for item in reports], [123, 124])

  def test_rejects_blocker_drift(self) -> None:
    config = make_config()
    config["issueClusters"][0]["macroSurfaceBlockers"] = ["erc20"]
    with self.assertRaisesRegex(IssueClusterError, "macroSurfaceBlockers drift detected"):
      validate_issue_clusters(config)

  def test_rejects_count_drift(self) -> None:
    config = make_config()
    config["issueClusters"][0]["blockerCoverageCounts"]["erc20"] = 1
    with self.assertRaisesRegex(IssueClusterError, "blockerCoverageCounts drift detected"):
      validate_issue_clusters(config)

  def test_rejects_unknown_operation(self) -> None:
    config = make_config()
    config["issueClusters"][0]["operations"].append("borrow")
    with self.assertRaisesRegex(IssueClusterError, "unknown operations"):
      validate_issue_clusters(config)

  def test_rejects_duplicate_issue(self) -> None:
    config = make_config()
    config["issueClusters"][1]["issue"] = 123
    with self.assertRaisesRegex(IssueClusterError, "duplicate issue cluster"):
      validate_issue_clusters(config)


if __name__ == "__main__":
  unittest.main()
