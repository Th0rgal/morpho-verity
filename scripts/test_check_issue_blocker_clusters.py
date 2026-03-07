#!/usr/bin/env python3
"""Unit tests for blocker-cluster blocker checker."""

from __future__ import annotations

import json
import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_issue_blocker_clusters import (  # noqa: E402
  IssueClusterError,
  ROOT,
  display_path,
  derive_cluster_blockers,
  load_config,
  load_obligations_by_operation,
  validate_issue_clusters,
)


def make_config() -> dict:
  return {
    "obligations": [
      {
        "issue": 123,
        "operation": "supply",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20", "internalCall"],
      },
      {
        "issue": 123,
        "operation": "withdraw",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["erc20", "internalCall"],
      },
      {
        "issue": 124,
        "operation": "liquidate",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn"],
      },
    ],
    "issueClusters": [
      {
        "issue": 123,
        "title": "Core flows",
      },
      {
        "issue": 124,
        "title": "Collateral flows",
      },
    ],
  }


class LoadObligationsTests(unittest.TestCase):
  def test_load_config_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_bytes(b"\x80")
      with self.assertRaisesRegex(IssueClusterError, "is not valid UTF-8"):
        load_config(path)

  def test_load_config_rejects_malformed_json(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_text("{not json", encoding="utf-8")
      with self.assertRaisesRegex(IssueClusterError, "failed to parse JSON config"):
        load_config(path)

  def test_load_config_rejects_non_object_root(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_text(json.dumps(["not", "an", "object"]), encoding="utf-8")
      with self.assertRaisesRegex(IssueClusterError, "config root must be an object"):
        load_config(path)

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

  def test_rejects_duplicate_blockers_within_single_operation(self) -> None:
    config = make_config()
    config["obligations"][0]["macroSurfaceBlockers"] = [
      "callbacks",
      "erc20",
      "erc20",
    ]
    by_operation = load_obligations_by_operation(config)
    with self.assertRaisesRegex(IssueClusterError, "duplicate macroSurfaceBlockers entries"):
      derive_cluster_blockers(["supply"], by_operation)


class DisplayPathTests(unittest.TestCase):
  def test_display_path_renders_repo_absolute_paths_relative_to_root(self) -> None:
    path = ROOT / "config" / "semantic-bridge-obligations.json"
    self.assertEqual(display_path(path), pathlib.Path("config/semantic-bridge-obligations.json"))

  def test_display_path_keeps_external_absolute_paths(self) -> None:
    path = pathlib.Path("/tmp/config.json")
    self.assertEqual(display_path(path), path)


class ValidateIssueClustersTests(unittest.TestCase):
  def test_validate_issue_clusters_passes(self) -> None:
    reports = validate_issue_clusters(make_config())
    self.assertEqual([item["issue"] for item in reports], [123, 124])
    self.assertEqual(reports[0]["operations"], ["supply", "withdraw"])
    self.assertEqual(reports[0]["blockerCoverageCounts"]["erc20"], 2)

  def test_rejects_unknown_issue_reference(self) -> None:
    config = make_config()
    config["obligations"][0]["issue"] = 999
    with self.assertRaisesRegex(IssueClusterError, "unknown issue cluster"):
      validate_issue_clusters(config)

  def test_rejects_issue_tag_once_obligation_is_migrated(self) -> None:
    config = make_config()
    config["obligations"][0]["macroMigrated"] = True
    with self.assertRaisesRegex(IssueClusterError, "cannot reference blocker cluster"):
      validate_issue_clusters(config)

  def test_rejects_missing_macro_surface_blockers(self) -> None:
    config = make_config()
    del config["obligations"][0]["macroSurfaceBlockers"]
    with self.assertRaisesRegex(IssueClusterError, "missing non-empty string-list 'macroSurfaceBlockers'"):
      validate_issue_clusters(config)

  def test_rejects_duplicate_macro_surface_blockers(self) -> None:
    config = make_config()
    config["obligations"][0]["macroSurfaceBlockers"] = [
      "callbacks",
      "erc20",
      "erc20",
    ]
    with self.assertRaisesRegex(IssueClusterError, "duplicate macroSurfaceBlockers entries"):
      validate_issue_clusters(config)

  def test_rejects_empty_issue_cluster(self) -> None:
    config = make_config()
    config["obligations"][2]["issue"] = 123
    with self.assertRaisesRegex(IssueClusterError, "does not cover any obligations"):
      validate_issue_clusters(config)

  def test_rejects_duplicate_issue(self) -> None:
    config = make_config()
    config["issueClusters"][1]["issue"] = 123
    with self.assertRaisesRegex(IssueClusterError, "duplicate issue cluster"):
      validate_issue_clusters(config)

  def test_rejects_boolean_issue_cluster_id(self) -> None:
    config = make_config()
    config["issueClusters"][0]["issue"] = True
    with self.assertRaisesRegex(IssueClusterError, "missing integer 'issue'"):
      validate_issue_clusters(config)

  def test_rejects_boolean_obligation_issue(self) -> None:
    config = make_config()
    config["obligations"][0]["issue"] = False
    with self.assertRaisesRegex(IssueClusterError, "has non-integer 'issue'"):
      validate_issue_clusters(config)


class CliTests(unittest.TestCase):
  def test_cli_reports_invalid_utf8_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      config_path = pathlib.Path(d) / "config.json"
      config_path.write_bytes(b"\x80")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_issue_blocker_clusters.py"),
          "--config",
          str(config_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("issue blocker clusters check failed:", proc.stderr)
    self.assertIn("is not valid UTF-8", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_invalid_json_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      config_path = pathlib.Path(d) / "config.json"
      config_path.write_text("{invalid\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_issue_blocker_clusters.py"),
          "--config",
          str(config_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("issue blocker clusters check failed: failed to parse JSON config", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_json_out_write_failure_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      config_path = pathlib.Path(d) / "config.json"
      json_out = pathlib.Path(d) / "occupied"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      json_out.write_text("not a directory", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_issue_blocker_clusters.py"),
          "--config",
          str(config_path),
          "--json-out",
          str(json_out / "report.json"),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("issue blocker clusters check failed: failed to write JSON report", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_json_out_records_actual_config_path(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_dir = root / "external"
      config_dir.mkdir()
      config_path = config_dir / "config.json"
      json_out = root / "report.json"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_issue_blocker_clusters.py"),
          "--config",
          str(config_path),
          "--json-out",
          str(json_out),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 0)
      report = json.loads(json_out.read_text(encoding="utf-8"))

    self.assertEqual(report["configPath"], str(config_path.resolve()))
    self.assertEqual(report["clusterCount"], 2)


if __name__ == "__main__":
  unittest.main()
