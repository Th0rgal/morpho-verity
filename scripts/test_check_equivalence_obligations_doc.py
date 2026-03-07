#!/usr/bin/env python3
"""Unit tests for EQUIVALENCE_OBLIGATIONS.md sync guard."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_equivalence_obligations_doc import (  # noqa: E402
  EquivalenceObligationsDocError,
  expected_table_lines,
  extract_issue_summary_table,
  main,
  validate_issue_summary_table,
)
from check_issue_blocker_clusters import validate_issue_clusters  # noqa: E402


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
      {"issue": 123, "title": "Core flows"},
      {"issue": 124, "title": "Collateral flows"},
    ],
  }


def make_doc(table_lines: list[str]) -> str:
  return "\n".join([
    "# Equivalence",
    "",
    "Some intro.",
    "",
    "### Open issue blocker summary",
    "",
    *table_lines,
    "",
    "Next section.",
    "",
  ])


class EquivalenceObligationsDocTests(unittest.TestCase):
  def test_extract_issue_summary_table(self) -> None:
    table_lines = [
      "| Issue | Operations | Blocker families | Coverage counts |",
      "|-------|------------|------------------|-----------------|",
      "| `#123` | `supply` | `erc20` | erc20\u00d71 |",
    ]
    self.assertEqual(extract_issue_summary_table(make_doc(table_lines)), table_lines)

  def test_validate_issue_summary_table_passes(self) -> None:
    clusters = validate_issue_clusters(make_config())
    validate_issue_summary_table(make_doc(expected_table_lines(clusters)), clusters)

  def test_validate_issue_summary_table_rejects_drift(self) -> None:
    clusters = validate_issue_clusters(make_config())
    drifted_doc = make_doc([
      "| Issue | Operations | Blocker families | Coverage counts |",
      "|-------|------------|------------------|-----------------|",
      "| `#123` | `supply` | `erc20` | erc20\u00d71 |",
      "| `#124` | `liquidate` | `erc20` | erc20\u00d71 |",
    ])
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "does not match derived issue-cluster report",
    ):
      validate_issue_summary_table(drifted_doc, clusters)

  def test_main_passes_on_synced_files(self) -> None:
    config = make_config()
    clusters = validate_issue_clusters(config)
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text(make_doc(expected_table_lines(clusters)), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_equivalence_obligations_doc.py",
          "--config",
          str(config_path),
          "--doc",
          str(doc_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_section_missing(self) -> None:
    config = make_config()
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text("# Equivalence\n", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_equivalence_obligations_doc.py",
          "--config",
          str(config_path),
          "--doc",
          str(doc_path),
        ]
        with self.assertRaisesRegex(
          EquivalenceObligationsDocError,
          "missing `### Open issue blocker summary` section",
        ):
          main()
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
