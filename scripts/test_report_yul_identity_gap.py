#!/usr/bin/env python3
"""Unit tests for Yul identity gap report helpers."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from report_yul_identity_gap import ROOT, build_report, display_path, normalize_yul  # noqa: E402


class ReportYulIdentityGapTests(unittest.TestCase):
  def test_normalize_yul_strips_comments(self) -> None:
    text = """
// leading
object "M" {
  /* block */
  code { let x := 1 } // trailing
}
"""
    normalized = normalize_yul(text)
    self.assertEqual(normalized, 'object "M" {\n  code { let x := 1 }\n}\n')

  def test_build_report_detects_mismatch(self) -> None:
    report, diff = build_report('object "A" {}', 'object "B" {}', max_diff_lines=50)
    self.assertEqual(report["status"], "mismatch")
    self.assertFalse(report["normalizedEqual"])
    self.assertGreater(report["diff"]["lineCount"], 0)
    self.assertIn("solidity/Morpho.irOptimized.normalized.yul", diff)

  def test_display_path_falls_back_for_paths_outside_repo(self) -> None:
    outside = pathlib.Path("/tmp/report-yul-identity-gap-test/report.json").resolve()
    self.assertEqual(display_path(outside), str(outside))
    inside = (ROOT / "out" / "parity-target" / "report.json").resolve()
    self.assertEqual(display_path(inside), "out/parity-target/report.json")


if __name__ == "__main__":
  unittest.main()
