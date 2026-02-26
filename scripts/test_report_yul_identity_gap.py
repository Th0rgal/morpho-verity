#!/usr/bin/env python3
"""Unit tests for Yul identity gap report helpers."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from report_yul_identity_gap import build_report, normalize_yul  # noqa: E402


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


if __name__ == "__main__":
  unittest.main()
