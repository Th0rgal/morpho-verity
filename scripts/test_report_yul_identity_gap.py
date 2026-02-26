#!/usr/bin/env python3
"""Unit tests for Yul identity gap report helpers."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from report_yul_identity_gap import (  # noqa: E402
  ROOT,
  build_report,
  compare_function_hashes,
  display_path,
  evaluate_unsupported_manifest,
  extract_function_blocks,
  normalize_yul,
)


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

  def test_extract_function_blocks_tracks_ordinals(self) -> None:
    yul = normalize_yul(
      """
      object "M" {
        code {
          function f(x) -> r { r := add(x, 1) }
          function f(y) -> z { z := sub(y, 1) }
          function g() { leave }
        }
      }
      """
    )
    blocks = extract_function_blocks(yul)
    keys = [block.key for block in blocks]
    self.assertEqual(keys, ["f#0", "f#1", "g#0"])

  def test_compare_function_hashes(self) -> None:
    deltas = compare_function_hashes(
      {"f#0": "a", "g#0": "b"},
      {"f#0": "c", "h#0": "d"},
    )
    self.assertEqual(deltas["hashMismatch"], ["f#0"])
    self.assertEqual(deltas["onlyInSolidity"], ["g#0"])
    self.assertEqual(deltas["onlyInVerity"], ["h#0"])

  def test_manifest_check_detects_drift(self) -> None:
    deltas = {
      "hashMismatch": ["f#0"],
      "onlyInSolidity": ["g#0"],
      "onlyInVerity": [],
    }
    manifest = {
      "parityTarget": "target-v1",
      "allowedHashMismatchKeys": ["f#0"],
      "allowedOnlyInSolidityKeys": [],
      "allowedOnlyInVerityKeys": [],
    }
    check = evaluate_unsupported_manifest(deltas, manifest, "target-v1")
    self.assertFalse(check["ok"])
    self.assertTrue(check["parityTarget"]["ok"])
    self.assertEqual(check["unexpected"]["onlyInSolidity"], ["g#0"])
    self.assertEqual(check["missingExpected"]["hashMismatch"], [])

  def test_manifest_check_detects_target_mismatch(self) -> None:
    deltas = {
      "hashMismatch": [],
      "onlyInSolidity": [],
      "onlyInVerity": [],
    }
    manifest = {
      "parityTarget": "target-v1",
      "allowedHashMismatchKeys": [],
      "allowedOnlyInSolidityKeys": [],
      "allowedOnlyInVerityKeys": [],
    }
    check = evaluate_unsupported_manifest(deltas, manifest, "target-v2")
    self.assertFalse(check["ok"])
    self.assertFalse(check["parityTarget"]["ok"])


if __name__ == "__main__":
  unittest.main()
