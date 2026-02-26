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
  function_ast_digests,
  normalize_yul,
  tokenize_normalized_yul,
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
    self.assertFalse(report["astEqual"])
    self.assertGreater(report["diff"]["lineCount"], 0)
    self.assertIn("solidity/Morpho.irOptimized.normalized.yul", diff)
    self.assertEqual(report["astComparatorVersion"], "yul-struct-v1")

  def test_display_path_falls_back_for_paths_outside_repo(self) -> None:
    outside = pathlib.Path("/tmp/report-yul-identity-gap-test/report.json").resolve()
    self.assertEqual(display_path(outside), str(outside))
    inside = (ROOT / "out" / "parity-target" / "report.json").resolve()
    self.assertEqual(display_path(inside), "out/parity-target/report.json")

  def test_compare_function_hashes(self) -> None:
    deltas = compare_function_hashes(
      {
        "f#0": function_ast_digests("function f() { let x := 1 }\n")["f#0"],
        "g#0": function_ast_digests("function g() { leave }\n")["g#0"],
      },
      {
        "f#0": function_ast_digests("function f() { let x := 2 }\n")["f#0"],
        "h#0": function_ast_digests("function h() { leave }\n")["h#0"],
      },
    )
    self.assertEqual(deltas["hashMismatch"], ["f#0"])
    self.assertEqual(deltas["onlyInSolidity"], ["g#0"])
    self.assertEqual(deltas["onlyInVerity"], ["h#0"])

  def test_tokenizer_keeps_strings_and_compound_tokens(self) -> None:
    tokens = tokenize_normalized_yul('let x := add("a b", 0x10) -> y')
    self.assertEqual(tokens, ["let", "x", ":=", "add", "(", '"a b"', ",", "0x10", ")", "->", "y"])

  def test_build_report_includes_function_mismatch_details(self) -> None:
    report, _ = build_report(
      normalize_yul('object "M" { code { function f() -> r { r := 1 } } }'),
      normalize_yul('object "M" { code { function f() -> r { r := 2 } } }'),
      max_diff_lines=50,
    )
    details = report["functionBlocks"]["mismatchDetails"]
    self.assertEqual(len(details), 1)
    self.assertEqual(details[0]["key"], "f#0")
    self.assertEqual(details[0]["firstMismatch"]["solidityToken"], "2")
    self.assertEqual(details[0]["firstMismatch"]["verityToken"], "1")
    self.assertEqual(details[0]["firstMismatch"]["solidityLine"], 1)
    self.assertEqual(details[0]["firstMismatch"]["verityLine"], 1)

  def test_build_report_top_level_mismatch_includes_token_coordinates(self) -> None:
    report, _ = build_report(
      normalize_yul('object "M" { code { function f() -> r { r := 1 } } }'),
      normalize_yul('object "M" { code { function f() -> r { r := 2 } } }'),
      max_diff_lines=50,
    )
    mismatch = report["ast"]["firstMismatch"]
    self.assertEqual(mismatch["solidityToken"], "2")
    self.assertEqual(mismatch["verityToken"], "1")
    self.assertEqual(mismatch["tokenIndex"], 14)
    self.assertEqual(mismatch["solidityLine"], 1)
    self.assertEqual(mismatch["solidityColumn"], 46)
    self.assertEqual(mismatch["verityLine"], 1)
    self.assertEqual(mismatch["verityColumn"], 46)

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
