#!/usr/bin/env python3
"""Unit tests for Yul identity gap report helpers."""

from __future__ import annotations

import pathlib
import os
import tempfile
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from report_yul_identity_gap import (  # noqa: E402
  ROOT,
  build_function_family_summary,
  build_name_insensitive_pairs,
  build_report,
  copy_prepared_verity_yul,
  compare_function_hashes,
  display_path,
  evaluate_unsupported_manifest,
  function_family_for_key,
  function_ast_digests,
  normalize_yul,
  prepared_verity_artifact_dir,
  tokenize_normalized_yul,
)


class ReportYulIdentityGapTests(unittest.TestCase):
  def test_prepared_artifact_dir_prefers_nested_edsl(self) -> None:
    old = os.environ.get("MORPHO_VERITY_PREPARED_ARTIFACT_DIR")
    with tempfile.TemporaryDirectory() as d:
      base = pathlib.Path(d)
      (base / "edsl").mkdir(parents=True, exist_ok=True)
      (base / "edsl" / "Morpho.yul").write_text("nested", encoding="utf-8")
      os.environ["MORPHO_VERITY_PREPARED_ARTIFACT_DIR"] = str(base)
      self.assertEqual(prepared_verity_artifact_dir(), base / "edsl")
    if old is None:
      os.environ.pop("MORPHO_VERITY_PREPARED_ARTIFACT_DIR", None)
    else:
      os.environ["MORPHO_VERITY_PREPARED_ARTIFACT_DIR"] = old

  def test_copy_prepared_verity_yul_fails_closed_when_missing(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      with self.assertRaisesRegex(RuntimeError, "Missing prepared Verity Yul artifact"):
        copy_prepared_verity_yul(pathlib.Path(d))

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

  def test_name_insensitive_pairs_detects_rename_only_candidate(self) -> None:
    sol = function_ast_digests("function g() { leave }\n")
    ver = function_ast_digests("function h() { leave }\n")
    deltas = {
      "hashMismatch": [],
      "onlyInSolidity": ["g#0"],
      "onlyInVerity": ["h#0"],
    }
    pairs = build_name_insensitive_pairs(sol, ver, deltas)
    self.assertEqual(pairs["pairCount"], 1)
    self.assertEqual(pairs["ambiguousGroupCount"], 0)
    self.assertEqual(pairs["pairs"][0]["solidity"]["key"], "g#0")
    self.assertEqual(pairs["pairs"][0]["verity"]["key"], "h#0")

  def test_name_insensitive_pairs_surfaces_ambiguous_groups(self) -> None:
    sol = function_ast_digests("function s0() { leave }\nfunction s1() { leave }\n")
    ver = function_ast_digests("function v0() { leave }\nfunction v1() { leave }\n")
    deltas = {
      "hashMismatch": [],
      "onlyInSolidity": ["s0#0", "s1#0"],
      "onlyInVerity": ["v0#0", "v1#0"],
    }
    pairs = build_name_insensitive_pairs(sol, ver, deltas)
    self.assertEqual(pairs["pairCount"], 0)
    self.assertEqual(pairs["ambiguousGroupCount"], 1)
    self.assertEqual(len(pairs["ambiguousGroups"][0]["solidity"]), 2)
    self.assertEqual(len(pairs["ambiguousGroups"][0]["verity"]), 2)

  def test_function_family_for_key_canonicalizes_known_patterns(self) -> None:
    self.assertEqual(function_family_for_key("abi_decode_address_27017#0"), "abi_decode_address")
    self.assertEqual(function_family_for_key("abi_encode_struct_MarketParams#0"), "abi_encode_struct")
    self.assertEqual(function_family_for_key("checked_add_uint256#0"), "checked_add")
    self.assertEqual(function_family_for_key("copy_literal_to_memory_deadbeef#0"), "copy_literal_to_memory")
    self.assertEqual(function_family_for_key("finalize_allocation_27020#0"), "finalize_allocation")
    self.assertEqual(function_family_for_key("fun_safeTransfer#0"), "fun")

  def test_build_function_family_summary_groups_and_sorts(self) -> None:
    summary = build_function_family_summary(
      {
        "hashMismatch": [],
        "onlyInSolidity": [
          "checked_add_uint128#0",
          "checked_add_uint256#0",
          "copy_literal_to_memory_a#0",
          "copy_literal_to_memory_b#0",
          "abi_decode_address_27017#0",
        ],
        "onlyInVerity": [
          "mappingSlot#0",
          "mappingSlot#1",
          "keccakMarketParams#0",
        ],
      }
    )
    self.assertEqual(summary["version"], "function-family-v1")
    self.assertEqual(summary["onlyInSolidity"][0]["family"], "checked_add")
    self.assertEqual(summary["onlyInSolidity"][0]["count"], 2)
    self.assertEqual(summary["onlyInSolidity"][1]["family"], "copy_literal_to_memory")
    self.assertEqual(summary["onlyInVerity"][0]["family"], "mappingSlot")
    self.assertEqual(summary["onlyInVerity"][0]["count"], 2)
    self.assertEqual(
      summary["priorityOnlyInSolidityFamilies"][0], {"family": "checked_add", "count": 2}
    )

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

  def test_build_report_includes_name_insensitive_pairs(self) -> None:
    report, _ = build_report(
      normalize_yul('object "M" { code { function g() { leave } } }'),
      normalize_yul('object "M" { code { function h() { leave } } }'),
      max_diff_lines=50,
    )
    pairs = report["functionBlocks"]["nameInsensitivePairs"]
    self.assertEqual(pairs["pairCount"], 1)
    self.assertEqual(pairs["pairs"][0]["solidity"]["key"], "h#0")
    self.assertEqual(pairs["pairs"][0]["verity"]["key"], "g#0")

  def test_build_report_includes_family_summary(self) -> None:
    report, _ = build_report(
      normalize_yul(
        'object "M" { code { function checked_add_uint256() { leave } function mappingSlot() { leave } } }'
      ),
      normalize_yul('object "M" { code { function checked_add_uint128() { leave } } }'),
      max_diff_lines=50,
    )
    summary = report["functionBlocks"]["familySummary"]
    self.assertEqual(summary["version"], "function-family-v1")
    self.assertEqual(summary["onlyInSolidity"][0]["family"], "checked_add")
    self.assertEqual(summary["onlyInVerity"][0]["family"], "checked_add")
    self.assertIn("mappingSlot", [entry["family"] for entry in summary["onlyInVerity"]])

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
