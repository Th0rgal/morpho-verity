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
  build_exactness_summary,
  build_function_family_summary,
  build_parity_metadata,
  build_rewrite_family_summary,
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
  yul_identity_gate_mode,
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

  def test_build_rewrite_family_summary_groups_kinds_and_priority(self) -> None:
    summary = build_rewrite_family_summary(
      {
        "hashMismatch": ["checked_add_uint256#0"],
        "onlyInSolidity": ["checked_add_uint128#0", "mappingSlot#0"],
        "onlyInVerity": ["checked_add_uint64#0"],
      },
      {
        "pairs": [
          {
            "solidity": {"key": "copy_literal_to_memory_a#0"},
            "verity": {"key": "copy_literal_to_memory_b#0"},
          }
        ]
      },
    )
    self.assertEqual(summary["version"], "rewrite-family-v1")
    self.assertEqual(summary["entries"][0]["family"], "checked_add")
    self.assertEqual(summary["entries"][0]["count"], 1)
    self.assertEqual(summary["entries"][0]["kind"], "hashMismatch")
    checked_add_kinds = {(entry["family"], entry["kind"], entry["count"]) for entry in summary["entries"]}
    self.assertIn(("checked_add", "hashMismatch", 1), checked_add_kinds)
    self.assertIn(("checked_add", "onlyInSolidity", 1), checked_add_kinds)
    self.assertIn(("checked_add", "onlyInVerity", 1), checked_add_kinds)
    rename_entry = next(entry for entry in summary["entries"] if entry["kind"] == "renameOnly")
    self.assertEqual(rename_entry["family"], "copy_literal_to_memory")
    self.assertEqual(rename_entry["pairs"][0]["solidityKey"], "copy_literal_to_memory_a#0")
    self.assertEqual(summary["priorityFamilies"][0], {"family": "checked_add", "count": 3})

  def test_build_rewrite_family_summary_includes_ambiguous_rename_groups(self) -> None:
    summary = build_rewrite_family_summary(
      {
        "hashMismatch": [],
        "onlyInSolidity": ["checked_add_uint128#0", "checked_add_uint256#0"],
        "onlyInVerity": ["checked_add_uint64#0", "checked_add_uint32#0"],
      },
      {
        "pairs": [],
        "ambiguousGroups": [
          {
            "solidity": [
              {"key": "checked_add_uint128#0"},
              {"key": "checked_add_uint256#0"},
            ],
            "verity": [
              {"key": "checked_add_uint32#0"},
              {"key": "checked_add_uint64#0"},
            ],
          }
        ],
      },
    )
    ambiguous_entry = next(entry for entry in summary["entries"] if entry["kind"] == "renameAmbiguous")
    self.assertEqual(ambiguous_entry["family"], "checked_add")
    self.assertEqual(ambiguous_entry["count"], 1)
    self.assertEqual(
      ambiguous_entry["groups"][0],
      {
        "solidityKeys": ["checked_add_uint128#0", "checked_add_uint256#0"],
        "verityKeys": ["checked_add_uint32#0", "checked_add_uint64#0"],
      },
    )
    self.assertIn({"family": "checked_add", "count": 4}, summary["priorityFamilies"])

  def test_build_rewrite_family_summary_does_not_double_count_rename_pairs_in_priority(self) -> None:
    summary = build_rewrite_family_summary(
      {
        "hashMismatch": [],
        "onlyInSolidity": ["copy_literal_to_memory_a#0"],
        "onlyInVerity": ["copy_literal_to_memory_b#0"],
      },
      {
        "pairs": [
          {
            "solidity": {"key": "copy_literal_to_memory_a#0"},
            "verity": {"key": "copy_literal_to_memory_b#0"},
          }
        ],
        "ambiguousGroups": [],
      },
    )
    self.assertEqual(
      summary["priorityFamilies"],
      [{"family": "copy_literal_to_memory", "count": 2}],
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

  def test_build_report_includes_rewrite_family_summary(self) -> None:
    report, _ = build_report(
      normalize_yul(
        'object "M" { code { function checked_add_uint128() { pop(1) } function helper() { mstore(0, 7) } } }'
      ),
      normalize_yul(
        'object "M" { code { function checked_add_uint256() { pop(2) } function helper_renamed() { mstore(0, 7) } } }'
      ),
      max_diff_lines=50,
    )
    summary = report["functionBlocks"]["rewriteFamilies"]
    self.assertEqual(summary["version"], "rewrite-family-v1")
    self.assertEqual(summary["priorityFamilies"][0], {"family": "checked_add", "count": 2})
    self.assertIn(
      ("checked_add", "onlyInSolidity"),
      {(entry["family"], entry["kind"]) for entry in summary["entries"]},
    )
    self.assertIn(
      ("checked_add", "onlyInVerity"),
      {(entry["family"], entry["kind"]) for entry in summary["entries"]},
    )
    rename_entry = next(entry for entry in summary["entries"] if entry["kind"] == "renameOnly")
    self.assertEqual(rename_entry["family"], "helper_renamed->helper")

  def test_build_report_rewrite_family_summary_keeps_ambiguous_groups(self) -> None:
    report, _ = build_report(
      normalize_yul(
        'object "M" { code { function checked_add_uint128() { leave } function checked_add_uint256() { leave } } }'
      ),
      normalize_yul(
        'object "M" { code { function checked_add_uint32() { leave } function checked_add_uint64() { leave } } }'
      ),
      max_diff_lines=50,
    )
    ambiguous_entry = next(
      entry
      for entry in report["functionBlocks"]["rewriteFamilies"]["entries"]
      if entry["kind"] == "renameAmbiguous"
    )
    self.assertEqual(ambiguous_entry["family"], "checked_add")
    self.assertEqual(len(ambiguous_entry["groups"][0]["solidityKeys"]), 2)

  def test_build_exactness_summary_requires_ast_and_function_match(self) -> None:
    report, _ = build_report(
      normalize_yul('object "M" { code { function f() { leave } } }'),
      normalize_yul('object "M" { code { function g() { leave } } }'),
      max_diff_lines=50,
    )
    exactness = build_exactness_summary(report)
    self.assertFalse(exactness["raw"])
    self.assertFalse(exactness["normalized"])
    self.assertFalse(exactness["ast"])
    self.assertFalse(exactness["functionLevel"])
    self.assertFalse(exactness["fullyExact"])

  def test_build_exactness_summary_marks_exact_match(self) -> None:
    report, _ = build_report(
      normalize_yul('object "M" { code { function f() { leave } } }'),
      normalize_yul('object "M" { code { function f() { leave } } }'),
      max_diff_lines=50,
    )
    self.assertEqual(
      build_exactness_summary(report),
      {
        "raw": True,
        "normalized": True,
        "ast": True,
        "functionLevel": True,
        "fullyExact": True,
      },
    )

  def test_build_exactness_summary_tolerates_formatting_only_differences(self) -> None:
    report, _ = build_report(
      'object "M" {\n  code {\n    function f() { leave }\n  }\n}\n',
      'object "M"{code{function f(){leave}}}\n',
      max_diff_lines=50,
    )
    self.assertEqual(
      build_exactness_summary(report),
      {
        "raw": False,
        "normalized": False,
        "ast": True,
        "functionLevel": True,
        "fullyExact": True,
      },
    )

  def test_build_parity_metadata_extracts_pack_id(self) -> None:
    self.assertEqual(
      build_parity_metadata(
        {
          "id": "target-id",
          "verity": {"parityPackId": "solc-pack"},
          "yulIdentity": {"gateMode": "unsupported-manifest"},
        },
        "unsupported-manifest",
      ),
      {
        "id": "target-id",
        "verityParityPackId": "solc-pack",
        "yulIdentityGateMode": "unsupported-manifest",
      },
    )

  def test_build_parity_metadata_handles_missing_pack_id(self) -> None:
    self.assertEqual(
      build_parity_metadata(
        {"id": "target-id", "verity": {}, "yulIdentity": {"gateMode": "exact"}},
        "exact",
      ),
      {
        "id": "target-id",
        "verityParityPackId": None,
        "yulIdentityGateMode": "exact",
      },
    )

  def test_yul_identity_gate_mode_reads_supported_values(self) -> None:
    self.assertEqual(
      yul_identity_gate_mode({"yulIdentity": {"gateMode": "unsupported-manifest"}}),
      "unsupported-manifest",
    )
    self.assertEqual(yul_identity_gate_mode({"yulIdentity": {"gateMode": "exact"}}), "exact")

  def test_yul_identity_gate_mode_rejects_missing_or_unknown_values(self) -> None:
    with self.assertRaisesRegex(RuntimeError, "Missing required config `yulIdentity.gateMode`"):
      yul_identity_gate_mode({})
    with self.assertRaisesRegex(RuntimeError, "Invalid config `yulIdentity.gateMode`"):
      yul_identity_gate_mode({"yulIdentity": {"gateMode": "strict"}})

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
