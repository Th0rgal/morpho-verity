#!/usr/bin/env python3
"""Unit tests for macro migration selector/surface checker."""

from __future__ import annotations

import pathlib
import subprocess
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_surface import (  # noqa: E402
  MacroMigrationSurfaceError,
  ROOT,
  build_report,
  canonicalize_type,
  extract_spec_selector_entries,
  run_check,
  split_top_level_csv,
)


class CheckMacroMigrationSurfaceTests(unittest.TestCase):
  def test_split_top_level_csv_handles_nested_tuples(self) -> None:
    raw = "(address,address,address,address,uint256),uint256,address,bytes"
    self.assertEqual(
      split_top_level_csv(raw),
      ["(address,address,address,address,uint256)", "uint256", "address", "bytes"],
    )

  def test_canonicalize_type_struct_and_alias(self) -> None:
    structs = {
      "MarketParams": ["address", "address", "address", "address", "uint256"],
      "Signature": ["uint8", "bytes32", "bytes32"],
    }
    self.assertEqual(
      canonicalize_type("MarketParams memory", structs),
      "(address,address,address,address,uint256)",
    )
    self.assertEqual(canonicalize_type("Id", structs), "bytes32")
    self.assertEqual(canonicalize_type("Signature calldata", structs), "(uint8,bytes32,bytes32)")

  def test_split_top_level_csv_rejects_unbalanced_delimiters(self) -> None:
    with self.assertRaisesRegex(MacroMigrationSurfaceError, "unbalanced delimiters"):
      split_top_level_csv("(address,uint256")

  def test_extract_spec_selector_entries_requires_selector_block(self) -> None:
    with self.assertRaisesRegex(MacroMigrationSurfaceError, "unable to find morphoSelectors"):
      extract_spec_selector_entries("def somethingElse : List Nat := []")

  def test_build_report_marks_mismatch(self) -> None:
    report = build_report({"a()", "b(uint256)"}, {"a()", "c()"})
    self.assertEqual(report["status"], "mismatch")
    self.assertEqual(report["unexpectedOnlyInSpec"], ["b(uint256)"])
    self.assertEqual(report["onlyInInterface"], ["c()"])
    self.assertEqual(report["selectorMismatchCount"], 0)

  def test_build_report_marks_selector_mismatch(self) -> None:
    report = build_report(
      {"a()", "b(uint256)"},
      {"a()", "b(uint256)"},
      {"a()": 0xAAAAAAAA, "b(uint256)": 0xBBBBBBBB},
      {"a()": 0xAAAAAAAA, "b(uint256)": 0xCCCCCCCC},
    )
    self.assertEqual(report["status"], "mismatch")
    self.assertEqual(report["selectorMismatchCount"], 1)
    self.assertEqual(report["selectorMismatches"][0]["signature"], "b(uint256)")

  def test_extract_spec_selector_entries_parses_hex_and_signature(self) -> None:
    spec_text = """
def morphoSelectors : List Nat := [
  0x3644e515, -- DOMAIN_SEPARATOR()
  0x8da5cb5b  -- owner()
]
"""
    entries = extract_spec_selector_entries(spec_text)
    self.assertEqual(entries[0], ("DOMAIN_SEPARATOR()", 0x3644E515))
    self.assertEqual(entries[1], ("owner()", 0x8DA5CB5B))

  @unittest.skipUnless((ROOT / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol").exists(),
                       "requires initialized morpho-blue submodule (IMorpho.sol)")
  @unittest.skipUnless(shutil.which("solc"), "requires solc on PATH")
  def test_current_repo_surface_matches(self) -> None:
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["specPath"], "Morpho/Compiler/Spec.lean")
    self.assertEqual(report["interfacePath"], "morpho-blue/src/interfaces/IMorpho.sol")
    self.assertGreater(report["matchedSignatureCount"], 0)
    self.assertEqual(report["specSignatureCount"], report["interfaceSignatureCount"] + len(report["allowedOnlyInSpec"]))
    self.assertEqual(sorted(report["onlyInSpec"]), report["allowedOnlyInSpec"])
    self.assertEqual(report["selectorMismatchCount"], 0)
    self.assertIn("solc --hashes", report["selectorSource"])

  def test_root_constant_points_to_repo(self) -> None:
    self.assertTrue((ROOT / "Morpho" / "Compiler" / "Spec.lean").exists())

  def test_cli_reports_checker_error_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmpdir:
      bad_repo = pathlib.Path(tmpdir)
      (bad_repo / "scripts").mkdir()
      (bad_repo / "Morpho" / "Compiler").mkdir(parents=True)
      (bad_repo / "morpho-blue" / "src" / "interfaces").mkdir(parents=True)
      (bad_repo / "scripts" / "check_macro_migration_surface.py").write_text(
        (ROOT / "scripts" / "check_macro_migration_surface.py").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      (bad_repo / "Morpho" / "Compiler" / "Spec.lean").write_text(
        "def notMorphoSelectors : List Nat := []\n",
        encoding="utf-8",
      )
      (bad_repo / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol").write_text(
        "interface IMorpho {}\n",
        encoding="utf-8",
      )

      proc = subprocess.run(
        [sys.executable, str(bad_repo / "scripts" / "check_macro_migration_surface.py")],
        cwd=bad_repo,
        check=False,
        capture_output=True,
        text=True,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("macro-migration-surface check failed:", proc.stderr)
    self.assertIn("unable to find morphoSelectors", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
