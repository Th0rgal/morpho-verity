#!/usr/bin/env python3
"""Unit tests for macro migration selector/surface checker."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_surface import (  # noqa: E402
  ROOT,
  build_report,
  canonicalize_type,
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

  def test_build_report_marks_mismatch(self) -> None:
    report = build_report({"a()", "b(uint256)"}, {"a()", "c()"})
    self.assertEqual(report["status"], "mismatch")
    self.assertEqual(report["unexpectedOnlyInSpec"], ["b(uint256)"])
    self.assertEqual(report["onlyInInterface"], ["c()"])

  def test_current_repo_surface_matches(self) -> None:
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["specPath"], "Morpho/Compiler/Spec.lean")
    self.assertEqual(report["interfacePath"], "morpho-blue/src/interfaces/IMorpho.sol")
    self.assertGreater(report["matchedSignatureCount"], 0)
    self.assertEqual(report["specSignatureCount"], report["interfaceSignatureCount"] + len(report["allowedOnlyInSpec"]))
    self.assertEqual(sorted(report["onlyInSpec"]), report["allowedOnlyInSpec"])

  def test_root_constant_points_to_repo(self) -> None:
    self.assertTrue((ROOT / "Morpho" / "Compiler" / "Spec.lean").exists())


if __name__ == "__main__":
  unittest.main()
