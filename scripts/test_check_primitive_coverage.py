#!/usr/bin/env python3
"""Unit tests for primitive coverage analysis."""

from __future__ import annotations

import json
import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_primitive_coverage import (  # noqa: E402
    analyze_coverage,
    build_report,
    extract_primitives,
    is_stub,
    PRIMITIVE_BRIDGE_STATUS,
)


SAMPLE_READY_FN = """\
  function setOwner (newOwner : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    require (newOwner != currentOwner) "already set"
    setStorageAddr ownerSlot newOwner
"""

SAMPLE_GAPS_FN = """\
  function enableIrm (irm : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMapping isIrmEnabledSlot irm
    require (currentValue == 0) "already set"
    setMapping isIrmEnabledSlot irm 1
"""

SAMPLE_STUB_FN = """\
  function supply (marketParams : Tuple, assets : Uint256) : Unit := do
    let sender <- msgSender
    require (sender == sender) "supply noop"
"""

SAMPLE_MACRO = """\
verity_contract MorphoViewSlice where
  storage
    ownerSlot : Address := slot 0

  function setOwner (newOwner : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    require (newOwner != currentOwner) "already set"
    setStorageAddr ownerSlot newOwner

  function enableIrm (irm : Address) : Unit := do
    let sender <- msgSender
    let currentOwner <- getStorageAddr ownerSlot
    require (sender == currentOwner) "not owner"
    let currentValue <- getMapping isIrmEnabledSlot irm
    require (currentValue == 0) "already set"
    setMapping isIrmEnabledSlot irm 1

  function supply (marketParams : Tuple, assets : Uint256) : Unit := do
    let sender <- msgSender
    require (sender == sender) "supply noop"
"""


class ExtractPrimitivesTests(unittest.TestCase):
    def test_extracts_ready_function(self) -> None:
        prims = extract_primitives(SAMPLE_READY_FN)
        self.assertIn("msgSender", prims)
        self.assertIn("getStorageAddr", prims)
        self.assertIn("setStorageAddr", prims)
        self.assertIn("require_eq", prims)
        self.assertIn("require_neq", prims)

    def test_extracts_gap_function(self) -> None:
        prims = extract_primitives(SAMPLE_GAPS_FN)
        self.assertIn("getMapping", prims)
        self.assertIn("setMapping", prims)

    def test_no_false_positives_for_ready(self) -> None:
        """setOwner should not have mapping primitives."""
        prims = extract_primitives(SAMPLE_READY_FN)
        self.assertNotIn("getMapping", prims)
        self.assertNotIn("setMapping", prims)
        self.assertNotIn("getMapping2", prims)

    def test_empty_text(self) -> None:
        prims = extract_primitives("")
        self.assertEqual(prims, set())


class IsStubTests(unittest.TestCase):
    def test_detects_noop_stub(self) -> None:
        self.assertTrue(is_stub(SAMPLE_STUB_FN))

    def test_detects_non_stub(self) -> None:
        self.assertFalse(is_stub(SAMPLE_READY_FN))

    def test_detects_hardcoded_return(self) -> None:
        block = '    returnValues [0, 0, 0]'
        self.assertTrue(is_stub(block))


class AnalyzeCoverageTests(unittest.TestCase):
    def test_ready_operation(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        self.assertTrue(coverage["setOwner"]["fully_covered"])
        self.assertEqual(coverage["setOwner"]["missing"], [])

    def test_gap_operation(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"enableIrm"})
        self.assertFalse(coverage["enableIrm"]["fully_covered"])
        self.assertIn("getMapping", coverage["enableIrm"]["missing"])
        self.assertIn("setMapping", coverage["enableIrm"]["missing"])

    def test_stub_operation(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"supply"})
        self.assertIn("error", coverage["supply"])

    def test_missing_operation(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"nonexistent"})
        self.assertIn("error", coverage["nonexistent"])

    def test_mixed_operations(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner", "enableIrm"})
        self.assertTrue(coverage["setOwner"]["fully_covered"])
        self.assertFalse(coverage["enableIrm"]["fully_covered"])


class BuildReportTests(unittest.TestCase):
    def test_report_counts(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner", "enableIrm"})
        report = build_report(coverage)
        self.assertEqual(report["total"], 2)
        self.assertEqual(report["fully_covered"], 1)
        self.assertEqual(report["partially_covered"], 1)

    def test_report_json_serializable(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        report = build_report(coverage)
        json.dumps(report)

    def test_all_ready(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        report = build_report(coverage)
        self.assertEqual(report["fully_covered"], 1)
        self.assertEqual(report["partially_covered"], 0)


class PrimitiveBridgeStatusTests(unittest.TestCase):
    def test_core_proven_primitives(self) -> None:
        """Core operations that should have proven status."""
        for prim in ["msgSender", "getStorageAddr", "setStorageAddr",
                      "require_eq", "require_neq", "if_then_else"]:
            self.assertEqual(
                PRIMITIVE_BRIDGE_STATUS[prim], "proven",
                f"{prim} should be proven"
            )

    def test_mapping_missing(self) -> None:
        """Mapping operations should be missing."""
        for prim in ["getMapping", "setMapping", "getMapping2", "setMapping2"]:
            self.assertEqual(
                PRIMITIVE_BRIDGE_STATUS[prim], "missing",
                f"{prim} should be missing"
            )


class IntegrationTests(unittest.TestCase):
    def test_real_files(self) -> None:
        """Run against actual repo files."""
        root = pathlib.Path(__file__).resolve().parent.parent
        macro_path = root / "Morpho" / "Compiler" / "MacroSlice.lean"
        config_path = root / "config" / "semantic-bridge-obligations.json"

        if not macro_path.exists() or not config_path.exists():
            self.skipTest("repo files not available")

        macro_text = macro_path.read_text(encoding="utf-8")
        with config_path.open("r", encoding="utf-8") as f:
            config = json.load(f)
        migrated_ops = {
            o["operation"]
            for o in config["obligations"]
            if o.get("macroMigrated")
        }

        coverage = analyze_coverage(macro_text, migrated_ops)
        report = build_report(coverage)

        # 5 migrated operations
        self.assertEqual(report["total"], 5)

        # setOwner and setFeeRecipient should be fully covered
        self.assertTrue(coverage["setOwner"]["fully_covered"])
        self.assertTrue(coverage["setFeeRecipient"]["fully_covered"])

        # enableIrm, enableLltv, setAuthorization need mapping lemmas
        self.assertFalse(coverage["enableIrm"]["fully_covered"])
        self.assertFalse(coverage["enableLltv"]["fully_covered"])
        self.assertFalse(coverage["setAuthorization"]["fully_covered"])

        # At least 2 fully covered
        self.assertGreaterEqual(report["fully_covered"], 2)


if __name__ == "__main__":
    unittest.main()
