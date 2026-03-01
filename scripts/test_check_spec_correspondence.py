#!/usr/bin/env python3
"""Unit tests for spec correspondence checker."""

from __future__ import annotations

import json
import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_spec_correspondence import (  # noqa: E402
    CorrespondenceError,
    build_report,
    extract_macro_functions,
    extract_macro_slots,
    extract_spec_fields,
    extract_spec_functions,
    validate_correspondence,
)


SAMPLE_SPEC = """\
def morphoSpec : CompilationModel := {
  name := "Morpho"
  fields := [
    { name := "owner", ty := .address, slot := some 0, packed := none, members := [] },
    { name := "feeRecipient", ty := .address, slot := some 1, packed := none, members := [] }
  ]
  functions := [
    {
      name := "setOwner"
      params := [{ name := "newOwner", ty := .address }]
      returnType := none
      body := [
        Stmt.require (Expr.eq Expr.caller (Expr.storage "owner")) "not owner",
        Stmt.require (Expr.logicalNot (Expr.eq (Expr.param "newOwner") (Expr.storage "owner"))) "already set",
        Stmt.setStorage "owner" (Expr.param "newOwner"),
        Stmt.emit "SetOwner" [Expr.param "newOwner"],
        Stmt.stop
      ]
    },
    {
      name := "enableIrm"
      params := [{ name := "irm", ty := .address }]
      returnType := none
      body := [
        Stmt.require (Expr.eq Expr.caller (Expr.storage "owner")) "not owner",
        Stmt.require (Expr.eq (Expr.mapping "isIrmEnabled" (Expr.param "irm")) (Expr.literal 0)) "already set",
        Stmt.setMapping "isIrmEnabled" (Expr.param "irm") (Expr.literal 1),
        Stmt.emit "EnableIrm" [Expr.param "irm"],
        Stmt.stop
      ]
    }
  ]
}
"""

SAMPLE_MACRO = """\
verity_contract MorphoViewSlice where
  storage
    ownerSlot : Address := slot 0
    feeRecipientSlot : Address := slot 1

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

  function position (id : Bytes32, user : Address) : Tuple [Uint256, Uint256, Uint256] := do
    let _ignoredId := id
    let _ignoredUser := user
    returnValues [0, 0, 0]
"""


class ExtractSpecFunctionsTests(unittest.TestCase):
    def test_extracts_functions(self) -> None:
        fns = extract_spec_functions(SAMPLE_SPEC)
        self.assertIn("setOwner", fns)
        self.assertIn("enableIrm", fns)

    def test_param_count(self) -> None:
        fns = extract_spec_functions(SAMPLE_SPEC)
        self.assertEqual(fns["setOwner"]["param_count"], 1)
        self.assertEqual(fns["enableIrm"]["param_count"], 1)

    def test_require_count(self) -> None:
        fns = extract_spec_functions(SAMPLE_SPEC)
        # setOwner has 2 Stmt.require (owner check + already set)
        self.assertEqual(fns["setOwner"]["require_count"], 2)

    def test_mutation_count(self) -> None:
        fns = extract_spec_functions(SAMPLE_SPEC)
        self.assertEqual(fns["setOwner"]["mutation_count"], 1)
        self.assertEqual(fns["enableIrm"]["mutation_count"], 1)

    def test_empty_text(self) -> None:
        fns = extract_spec_functions("")
        self.assertEqual(fns, {})


class ExtractSpecFieldsTests(unittest.TestCase):
    def test_extracts_fields(self) -> None:
        fields = extract_spec_fields(SAMPLE_SPEC)
        self.assertEqual(fields["owner"], 0)
        self.assertEqual(fields["feeRecipient"], 1)

    def test_empty_text(self) -> None:
        fields = extract_spec_fields("")
        self.assertEqual(fields, {})


class ExtractMacroFunctionsTests(unittest.TestCase):
    def test_extracts_functions(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertIn("setOwner", fns)
        self.assertIn("enableIrm", fns)
        self.assertIn("supply", fns)

    def test_param_count(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertEqual(fns["setOwner"]["param_count"], 1)
        self.assertEqual(fns["enableIrm"]["param_count"], 1)

    def test_detects_stub(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertFalse(fns["setOwner"]["is_stub"])
        self.assertTrue(fns["supply"]["is_stub"])

    def test_detects_hardcoded_return_stub(self) -> None:
        """returnValues [0, 0, 0] should be detected as a stub."""
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertIn("position", fns)
        self.assertTrue(fns["position"]["is_stub"])

    def test_mutation_count(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertEqual(fns["setOwner"]["mutation_count"], 1)
        self.assertEqual(fns["enableIrm"]["mutation_count"], 1)


class ExtractMacroSlotsTests(unittest.TestCase):
    def test_extracts_slots(self) -> None:
        slots = extract_macro_slots(SAMPLE_MACRO)
        self.assertEqual(slots["owner"], 0)
        self.assertEqual(slots["feeRecipient"], 1)


class ValidateCorrespondenceTests(unittest.TestCase):
    def test_matching_passes(self) -> None:
        spec_fns = extract_spec_functions(SAMPLE_SPEC)
        macro_fns = extract_macro_functions(SAMPLE_MACRO)
        spec_fields = extract_spec_fields(SAMPLE_SPEC)
        macro_slots = extract_macro_slots(SAMPLE_MACRO)
        errors = validate_correspondence(
            spec_fns, macro_fns, spec_fields, macro_slots, {"setOwner", "enableIrm"}
        )
        self.assertEqual(errors, [])

    def test_slot_mismatch_fails(self) -> None:
        spec_fields = {"owner": 0}
        macro_slots = {"owner": 5}
        errors = validate_correspondence({}, {}, spec_fields, macro_slots, set())
        self.assertEqual(len(errors), 1)
        self.assertIn("slot mismatch", errors[0])

    def test_migrated_stub_fails(self) -> None:
        spec_fns = {"supply": {"param_count": 2, "require_count": 0, "mutation_count": 0}}
        macro_fns = {"supply": {"param_count": 2, "is_stub": True, "require_count": 1, "mutation_count": 0}}
        errors = validate_correspondence(spec_fns, macro_fns, {}, {}, {"supply"})
        self.assertTrue(any("stub" in e for e in errors))

    def test_param_count_mismatch_fails(self) -> None:
        spec_fns = {"f": {"param_count": 2, "require_count": 0, "mutation_count": 0}}
        macro_fns = {"f": {"param_count": 1, "is_stub": False, "require_count": 0, "mutation_count": 0}}
        errors = validate_correspondence(spec_fns, macro_fns, {}, {}, {"f"})
        self.assertTrue(any("param count" in e for e in errors))

    def test_missing_op_in_macro_fails(self) -> None:
        spec_fns = {"f": {"param_count": 1, "require_count": 0, "mutation_count": 0}}
        errors = validate_correspondence(spec_fns, {}, {}, {}, {"f"})
        self.assertTrue(any("not found in MacroSlice" in e for e in errors))

    def test_missing_op_in_spec_fails(self) -> None:
        macro_fns = {"f": {"param_count": 1, "is_stub": False, "require_count": 0, "mutation_count": 0}}
        errors = validate_correspondence({}, macro_fns, {}, {}, {"f"})
        self.assertTrue(any("not found in Spec" in e for e in errors))

    def test_branching_mutations_allowed(self) -> None:
        """MacroSlice may have 2x mutations due to if/else branching."""
        spec_fns = {"f": {"param_count": 1, "require_count": 0, "mutation_count": 1}}
        macro_fns = {"f": {"param_count": 1, "is_stub": False, "require_count": 0, "mutation_count": 2}}
        errors = validate_correspondence(spec_fns, macro_fns, {}, {}, {"f"})
        self.assertEqual(errors, [])

    def test_excess_mutations_beyond_branching_fails(self) -> None:
        """More than 2x mutations should fail."""
        spec_fns = {"f": {"param_count": 1, "require_count": 0, "mutation_count": 1}}
        macro_fns = {"f": {"param_count": 1, "is_stub": False, "require_count": 0, "mutation_count": 3}}
        errors = validate_correspondence(spec_fns, macro_fns, {}, {}, {"f"})
        self.assertTrue(any("extra mutations" in e for e in errors))


class BuildReportTests(unittest.TestCase):
    def test_report_structure(self) -> None:
        spec_fns = extract_spec_functions(SAMPLE_SPEC)
        macro_fns = extract_macro_functions(SAMPLE_MACRO)
        report = build_report(spec_fns, macro_fns, {"setOwner"}, [])
        self.assertEqual(report["migratedChecked"], 1)
        self.assertEqual(len(report["correspondences"]), 1)
        self.assertEqual(report["errors"], [])

    def test_report_json_serializable(self) -> None:
        report = build_report({}, {}, set(), [])
        json.dumps(report)


class IntegrationTests(unittest.TestCase):
    def test_real_files(self) -> None:
        """Validate that actual repo files pass correspondence check."""
        root = pathlib.Path(__file__).resolve().parent.parent
        spec_path = root / "Morpho" / "Compiler" / "Spec.lean"
        macro_path = root / "Morpho" / "Compiler" / "MacroSlice.lean"
        config_path = root / "config" / "semantic-bridge-obligations.json"

        if not all(p.exists() for p in [spec_path, macro_path, config_path]):
            self.skipTest("repo files not available")

        spec_text = spec_path.read_text(encoding="utf-8")
        macro_text = macro_path.read_text(encoding="utf-8")

        spec_fns = extract_spec_functions(spec_text)
        macro_fns = extract_macro_functions(macro_text)
        spec_fields = extract_spec_fields(spec_text)
        macro_slots = extract_macro_slots(macro_text)

        with config_path.open("r", encoding="utf-8") as f:
            config = json.load(f)
        migrated = {
            o["operation"] for o in config["obligations"] if o.get("macroMigrated")
        }

        errors = validate_correspondence(
            spec_fns, macro_fns, spec_fields, macro_slots, migrated
        )
        self.assertEqual(errors, [], f"Correspondence errors: {errors}")


if __name__ == "__main__":
    unittest.main()
