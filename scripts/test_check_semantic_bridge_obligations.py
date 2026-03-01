#!/usr/bin/env python3
"""Unit tests for semantic bridge obligation checker."""

from __future__ import annotations

import json
import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_semantic_bridge_obligations import (  # noqa: E402
    ObligationError,
    build_report,
    extract_macro_functions,
    extract_sem_eq_definitions,
    validate_config,
)


SAMPLE_BRIDGE = """\
namespace Morpho.Proofs.SolidityBridge

def supplySemEq (soliditySupply : SupplySem) : Prop :=
  sorry

def withdrawSemEq (solidityWithdraw : WithdrawSem) : Prop :=
  sorry
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

  function supply (marketParams : Tuple, assets : Uint256) : Unit := do
    let sender <- msgSender
    let marketParams' := marketParams
    let _ignoredMarket := marketParams'
    let _ignoredAssets := assets
    require (sender == sender) "supply noop"

  function position (id : Bytes32, user : Address) : Tuple [Uint256, Uint256, Uint256] := do
    let _ignoredId := id
    let _ignoredUser := user
    returnValues [0, 0, 0]
"""


def make_config(obligations: list[dict]) -> dict:
    return {
        "obligations": obligations,
        "notes": "test",
        "source": "test",
    }


def make_obligation(
    hyp: str, status: str = "assumed", macro_migrated: bool | None = None
) -> dict:
    obl: dict = {
        "id": f"OBL-{hyp.upper()}",
        "hypothesis": hyp,
        "operation": hyp.replace("SemEq", ""),
        "status": status,
        "blockedBy": "test",
    }
    if macro_migrated is not None:
        obl["macroMigrated"] = macro_migrated
    return obl


class ExtractSemEqTests(unittest.TestCase):
    def test_extracts_definitions(self) -> None:
        names = extract_sem_eq_definitions(SAMPLE_BRIDGE)
        self.assertEqual(names, ["supplySemEq", "withdrawSemEq"])

    def test_empty_file(self) -> None:
        names = extract_sem_eq_definitions("")
        self.assertEqual(names, [])

    def test_ignores_non_def_lines(self) -> None:
        text = "-- def fakeSemEq\ntheorem supplySemEq_preserves : True := trivial\n"
        names = extract_sem_eq_definitions(text)
        self.assertEqual(names, [])


class ExtractMacroFunctionTests(unittest.TestCase):
    def test_extracts_functions(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertIn("setOwner", fns)
        self.assertIn("supply", fns)

    def test_detects_real_implementation(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertTrue(fns["setOwner"])

    def test_detects_stub(self) -> None:
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertFalse(fns["supply"])

    def test_detects_hardcoded_return_stub(self) -> None:
        """returnValues [0, 0, 0] should be detected as a stub."""
        fns = extract_macro_functions(SAMPLE_MACRO)
        self.assertIn("position", fns)
        self.assertFalse(fns["position"])

    def test_empty_file(self) -> None:
        fns = extract_macro_functions("")
        self.assertEqual(fns, {})


class ValidateConfigTests(unittest.TestCase):
    def test_valid_config_passes(self) -> None:
        bridge_hyps = ["supplySemEq", "withdrawSemEq"]
        config = make_config([
            make_obligation("supplySemEq"),
            make_obligation("withdrawSemEq"),
        ])
        validate_config(config, bridge_hyps)

    def test_missing_obligation_fails(self) -> None:
        bridge_hyps = ["supplySemEq", "withdrawSemEq"]
        config = make_config([make_obligation("supplySemEq")])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("withdrawSemEq", str(ctx.exception))

    def test_extra_obligation_fails(self) -> None:
        bridge_hyps = ["supplySemEq"]
        config = make_config([
            make_obligation("supplySemEq"),
            make_obligation("withdrawSemEq"),
        ])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("withdrawSemEq", str(ctx.exception))

    def test_invalid_status_fails(self) -> None:
        bridge_hyps = ["supplySemEq"]
        config = make_config([make_obligation("supplySemEq", status="invalid")])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("invalid status", str(ctx.exception))

    def test_duplicate_id_fails(self) -> None:
        bridge_hyps = ["supplySemEq", "withdrawSemEq"]
        obl1 = make_obligation("supplySemEq")
        obl2 = make_obligation("withdrawSemEq")
        obl2["id"] = obl1["id"]  # duplicate
        config = make_config([obl1, obl2])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("duplicate", str(ctx.exception))

    def test_missing_field_fails(self) -> None:
        bridge_hyps = ["supplySemEq"]
        config = make_config([{"id": "OBL-X", "hypothesis": "supplySemEq"}])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("missing required field", str(ctx.exception))

    def test_missing_obligations_key_fails(self) -> None:
        bridge_hyps = ["supplySemEq"]
        config = {"notes": "test"}
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps)
        self.assertIn("missing 'obligations' array", str(ctx.exception))


class MacroMigrationValidationTests(unittest.TestCase):
    def test_correct_macro_migrated_passes(self) -> None:
        """macroMigrated=true for real impl, false for stub: passes."""
        bridge_hyps = ["supplySemEq", "withdrawSemEq"]
        macro_fns = {"supply": False, "withdraw": True}
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=False),
            make_obligation("withdrawSemEq", macro_migrated=True),
        ])
        validate_config(config, bridge_hyps, macro_fns)

    def test_false_positive_macro_migrated_fails(self) -> None:
        """macroMigrated=true but function is stub: fails."""
        bridge_hyps = ["supplySemEq"]
        macro_fns = {"supply": False}
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=True),
        ])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps, macro_fns)
        self.assertIn("stub", str(ctx.exception))

    def test_false_negative_macro_migrated_fails(self) -> None:
        """macroMigrated=false but function has full impl: fails."""
        bridge_hyps = ["supplySemEq"]
        macro_fns = {"supply": True}
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=False),
        ])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps, macro_fns)
        self.assertIn("full implementation", str(ctx.exception))

    def test_missing_function_with_migrated_true_fails(self) -> None:
        """macroMigrated=true but function not in MacroSlice: fails."""
        bridge_hyps = ["supplySemEq"]
        macro_fns = {}  # supply not found
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=True),
        ])
        with self.assertRaises(ObligationError) as ctx:
            validate_config(config, bridge_hyps, macro_fns)
        self.assertIn("not found", str(ctx.exception))

    def test_missing_function_with_migrated_false_passes(self) -> None:
        """macroMigrated=false and function not in MacroSlice: passes."""
        bridge_hyps = ["supplySemEq"]
        macro_fns = {}
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=False),
        ])
        validate_config(config, bridge_hyps, macro_fns)

    def test_no_macro_functions_skips_validation(self) -> None:
        """When macro_functions=None, skip macro validation."""
        bridge_hyps = ["supplySemEq"]
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=True),
        ])
        # Should not raise even though there's no macro data
        validate_config(config, bridge_hyps, None)


class BuildReportTests(unittest.TestCase):
    def test_report_counts_by_status(self) -> None:
        config = make_config([
            make_obligation("supplySemEq", status="assumed"),
            make_obligation("withdrawSemEq", status="discharged"),
        ])
        report = build_report(config)
        self.assertEqual(report["total"], 2)
        self.assertEqual(report["byStatus"]["assumed"], 1)
        self.assertEqual(report["byStatus"]["discharged"], 1)

    def test_report_macro_migration_counts(self) -> None:
        config = make_config([
            make_obligation("supplySemEq", macro_migrated=True),
            make_obligation("withdrawSemEq", macro_migrated=False),
            make_obligation("borrowSemEq"),  # no macroMigrated field
        ])
        report = build_report(config)
        self.assertEqual(report["macroMigrated"], 1)
        self.assertEqual(report["macroPending"], 2)

    def test_report_json_serializable(self) -> None:
        config = make_config([make_obligation("supplySemEq")])
        report = build_report(config)
        # Should not raise
        json.dumps(report)


class IntegrationTests(unittest.TestCase):
    def test_real_config_and_bridge(self) -> None:
        """Validate that the actual repo config matches actual source files."""
        root = pathlib.Path(__file__).resolve().parent.parent
        bridge_path = root / "Morpho" / "Proofs" / "SolidityBridge.lean"
        config_path = root / "config" / "semantic-bridge-obligations.json"
        macro_path = root / "Morpho" / "Compiler" / "MacroSlice.lean"

        if not bridge_path.exists() or not config_path.exists():
            self.skipTest("repo files not available")

        bridge_text = bridge_path.read_text(encoding="utf-8")
        bridge_hyps = extract_sem_eq_definitions(bridge_text)

        macro_fns = None
        if macro_path.exists():
            macro_text = macro_path.read_text(encoding="utf-8")
            macro_fns = extract_macro_functions(macro_text)

        with config_path.open("r", encoding="utf-8") as f:
            config = json.load(f)

        # Should not raise
        validate_config(config, bridge_hyps, macro_fns)

        # All should be assumed currently
        for obl in config["obligations"]:
            self.assertEqual(obl["status"], "assumed")

        # 6 should be macro-migrated
        migrated = [o for o in config["obligations"] if o.get("macroMigrated")]
        self.assertEqual(len(migrated), 6)
        migrated_ops = sorted(o["operation"] for o in migrated)
        self.assertEqual(
            migrated_ops,
            ["createMarket", "enableIrm", "enableLltv", "setAuthorization", "setFeeRecipient", "setOwner"],
        )


if __name__ == "__main__":
    unittest.main()
