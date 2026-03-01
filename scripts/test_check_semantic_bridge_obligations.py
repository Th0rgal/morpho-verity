#!/usr/bin/env python3
"""Unit tests for semantic bridge obligation checker."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_semantic_bridge_obligations import (  # noqa: E402
    ObligationError,
    build_report,
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


def make_config(obligations: list[dict]) -> dict:
    return {
        "obligations": obligations,
        "notes": "test",
        "source": "test",
    }


def make_obligation(hyp: str, status: str = "assumed") -> dict:
    return {
        "id": f"OBL-{hyp.upper()}",
        "hypothesis": hyp,
        "operation": hyp.replace("SemEq", ""),
        "status": status,
        "blockedBy": "test",
    }


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

    def test_report_json_serializable(self) -> None:
        config = make_config([make_obligation("supplySemEq")])
        report = build_report(config)
        # Should not raise
        json.dumps(report)


class IntegrationTests(unittest.TestCase):
    def test_real_config_and_bridge(self) -> None:
        """Validate that the actual repo config matches actual SolidityBridge.lean."""
        root = pathlib.Path(__file__).resolve().parent.parent
        bridge_path = root / "Morpho" / "Proofs" / "SolidityBridge.lean"
        config_path = root / "config" / "semantic-bridge-obligations.json"

        if not bridge_path.exists() or not config_path.exists():
            self.skipTest("repo files not available")

        bridge_text = bridge_path.read_text(encoding="utf-8")
        bridge_hyps = extract_sem_eq_definitions(bridge_text)

        with config_path.open("r", encoding="utf-8") as f:
            config = json.load(f)

        # Should not raise
        validate_config(config, bridge_hyps)

        # All should be assumed currently
        for obl in config["obligations"]:
            self.assertEqual(obl["status"], "assumed")


if __name__ == "__main__":
    unittest.main()
