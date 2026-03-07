#!/usr/bin/env python3
"""Unit tests for primitive coverage analysis."""

from __future__ import annotations

import json
import pathlib
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_primitive_coverage import (  # noqa: E402
    analyze_coverage,
    build_report,
    display_path,
    extract_primitives,
    is_stub,
    load_migrated_operations,
    PRIMITIVE_BRIDGE_STATUS,
    PrimitiveCoverageError,
    write_json_report,
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

SAMPLE_FLASH_LOAN_FN = """\
  function flashLoan (token : Address, assets : Uint256, data : Bytes) : Unit := do
    require (assets > 0) "zero assets"
    let sender <- msgSender
    let _ignoredData := data
    mstore 0 assets
    rawLog [1, sender, token] 0 32
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

    def test_extracts_flash_loan_gaps(self) -> None:
        prims = extract_primitives(SAMPLE_FLASH_LOAN_FN)
        self.assertIn("msgSender", prims)
        self.assertIn("require_gt", prims)
        self.assertIn("mstore", prims)
        self.assertIn("rawLog", prims)


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

    def test_edsl_ready_operation(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"enableIrm"})
        self.assertFalse(coverage["enableIrm"]["fully_covered"])
        self.assertTrue(coverage["enableIrm"]["edsl_ready"])
        self.assertIn("getMapping", coverage["enableIrm"]["edsl_proven"])
        self.assertIn("setMapping", coverage["enableIrm"]["edsl_proven"])
        self.assertEqual(coverage["enableIrm"]["missing"], [])

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
        self.assertTrue(coverage["enableIrm"]["edsl_ready"])


class BuildReportTests(unittest.TestCase):
    def test_report_counts(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner", "enableIrm"})
        report = build_report(
            coverage,
            pathlib.Path("Morpho/Compiler/MacroSlice.lean"),
            pathlib.Path("config/semantic-bridge-obligations.json"),
        )
        self.assertEqual(report["total"], 2)
        self.assertEqual(report["fully_covered"], 1)
        self.assertEqual(report["edsl_ready"], 1)
        self.assertEqual(report["gaps_remaining"], 0)

    def test_report_json_serializable(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        report = build_report(
            coverage,
            pathlib.Path("Morpho/Compiler/MacroSlice.lean"),
            pathlib.Path("config/semantic-bridge-obligations.json"),
        )
        json.dumps(report)

    def test_all_ready(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        report = build_report(
            coverage,
            pathlib.Path("Morpho/Compiler/MacroSlice.lean"),
            pathlib.Path("config/semantic-bridge-obligations.json"),
        )
        self.assertEqual(report["fully_covered"], 1)
        self.assertEqual(report["edsl_ready"], 0)
        self.assertEqual(report["gaps_remaining"], 0)

    def test_report_records_actual_input_paths(self) -> None:
        coverage = analyze_coverage(SAMPLE_MACRO, {"setOwner"})
        report = build_report(
            coverage,
            pathlib.Path("/tmp/MacroSlice.lean"),
            pathlib.Path("/tmp/semantic-bridge-obligations.json"),
        )
        self.assertEqual(report["macroSlice"], "/tmp/MacroSlice.lean")
        self.assertEqual(report["config"], "/tmp/semantic-bridge-obligations.json")


class DisplayPathTests(unittest.TestCase):
    def test_display_path_keeps_external_absolute_paths(self) -> None:
        path = pathlib.Path("/tmp/config.json")
        self.assertEqual(display_path(path), "/tmp/config.json")


class PrimitiveBridgeStatusTests(unittest.TestCase):
    def test_core_proven_primitives(self) -> None:
        """Core operations that should have proven status."""
        for prim in ["msgSender", "getStorageAddr", "setStorageAddr",
                      "require_eq", "require_neq", "if_then_else"]:
            self.assertEqual(
                PRIMITIVE_BRIDGE_STATUS[prim], "proven",
                f"{prim} should be proven"
            )

    def test_mapping_edsl_proven(self) -> None:
        """Mapping operations should be edsl_proven (EDSL-level lemmas exist)."""
        for prim in ["getMapping", "setMapping", "getMapping2", "setMapping2",
                      "getMappingUint", "setMappingUint"]:
            self.assertEqual(
                PRIMITIVE_BRIDGE_STATUS[prim], "edsl_proven",
                f"{prim} should be edsl_proven"
            )

    def test_event_memory_primitives_marked_missing(self) -> None:
        for prim in ["mstore", "rawLog"]:
            self.assertEqual(
                PRIMITIVE_BRIDGE_STATUS[prim], "missing",
                f"{prim} should be missing"
            )


class LoadMigratedOperationsTests(unittest.TestCase):
    def write_config(self, payload: str) -> pathlib.Path:
        temp_dir = tempfile.TemporaryDirectory()
        self.addCleanup(temp_dir.cleanup)
        path = pathlib.Path(temp_dir.name) / "config.json"
        path.write_text(payload, encoding="utf-8")
        return path

    def test_loads_migrated_operations(self) -> None:
        path = self.write_config(json.dumps({
            "obligations": [
                {"operation": "setOwner", "macroMigrated": True},
                {"operation": "flashLoan", "macroMigrated": False},
            ]
        }))
        self.assertEqual(load_migrated_operations(path), {"setOwner"})

    def test_rejects_malformed_json(self) -> None:
        path = self.write_config("{")
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "failed to parse semantic bridge obligations JSON",
        ):
            load_migrated_operations(path)

    def test_rejects_non_object_root(self) -> None:
        path = self.write_config("[]")
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "config must be a JSON object",
        ):
            load_migrated_operations(path)

    def test_rejects_non_list_obligations(self) -> None:
        path = self.write_config(json.dumps({"obligations": {}}))
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "must contain an `obligations` list",
        ):
            load_migrated_operations(path)

    def test_rejects_non_object_obligation(self) -> None:
        path = self.write_config(json.dumps({"obligations": ["setOwner"]}))
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "obligation #0 must be a JSON object",
        ):
            load_migrated_operations(path)

    def test_rejects_invalid_operation(self) -> None:
        path = self.write_config(json.dumps({
            "obligations": [{"operation": "", "macroMigrated": True}]
        }))
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "invalid `operation`",
        ):
            load_migrated_operations(path)

    def test_rejects_non_boolean_macro_migrated(self) -> None:
        path = self.write_config(json.dumps({
            "obligations": [{"operation": "setOwner", "macroMigrated": "yes"}]
        }))
        with self.assertRaisesRegex(
            PrimitiveCoverageError,
            "invalid `macroMigrated`",
        ):
            load_migrated_operations(path)


class WriteJsonReportTests(unittest.TestCase):
    def test_wraps_write_failures(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            target = root / "report-dir"
            target.mkdir()

            with self.assertRaisesRegex(
                PrimitiveCoverageError,
                "failed to write primitive coverage JSON report",
            ):
                write_json_report(target, {"total": 0})


class IntegrationTests(unittest.TestCase):
    def test_real_files(self) -> None:
        """Run against actual repo files."""
        root = pathlib.Path(__file__).resolve().parent.parent
        macro_path = root / "Morpho" / "Compiler" / "MacroSlice.lean"
        config_path = root / "config" / "semantic-bridge-obligations.json"

        if not macro_path.exists() or not config_path.exists():
            self.skipTest("repo files not available")

        macro_text = macro_path.read_text(encoding="utf-8")
        migrated_ops = load_migrated_operations(config_path)

        coverage = analyze_coverage(macro_text, migrated_ops)
        report = build_report(coverage, macro_path, config_path)

        # 6 migrated operations (admin cluster + flashLoan)
        self.assertEqual(report["total"], 6)

        # setOwner and setFeeRecipient should be fully covered
        self.assertTrue(coverage["setOwner"]["fully_covered"])
        self.assertTrue(coverage["setFeeRecipient"]["fully_covered"])

        # enableIrm, enableLltv, setAuthorization: EDSL-ready (bridge lemmas needed)
        for op in ["enableIrm", "enableLltv", "setAuthorization"]:
            self.assertFalse(coverage[op]["fully_covered"])
            self.assertTrue(coverage[op]["edsl_ready"])

        # createMarket is no longer in migrated set (hard stub)
        self.assertNotIn("createMarket", coverage)

        # Newly migrated flash-loan flow should be present in the coverage set
        self.assertIn("flashLoan", coverage)
        self.assertNotIn("setAuthorizationWithSig", coverage)
        self.assertIn("mstore", coverage["flashLoan"]["missing"])
        self.assertIn("rawLog", coverage["flashLoan"]["missing"])

        # At least 2 fully covered
        self.assertGreaterEqual(report["fully_covered"], 2)

    def test_cli_reports_invalid_utf8_config_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            macro_path = root / "MacroSlice.lean"
            config_path = root / "semantic-bridge-obligations.json"
            macro_path.write_text(SAMPLE_MACRO, encoding="utf-8")
            config_path.write_bytes(b"\xff")

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_primitive_coverage.py"),
                    "--macro-slice",
                    str(macro_path),
                    "--config",
                    str(config_path),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(proc.returncode, 1)
            self.assertIn("primitive coverage analysis failed:", proc.stderr)
            self.assertIn(
                "failed to decode semantic bridge obligations JSON as UTF-8",
                proc.stderr,
            )
            self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_missing_macro_file_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            macro_path = root / "missing-MacroSlice.lean"
            config_path = root / "semantic-bridge-obligations.json"
            config_path.write_text(
                json.dumps(
                    {
                        "obligations": [
                            {"operation": "setOwner", "macroMigrated": True},
                        ]
                    }
                ),
                encoding="utf-8",
            )

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_primitive_coverage.py"),
                    "--macro-slice",
                    str(macro_path),
                    "--config",
                    str(config_path),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(proc.returncode, 1)
            self.assertIn("primitive coverage analysis failed:", proc.stderr)
            self.assertIn("failed to read text file", proc.stderr)
            self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_invalid_utf8_macro_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            macro_path = root / "MacroSlice.lean"
            config_path = root / "semantic-bridge-obligations.json"
            macro_path.write_bytes(b"\xff")
            config_path.write_text(
                json.dumps(
                    {
                        "obligations": [
                            {"operation": "setOwner", "macroMigrated": True},
                        ]
                    }
                ),
                encoding="utf-8",
            )

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_primitive_coverage.py"),
                    "--macro-slice",
                    str(macro_path),
                    "--config",
                    str(config_path),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(proc.returncode, 1)
            self.assertIn("primitive coverage analysis failed:", proc.stderr)
            self.assertIn("failed to decode UTF-8 text file", proc.stderr)
            self.assertNotIn("Traceback", proc.stderr)

    def test_cli_reports_json_out_write_failure_without_traceback(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            macro_path = root / "MacroSlice.lean"
            config_path = root / "semantic-bridge-obligations.json"
            json_out = root / "report-dir"
            macro_path.write_text(SAMPLE_MACRO, encoding="utf-8")
            config_path.write_text(
                json.dumps(
                    {
                        "obligations": [
                            {"operation": "setOwner", "macroMigrated": True},
                        ]
                    }
                ),
                encoding="utf-8",
            )
            json_out.mkdir()

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_primitive_coverage.py"),
                    "--macro-slice",
                    str(macro_path),
                    "--config",
                    str(config_path),
                    "--json-out",
                    str(json_out),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(proc.returncode, 1)
            self.assertIn("primitive coverage analysis failed:", proc.stderr)
            self.assertIn("failed to write primitive coverage JSON report", proc.stderr)
            self.assertNotIn("Traceback", proc.stderr)

    def test_cli_json_report_records_actual_input_paths(self) -> None:
        with tempfile.TemporaryDirectory() as d:
            root = pathlib.Path(d)
            macro_path = root / "MacroSlice.lean"
            config_path = root / "semantic-bridge-obligations.json"
            json_out = root / "report.json"
            macro_path.write_text(SAMPLE_MACRO, encoding="utf-8")
            config_path.write_text(
                json.dumps(
                    {
                        "obligations": [
                            {"operation": "setOwner", "macroMigrated": True},
                        ]
                    }
                ),
                encoding="utf-8",
            )

            proc = subprocess.run(
                [
                    sys.executable,
                    str(pathlib.Path(__file__).resolve().parent / "check_primitive_coverage.py"),
                    "--macro-slice",
                    str(macro_path),
                    "--config",
                    str(config_path),
                    "--json-out",
                    str(json_out),
                ],
                check=False,
                capture_output=True,
                text=True,
            )

            self.assertEqual(proc.returncode, 0)
            report = json.loads(json_out.read_text(encoding="utf-8"))
            self.assertEqual(report["macroSlice"], str(macro_path))
            self.assertEqual(report["config"], str(config_path))


if __name__ == "__main__":
    unittest.main()
