#!/usr/bin/env python3
"""Unit tests for macro migration blocker checker."""

from __future__ import annotations

import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parent.parent

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_blockers import (  # noqa: E402
  MigrationGateError,
  build_report,
  build_operation_blocker_report,
  load_baseline,
  load_obligations,
  parse_constructor_usage,
  validate_operation_blockers,
  validate_against_baseline,
)
from macro_blocker_regression_cases import (  # noqa: E402
  COLLATERAL_FRONTEND_REGRESSION_CASES,
  CORE_FLOW_FRONTEND_REGRESSION_CASES,
  ISSUE_FRONTEND_REGRESSION_CASES,
)


class ParseUsageTests(unittest.TestCase):
  def test_extracts_stmt_and_expr_counts(self) -> None:
    text = """
Stmt.letVar "x" (Expr.literal 1)
Stmt.setStorage "count" (Expr.add (Expr.localVar "x") (Expr.param "n"))
Stmt.setStorage "count" (Expr.add (Expr.localVar "x") (Expr.param "n"))
Stmt.internalCall "foo" []
"""
    usage = parse_constructor_usage(text)

    self.assertEqual(usage.stmt_counts["letVar"], 1)
    self.assertEqual(usage.stmt_counts["setStorage"], 2)
    self.assertEqual(usage.stmt_counts["internalCall"], 1)
    self.assertEqual(usage.expr_counts["literal"], 1)
    self.assertEqual(usage.expr_counts["add"], 2)


class BaselineValidationTests(unittest.TestCase):
  def test_build_report_uses_provided_input_paths(self) -> None:
    text = "Stmt.letVar \"x\" (Expr.literal 1)\n"
    with tempfile.TemporaryDirectory() as tmp:
      spec_path = pathlib.Path(tmp) / "external" / "Spec.lean"
      baseline_path = pathlib.Path(tmp) / "external" / "macro-migration-blockers.json"
      obligations_path = pathlib.Path(tmp) / "external" / "semantic-bridge-obligations.json"
      report = build_report(
        parse_constructor_usage(text),
        source_path=spec_path,
        baseline_path=baseline_path,
        obligations_path=obligations_path,
      )

      self.assertEqual(report["sourcePath"], str(spec_path))
      self.assertEqual(report["baselinePath"], str(baseline_path))
      self.assertEqual(report["obligationsPath"], str(obligations_path))

  def test_validation_passes_when_sets_match(self) -> None:
    text = "Stmt.letVar \"x\" (Expr.literal 1)\nStmt.internalCall \"foo\" []\n"
    report = build_report(parse_constructor_usage(text))

    baseline = {
      "expectedUnsupported": {
        "stmt": [],
        "expr": [],
      }
    }
    validate_against_baseline(report, baseline)

  def test_validation_fails_on_drift(self) -> None:
    text = "Stmt.letVar \"x\" (Expr.literal 1)\nStmt.internalCall \"foo\" []\n"
    report = build_report(parse_constructor_usage(text))

    baseline = {
      "expectedUnsupported": {
        "stmt": ["internalCall"],
        "expr": [],
      }
    }
    with self.assertRaises(MigrationGateError):
      validate_against_baseline(report, baseline)

  def test_write_style_baseline_data_shape(self) -> None:
    text = "Stmt.letVar \"x\" (Expr.literal 1)\nExpr.externalCall \"f\" []\n"
    report = build_report(parse_constructor_usage(text))

    with tempfile.TemporaryDirectory() as tmp:
      baseline_path = pathlib.Path(tmp) / "baseline.json"
      baseline = {
        "sourcePath": report["sourcePath"],
        "expectedUnsupported": {
          "stmt": report["unsupported"]["stmt"],
          "expr": report["unsupported"]["expr"],
        },
      }
      baseline_path.write_text(str(baseline), encoding="utf-8")
      self.assertTrue(baseline_path.exists())


class OperationBlockerTests(unittest.TestCase):
  def test_load_baseline_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "macro-migration-blockers.json"
      path.write_bytes(b"\xff\xfe")

      with self.assertRaisesRegex(MigrationGateError, "failed to decode .* as UTF-8"):
        load_baseline(path)

  def test_load_baseline_rejects_invalid_json(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "macro-migration-blockers.json"
      path.write_text("{\n", encoding="utf-8")

      with self.assertRaisesRegex(MigrationGateError, "invalid JSON"):
        load_baseline(path)

  def test_load_obligations_rejects_non_object_root(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "semantic-bridge-obligations.json"
      path.write_text("[]\n", encoding="utf-8")

      with self.assertRaisesRegex(MigrationGateError, "expected top-level object"):
        load_obligations(path)

  def test_load_obligations_rejects_duplicate_operations(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "semantic-bridge-obligations.json"
      path.write_text(
        """\
{
  "obligations": [
    {"operation": "supply"},
    {"operation": "supply"}
  ]
}
""",
        encoding="utf-8",
      )

      with self.assertRaisesRegex(MigrationGateError, "duplicate obligation operation `supply`"):
        load_obligations(path)

  def test_build_operation_blocker_report_returns_empty_when_no_ops_tracked(self) -> None:
    """All previously tracked ops are now macro-migrated, so the report is empty."""
    spec_text = """
def morphoSpec : CompilationModel := {
  functions := [
    { name := "supply", body := supplyBody }
  ]
}
private def supplyBody : List Stmt := [
  Stmt.mstore (Expr.literal 0) Expr.caller
]
"""
    report = build_operation_blocker_report(spec_text)
    self.assertEqual(report, {})

  def test_validate_operation_blockers_accepts_matching_config(self) -> None:
    report = {
      "borrow": ["erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
      "liquidate": ["callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
      "repay": ["callbacks", "erc20", "internalCall", "memoryOps", "structMember2"],
      "supply": ["callbacks", "erc20", "internalCall", "memoryOps", "structMember2"],
      "supplyCollateral": ["callbacks", "erc20", "memoryOps", "structMember2"],
      "withdraw": ["erc20", "internalCall", "memoryOps", "structMember2"],
      "withdrawCollateral": ["erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
    }
    obligations = {
      op: {"operation": op, "macroMigrated": False, "macroSurfaceBlockers": blockers}
      for op, blockers in report.items()
    }
    validate_operation_blockers(report, obligations)

  def test_validate_operation_blockers_accepts_empty_tracked_set(self) -> None:
    """With no tracked ops, validation passes trivially."""
    report = {"supply": ["erc20"]}
    obligations = {"supply": {"operation": "supply", "macroMigrated": False, "macroSurfaceBlockers": ["erc20"]}}
    # Should not raise since TRACKED_OPERATION_BLOCKER_OPS is empty
    validate_operation_blockers(report, obligations)


class CreateMarketFrontendRegressionTests(unittest.TestCase):
  def compile_contract(self, source: str) -> subprocess.CompletedProcess[str]:
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "MacroFrontendRegression.lean"
      path.write_text(source, encoding="utf-8")
      return subprocess.run(
        ["lake", "env", "lean", str(path)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

  def test_create_market_tuple_and_struct_surface_now_compile(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

    source = """\
import Verity.Core
import Verity.Macro

open Verity

def externalCall (_name : String) (_args : List Uint256) : Uint256 := 0
def setStructMember (_slot : StorageSlot (Uint256 → Uint256)) (_key : Uint256) (_member : String)
    (_value : Uint256) :
    Contract Unit := Verity.pure ()
def blockTimestamp : Uint256 := 0

verity_contract Tmp where
  storage
    marketSlot : MappingStruct(Uint256,[
      lastUpdate @word 0
    ]) := slot 0

  function f (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let loanToken := marketParams_0
    let id := externalCall "keccakMarketParams" [loanToken]
    setStructMember marketSlot id "lastUpdate" blockTimestamp
"""
    proc = self.compile_contract(source)
    self.assertEqual(proc.returncode, 0, proc.stdout + proc.stderr)

  def test_core_flow_frontend_blockers_still_fail_at_current_pin(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

    for case in CORE_FLOW_FRONTEND_REGRESSION_CASES:
      with self.subTest(name=case["name"]):
        proc = self.compile_contract(case["source"])
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(case["expected"], proc.stdout + proc.stderr)

  def test_collateral_liquidation_frontend_blockers_still_fail_at_current_pin(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

    for case in COLLATERAL_FRONTEND_REGRESSION_CASES:
      with self.subTest(name=case["name"]):
        proc = self.compile_contract(case["source"])
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(case["expected"], proc.stdout + proc.stderr)


class MainFailureTests(unittest.TestCase):
  def test_cli_reports_invalid_utf8_spec_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      spec_path = tmp_path / "Spec.lean"
      spec_path.write_bytes(b"\xff\xfe")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--spec",
          str(spec_path),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("macro-migration blockers check failed: failed to decode", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_invalid_baseline_json_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      baseline_path = tmp_path / "macro-migration-blockers.json"
      baseline_path.write_text("{\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--baseline",
          str(baseline_path),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("macro-migration blockers check failed: invalid JSON", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_non_object_obligations_root_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      obligations_path = tmp_path / "semantic-bridge-obligations.json"
      obligations_path.write_text("[]\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--obligations",
          str(obligations_path),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertNotEqual(proc.returncode, 0)
      self.assertIn(
        "macro-migration blockers check failed: expected top-level object",
        proc.stderr,
      )
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_json_out_write_failure_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      json_out = tmp_path / "not-a-dir"
      json_out.write_text("occupied", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--json-out",
          str(json_out / "report.json"),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("macro-migration blockers check failed: failed to write", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)

  def test_cli_json_report_uses_explicit_input_paths(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      spec_path = tmp_path / "Spec.lean"
      baseline_path = tmp_path / "macro-migration-blockers.json"
      obligations_path = tmp_path / "semantic-bridge-obligations.json"
      json_out = tmp_path / "report.json"

      spec_path.write_text((ROOT / "Morpho" / "Compiler" / "Spec.lean").read_text(encoding="utf-8"), encoding="utf-8")
      baseline_path.write_text(
        (ROOT / "config" / "macro-migration-blockers.json").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      obligations_path.write_text(
        (ROOT / "config" / "semantic-bridge-obligations.json").read_text(encoding="utf-8"),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--spec",
          str(spec_path),
          "--baseline",
          str(baseline_path),
          "--obligations",
          str(obligations_path),
          "--json-out",
          str(json_out),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 0, msg=proc.stderr)
      report = json.loads(json_out.read_text(encoding="utf-8"))
      self.assertEqual(report["sourcePath"], str(spec_path))
      self.assertEqual(report["baselinePath"], str(baseline_path))
      self.assertEqual(report["obligationsPath"], str(obligations_path))

  def test_cli_json_report_normalizes_relative_external_input_paths(self) -> None:
    with tempfile.TemporaryDirectory() as tmp:
      tmp_path = pathlib.Path(tmp)
      root = tmp_path / "runner"
      external = tmp_path / "external"
      root.mkdir()
      external.mkdir()

      spec_path = external / "Spec.lean"
      baseline_path = external / "macro-migration-blockers.json"
      obligations_path = external / "semantic-bridge-obligations.json"
      json_out = external / "report.json"

      spec_path.write_text(
        (ROOT / "Morpho" / "Compiler" / "Spec.lean").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      baseline_path.write_text(
        (ROOT / "config" / "macro-migration-blockers.json").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      obligations_path.write_text(
        (ROOT / "config" / "semantic-bridge-obligations.json").read_text(encoding="utf-8"),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(ROOT / "scripts" / "check_macro_migration_blockers.py"),
          "--spec",
          str(pathlib.Path("..") / "external" / "Spec.lean"),
          "--baseline",
          str(pathlib.Path("..") / "external" / "macro-migration-blockers.json"),
          "--obligations",
          str(pathlib.Path("..") / "external" / "semantic-bridge-obligations.json"),
          "--json-out",
          str(json_out),
        ],
        cwd=root,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(proc.returncode, 0, msg=proc.stderr)
      report = json.loads(json_out.read_text(encoding="utf-8"))
      self.assertEqual(report["sourcePath"], str(spec_path.resolve()))
      self.assertEqual(report["baselinePath"], str(baseline_path.resolve()))
      self.assertEqual(report["obligationsPath"], str(obligations_path.resolve()))


if __name__ == "__main__":
  unittest.main()
