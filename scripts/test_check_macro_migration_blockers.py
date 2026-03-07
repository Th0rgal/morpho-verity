#!/usr/bin/env python3
"""Unit tests for macro migration blocker checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parent.parent

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_blockers import (  # noqa: E402
  MigrationGateError,
  build_report,
  parse_constructor_usage,
  validate_against_baseline,
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
        "source": report["source"],
        "expectedUnsupported": {
          "stmt": report["unsupported"]["stmt"],
          "expr": report["unsupported"]["expr"],
        },
      }
      baseline_path.write_text(str(baseline), encoding="utf-8")
      self.assertTrue(baseline_path.exists())


class CreateMarketFrontendRegressionTests(unittest.TestCase):
  def test_create_market_frontend_blockers_still_fail_at_current_pin(self) -> None:
    source = """\
import Verity.Core
import Verity.Macro

open Verity

def setMappingWord (_slot : StorageSlot (Uint256 → Uint256)) (_key _wordOffset _value : Uint256) :
    Contract Unit := Verity.pure ()

verity_contract Tmp where
  storage
    marketSlot : Uint256 → Uint256 := slot 0

  function f (marketParams : Tuple [Address, Address, Address, Address, Uint256]) : Unit := do
    let loanToken := marketParams_0
    let id := externalCall "keccakMarketParams" [loanToken]
    let ts := blockTimestamp
    setMappingWord marketSlot id 0 ts
"""
    with tempfile.TemporaryDirectory() as tmp:
      path = pathlib.Path(tmp) / "CreateMarketFrontendRegression.lean"
      path.write_text(source, encoding="utf-8")
      proc = subprocess.run(
        ["lake", "env", "lean", str(path)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertNotEqual(proc.returncode, 0)
    output = proc.stdout + proc.stderr
    self.assertIn("unknown identifier 'marketParams_0'", output)
    self.assertIn("unknown identifier 'externalCall'", output)
    self.assertIn("Contract Uint256", output)


if __name__ == "__main__":
  unittest.main()
