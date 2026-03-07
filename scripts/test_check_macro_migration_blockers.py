#!/usr/bin/env python3
"""Unit tests for macro migration blocker checker."""

from __future__ import annotations

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
  parse_constructor_usage,
  validate_operation_blockers,
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


class OperationBlockerTests(unittest.TestCase):
  def test_build_operation_blocker_report_for_tracked_ops(self) -> None:
    spec_text = """
def morphoSpec : CompilationModel := {
  functions := [
    {
      name := "supply"
      body := supplyBody
    },
    {
      name := "withdraw"
      body := withdrawBody
    },
    {
      name := "supplyCollateral"
      body := supplyCollateralBody
    },
    {
      name := "borrow"
      body := borrowBody
    },
    {
      name := "repay"
      body := repayBody
    },
    {
      name := "withdrawCollateral"
      body := withdrawCollateralBody
    },
    {
      name := "liquidate"
      body := liquidateBody
    }
  ]
}

private def supplyBody : List Stmt := [
  callAccrueInterest,
  Stmt.letVar "x" (Expr.structMember2 "position" (Expr.localVar "id") (Expr.param "user") "supplyShares"),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  Callbacks.callback Expr.caller 0 [] "data",
  ERC20.safeTransferFrom (Expr.localVar "loanToken") Expr.caller Expr.contractAddress (Expr.literal 1)
]
private def withdrawBody : List Stmt := [
  callAccrueInterest,
  Stmt.setStructMember2 "position" (Expr.localVar "id") (Expr.param "user") "supplyShares" (Expr.literal 0),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  ERC20.safeTransfer (Expr.localVar "loanToken") (Expr.param "receiver") (Expr.literal 1)
]
private def supplyCollateralBody : List Stmt := [
  Stmt.letVar "x" (Expr.structMember2 "position" (Expr.localVar "id") (Expr.param "user") "collateral"),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  Callbacks.callback Expr.caller 0 [] "data",
  ERC20.safeTransferFrom (Expr.localVar "token") Expr.caller Expr.contractAddress (Expr.literal 1)
]
private def borrowBody : List Stmt := [
  callAccrueInterest,
  Calls.withReturn "price" (Expr.localVar "oracle") 0 [] (isStatic := true),
  Stmt.letVar "x" (Expr.structMember2 "position" (Expr.localVar "id") (Expr.param "user") "borrowShares"),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  ERC20.safeTransfer (Expr.localVar "loanToken") (Expr.param "receiver") (Expr.literal 1)
]
private def repayBody : List Stmt := [
  callAccrueInterest,
  Stmt.setStructMember2 "position" (Expr.localVar "id") (Expr.param "user") "borrowShares" (Expr.literal 0),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  Callbacks.callback Expr.caller 0 [] "data",
  ERC20.safeTransferFrom (Expr.localVar "loanToken") Expr.caller Expr.contractAddress (Expr.literal 1)
]
private def withdrawCollateralBody : List Stmt := [
  callAccrueInterest,
  Calls.withReturn "price" (Expr.localVar "oracle") 0 [] (isStatic := true),
  Stmt.letVar "x" (Expr.structMember2 "position" (Expr.localVar "id") (Expr.param "user") "collateral"),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  ERC20.safeTransfer (Expr.localVar "token") (Expr.param "receiver") (Expr.literal 1)
]
private def liquidateBody : List Stmt := [
  callAccrueInterest,
  Calls.withReturn "price" (Expr.localVar "oracle") 0 [] (isStatic := true),
  Stmt.letVar "x" (Expr.structMember2 "position" (Expr.localVar "id") (Expr.param "user") "collateral"),
  Stmt.mstore (Expr.literal 0) Expr.caller,
  Expr.mload (Expr.literal 0),
  Callbacks.callback Expr.caller 0 [] "data",
  ERC20.safeTransfer (Expr.localVar "token") Expr.caller (Expr.literal 1)
]
"""
    report = build_operation_blocker_report(spec_text)
    self.assertEqual(
      report["supply"],
      ["callbacks", "erc20", "internalCall", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["withdraw"],
      ["erc20", "internalCall", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["supplyCollateral"],
      ["callbacks", "erc20", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["borrow"],
      ["erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["repay"],
      ["callbacks", "erc20", "internalCall", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["withdrawCollateral"],
      ["erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
    )
    self.assertEqual(
      report["liquidate"],
      ["callbacks", "erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
    )

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

  def test_validate_operation_blockers_rejects_drift(self) -> None:
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
    obligations["supply"]["macroSurfaceBlockers"] = ["erc20", "internalCall"]
    with self.assertRaisesRegex(MigrationGateError, "macroSurfaceBlockers drift detected"):
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

  def test_create_market_frontend_blockers_still_fail_at_current_pin(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

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
    proc = self.compile_contract(source)

    self.assertNotEqual(proc.returncode, 0)
    output = proc.stdout + proc.stderr
    self.assertIn("unknown identifier 'marketParams_0'", output)
    self.assertIn("unknown identifier 'externalCall'", output)
    self.assertIn("Contract Uint256", output)

  def test_core_flow_frontend_blockers_still_fail_at_current_pin(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

    cases = [
      (
        "calls_with_return",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (oracle : Address) : Unit := do
    let _ <- Calls.withReturn oracle 0 []
    pure ()
""",
        "unsupported do element",
      ),
      (
        "internal_call",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f () : Unit := do
    let _ <- call g()
    pure ()

  function g () : Unit := do
    pure ()
""",
        "unsupported do element",
      ),
      (
        "callback",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (onBehalf : Address, data : Bytes) : Unit := do
    Callbacks.callback onBehalf 0 [] data
""",
        "unsupported statement in do block",
      ),
      (
        "erc20_transfer",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransfer token receiver amount
""",
        "unsupported statement in do block",
      ),
      (
        "erc20_transfer_from",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, sender : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransferFrom token sender receiver amount
""",
        "unsupported statement in do block",
      ),
    ]

    for name, source, expected in cases:
      with self.subTest(name=name):
        proc = self.compile_contract(source)
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(expected, proc.stdout + proc.stderr)

  def test_collateral_liquidation_frontend_blockers_still_fail_at_current_pin(self) -> None:
    if shutil.which("lake") is None:
      self.skipTest("lake is not available in this test environment")

    cases = [
      (
        "struct_member2_read",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address) : Unit := do
    let collateral := position[id][user]._1
    let _ := collateral
    pure ()
""",
        "unsupported expression in verity_contract body",
      ),
      (
        "struct_member2_write",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address, x : Uint256) : Unit := do
    position[id][user]._1 := x
    pure ()
""",
        "unsupported do element",
      ),
      (
        "memory_ops",
        """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (x : Uint256) : Unit := do
    mstore 0 x
    let y := mload 0
    let _ := y
    pure ()
""",
        "unsupported do element",
      ),
    ]

    for name, source, expected in cases:
      with self.subTest(name=name):
        proc = self.compile_contract(source)
        self.assertNotEqual(proc.returncode, 0)
        self.assertIn(expected, proc.stdout + proc.stderr)


if __name__ == "__main__":
  unittest.main()
