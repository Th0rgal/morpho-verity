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
  load_baseline,
  load_obligations,
  parse_constructor_usage,
  validate_operation_blockers,
  validate_against_baseline,
)

CORE_FLOW_FRONTEND_REGRESSION_CASES = (
  {
    "name": "calls_with_return",
    "blocker": "externalWithReturn",
    "source": """\
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
    "expected": "unsupported do element",
  },
  {
    "name": "internal_call",
    "blocker": "internalCall",
    "source": """\
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
    "expected": "unsupported do element",
  },
  {
    "name": "callback",
    "blocker": "callbacks",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (onBehalf : Address, data : Bytes) : Unit := do
    Callbacks.callback onBehalf 0 [] data
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "erc20_transfer",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransfer token receiver amount
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "erc20_transfer_from",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, sender : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransferFrom token sender receiver amount
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "struct_member2_read",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address) : Unit := do
    let shares := position[id][user]._0
    let _ := shares
    pure ()
""",
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_write",
    "blocker": "structMember2",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    position : Uint256 := slot 0

  function f (id : Uint256, user : Address, x : Uint256) : Unit := do
    position[id][user]._0 := x
    pure ()
""",
    "expected": "unsupported do element",
  },
  {
    "name": "memory_ops",
    "blocker": "memoryOps",
    "source": """\
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
    "expected": "unsupported do element",
  },
)

COLLATERAL_FRONTEND_REGRESSION_CASES = (
  {
    "name": "calls_with_return",
    "blocker": "externalWithReturn",
    "source": """\
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
    "expected": "unsupported do element",
  },
  {
    "name": "internal_call",
    "blocker": "internalCall",
    "source": """\
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
    "expected": "unsupported do element",
  },
  {
    "name": "callback",
    "blocker": "callbacks",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (onBehalf : Address, data : Bytes) : Unit := do
    Callbacks.callback onBehalf 0 [] data
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "erc20_transfer",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransfer token receiver amount
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "erc20_transfer_from",
    "blocker": "erc20",
    "source": """\
import Verity.Core
import Verity.Macro

open Verity

verity_contract Tmp where
  storage
    dummy : Uint256 := slot 0

  function f (token : Address, sender : Address, receiver : Address, amount : Uint256) : Unit := do
    ERC20.safeTransferFrom token sender receiver amount
""",
    "expected": "unsupported statement in do block",
  },
  {
    "name": "struct_member2_read",
    "blocker": "structMember2",
    "source": """\
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
    "expected": "unsupported expression in verity_contract body",
  },
  {
    "name": "struct_member2_write",
    "blocker": "structMember2",
    "source": """\
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
    "expected": "unsupported do element",
  },
  {
    "name": "memory_ops",
    "blocker": "memoryOps",
    "source": """\
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
    "expected": "unsupported do element",
  },
)

ISSUE_FRONTEND_REGRESSION_CASES = {
  123: CORE_FLOW_FRONTEND_REGRESSION_CASES,
  124: COLLATERAL_FRONTEND_REGRESSION_CASES,
}


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

  def test_validate_operation_blockers_rejects_duplicate_blocker_entries(self) -> None:
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
    obligations["supply"]["macroSurfaceBlockers"] = [
      "callbacks",
      "erc20",
      "internalCall",
      "memoryOps",
      "memoryOps",
      "structMember2",
    ]
    with self.assertRaisesRegex(MigrationGateError, "duplicate entries in `macroSurfaceBlockers`"):
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


if __name__ == "__main__":
  unittest.main()
