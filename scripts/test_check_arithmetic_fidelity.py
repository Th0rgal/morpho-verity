#!/usr/bin/env python3
"""Unit tests for arithmetic-fidelity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_arithmetic_fidelity import (  # noqa: E402
  extract_proof_overflow_assumptions,
  extract_uint128_guard_names,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_arithmetic_fidelity.py"
DOC_PATH = ROOT / "docs" / "ARITHMETIC_FIDELITY.md"
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"


class CheckArithmeticFidelityTests(unittest.TestCase):
  def run_script(self) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )

  def test_repo_arithmetic_fidelity_is_clean(self) -> None:
    proc = self.run_script()
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_extracts_uint128_guard_names(self) -> None:
    names = extract_uint128_guard_names(MACRO_PATH.read_text(encoding="utf-8"))
    self.assertIn("newTotalBorrowAssets", names)
    self.assertIn("newFeeRecipientShares", names)
    self.assertIn("newCollateral", names)

  def test_extracts_proof_overflow_assumption_names(self) -> None:
    names = extract_proof_overflow_assumptions(
      "theorem sample (h_no_overflow : True) (h_supply_no_overflow : True) : True := by trivial"
    )
    self.assertEqual(names, {"h_no_overflow", "h_supply_no_overflow"})

  def test_detects_missing_uint128_guard_doc(self) -> None:
    original = DOC_PATH.read_text(encoding="utf-8")
    try:
      DOC_PATH.write_text(
        original.replace("`newCollateral`", "`newCollateral_removed`", 1),
        encoding="utf-8",
      )
      proc = self.run_script()
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("missing uint128 guard docs", proc.stderr)
      self.assertIn("newCollateral", proc.stderr)
    finally:
      DOC_PATH.write_text(original, encoding="utf-8")

  def test_detects_missing_proof_assumption_doc(self) -> None:
    original = DOC_PATH.read_text(encoding="utf-8")
    try:
      DOC_PATH.write_text(
        original.replace("`h_supply_no_overflow`", "`h_supply_no_overflow_removed`", 1),
        encoding="utf-8",
      )
      proc = self.run_script()
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("h_supply_no_overflow", proc.stderr)
    finally:
      DOC_PATH.write_text(original, encoding="utf-8")

  def test_reports_invalid_utf8_without_traceback(self) -> None:
    original = DOC_PATH.read_bytes()
    try:
      DOC_PATH.write_bytes(b"\xff")
      proc = self.run_script()
      self.assertEqual(proc.returncode, 1)
      self.assertIn("arithmetic-fidelity check failed:", proc.stderr)
      self.assertIn("not valid UTF-8", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)
    finally:
      DOC_PATH.write_bytes(original)


if __name__ == "__main__":
  unittest.main()
