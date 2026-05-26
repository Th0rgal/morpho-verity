#!/usr/bin/env python3
"""Unit tests for morpho generated-boundary checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_morpho_generated_boundary.py"
MAIN_PATH = ROOT / "Morpho" / "Compiler" / "Main.lean"
GENERATED_PATH = ROOT / "Morpho" / "Compiler" / "Generated.lean"
TRUST_DOC_PATH = ROOT / "docs" / "TRUST_BOUNDARIES.md"


class CheckMorphoGeneratedBoundaryTests(unittest.TestCase):
  def test_repo_boundary_is_clean(self) -> None:
    proc = subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_detects_direct_manual_surface_usage(self) -> None:
    original = MAIN_PATH.read_text(encoding="utf-8")
    try:
      MAIN_PATH.write_text(
        original + "\n-- test sentinel\n#eval Morpho.Compiler.Spec.morphoSelectors.length\n",
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("Main.lean", proc.stderr)
    finally:
      MAIN_PATH.write_text(original, encoding="utf-8")

  def test_detects_missing_external_axiom_name(self) -> None:
    original = GENERATED_PATH.read_text(encoding="utf-8")
    try:
      GENERATED_PATH.write_text(
        original.replace(
          'axiomNames := ["irm_borrow_rate_boundary"]',
          'axiomNames := []',
          1,
        ),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("borrowRate", proc.stderr)
      self.assertIn("irm_borrow_rate_boundary", proc.stderr)
    finally:
      GENERATED_PATH.write_text(original, encoding="utf-8")

  def test_external_axiom_validator_accepts_required_boundaries(self) -> None:
    from check_morpho_generated_boundary import validate_generated_external_axioms

    validate_generated_external_axioms(GENERATED_PATH.read_text(encoding="utf-8"))

  def test_detects_missing_trust_boundary_doc_entry(self) -> None:
    original = TRUST_DOC_PATH.read_text(encoding="utf-8")
    try:
      TRUST_DOC_PATH.write_text(
        original.replace("`flash_loan_transfers`", "`flash_loan_transfers_removed`"),
        encoding="utf-8",
      )
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("TRUST_BOUNDARIES.md drift", proc.stderr)
      self.assertIn("flash_loan_transfers", proc.stderr)
    finally:
      TRUST_DOC_PATH.write_text(original, encoding="utf-8")

  def test_reports_invalid_utf8_without_traceback(self) -> None:
    original = MAIN_PATH.read_bytes()
    try:
      MAIN_PATH.write_bytes(b"\xff")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertEqual(proc.returncode, 1)
      self.assertIn("morpho-generated-boundary check failed:", proc.stderr)
      self.assertIn("Main.lean is not valid UTF-8", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)
    finally:
      MAIN_PATH.write_bytes(original)


if __name__ == "__main__":
  unittest.main()
