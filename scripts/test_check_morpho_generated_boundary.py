#!/usr/bin/env python3
"""Unit tests for morpho generated-boundary checker."""

from __future__ import annotations

import pathlib
import subprocess
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_morpho_generated_boundary.py"
MAIN_PATH = ROOT / "Morpho" / "Compiler" / "Main.lean"


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


if __name__ == "__main__":
  unittest.main()
