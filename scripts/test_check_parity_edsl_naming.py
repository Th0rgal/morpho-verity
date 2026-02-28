#!/usr/bin/env python3
"""Unit tests for parity EDSL-only naming checker."""

from __future__ import annotations

import pathlib
import subprocess
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_parity_edsl_naming.py"
README = ROOT / "README.md"


class CheckParityEdslNamingTests(unittest.TestCase):
  def test_repo_naming_is_clean(self) -> None:
    proc = subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_detects_legacy_wording(self) -> None:
    original = README.read_text(encoding="utf-8")
    try:
      README.write_text(original + "\nlegacy input mode mention\n", encoding="utf-8")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("README.md", proc.stderr)
    finally:
      README.write_text(original, encoding="utf-8")


if __name__ == "__main__":
  unittest.main()
