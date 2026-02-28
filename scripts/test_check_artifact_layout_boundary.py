#!/usr/bin/env python3
"""Unit tests for artifact layout boundary checker."""

from __future__ import annotations

import pathlib
import shutil
import subprocess
import unittest


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_artifact_layout_boundary.py"
README = ROOT / "README.md"
REQUIRED = ROOT / "artifacts" / "inputs" / "MarketParamsHash.yul"
FORBIDDEN = ROOT / "compiler" / "external-libs" / "MarketParamsHash.yul"


class CheckArtifactLayoutBoundaryTests(unittest.TestCase):
  def test_repo_layout_is_clean(self) -> None:
    proc = subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_detects_legacy_path_reference(self) -> None:
    original = README.read_text(encoding="utf-8")
    try:
      README.write_text(original + "\nlegacy path: compiler/yul\n", encoding="utf-8")
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("README.md", proc.stderr)
      self.assertIn("compiler/yul", proc.stderr)
    finally:
      README.write_text(original, encoding="utf-8")

  def test_detects_missing_required_library(self) -> None:
    backup = REQUIRED.with_suffix(REQUIRED.suffix + ".bak")
    self.assertTrue(REQUIRED.is_file())
    try:
      REQUIRED.rename(backup)
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("missing required file", proc.stderr)
    finally:
      backup.rename(REQUIRED)

  def test_detects_forbidden_legacy_library(self) -> None:
    FORBIDDEN.parent.mkdir(parents=True, exist_ok=True)
    shutil.copyfile(REQUIRED, FORBIDDEN)
    try:
      proc = subprocess.run(
        ["python3", str(SCRIPT)],
        cwd=ROOT,
        capture_output=True,
        text=True,
        check=False,
      )
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("forbidden legacy file present", proc.stderr)
    finally:
      if FORBIDDEN.exists():
        FORBIDDEN.unlink()
      try:
        FORBIDDEN.parent.rmdir()
      except OSError:
        pass
      try:
        FORBIDDEN.parent.parent.rmdir()
      except OSError:
        pass


if __name__ == "__main__":
  unittest.main()
