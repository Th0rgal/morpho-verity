#!/usr/bin/env python3
"""Unit tests for CI check-script coverage sync check."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_check_coverage import (  # noqa: E402
  collect_repo_check_scripts,
  collect_workflow_check_scripts,
  main,
)


class CheckCiCheckCoverageTests(unittest.TestCase):
  def test_collect_repo_check_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.sh").write_text("", encoding="utf-8")
      self.assertEqual(collect_repo_check_scripts(scripts_dir), {"check_alpha.py"})

  def test_collect_workflow_check_scripts(self) -> None:
    workflow = (
      "run: python3 scripts/check_alpha.py\n"
      "run: python3 scripts/test_alpha.py\n"
    )
    self.assertEqual(collect_workflow_check_scripts(workflow), {"check_alpha.py"})

  def test_main_passes_when_repo_and_workflow_match(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.py").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 scripts/check_alpha.py\n"
        "run: python3 scripts/check_beta.py\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_check_coverage.py",
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_repo_check_missing_from_workflow(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.py").write_text("", encoding="utf-8")
      workflow.write_text("run: python3 scripts/check_alpha.py\n", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_check_coverage.py",
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_workflow_has_stale_reference(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 scripts/check_alpha.py\n"
        "run: python3 scripts/check_beta.py\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_check_coverage.py",
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
