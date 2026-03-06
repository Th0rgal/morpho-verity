#!/usr/bin/env python3
"""Unit tests for CI script-test coverage sync check."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_test_coverage import (  # noqa: E402
  collect_repo_python_script_tests,
  collect_repo_script_tests,
  collect_repo_shell_script_tests,
  collect_workflow_script_tests,
  has_workflow_python_discovery,
  has_workflow_shell_glob,
  main,
)


class CheckCiTestCoverageTests(unittest.TestCase):
  def test_collect_repo_script_tests(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "test_gamma.txt").write_text("", encoding="utf-8")
      self.assertEqual(collect_repo_script_tests(scripts_dir), {"test_alpha.py", "test_beta.sh"})
      self.assertEqual(collect_repo_python_script_tests(scripts_dir), {"test_alpha.py"})
      self.assertEqual(collect_repo_shell_script_tests(scripts_dir), {"test_beta.sh"})

  def test_collect_workflow_script_tests(self) -> None:
    workflow = (
      "run: python3 scripts/test_alpha.py\n"
      "run: ./scripts/test_beta.sh\n"
      "run: python3 scripts/check_something.py\n"
    )
    self.assertEqual(collect_workflow_script_tests(workflow), {"test_alpha.py", "test_beta.sh"})

  def test_detection_of_discovery_modes(self) -> None:
    workflow = (
      "run: python3 -m unittest discover -s scripts -p 'test_*.py'\n"
      "run: for t in scripts/test_*.sh; do echo \"$t\"; done\n"
    )
    self.assertTrue(has_workflow_python_discovery(workflow))
    self.assertTrue(has_workflow_shell_glob(workflow))

  def test_main_passes_when_repo_and_workflow_match(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.sh").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 scripts/test_alpha.py\n"
        "run: ./scripts/test_beta.sh\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_test_coverage.py",
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_repo_test_missing_from_workflow(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.sh").write_text("", encoding="utf-8")
      workflow.write_text("run: python3 scripts/test_alpha.py\n", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_test_coverage.py",
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
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 scripts/test_alpha.py\n"
        "run: ./scripts/test_beta.sh\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_test_coverage.py",
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

  def test_main_passes_with_discovery_and_glob_coverage(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_gamma.sh").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 -m unittest discover -s scripts -p 'test_*.py'\n"
        "run: for t in scripts/test_*.sh; do echo \"$t\"; done\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_test_coverage.py",
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_on_stale_explicit_ref_even_with_discovery(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 -m unittest discover -s scripts -p 'test_*.py'\n"
        "run: python3 scripts/test_stale.py\n",
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_test_coverage.py",
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
