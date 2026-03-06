#!/usr/bin/env python3
"""Unit tests for script-to-test pairing guard."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_script_test_pairs import (  # noqa: E402
  collect_repo_scripts,
  collect_repo_script_tests,
  find_missing_tests,
  find_stale_tests,
  main,
)


class CheckScriptTestPairsTests(unittest.TestCase):
  def test_collect_repo_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "notes.txt").write_text("", encoding="utf-8")
      self.assertEqual(collect_repo_scripts(scripts_dir), {"alpha.py", "beta.sh"})

  def test_collect_repo_script_tests(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")
      self.assertEqual(collect_repo_script_tests(scripts_dir), {"test_alpha.py", "test_beta.sh"})

  def test_find_missing_tests(self) -> None:
    repo_scripts = {"alpha.py", "beta.sh"}
    repo_tests = {"test_alpha.py"}
    self.assertEqual(find_missing_tests(repo_scripts, repo_tests), ["beta.sh"])

  def test_find_stale_tests(self) -> None:
    repo_scripts = {"alpha.py"}
    repo_tests = {"test_alpha.py", "test_beta.py"}
    self.assertEqual(find_stale_tests(repo_scripts, repo_tests), ["test_beta.py"])

  def test_main_passes_when_pairs_match(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.sh").write_text("", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_script_test_pairs.py",
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_script_missing_test(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_script_test_pairs.py",
          "--scripts-dir",
          str(scripts_dir),
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_stale_test_exists(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "test_beta.py").write_text("", encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_script_test_pairs.py",
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
