#!/usr/bin/env python3
"""Unit tests for duplicate top-level Python definition checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import textwrap
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPT_DIR))

from check_duplicate_top_level_defs import (  # noqa: E402
  DuplicateTopLevelDefsError,
  collect_python_scripts,
  duplicate_top_level_defs,
  find_duplicate_top_level_defs,
)


class DuplicateTopLevelDefsTests(unittest.TestCase):
  def test_collect_python_scripts_ignores_non_python_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      scripts_dir = pathlib.Path(d)
      (scripts_dir / "check_alpha.py").write_text("def alpha():\n  return 1\n", encoding="utf-8")
      (scripts_dir / "notes.txt").write_text("ignore me\n", encoding="utf-8")

      self.assertEqual(
        [path.name for path in collect_python_scripts(scripts_dir)],
        ["check_alpha.py"],
      )

  def test_duplicate_top_level_defs_detects_shadowed_function(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "check_shadow.py"
      path.write_text(
        textwrap.dedent(
          """
          def alpha():
            return 1

          def beta():
            return 2

          def alpha():
            return 3
          """
        ).lstrip(),
        encoding="utf-8",
      )

      self.assertEqual(duplicate_top_level_defs(path), {"alpha": [1, 7]})

  def test_duplicate_top_level_defs_detects_shadowed_class(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "check_shadow.py"
      path.write_text(
        textwrap.dedent(
          """
          class Gate:
            pass

          class Gate:
            pass
          """
        ).lstrip(),
        encoding="utf-8",
      )

      self.assertEqual(duplicate_top_level_defs(path), {"Gate": [1, 4]})

  def test_duplicate_top_level_defs_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "check_shadow.py"
      path.write_bytes(b"\xff")

      with self.assertRaisesRegex(DuplicateTopLevelDefsError, "failed to decode Python source"):
        duplicate_top_level_defs(path)

  def test_duplicate_top_level_defs_rejects_syntax_errors(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "check_shadow.py"
      path.write_text("def broken(:\n  pass\n", encoding="utf-8")

      with self.assertRaisesRegex(DuplicateTopLevelDefsError, "failed to parse Python source"):
        duplicate_top_level_defs(path)

  def test_find_duplicate_top_level_defs_reports_only_affected_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      scripts_dir = pathlib.Path(d)
      (scripts_dir / "check_ok.py").write_text("def alpha():\n  return 1\n", encoding="utf-8")
      (scripts_dir / "check_dup.py").write_text(
        "def alpha():\n  return 1\n\ndef alpha():\n  return 2\n",
        encoding="utf-8",
      )

      duplicates = find_duplicate_top_level_defs(scripts_dir)

    self.assertEqual(
      {path.name: entries for path, entries in duplicates.items()},
      {"check_dup.py": {"alpha": [1, 4]}},
    )


class CliTests(unittest.TestCase):
  def test_cli_reports_duplicate_definitions_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      scripts_dir = pathlib.Path(d)
      (scripts_dir / "check_dup.py").write_text(
        "def alpha():\n  return 1\n\ndef alpha():\n  return 2\n",
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_duplicate_top_level_defs.py"),
          "--scripts-dir",
          str(scripts_dir),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("duplicate-top-level-defs check failed:", proc.stderr)
    self.assertIn("check_dup.py (alpha lines 1,4)", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_succeeds_for_clean_scripts_dir(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      scripts_dir = pathlib.Path(d)
      (scripts_dir / "check_alpha.py").write_text("def alpha():\n  return 1\n", encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_duplicate_top_level_defs.py"),
          "--scripts-dir",
          str(scripts_dir),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 0, proc.stderr)
    self.assertIn("duplicate-top-level-defs check: OK", proc.stdout)
    self.assertEqual(proc.stderr, "")
