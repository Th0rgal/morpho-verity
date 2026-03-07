#!/usr/bin/env python3
"""Unit tests for CI check-script coverage sync check."""

from __future__ import annotations

import io
import pathlib
import subprocess
import tempfile
import unittest

import sys
from contextlib import redirect_stderr
from unittest import mock

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_check_coverage import (  # noqa: E402
  CiCheckCoverageError,
  collect_repo_check_scripts,
  collect_workflow_check_scripts,
  main,
  read_text,
)


class CheckCiCheckCoverageTests(unittest.TestCase):
  def test_collect_repo_check_scripts(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      scripts_dir = pathlib.Path(tmp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.sh").write_text("", encoding="utf-8")
      self.assertEqual(
        collect_repo_check_scripts(scripts_dir),
        {"check_alpha.py", "check_beta.sh"},
      )

  def test_collect_workflow_check_scripts(self) -> None:
    workflow = (
      "run: python3 scripts/check_alpha.py\n"
      "run: ./scripts/check_beta.sh\n"
      "run: python3 scripts/test_alpha.py\n"
    )
    self.assertEqual(
      collect_workflow_check_scripts(workflow),
      {"check_alpha.py", "check_beta.sh"},
    )

  def test_collect_workflow_check_scripts_ignores_non_run_references(self) -> None:
    workflow = (
      "# docs: scripts/check_from_comment.py\n"
      "name: scripts/check_from_name.sh\n"
      "run: python3 scripts/check_alpha.py\n"
    )
    self.assertEqual(collect_workflow_check_scripts(workflow), {"check_alpha.py"})

  def test_main_passes_when_repo_and_workflow_match(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "check_beta.sh").write_text("", encoding="utf-8")
      workflow.write_text(
        "run: python3 scripts/check_alpha.py\n"
        "run: ./scripts/check_beta.sh\n",
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
      (scripts_dir / "check_beta.sh").write_text("", encoding="utf-8")
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
        "run: ./scripts/check_beta.sh\n",
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

  def test_read_text_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_bytes(b"\x80")

      with self.assertRaises(CiCheckCoverageError) as ctx:
        read_text(workflow, context="workflow")

      self.assertIn("failed to decode workflow", str(ctx.exception))

  def test_collect_workflow_check_scripts_wraps_parser_errors(self) -> None:
    with mock.patch(
      "check_ci_check_coverage.extract_workflow_run_text",
      side_effect=ValueError("bad workflow fixture"),
    ):
      with self.assertRaises(CiCheckCoverageError) as ctx:
        collect_workflow_check_scripts("run: python3 scripts/check_alpha.py\n")

    self.assertEqual(
      str(ctx.exception), "failed to parse workflow run commands: bad workflow fixture"
    )

  def test_main_reports_missing_workflow_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      missing_workflow = root / "missing.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")

      old_argv = sys.argv
      stderr = io.StringIO()
      try:
        sys.argv = [
          "check_ci_check_coverage.py",
          "--workflow",
          str(missing_workflow),
          "--scripts-dir",
          str(scripts_dir),
        ]
        with redirect_stderr(stderr):
          with self.assertRaises(SystemExit) as ctx:
            main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

      message = stderr.getvalue()
      self.assertIn("ci-check-coverage check failed:", message)
      self.assertIn("failed to read workflow", message)
      self.assertNotIn("Traceback", message)

  def test_cli_rejects_invalid_utf8_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow = root / "verify.yml"
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      workflow.write_bytes(b"\x80")

      result = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_check_coverage.py"),
          "--workflow",
          str(workflow),
          "--scripts-dir",
          str(scripts_dir),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(result.returncode, 1)
      self.assertIn("ci-check-coverage check failed:", result.stderr)
      self.assertIn("failed to decode workflow", result.stderr)
      self.assertNotIn("Traceback", result.stderr)

  def test_cli_uses_repo_relative_defaults_from_another_working_directory(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      result = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_check_coverage.py"),
        ],
        cwd=tmp_dir,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(result.returncode, 0)
      self.assertIn("ci-check-coverage check: OK", result.stdout)
      self.assertNotIn("Traceback", result.stderr)


if __name__ == "__main__":
  unittest.main()
