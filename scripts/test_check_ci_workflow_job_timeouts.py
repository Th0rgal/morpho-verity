#!/usr/bin/env python3
"""Unit tests for workflow job timeout policy checker."""

from __future__ import annotations

import os
import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_job_timeouts import (  # noqa: E402
  collect_job_timeout_values,
  main,
  validate_job_timeout_value,
  validate_job_timeouts,
)


class CollectJobTimeoutValuesTests(unittest.TestCase):
  def test_collects_job_level_timeout_only(self) -> None:
    job_text = "\n".join(
      [
        "  build:",
        "    timeout-minutes: 15",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_job_timeout_values(job_text), ["15"])

  def test_strips_inline_yaml_comment_from_job_timeout(self) -> None:
    job_text = "\n".join(
      [
        "  build:",
        "    timeout-minutes: 15 # bounded by policy",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_job_timeout_values(job_text), ["15"])


class ValidateJobTimeoutValueTests(unittest.TestCase):
  def test_accepts_literal_positive_integer(self) -> None:
    self.assertIsNone(validate_job_timeout_value("build", "15"))

  def test_rejects_expression(self) -> None:
    self.assertEqual(
      validate_job_timeout_value("build", "${{ matrix.timeout }}"),
      "job build timeout-minutes must be a literal integer, found expression: ${{ matrix.timeout }}",
    )

  def test_rejects_zero(self) -> None:
    self.assertEqual(
      validate_job_timeout_value("build", "0"),
      "job build timeout-minutes must be greater than zero, found: 0",
    )


class ValidateJobTimeoutsTests(unittest.TestCase):
  def test_reports_missing_timeout(self) -> None:
    job_blocks = {
      "build": "\n".join(
        [
          "  build:",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_job_timeouts(job_blocks),
      ["job build is missing timeout-minutes"],
    )

  def test_reports_multiple_values(self) -> None:
    job_blocks = {
      "build": "\n".join(
        [
          "  build:",
          "    timeout-minutes: 15",
          "    timeout-minutes: 20",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_job_timeouts(job_blocks),
      ["job build has multiple timeout-minutes values: 15, 20"],
    )


class CliTests(unittest.TestCase):
  def test_main_accepts_timeout_with_inline_comment(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    timeout-minutes: 10 # literal with comment",
            "    steps:",
            "      - run: echo hi",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_timeouts.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-job-timeouts check: OK", proc.stdout)

  def test_main_reports_expression_timeout(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    timeout-minutes: ${{ matrix.timeout }}",
            "    steps:",
            "      - run: echo hi",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_timeouts.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-job-timeouts check failed: "
        "job build timeout-minutes must be a literal integer, found expression: ${{ matrix.timeout }}",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_job_timeouts.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-job-timeouts check: OK", proc.stdout)

  def test_main_function_accepts_relative_workflow_path(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow_dir = root / "inputs"
      workflow_dir.mkdir()
      workflow_path = workflow_dir / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    timeout-minutes: 10",
            "    steps:",
            "      - run: echo hi",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      old_argv = sys.argv
      old_cwd = pathlib.Path.cwd()
      try:
        sys.argv = [
          "check_ci_workflow_job_timeouts.py",
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ]
        os.chdir(runner)
        self.assertEqual(main(), 0)
      finally:
        os.chdir(old_cwd)
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
