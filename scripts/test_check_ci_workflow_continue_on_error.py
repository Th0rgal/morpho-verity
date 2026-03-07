#!/usr/bin/env python3
"""Unit tests for workflow continue-on-error policy checker."""

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

from check_ci_workflow_continue_on_error import (  # noqa: E402
  ALLOWED_JOB_CONTINUE_ON_ERROR,
  collect_job_continue_on_error_values,
  collect_step_level_continue_on_error_jobs,
  main,
  validate_continue_on_error,
)


class CollectJobContinueOnErrorValuesTests(unittest.TestCase):
  def test_collects_top_level_job_field(self) -> None:
    job_text = "\n".join(
      [
        "  yul-identity-report:",
        "    runs-on: ubuntu-latest",
        "    continue-on-error: true",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_job_continue_on_error_values(job_text), ["true"])

  def test_ignores_step_level_field(self) -> None:
    job_text = "\n".join(
      [
        "  build:",
        "    runs-on: ubuntu-latest",
        "    steps:",
        "      - run: echo hi",
        "        continue-on-error: true",
      ]
    )

    self.assertEqual(collect_job_continue_on_error_values(job_text), [])


class CollectStepLevelContinueOnErrorJobsTests(unittest.TestCase):
  def test_collects_jobs_with_step_level_continue_on_error(self) -> None:
    job_blocks = {
      "build": "\n".join(
        [
          "  build:",
          "    steps:",
          "      - run: echo hi",
          "        continue-on-error: true",
        ]
      )
    }

    self.assertEqual(collect_step_level_continue_on_error_jobs(job_blocks), ["build"])


class ValidateContinueOnErrorTests(unittest.TestCase):
  def test_accepts_allowlisted_jobs(self) -> None:
    job_blocks = {
      "yul-identity-report": "\n".join(
        [
          "  yul-identity-report:",
          "    runs-on: ubuntu-latest",
          "    continue-on-error: true",
          "    steps:",
          "      - run: echo hi",
        ]
      ),
      "parity-target": "\n".join(
        [
          "  parity-target:",
          "    runs-on: ubuntu-latest",
          "    steps:",
          "      - run: echo hi",
        ]
      ),
    }

    self.assertEqual(validate_continue_on_error(job_blocks), [])

  def test_rejects_disallowed_job(self) -> None:
    job_blocks = {
      "parity-target": "\n".join(
        [
          "  parity-target:",
          "    continue-on-error: true",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_continue_on_error(job_blocks),
      [
        "job parity-target must not set continue-on-error; only allowlisted jobs may do so: "
        + ", ".join(sorted(ALLOWED_JOB_CONTINUE_ON_ERROR))
      ],
    )

  def test_rejects_expression_value(self) -> None:
    job_blocks = {
      "yul-identity-report": "\n".join(
        [
          "  yul-identity-report:",
          "    continue-on-error: ${{ github.event_name == 'pull_request' }}",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_continue_on_error(job_blocks),
      [
        "job yul-identity-report continue-on-error must be a literal boolean, "
        "found expression: ${{ github.event_name == 'pull_request' }}"
      ],
    )

  def test_rejects_false_value(self) -> None:
    job_blocks = {
      "yul-identity-report": "\n".join(
        [
          "  yul-identity-report:",
          "    continue-on-error: false",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_continue_on_error(job_blocks),
      ["job yul-identity-report continue-on-error must be literal true when present, found: false"],
    )

  def test_rejects_multiple_values(self) -> None:
    job_blocks = {
      "yul-identity-report": "\n".join(
        [
          "  yul-identity-report:",
          "    continue-on-error: true",
          "    continue-on-error: true",
          "    steps:",
          "      - run: echo hi",
        ]
      )
    }

    self.assertEqual(
      validate_continue_on_error(job_blocks),
      ["job yul-identity-report has multiple continue-on-error values: true, true"],
    )

  def test_rejects_step_level_usage(self) -> None:
    job_blocks = {
      "build": "\n".join(
        [
          "  build:",
          "    steps:",
          "      - run: echo hi",
          "        continue-on-error: true",
        ]
      )
    }

    self.assertEqual(
      validate_continue_on_error(job_blocks),
      ["workflow steps must not set continue-on-error: build"],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_disallowed_job(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  parity-target:",
            "    runs-on: ubuntu-latest",
            "    continue-on-error: true",
            "    steps:",
            "      - run: echo hi",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_continue_on_error.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-continue-on-error check failed: "
        "job parity-target must not set continue-on-error",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_continue_on_error.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-continue-on-error check: OK", proc.stdout)

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
            "  yul-identity-report:",
            "    runs-on: ubuntu-latest",
            "    continue-on-error: true",
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
          "check_ci_workflow_continue_on_error.py",
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
