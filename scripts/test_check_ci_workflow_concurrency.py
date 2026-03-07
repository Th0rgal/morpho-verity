#!/usr/bin/env python3
"""Unit tests for workflow concurrency policy checker."""

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

from check_ci_workflow_concurrency import (  # noqa: E402
  collect_top_level_concurrency,
  main,
  validate_concurrency,
)


class CollectTopLevelConcurrencyTests(unittest.TestCase):
  def test_collects_mapping_before_jobs(self) -> None:
    workflow_text = "\n".join(
      [
        "name: Verify",
        "concurrency:",
        "  group: ${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}",
        "  cancel-in-progress: true",
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(
      collect_top_level_concurrency(workflow_text),
      {
        "group": "${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}",
        "cancel-in-progress": "true",
      },
    )

  def test_preserves_hash_inside_quoted_value(self) -> None:
    workflow_text = "\n".join(
      [
        "concurrency:",
        '  group: "ci#group"',
        "  cancel-in-progress: true",
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(
      collect_top_level_concurrency(workflow_text),
      {"group": "ci#group", "cancel-in-progress": "true"},
    )

  def test_rejects_scalar_concurrency_value(self) -> None:
    workflow_text = "\n".join(["concurrency: ci", "jobs:", "  build:", "    steps:"])

    with self.assertRaisesRegex(
      RuntimeError,
      "workflow top-level concurrency: must use a mapping, not a scalar value",
    ):
      collect_top_level_concurrency(workflow_text)


class ValidateConcurrencyTests(unittest.TestCase):
  def test_accepts_expected_concurrency(self) -> None:
    self.assertEqual(
      validate_concurrency(
        {
          "group": "${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}",
          "cancel-in-progress": "true",
        }
      ),
      [],
    )

  def test_rejects_mismatched_group(self) -> None:
    self.assertEqual(
      validate_concurrency(
        {
          "group": "${{ github.workflow }}-${{ github.ref }}",
          "cancel-in-progress": "true",
        }
      ),
      [
        "workflow top-level concurrency must be exactly "
        "cancel-in-progress=true, "
        "group=${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}, "
        "found: cancel-in-progress=true, group=${{ github.workflow }}-${{ github.ref }}"
      ],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_missing_concurrency(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(["jobs:", "  build:", "    steps:", "      - run: echo hi"]),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_concurrency.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-concurrency check failed: workflow is missing a top-level concurrency: block",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_concurrency.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-concurrency check: OK", proc.stdout)

  def test_main_function_accepts_relative_workflow_path(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow_dir = root / "inputs"
      workflow_dir.mkdir()
      workflow_path = workflow_dir / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "concurrency:",
            "  group: ${{ github.workflow }}-${{ github.ref }}-${{ github.event_name }}",
            "  cancel-in-progress: true",
            "jobs:",
            "  build:",
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
          "check_ci_workflow_concurrency.py",
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
