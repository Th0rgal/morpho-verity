#!/usr/bin/env python3
"""Unit tests for workflow trigger policy checker."""

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

from check_ci_workflow_triggers import (  # noqa: E402
  collect_top_level_triggers,
  main,
  validate_triggers,
)


class CollectTopLevelTriggersTests(unittest.TestCase):
  def test_collects_expected_trigger_mapping(self) -> None:
    workflow_text = "\n".join(
      [
        "name: Verify",
        "on:",
        "  push:",
        "    branches:",
        "      - master",
        "  pull_request:",
        "  workflow_dispatch:",
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(
      collect_top_level_triggers(workflow_text),
      {
        "push": {"branches": ["master"]},
        "pull_request": {},
        "workflow_dispatch": {},
      },
    )

  def test_collects_trigger_with_inline_comment_and_quoted_hash(self) -> None:
    workflow_text = "\n".join(
      [
        "on:",
        "  push: # branch filter",
        "    branches:",
        '      - "master#stable"',
        "  pull_request:",
        "  workflow_dispatch:",
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(
      collect_top_level_triggers(workflow_text),
      {
        "push": {"branches": ["master#stable"]},
        "pull_request": {},
        "workflow_dispatch": {},
      },
    )

  def test_rejects_inline_top_level_on_value(self) -> None:
    with self.assertRaisesRegex(
      RuntimeError,
      "workflow top-level on: must use a mapping, not an inline value",
    ):
      collect_top_level_triggers("on: [push, pull_request]\njobs:\n  build:\n    steps:\n")


class ValidateTriggersTests(unittest.TestCase):
  def test_accepts_expected_trigger_policy(self) -> None:
    self.assertEqual(
      validate_triggers(
        {
          "push": {"branches": ["master"]},
          "pull_request": {},
          "workflow_dispatch": {},
        }
      ),
      [],
    )

  def test_rejects_extra_trigger(self) -> None:
    self.assertEqual(
      validate_triggers(
        {
          "push": {"branches": ["master"]},
          "pull_request": {},
          "schedule": {},
          "workflow_dispatch": {},
        }
      ),
      [
        "workflow top-level on: must be exactly pull_request; push(branches=[master]); "
        "workflow_dispatch, found: pull_request; push(branches=[master]); schedule; "
        "workflow_dispatch"
      ],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_missing_on_block(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(["jobs:", "  build:", "    steps:", "      - run: echo hi"]),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_triggers.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-triggers check failed: workflow is missing a top-level on: block",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_triggers.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-triggers check: OK", proc.stdout)

  def test_main_function_accepts_relative_workflow_path(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow_dir = root / "inputs"
      workflow_dir.mkdir()
      workflow_path = workflow_dir / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "on:",
            "  push:",
            "    branches:",
            "      - master",
            "  pull_request:",
            "  workflow_dispatch:",
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
          "check_ci_workflow_triggers.py",
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
