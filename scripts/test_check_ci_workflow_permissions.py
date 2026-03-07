#!/usr/bin/env python3
"""Unit tests for workflow permissions policy checker."""

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

from check_ci_workflow_permissions import (  # noqa: E402
  collect_job_level_permissions,
  collect_top_level_permissions,
  main,
  validate_permissions,
)


class CollectTopLevelPermissionsTests(unittest.TestCase):
  def test_collects_mapping_before_jobs(self) -> None:
    workflow_text = "\n".join(
      [
        "name: Verify",
        "permissions:",
        "  contents: read",
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_top_level_permissions(workflow_text), {"contents": "read"})

  def test_preserves_hash_inside_quoted_access_level(self) -> None:
    workflow_text = "\n".join(
      [
        "permissions:",
        '  contents: "read#only"',
        "jobs:",
        "  build:",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_top_level_permissions(workflow_text), {"contents": "read#only"})

  def test_rejects_scalar_permissions_value(self) -> None:
    workflow_text = "\n".join(["permissions: read-all", "jobs:", "  build:", "    steps:"])

    with self.assertRaisesRegex(
      RuntimeError,
      "workflow top-level permissions: must use a mapping, not a scalar value",
    ):
      collect_top_level_permissions(workflow_text)


class CollectJobLevelPermissionsTests(unittest.TestCase):
  def test_collects_job_level_overrides(self) -> None:
    workflow_text = "\n".join(
      [
        "permissions:",
        "  contents: read",
        "jobs:",
        "  build:",
        "    permissions:",
        "      actions: write",
        "    steps:",
        "      - run: echo hi",
      ]
    )

    self.assertEqual(collect_job_level_permissions(workflow_text), ["build"])


class ValidatePermissionsTests(unittest.TestCase):
  def test_accepts_minimal_contents_read_permissions(self) -> None:
    self.assertEqual(validate_permissions({"contents": "read"}, []), [])

  def test_rejects_extra_scope(self) -> None:
    self.assertEqual(
      validate_permissions({"actions": "read", "contents": "read"}, []),
      [
        "workflow top-level permissions must be exactly contents=read, found: "
        "actions=read, contents=read"
      ],
    )

  def test_rejects_job_level_override(self) -> None:
    self.assertEqual(
      validate_permissions({"contents": "read"}, ["build"]),
      ["workflow jobs must not override top-level permissions: build"],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_missing_permissions(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(["jobs:", "  build:", "    steps:", "      - run: echo hi"]),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_permissions.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-permissions check failed: workflow is missing a top-level permissions: block",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_permissions.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-permissions check: OK", proc.stdout)

  def test_main_function_accepts_relative_workflow_path(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow_dir = root / "inputs"
      workflow_dir.mkdir()
      workflow_path = workflow_dir / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "permissions:",
            "  contents: read",
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
          "check_ci_workflow_permissions.py",
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
