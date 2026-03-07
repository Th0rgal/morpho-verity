#!/usr/bin/env python3
"""Unit tests for workflow working-directory reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_working_directory_refs import (  # noqa: E402
  CiWorkflowWorkingDirectoryRefsError,
  collect_workflow_working_directories,
  main,
  resolve_working_directory,
)


class CollectWorkflowWorkingDirectoriesTests(unittest.TestCase):
  def test_collects_step_and_job_level_working_directories(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  build:",
        "    working-directory: tools",
        "    steps:",
        "      - run: pwd",
        "        working-directory: morpho-blue",
      ]
    )

    self.assertEqual(
      collect_workflow_working_directories(workflow_text),
      ["tools", "morpho-blue"],
    )

  def test_rejects_expression_working_directory(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowWorkingDirectoryRefsError,
      r"working-directory must be a literal repo-relative path",
    ):
      collect_workflow_working_directories("working-directory: ${{ matrix.project }}")

  def test_rejects_repo_escape_working_directory(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowWorkingDirectoryRefsError,
      r"working-directory must stay inside the repository",
    ):
      collect_workflow_working_directories("working-directory: ../outside")


class ResolveWorkingDirectoryTests(unittest.TestCase):
  def test_resolves_repo_relative_directory(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      target = root / "morpho-blue"
      target.mkdir()

      self.assertEqual(resolve_working_directory(root, "morpho-blue"), target.resolve())


class CliTests(unittest.TestCase):
  def test_main_reports_missing_working_directory(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  test:",
            "    steps:",
            "      - run: pwd",
            "        working-directory: missing-dir",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_working_directory_refs.py"),
          "--workflow",
          str(workflow_path),
          "--repo-root",
          str(root),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-working-directory-refs check failed: "
        "workflow references missing working-directory paths: missing-dir",
        proc.stderr,
      )

  def test_main_accepts_relative_workflow_and_repo_root_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.parent.mkdir(parents=True)
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  test:",
            "    steps:",
            "      - run: pwd",
            "        working-directory: morpho-blue",
          ]
        ),
        encoding="utf-8",
      )
      repo_root = root / "repo"
      (repo_root / "morpho-blue").mkdir(parents=True)
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_working_directory_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
          "--repo-root",
          str(pathlib.Path("..") / "repo"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-working-directory-refs check: OK", proc.stdout)

  def test_main_function_passes_when_working_directories_exist(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      (root / "morpho-blue").mkdir()
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  test:",
            "    steps:",
            "      - run: pwd",
            "        working-directory: morpho-blue",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_working_directory_refs.py",
          "--workflow",
          str(workflow_path),
          "--repo-root",
          str(root),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
