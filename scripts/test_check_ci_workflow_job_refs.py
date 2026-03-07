#!/usr/bin/env python3
"""Unit tests for workflow job reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_job_refs import (  # noqa: E402
  collect_workflow_job_graph,
  find_cycle,
  main,
)


class CollectWorkflowJobGraphTests(unittest.TestCase):
  def test_collects_jobs_and_needs_variants(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  build:",
        "    runs-on: ubuntu-latest",
        "  lint:",
        "    needs: build",
        "  test:",
        "    needs: [build, lint]",
        "  release:",
        "    needs:",
        "      - test",
        "      - lint",
      ]
    )

    self.assertEqual(
      collect_workflow_job_graph(workflow_text),
      (
        ["build", "lint", "test", "release"],
        {
          "build": [],
          "lint": ["build"],
          "test": ["build", "lint"],
          "release": ["test", "lint"],
        },
      ),
    )

  def test_rejects_quoted_hash_in_needs_without_truncating_value(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  build:",
        "    runs-on: ubuntu-latest",
        "  test:",
        '    needs: ["build#fast"]',
      ]
    )

    with self.assertRaisesRegex(RuntimeError, "needs references invalid job ids: build#fast"):
      collect_workflow_job_graph(workflow_text)


class FindCycleTests(unittest.TestCase):
  def test_returns_none_for_acyclic_graph(self) -> None:
    self.assertIsNone(
      find_cycle(
        {
          "build": [],
          "test": ["build"],
          "release": ["test"],
        }
      )
    )

  def test_returns_cycle_path_for_multi_job_cycle(self) -> None:
    self.assertEqual(
      find_cycle(
        {
          "build": ["release"],
          "test": ["build"],
          "release": ["test"],
        }
      ),
      ["build", "release", "test", "build"],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_duplicate_job_ids(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    runs-on: ubuntu-latest",
            "  verify:",
            "    runs-on: ubuntu-latest",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-job-refs check failed: workflow defines duplicate job ids: verify",
        proc.stderr,
      )

  def test_main_reports_undefined_needs_reference(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    runs-on: ubuntu-latest",
            "  test:",
            "    needs: [build, release]",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-job-refs check failed: workflow needs references undefined jobs: release",
        proc.stderr,
      )

  def test_main_reports_self_needs_reference(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    needs: verify",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-job-refs check failed: workflow jobs cannot need themselves: verify",
        proc.stderr,
      )

  def test_main_reports_multi_job_cycle(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    needs: release",
            "  test:",
            "    needs: build",
            "  release:",
            "    needs: test",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_refs.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-job-refs check failed: workflow jobs contain dependency cycle: build -> release -> test -> build",
        proc.stderr,
      )

  def test_main_accepts_relative_workflow_path_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.parent.mkdir(parents=True)
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    runs-on: ubuntu-latest",
            "  test:",
            "    needs: build",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_job_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-job-refs check: OK", proc.stdout)

  def test_main_function_passes_for_valid_job_graph(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    runs-on: ubuntu-latest",
            "  test:",
            "    needs: build",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_job_refs.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
