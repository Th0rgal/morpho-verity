#!/usr/bin/env python3
"""Unit tests for workflow cache/artifact path reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_path_refs import (  # noqa: E402
  CiWorkflowPathRefsError,
  collect_workflow_action_paths,
  main,
  validate_path_ref,
)


class CollectWorkflowActionPathsTests(unittest.TestCase):
  def test_collects_cache_and_artifact_paths(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/cache@v4",
        "        with:",
        "          path: |",
        "            ~/.elan",
        "            .lake/build",
        "      - uses: actions/upload-artifact@v4",
        "        with:",
        "          name: logs",
        "          path: out/parity/*.log",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        "          name: bundle",
        "          path: out/parity-shared",
      ]
    )

    self.assertEqual(
      collect_workflow_action_paths(workflow_text),
      [
        ("cache", "~/.elan"),
        ("cache", ".lake/build"),
        ("upload-artifact", "out/parity/*.log"),
        ("download-artifact", "out/parity-shared"),
      ],
    )

  def test_ignores_unsupported_action_paths(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/checkout@v5",
        "        with:",
        "          path: nested-repo",
      ]
    )

    self.assertEqual(collect_workflow_action_paths(workflow_text), [])


class ValidatePathRefTests(unittest.TestCase):
  def test_rejects_expression_path(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowPathRefsError,
      r"workflow path references must be literal values",
    ):
      validate_path_ref("${{ github.workspace }}/out")

  def test_rejects_repo_escape_path(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowPathRefsError,
      r"workflow path references must stay inside the repository",
    ):
      validate_path_ref("../outside")

  def test_allows_home_relative_and_repo_relative_paths(self) -> None:
    validate_path_ref("~/.cache/pip")
    validate_path_ref("out/parity/*.log")


class CliTests(unittest.TestCase):
  def test_main_reports_invalid_repo_escape_path(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: logs",
            "          path: ../outside",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_path_refs.py"),
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
        "ci-workflow-path-refs check failed: "
        "workflow path references must stay inside the repository: ../outside",
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
            "  verify:",
            "    steps:",
            "      - uses: actions/cache@v4",
            "        with:",
            "          path: |",
            "            ~/.elan",
            "            .lake/build",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_path_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-path-refs check: OK", proc.stdout)

  def test_main_function_passes_for_literal_paths(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/download-artifact@v4",
            "        with:",
            "          name: bundle",
            "          path: out/parity-shared",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_path_refs.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
