#!/usr/bin/env python3
"""Unit tests for workflow upload-artifact missing-file policy checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_upload_artifact_policy import (  # noqa: E402
  CiWorkflowUploadArtifactPolicyError,
  collect_upload_artifact_policies,
  main,
  validate_upload_artifact_policy,
)


class CollectUploadArtifactPoliciesTests(unittest.TestCase):
  def test_collects_named_and_unnamed_upload_steps(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/upload-artifact@v4",
        "        with:",
        "          name: logs",
        "          if-no-files-found: error",
        "      - uses: actions/upload-artifact@v4",
        "        with:",
        "          path: out/report",
        "          if-no-files-found: error",
      ]
    )

    self.assertEqual(
      collect_upload_artifact_policies(workflow_text),
      [("logs", "error"), (None, "error")],
    )

  def test_ignores_non_upload_steps(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/download-artifact@v4",
        "        with:",
        "          name: logs",
      ]
    )

    self.assertEqual(collect_upload_artifact_policies(workflow_text), [])


class ValidateUploadArtifactPolicyTests(unittest.TestCase):
  def test_rejects_missing_policy(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowUploadArtifactPolicyError,
      r"must set with\.if-no-files-found: error",
    ):
      validate_upload_artifact_policy("logs", None)

  def test_rejects_expression_policy(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowUploadArtifactPolicyError,
      r"must be a literal value",
    ):
      validate_upload_artifact_policy("logs", "${{ env.POLICY }}")

  def test_allows_required_policy(self) -> None:
    validate_upload_artifact_policy("logs", "error")


class CliTests(unittest.TestCase):
  def test_main_reports_missing_policy(self) -> None:
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
            "          path: out/logs",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_upload_artifact_policy.py"),
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
        "ci-workflow-upload-artifact-policy check failed: "
        "upload-artifact step logs must set with.if-no-files-found: error",
        proc.stderr,
      )

  def test_main_reports_non_error_policy(self) -> None:
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
            "          path: out/logs",
            "          if-no-files-found: warn",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_upload_artifact_policy.py"),
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
        "ci-workflow-upload-artifact-policy check failed: "
        "upload-artifact step logs must set with.if-no-files-found to 'error', got 'warn'",
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
            "      - uses: actions/upload-artifact@v4",
            "        with:",
            "          name: logs",
            "          path: out/logs",
            "          if-no-files-found: error",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_upload_artifact_policy.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-upload-artifact-policy check: OK", proc.stdout)

  def test_main_function_passes_for_error_policy(self) -> None:
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
            "          path: out/logs",
            "          if-no-files-found: error",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_upload_artifact_policy.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
