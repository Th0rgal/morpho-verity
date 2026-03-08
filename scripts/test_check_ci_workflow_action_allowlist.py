#!/usr/bin/env python3
"""Unit tests for workflow action allowlist checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_action_allowlist import (  # noqa: E402
  CiWorkflowActionAllowlistError,
  collect_external_uses_references,
  main,
  validate_action_allowlist,
)


class CollectExternalUsesReferencesTests(unittest.TestCase):
  def test_collects_external_references_and_skips_local_and_docker(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - uses: actions/checkout@v5",
        "      - uses: docker://alpine:3.20",
        "      - uses: ./.github/actions/example",
        "  reusable:",
        "    uses: owner/repo/.github/workflows/reusable.yml@main",
      ]
    )

    self.assertEqual(
      collect_external_uses_references(workflow_text),
      [
        "actions/checkout@v5",
        "owner/repo/.github/workflows/reusable.yml@main",
      ],
    )

  def test_preserves_hash_inside_quoted_external_reference(self) -> None:
    workflow_text = '\n'.join(["jobs:", "  verify:", '    uses: "actions/checkout@v5#pinned"'])

    self.assertEqual(
      collect_external_uses_references(workflow_text),
      ["actions/checkout@v5#pinned"],
    )


class ValidateActionAllowlistTests(unittest.TestCase):
  def test_accepts_current_allowlist(self) -> None:
    validate_action_allowlist(
      [
        "actions/checkout@v5",
        "actions/cache@v4",
        "actions/cache/save@v4",
        "actions/download-artifact@v4",
        "actions/upload-artifact@v4",
      ]
    )

  def test_rejects_unapproved_action_reference(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowActionAllowlistError,
      r"workflow uses: references must stay inside the approved allowlist",
    ):
      validate_action_allowlist(["actions/setup-python@v5"])


class CliTests(unittest.TestCase):
  def test_main_reports_unapproved_reference(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/checkout@v5",
            "      - uses: actions/setup-python@v5",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_action_allowlist.py"),
          "--workflow",
          str(workflow_path),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn("actions/setup-python@v5", proc.stderr)

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
            "      - uses: actions/checkout@v5",
            "      - uses: actions/cache@v4",
            "      - uses: actions/cache/save@v4",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_action_allowlist.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-action-allowlist check: OK", proc.stdout)

  def test_main_function_passes_for_current_actions(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/checkout@v5",
            "      - uses: actions/cache@v4",
            "      - uses: actions/cache/save@v4",
            "      - uses: actions/download-artifact@v4",
            "      - uses: actions/upload-artifact@v4",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_action_allowlist.py",
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
