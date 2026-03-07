#!/usr/bin/env python3
"""Unit tests for workflow uses reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_uses_refs import (  # noqa: E402
  CiWorkflowUsesRefsError,
  collect_workflow_uses_references,
  main,
  validate_uses_reference,
)


class CollectWorkflowUsesReferencesTests(unittest.TestCase):
  def test_collects_step_and_job_level_uses_references(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  call-reusable:",
        "    uses: owner/repo/.github/workflows/reusable.yml@main",
        "  verify:",
        "    steps:",
        "      - uses: actions/checkout@v5",
        "      - uses: docker://alpine:3.20",
      ]
    )

    self.assertEqual(
      collect_workflow_uses_references(workflow_text),
      [
        "owner/repo/.github/workflows/reusable.yml@main",
        "actions/checkout@v5",
        "docker://alpine:3.20",
      ],
    )

  def test_preserves_hash_inside_quoted_uses_reference(self) -> None:
    workflow_text = '\n'.join(["jobs:", "  verify:", '    uses: "owner/repo/path@ref#fragment"'])

    self.assertEqual(
      collect_workflow_uses_references(workflow_text),
      ["owner/repo/path@ref#fragment"],
    )


class ValidateUsesReferenceTests(unittest.TestCase):
  def test_rejects_expression_reference(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowUsesRefsError,
      r"workflow uses: references must be literal values",
    ):
      validate_uses_reference(pathlib.Path("/repo"), "${{ matrix.action }}")

  def test_rejects_malformed_external_reference(self) -> None:
    with self.assertRaisesRegex(
      CiWorkflowUsesRefsError,
      r"workflow external uses: references must match owner/repo\[/path\]@ref",
    ):
      validate_uses_reference(pathlib.Path("/repo"), "actions/checkout")

  def test_rejects_missing_local_action_metadata(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      repo_root = pathlib.Path(temp_dir)
      action_dir = repo_root / ".github" / "actions" / "lint"
      action_dir.mkdir(parents=True)

      with self.assertRaisesRegex(
        CiWorkflowUsesRefsError,
        r"workflow local action is missing action metadata",
      ):
        validate_uses_reference(repo_root, "./.github/actions/lint")

  def test_allows_external_docker_and_local_action_references(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      repo_root = pathlib.Path(temp_dir)
      action_dir = repo_root / ".github" / "actions" / "lint"
      action_dir.mkdir(parents=True)
      (action_dir / "action.yml").write_text("name: lint\nruns:\n  using: composite\n", encoding="utf-8")

      validate_uses_reference(repo_root, "actions/checkout@v5")
      validate_uses_reference(repo_root, "owner/repo/path/to/action@main")
      validate_uses_reference(repo_root, "docker://alpine:3.20")
      validate_uses_reference(repo_root, "./.github/actions/lint")


class CliTests(unittest.TestCase):
  def test_main_reports_missing_local_action_path(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: ./.github/actions/missing",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_uses_refs.py"),
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
        "ci-workflow-uses-refs check failed: "
        "workflow references missing local action path: ./.github/actions/missing",
        proc.stderr,
      )

  def test_main_accepts_relative_paths_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      action_dir = root / ".github" / "actions" / "lint"
      action_dir.mkdir(parents=True)
      (action_dir / "action.yml").write_text(
        "name: lint\nruns:\n  using: composite\n",
        encoding="utf-8",
      )

      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.parent.mkdir(parents=True)
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: ./.github/actions/lint",
            "      - uses: actions/checkout@v5",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_uses_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
          "--repo-root",
          str(pathlib.Path("..")),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-uses-refs check: OK", proc.stdout)

  def test_main_function_passes_for_realistic_references(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      action_dir = root / ".github" / "actions" / "lint"
      action_dir.mkdir(parents=True)
      (action_dir / "Dockerfile").write_text("FROM alpine:3.20\n", encoding="utf-8")

      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - uses: actions/cache@v4",
            "      - uses: docker://alpine:3.20",
            "      - uses: ./.github/actions/lint",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_uses_refs.py",
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
