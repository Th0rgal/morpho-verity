#!/usr/bin/env python3
"""Unit tests for workflow script reference integrity checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_script_refs import (  # noqa: E402
  collect_repo_scripts,
  collect_workflow_script_references,
  main,
)


class CollectWorkflowScriptReferencesTests(unittest.TestCase):
  def test_collects_python_and_shell_script_references(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  verify:",
        "    steps:",
        "      - run: python3 scripts/check_alpha.py",
        "      - run: ./scripts/install_beta.sh",
        "      - run: ../scripts/run_gamma.sh",
        "      - run: env python3 ./scripts/check_alpha.py",
      ]
    )

    self.assertEqual(
      collect_workflow_script_references(workflow_text),
      {"check_alpha.py", "install_beta.sh", "run_gamma.sh"},
    )


class CollectRepoScriptsTests(unittest.TestCase):
  def test_collect_repo_scripts_includes_python_and_shell_sources(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      scripts_dir = pathlib.Path(temp_dir) / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "beta.sh").write_text("", encoding="utf-8")
      (scripts_dir / "gamma.txt").write_text("", encoding="utf-8")

      self.assertEqual(collect_repo_scripts(scripts_dir), {"alpha.py", "beta.sh"})


class CliTests(unittest.TestCase):
  def test_main_reports_missing_workflow_script_reference(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - run: python3 scripts/check_alpha.py",
            "      - run: ./scripts/missing_beta.sh",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_script_refs.py"),
          "--workflow",
          str(workflow_path),
          "--scripts-dir",
          str(scripts_dir),
        ],
        cwd=root,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-script-refs check failed: workflow references missing scripts: missing_beta.sh",
        proc.stderr,
      )

  def test_main_accepts_relative_inputs_from_other_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "inputs" / "scripts"
      scripts_dir.mkdir(parents=True)
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      (scripts_dir / "run_beta.sh").write_text("", encoding="utf-8")
      workflow_path = root / "inputs" / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - run: python3 scripts/check_alpha.py",
            "      - run: ../scripts/run_beta.sh",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_script_refs.py"),
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
          "--scripts-dir",
          str(pathlib.Path("..") / "inputs" / "scripts"),
        ],
        cwd=runner,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-script-refs check: OK", proc.stdout)

  def test_main_function_passes_for_existing_matches(self) -> None:
    with tempfile.TemporaryDirectory() as temp_dir:
      root = pathlib.Path(temp_dir)
      scripts_dir = root / "scripts"
      scripts_dir.mkdir()
      (scripts_dir / "check_alpha.py").write_text("", encoding="utf-8")
      workflow_path = root / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  verify:",
            "    steps:",
            "      - run: python3 scripts/check_alpha.py",
          ]
        ),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_workflow_script_refs.py",
          "--workflow",
          str(workflow_path),
          "--scripts-dir",
          str(scripts_dir),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
