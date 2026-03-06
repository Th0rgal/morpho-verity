#!/usr/bin/env python3
"""Unit tests for CI script timeout-wrapper coverage guard."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_script_timeout_wrapper_coverage import (  # noqa: E402
  collect_workflow_script_references,
  collect_wrapped_script_targets,
  main,
)


class CheckCiScriptTimeoutWrapperCoverageTests(unittest.TestCase):
  def test_collect_workflow_script_references(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/install_lean.sh",
        "run: python3 scripts/check_alpha.py",
        "run: ./scripts/run_with_timeout.sh M 1 d -- ./scripts/install_solc.sh 0.8.28",
      ]
    )
    self.assertEqual(
      collect_workflow_script_references(workflow_text),
      {"install_lean.sh", "check_alpha.py", "run_with_timeout.sh", "install_solc.sh"},
    )

  def test_collect_wrapped_script_targets(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 d -- python3 scripts/check_alpha.py",
        "run: ./scripts/run_with_timeout.sh M 1 d -- ./scripts/install_solc.sh 0.8.28",
        "run: ../scripts/run_with_timeout.sh M 1 d -- ./scripts/install_lean.sh",
      ]
    )
    self.assertEqual(
      collect_wrapped_script_targets(workflow_text),
      {"check_alpha.py", "install_solc.sh", "install_lean.sh"},
    )

  def test_main_passes_when_all_scripts_are_wrapped(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 d -- ./scripts/install_lean.sh",
        "run: ./scripts/run_with_timeout.sh M 1 d -- python3 scripts/check_alpha.py",
      ]
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = ["check_ci_script_timeout_wrapper_coverage.py", "--workflow", str(workflow)]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_on_unwrapped_script(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/install_lean.sh",
        "run: ./scripts/run_with_timeout.sh M 1 d -- python3 scripts/check_alpha.py",
      ]
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = ["check_ci_script_timeout_wrapper_coverage.py", "--workflow", str(workflow)]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

  def test_main_passes_for_allowlisted_unwrapped_script(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/install_lean.sh",
        "run: ./scripts/run_with_timeout.sh M 1 d -- python3 scripts/check_alpha.py",
      ]
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_script_timeout_wrapper_coverage.py",
          "--workflow",
          str(workflow),
          "--allow-unwrapped",
          "install_lean.sh",
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_on_stale_allowlist(self) -> None:
    workflow_text = "run: ./scripts/run_with_timeout.sh M 1 d -- ./scripts/install_lean.sh\n"
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_script_timeout_wrapper_coverage.py",
          "--workflow",
          str(workflow),
          "--allow-unwrapped",
          "install_solc.sh",
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
