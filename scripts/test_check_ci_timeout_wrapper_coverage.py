#!/usr/bin/env python3
"""Unit tests for CI timeout-wrapper coverage guard."""

from __future__ import annotations

import pathlib
import tempfile
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_timeout_wrapper_coverage import (  # noqa: E402
  collect_workflow_check_scripts,
  collect_wrapped_check_scripts,
  main,
)


class CheckCiTimeoutWrapperCoverageTests(unittest.TestCase):
  def test_collect_workflow_check_scripts(self) -> None:
    workflow_text = "\n".join(
      [
        "run: python3 scripts/check_alpha.py",
        "run: python3 scripts/check_beta.py --strict",
        "run: ./scripts/check_gamma.sh --require lean",
      ]
    )
    self.assertEqual(
      collect_workflow_check_scripts(workflow_text),
      {"check_alpha.py", "check_beta.py", "check_gamma.sh"},
    )

  def test_collect_wrapped_check_scripts(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 desc -- python3 scripts/check_alpha.py",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- python3 scripts/check_beta.py --strict",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- ./scripts/check_gamma.sh --require lean",
      ]
    )
    self.assertEqual(
      collect_wrapped_check_scripts(workflow_text),
      {"check_alpha.py", "check_beta.py", "check_gamma.sh"},
    )

  def test_main_passes_when_all_check_scripts_are_wrapped(self) -> None:
    workflow_text = (
      "run: ./scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "
      '"check" -- python3 scripts/check_alpha.py\n'
      "run: ./scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "
      '"check" -- python3 scripts/check_beta.py --json-out out/x.json\n'
      "run: ./scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 "
      '"check" -- ./scripts/check_gamma.sh --require lean\n'
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = ["check_ci_timeout_wrapper_coverage.py", "--workflow", str(workflow)]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_check_script_is_unwrapped(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 check -- python3 scripts/check_alpha.py",
        "run: ./scripts/check_beta.sh --require solc",
      ]
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = ["check_ci_timeout_wrapper_coverage.py", "--workflow", str(workflow)]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv

  def test_main_passes_for_allowlisted_unwrapped_script(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 check -- python3 scripts/check_alpha.py",
        "run: ./scripts/check_beta.sh --require solc",
      ]
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_timeout_wrapper_coverage.py",
          "--workflow",
          str(workflow),
          "--allow-unwrapped",
          "check_beta.sh",
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_on_stale_allowlist(self) -> None:
    workflow_text = "run: ./scripts/run_with_timeout.sh M 1 check -- python3 scripts/check_alpha.py\n"
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text(workflow_text, encoding="utf-8")
      old_argv = sys.argv
      try:
        sys.argv = [
          "check_ci_timeout_wrapper_coverage.py",
          "--workflow",
          str(workflow),
          "--allow-unwrapped",
          "check_beta.py",
        ]
        with self.assertRaises(SystemExit) as ctx:
          main()
        self.assertEqual(ctx.exception.code, 1)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
