#!/usr/bin/env python3
"""Unit tests for CI timeout-wrapper coverage guard."""

from __future__ import annotations

import io
import pathlib
import subprocess
import tempfile
import unittest
from unittest import mock

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_timeout_wrapper_coverage import (  # noqa: E402
  CiTimeoutWrapperCoverageError,
  collect_workflow_check_scripts,
  collect_wrapped_check_scripts,
  main,
  read_workflow_text,
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

  def test_collect_workflow_check_scripts_ignores_non_run_references(self) -> None:
    workflow_text = "\n".join(
      [
        "# docs: scripts/check_from_comment.py",
        "name: scripts/check_from_name.sh",
        "run: python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(collect_workflow_check_scripts(workflow_text), {"check_alpha.py"})

  def test_collect_workflow_check_scripts_ignores_yaml_comments_on_run_lines(self) -> None:
    workflow_text = "\n".join(
      [
        "run: python3 scripts/check_alpha.py # scripts/check_from_comment.py",
      ]
    )
    self.assertEqual(collect_workflow_check_scripts(workflow_text), {"check_alpha.py"})

  def test_collect_workflow_check_scripts_supports_quoted_paths(self) -> None:
    workflow_text = "\n".join(
      [
        'run: python3 "./scripts/check_alpha.py"',
        'run: "./scripts/check_beta.sh" --require solc',
      ]
    )
    self.assertEqual(
      collect_workflow_check_scripts(workflow_text),
      {"check_alpha.py", "check_beta.sh"},
    )

  def test_collect_wrapped_check_scripts(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 desc -- python3 scripts/check_alpha.py",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- python3 scripts/check_beta.py --strict",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- ./scripts/check_gamma.sh --require lean",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- bash scripts/check_delta.sh",
        "run: ./scripts/run_with_timeout.sh M 1 desc -- env sh scripts/check_epsilon.sh",
      ]
    )
    self.assertEqual(
      collect_wrapped_check_scripts(workflow_text),
      {
        "check_alpha.py",
        "check_beta.py",
        "check_gamma.sh",
        "check_delta.sh",
        "check_epsilon.sh",
      },
    )

  def test_collect_wrapped_check_scripts_does_not_cross_lines(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 desc -- python3 scripts/check_alpha.py",
        "run: ./scripts/check_beta.sh --require solc",
      ]
    )
    self.assertEqual(collect_wrapped_check_scripts(workflow_text), {"check_alpha.py"})

  def test_collect_wrapped_check_scripts_supports_line_continuation(self) -> None:
    workflow_text = "\n".join(
      [
        "run: ./scripts/run_with_timeout.sh M 1 check \\",
        "  -- python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(collect_wrapped_check_scripts(workflow_text), {"check_alpha.py"})

  def test_collect_wrapped_check_scripts_supports_quoted_paths(self) -> None:
    workflow_text = "\n".join(
      [
        'run: ./scripts/run_with_timeout.sh M 1 desc -- python3 "./scripts/check_alpha.py"',
        'run: ./scripts/run_with_timeout.sh M 1 desc -- "./scripts/check_beta.sh" --require solc',
      ]
    )
    self.assertEqual(
      collect_wrapped_check_scripts(workflow_text),
      {"check_alpha.py", "check_beta.sh"},
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
        "run: bash scripts/check_beta.sh --require solc",
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

  def test_read_workflow_text_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_bytes(b"\x80")

      with self.assertRaisesRegex(
        CiTimeoutWrapperCoverageError,
        "is not valid UTF-8",
      ):
        read_workflow_text(workflow)

  def test_collect_workflow_check_scripts_wraps_parser_errors(self) -> None:
    with mock.patch(
      "check_ci_timeout_wrapper_coverage.extract_workflow_run_text",
      side_effect=ValueError("bad workflow"),
    ):
      with self.assertRaisesRegex(
        CiTimeoutWrapperCoverageError,
        "failed to parse workflow run steps: bad workflow",
      ):
        collect_workflow_check_scripts("jobs:\n")

  def test_cli_reports_invalid_utf8_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_bytes(b"\x80")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_timeout_wrapper_coverage.py"),
          "--workflow",
          str(workflow),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("ci-timeout-wrapper-coverage check failed:", proc.stderr)
    self.assertIn("is not valid UTF-8", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_cli_reports_parser_error_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow = pathlib.Path(tmp_dir) / "verify.yml"
      workflow.write_text("jobs:\n", encoding="utf-8")

      with mock.patch(
        "check_ci_timeout_wrapper_coverage.extract_workflow_run_text",
        side_effect=ValueError("bad workflow"),
      ):
        old_argv = sys.argv
        old_stderr = sys.stderr
        try:
          sys.argv = [
            "check_ci_timeout_wrapper_coverage.py",
            "--workflow",
            str(workflow),
          ]
          stderr = io.StringIO()
          sys.stderr = stderr
          with self.assertRaises(SystemExit) as ctx:
            main()
          self.assertEqual(ctx.exception.code, 1)
        finally:
          sys.argv = old_argv
          sys.stderr = old_stderr

    self.assertIn("ci-timeout-wrapper-coverage check failed:", stderr.getvalue())
    self.assertIn("failed to parse workflow run steps: bad workflow", stderr.getvalue())
    self.assertNotIn("Traceback", stderr.getvalue())

  def test_cli_uses_repo_workflow_from_another_cwd_without_args(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_timeout_wrapper_coverage.py")],
        cwd=tmp_dir,
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 0)
    self.assertIn("ci-timeout-wrapper-coverage check: OK", proc.stdout)


if __name__ == "__main__":
  unittest.main()
