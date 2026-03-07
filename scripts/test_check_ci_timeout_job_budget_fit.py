#!/usr/bin/env python3
"""Unit tests for CI timeout-job budget fit check."""

from __future__ import annotations

import pathlib
import subprocess
import tempfile
import unittest

import sys
from unittest import mock

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_timeout_job_budget_fit import (  # noqa: E402
  CiTimeoutJobBudgetFitError,
  collect_job_blocks,
  collect_job_run_with_timeout_literals,
  main,
  parse_job_timeout_minutes,
  validate_job_timeout_budgets,
)


class CheckCiTimeoutJobBudgetFitTests(unittest.TestCase):
  def test_collect_job_blocks(self) -> None:
    workflow = (
      "name: Test\n"
      "jobs:\n"
      "  lint:\n"
      "    timeout-minutes: 5\n"
      "    steps:\n"
      "      - run: echo lint\n"
      "  tests:\n"
      "    timeout-minutes: 10\n"
      "    steps:\n"
      "      - run: echo tests\n"
    )
    self.assertEqual(set(collect_job_blocks(workflow)), {"lint", "tests"})

  def test_parse_job_timeout_minutes(self) -> None:
    job_text = (
      "  parity-target:\n"
      "    timeout-minutes: 15\n"
      "    steps:\n"
      "      - run: echo hi\n"
    )
    self.assertEqual(parse_job_timeout_minutes("parity-target", job_text), 15)

  def test_parse_job_timeout_minutes_requires_value(self) -> None:
    job_text = (
      "  parity-target:\n"
      "    steps:\n"
      "      - run: echo hi\n"
    )
    with self.assertRaises(CiTimeoutJobBudgetFitError):
      parse_job_timeout_minutes("parity-target", job_text)

  def test_collect_job_run_with_timeout_literals_supports_line_continuation(self) -> None:
    job_text = (
      "  parity-target:\n"
      "    timeout-minutes: 15\n"
      "    steps:\n"
      "      - name: Wrapped step\n"
      "        run: ./scripts/run_with_timeout.sh \\\n"
      "          MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC 300 \"Validate\" -- python3 scripts/check.py\n"
    )
    self.assertEqual(
      collect_job_run_with_timeout_literals(job_text),
      [("MORPHO_PARITY_TARGET_VALIDATE_TIMEOUT_SEC", 300)],
    )

  def test_validate_job_timeout_budgets_reports_oversized_budget(self) -> None:
    job_blocks = {
      "parity-target": (
        "  parity-target:\n"
        "    timeout-minutes: 15\n"
        "    steps:\n"
        "      - name: Oversized step\n"
        "        run: ./scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_TEST_TIMEOUT_SEC 901 \"Tests\" -- python3 -m unittest\n"
      )
    }
    self.assertEqual(
      validate_job_timeout_budgets(job_blocks),
      [
        "job parity-target timeout-minutes=15 (900s) is lower than "
        "MORPHO_PARITY_TARGET_TEST_TIMEOUT_SEC=901s"
      ],
    )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    script = SCRIPT_DIR / "check_ci_timeout_job_budget_fit.py"
    with tempfile.TemporaryDirectory() as tmp_dir:
      result = subprocess.run(
        [sys.executable, str(script)],
        cwd=tmp_dir,
        capture_output=True,
        text=True,
        check=False,
      )
    self.assertEqual(result.returncode, 0, result.stderr)
    self.assertIn("ci-timeout-job-budget-fit check: OK", result.stdout)

  def test_main_reports_failure_for_oversized_budget(self) -> None:
    workflow = (
      "jobs:\n"
      "  parity-target:\n"
      "    timeout-minutes: 15\n"
      "    steps:\n"
      "      - name: Oversized step\n"
      "        run: ./scripts/run_with_timeout.sh MORPHO_PARITY_TARGET_TEST_TIMEOUT_SEC 901 \"Tests\" -- python3 -m unittest\n"
    )
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(workflow, encoding="utf-8")
      with self.assertRaises(SystemExit) as exc:
        with mock.patch.object(
          sys,
          "argv",
          ["check_ci_timeout_job_budget_fit.py", "--workflow", str(workflow_path)],
        ):
          main()
    self.assertEqual(exc.exception.code, 1)


if __name__ == "__main__":
  unittest.main()
