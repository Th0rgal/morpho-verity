#!/usr/bin/env python3
"""Unit tests for workflow run-step parser helper."""

from __future__ import annotations

import pathlib
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from workflow_run_parser import extract_workflow_run_text  # noqa: E402


class WorkflowRunParserTests(unittest.TestCase):
  def test_extract_workflow_run_text_ignores_non_run_fields(self) -> None:
    workflow_text = "\n".join(
      [
        "name: scripts/check_from_name.py",
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Example scripts/check_from_step_name.py",
        "        run: python3 scripts/check_real.py",
      ]
    )
    self.assertEqual(extract_workflow_run_text(workflow_text), "python3 scripts/check_real.py")

  def test_extract_workflow_run_text_handles_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: |",
        "          echo before",
        "          python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "echo before\npython3 scripts/check_alpha.py",
    )

  def test_extract_workflow_run_text_handles_inline_with_nested_lines(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: python3 scripts/check_alpha.py \\",
        "          --strict",
        "        env:",
        "          FOO: bar",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "python3 scripts/check_alpha.py \\\n--strict",
    )


if __name__ == "__main__":
  unittest.main()
