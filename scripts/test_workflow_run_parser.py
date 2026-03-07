#!/usr/bin/env python3
"""Unit tests for workflow run-step parser helper."""

from __future__ import annotations

import pathlib
import unittest

import sys

SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
ROOT = SCRIPT_DIR.parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from workflow_run_parser import (  # noqa: E402
  extract_named_step_runs,
  extract_workflow_env_literals,
  extract_workflow_run_text,
)


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

  def test_extract_workflow_run_text_handles_inline_run_step_item(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - run: python3 scripts/check_alpha.py",
        "      - uses: actions/checkout@v5",
      ]
    )
    self.assertEqual(extract_workflow_run_text(workflow_text), "python3 scripts/check_alpha.py")

  def test_extract_workflow_run_text_keeps_top_level_run_fixtures(self) -> None:
    workflow_text = "\n".join(
      [
        "run: python3 scripts/check_alpha.py",
        "run: ./scripts/check_beta.sh",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "python3 scripts/check_alpha.py\n./scripts/check_beta.sh",
    )

  def test_extract_workflow_run_text_ignores_nested_list_items_within_step(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Restore cache",
        "        with:",
        "          path:",
        "            - ~/.elan",
        "            - ~/.lake",
        "        run: python3 scripts/check_real.py",
      ]
    )
    self.assertEqual(extract_workflow_run_text(workflow_text), "python3 scripts/check_real.py")

  def test_extract_workflow_run_text_ignores_non_step_run_mapping(self) -> None:
    workflow_text = "\n".join(
      [
        "defaults:",
        "  run:",
        "    shell: bash",
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Real step",
        "        run: python3 scripts/check_real.py",
      ]
    )
    self.assertEqual(extract_workflow_run_text(workflow_text), "python3 scripts/check_real.py")

  def test_extract_workflow_run_text_ignores_nested_run_field_within_step_mapping(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Composite metadata",
        "        with:",
        "          run: python3 scripts/check_fake.py",
        "        run: python3 scripts/check_real.py",
      ]
    )
    self.assertEqual(extract_workflow_run_text(workflow_text), "python3 scripts/check_real.py")

  def test_extract_named_step_runs_ignores_nested_run_field_within_step_mapping(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        with:",
        "          run: python3 scripts/check_fake.py",
        "        run: python3 scripts/check_real.py",
        "      - env:",
        "          run: python3 scripts/check_helper.py",
        "        run: echo helper",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      ({"Validate alpha": 1}, {"Validate alpha": ["python3 scripts/check_real.py"]}),
    )

  def test_extract_named_step_runs_handles_inline_continuation_lines(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        run: python3 scripts/check_alpha.py \\",
        "          --strict",
        "        env:",
        "          FOO: bar",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      ({"Validate alpha": 1}, {"Validate alpha": ["python3 scripts/check_alpha.py \\\n--strict"]}),
    )

  def test_extract_named_step_runs_handles_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        run: |",
        "          echo before",
        "          python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      ({"Validate alpha": 1}, {"Validate alpha": ["echo before\npython3 scripts/check_alpha.py"]}),
    )

  def test_extract_workflow_run_text_handles_folded_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: >",
        "          python3 scripts/check_alpha.py",
        "          --strict",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "python3 scripts/check_alpha.py --strict",
    )

  def test_extract_named_step_runs_handles_folded_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        run: >",
        "          python3 scripts/check_alpha.py",
        "          --strict",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      ({"Validate alpha": 1}, {"Validate alpha": ["python3 scripts/check_alpha.py --strict"]}),
    )

  def test_extract_workflow_run_text_preserves_more_indented_lines_in_folded_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: >",
        "          cat <<'EOF' > sample.txt",
        "            preserved indent",
        "          EOF",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "cat <<'EOF' > sample.txt\n  preserved indent\nEOF",
    )

  def test_extract_workflow_run_text_preserves_relative_indentation_in_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: |",
        "          cat <<'EOF' > sample.txt",
        "            preserved indent",
        "          EOF",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "cat <<'EOF' > sample.txt\n  preserved indent\nEOF",
    )

  def test_extract_named_step_runs_preserves_relative_indentation_in_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        run: |",
        "          cat <<'EOF' > sample.txt",
        "            preserved indent",
        "          EOF",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      ({"Validate alpha": 1}, {"Validate alpha": ["cat <<'EOF' > sample.txt\n  preserved indent\nEOF"]}),
    )

  def test_extract_workflow_run_text_handles_empty_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Step",
        "        run: |",
        "      - name: Next",
        "        run: python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(
      extract_workflow_run_text(workflow_text),
      "\npython3 scripts/check_alpha.py",
    )

  def test_extract_named_step_runs_handles_empty_block_scalar(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        run: |",
        "      - name: Validate beta",
        "        run: python3 scripts/check_beta.py",
      ]
    )
    self.assertEqual(
      extract_named_step_runs(workflow_text),
      (
        {"Validate alpha": 1, "Validate beta": 1},
        {"Validate alpha": [""], "Validate beta": ["python3 scripts/check_beta.py"]},
      ),
    )

  def test_extract_workflow_env_literals_collects_workflow_job_and_step_env(self) -> None:
    workflow_text = "\n".join(
      [
        "env:",
        "  WORKFLOW_TIMEOUT_SEC: \"10\"",
        "jobs:",
        "  test:",
        "    env:",
        "      JOB_TIMEOUT_SEC: \"20\"",
        "    steps:",
        "      - name: Validate alpha",
        "        env:",
        "          STEP_TIMEOUT_SEC: \"30\"",
        "        run: python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(
      extract_workflow_env_literals(workflow_text),
      {
        "WORKFLOW_TIMEOUT_SEC": ["10"],
        "JOB_TIMEOUT_SEC": ["20"],
        "STEP_TIMEOUT_SEC": ["30"],
      },
    )

  def test_extract_workflow_env_literals_ignores_nested_non_env_metadata(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  test:",
        "    steps:",
        "      - name: Validate alpha",
        "        with:",
        "          env:",
        "            FAKE_TIMEOUT_SEC: \"99\"",
        "        env:",
        "          REAL_TIMEOUT_SEC: \"10\"",
        "        run: python3 scripts/check_alpha.py",
      ]
    )
    self.assertEqual(extract_workflow_env_literals(workflow_text), {"REAL_TIMEOUT_SEC": ["10"]})

  def test_extract_workflow_run_text_covers_real_verify_workflow(self) -> None:
    workflow_text = (ROOT / ".github" / "workflows" / "verify.yml").read_text(encoding="utf-8")
    run_text = extract_workflow_run_text(workflow_text)
    self.assertIn("python3 scripts/check_ci_check_coverage.py", run_text)
    self.assertIn("python3 -m unittest discover -s scripts -p 'test_*.py'", run_text)
    self.assertIn("./scripts/install_solc.sh 0.8.28", run_text)
    self.assertIn("set -euo pipefail", run_text)
    self.assertIn('for t in scripts/test_*.sh; do', run_text)
    self.assertIn('python3 scripts/report_yul_identity_gap.py --max-diff-lines 12000 --enforce-configured-gate', run_text)
    self.assertNotIn("actions/cache@v4", run_text)
    self.assertNotIn("${{ hashFiles('config/parity-target.json') }}", run_text)


if __name__ == "__main__":
  unittest.main()
