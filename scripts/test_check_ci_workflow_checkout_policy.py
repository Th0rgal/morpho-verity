#!/usr/bin/env python3
"""Unit tests for workflow checkout policy checker."""

from __future__ import annotations

import os
import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_ci_workflow_checkout_policy import (  # noqa: E402
  collect_checkout_steps,
  main,
  validate_checkout_steps,
)


class CollectCheckoutStepsTests(unittest.TestCase):
  def test_collects_checkout_steps_and_with_values(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  build:",
        "    steps:",
        "      - name: Checkout source",
        "        uses: actions/checkout@v5",
        "        with:",
        "          fetch-depth: 1",
        "          persist-credentials: false",
      ]
    )

    self.assertEqual(
      collect_checkout_steps(workflow_text),
      [
        {
          "name": "Checkout source",
          "uses": "actions/checkout@v5",
          "with": {
            "fetch-depth": "1",
            "persist-credentials": "false",
          },
        }
      ],
    )

  def test_rejects_nested_with_structures(self) -> None:
    workflow_text = "\n".join(
      [
        "jobs:",
        "  build:",
        "    steps:",
        "      - name: Checkout source",
        "        uses: actions/checkout@v5",
        "        with:",
        "          fetch-depth:",
        "            nested: 1",
      ]
    )

    with self.assertRaisesRegex(
      RuntimeError,
      "checkout step Checkout source key fetch-depth must have a non-empty literal value",
    ):
      collect_checkout_steps(workflow_text)


class ValidateCheckoutStepsTests(unittest.TestCase):
  def test_accepts_explicit_safe_checkout_defaults(self) -> None:
    self.assertEqual(
      validate_checkout_steps(
        [
          {
            "name": "Checkout",
            "uses": "actions/checkout@v5",
            "with": {
              "fetch-depth": "1",
              "persist-credentials": "false",
            },
          }
        ]
      ),
      [],
    )

  def test_rejects_missing_persist_credentials(self) -> None:
    self.assertEqual(
      validate_checkout_steps(
        [
          {
            "name": "Checkout",
            "uses": "actions/checkout@v5",
            "with": {"fetch-depth": "1"},
          }
        ]
      ),
      ["checkout step Checkout must set with.persist-credentials: false"],
    )

  def test_rejects_wrong_fetch_depth(self) -> None:
    self.assertEqual(
      validate_checkout_steps(
        [
          {
            "name": "Checkout",
            "uses": "actions/checkout@v5",
            "with": {
              "fetch-depth": "0",
              "persist-credentials": "false",
            },
          }
        ]
      ),
      ["checkout step Checkout must set with.fetch-depth: 1, found: 0"],
    )

  def test_rejects_expression_values(self) -> None:
    self.assertEqual(
      validate_checkout_steps(
        [
          {
            "name": "Checkout",
            "uses": "actions/checkout@v5",
            "with": {
              "fetch-depth": "${{ env.FETCH_DEPTH }}",
              "persist-credentials": "${{ env.PERSIST_CREDENTIALS }}",
            },
          }
        ]
      ),
      [
        "checkout step Checkout must set with.fetch-depth: 1, found: ${{ env.FETCH_DEPTH }}",
        "checkout step Checkout must set with.persist-credentials: false, found: "
        "${{ env.PERSIST_CREDENTIALS }}",
      ],
    )


class CliTests(unittest.TestCase):
  def test_main_reports_policy_violation(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      workflow_path = pathlib.Path(tmp_dir) / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    steps:",
            "      - name: Checkout",
            "        uses: actions/checkout@v5",
          ]
        ),
        encoding="utf-8",
      )

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_ci_workflow_checkout_policy.py"),
          "--workflow",
          str(workflow_path),
        ],
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 1)
      self.assertIn(
        "ci-workflow-checkout-policy check failed: checkout step Checkout must set "
        "with.fetch-depth: 1; checkout step Checkout must set with.persist-credentials: false",
        proc.stderr,
      )

  def test_main_passes_on_real_workflow_from_another_cwd(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      proc = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_ci_workflow_checkout_policy.py")],
        cwd=tmp_dir,
        check=False,
        capture_output=True,
        text=True,
      )

      self.assertEqual(proc.returncode, 0, proc.stderr)
      self.assertIn("ci-workflow-checkout-policy check: OK", proc.stdout)

  def test_main_function_accepts_relative_workflow_path(self) -> None:
    with tempfile.TemporaryDirectory() as tmp_dir:
      root = pathlib.Path(tmp_dir)
      workflow_dir = root / "inputs"
      workflow_dir.mkdir()
      workflow_path = workflow_dir / "verify.yml"
      workflow_path.write_text(
        "\n".join(
          [
            "jobs:",
            "  build:",
            "    steps:",
            "      - name: Checkout",
            "        uses: actions/checkout@v5",
            "        with:",
            "          fetch-depth: 1",
            "          persist-credentials: false",
          ]
        ),
        encoding="utf-8",
      )
      runner = root / "runner"
      runner.mkdir()

      old_argv = sys.argv
      old_cwd = pathlib.Path.cwd()
      try:
        sys.argv = [
          "check_ci_workflow_checkout_policy.py",
          "--workflow",
          str(pathlib.Path("..") / "inputs" / "verify.yml"),
        ]
        os.chdir(runner)
        self.assertEqual(main(), 0)
      finally:
        os.chdir(old_cwd)
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
