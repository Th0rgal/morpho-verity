#!/usr/bin/env python3
"""Unit tests for RELEASE_CRITERIA.md status sync guard."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_release_criteria_status import (  # noqa: E402
  EXPECTED_PARTIALLY_ENFORCED_ITEMS,
  EXPECTED_WORKFLOW_RUN_LINES,
  EXPECTED_WORKFLOW_STEPS,
  FORBIDDEN_NOT_YET_ENFORCED_ITEMS,
  ReleaseCriteriaStatusError,
  filter_tracked_items,
  main,
  parse_numbered_items,
  validate_doc_status,
  validate_workflow,
)


def make_doc(
  *,
  partially_enforced_items: list[str] | None = None,
  not_yet_enforced_items: list[str] | None = None,
) -> str:
  partially_enforced_items = (
    EXPECTED_PARTIALLY_ENFORCED_ITEMS
    if partially_enforced_items is None
    else partially_enforced_items
  )
  not_yet_enforced_items = (
    ["strict `yul-identity-check` (zero structural AST mismatch for supported fragment)."]
    if not_yet_enforced_items is None
    else not_yet_enforced_items
  )
  partial_lines = [f"{index}. {item}" for index, item in enumerate(partially_enforced_items, start=1)]
  pending_lines = [f"{index}. {item}" for index, item in enumerate(not_yet_enforced_items, start=1)]
  return "\n".join([
    "# Release Criteria",
    "",
    "## Status",
    "",
    "Partially enforced:",
    *partial_lines,
    "",
    "Not yet enforced:",
    *pending_lines,
    "",
    "## Required Gates (Target State)",
    "",
    "1. Placeholder gate.",
    "",
  ])


def make_workflow(*, steps: list[str] | None = None) -> str:
  steps = EXPECTED_WORKFLOW_STEPS if steps is None else steps
  return "\n".join([
    "jobs:",
    "  parity-target:",
    "    steps:",
    *[
      "\n".join([
        f"      - name: {step}",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES.get(step, 'echo placeholder')}",
      ])
      for step in steps
    ],
    "",
  ])


class ReleaseCriteriaStatusTests(unittest.TestCase):
  def test_parse_numbered_items(self) -> None:
    self.assertEqual(parse_numbered_items("1. alpha\n2. beta\n"), ["alpha", "beta"])

  def test_filter_tracked_items(self) -> None:
    self.assertEqual(
      filter_tracked_items([
        "parity-target tuple drift gate is enforced in CI.",
        EXPECTED_PARTIALLY_ENFORCED_ITEMS[0],
        EXPECTED_PARTIALLY_ENFORCED_ITEMS[-1],
      ]),
      [
        EXPECTED_PARTIALLY_ENFORCED_ITEMS[0],
        EXPECTED_PARTIALLY_ENFORCED_ITEMS[-1],
      ],
    )

  def test_validate_doc_status_passes(self) -> None:
    validate_doc_status(make_doc())

  def test_validate_doc_status_rejects_missing_partially_enforced_item(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing expected partially-enforced status items",
    ):
      validate_doc_status(make_doc(partially_enforced_items=EXPECTED_PARTIALLY_ENFORCED_ITEMS[:-1]))

  def test_validate_doc_status_rejects_stale_not_yet_enforced_item(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "still lists already-enforced gates as not yet enforced",
    ):
      validate_doc_status(
        make_doc(
          not_yet_enforced_items=[
            "strict `yul-identity-check` (zero structural AST mismatch for supported fragment).",
            FORBIDDEN_NOT_YET_ENFORCED_ITEMS[0],
          ]
        )
      )

  def test_validate_doc_status_rejects_unexpected_tracked_partial_item(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "unexpected semantic-bridge/equivalence partially-enforced items",
    ):
      validate_doc_status(
        make_doc(
          partially_enforced_items=[
            *EXPECTED_PARTIALLY_ENFORCED_ITEMS,
            "Legacy semantic-bridge status bullet that should no longer appear.",
          ]
        )
      )

  def test_validate_doc_status_rejects_duplicate_tracked_partial_item(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "duplicate semantic-bridge/equivalence partially-enforced items",
    ):
      validate_doc_status(
        make_doc(
          partially_enforced_items=[
            *EXPECTED_PARTIALLY_ENFORCED_ITEMS,
            EXPECTED_PARTIALLY_ENFORCED_ITEMS[0],
          ]
        )
      )

  def test_validate_workflow_passes(self) -> None:
    validate_workflow(make_workflow())

  def test_validate_workflow_rejects_missing_step(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing workflow steps",
    ):
      validate_workflow(make_workflow(steps=EXPECTED_WORKFLOW_STEPS[:-1]))

  def test_validate_workflow_rejects_commented_step(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing workflow steps",
    ):
      validate_workflow("\n".join([
        "jobs:",
        "  parity-target:",
        "    steps:",
        "      # - name: Validate semantic bridge obligations",
        f"      - name: {EXPECTED_WORKFLOW_STEPS[1]}",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[1]]}",
        "",
      ]))

  def test_validate_workflow_rejects_duplicate_step(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "duplicates workflow steps",
    ):
      validate_workflow(make_workflow(
        steps=[
          EXPECTED_WORKFLOW_STEPS[0],
          EXPECTED_WORKFLOW_STEPS[0],
          *EXPECTED_WORKFLOW_STEPS[1:],
        ]
      ))

  def test_validate_workflow_rejects_wrong_run_command(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "drifted workflow commands",
    ):
      validate_workflow("\n".join([
        "jobs:",
        "  parity-target:",
        "    steps:",
        f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
        "        run: echo noop",
        *[
          "\n".join([
            f"      - name: {step}",
            f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
          ])
          for step in EXPECTED_WORKFLOW_STEPS[1:]
        ],
        "",
      ]))

  def test_main_passes_on_synced_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      doc_path = root / "RELEASE_CRITERIA.md"
      workflow_path = root / "verify.yml"
      doc_path.write_text(make_doc(), encoding="utf-8")
      workflow_path.write_text(make_workflow(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_release_criteria_status.py",
          "--doc",
          str(doc_path),
          "--workflow",
          str(workflow_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
