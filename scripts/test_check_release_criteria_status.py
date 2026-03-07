#!/usr/bin/env python3
"""Unit tests for RELEASE_CRITERIA.md status sync guard."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import tempfile
import unittest
from unittest import mock


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_release_criteria_status import (  # noqa: E402
  EXPECTED_PARTIALLY_ENFORCED_ITEMS,
  EXPECTED_WORKFLOW_RUN_LINES,
  EXPECTED_WORKFLOW_STEPS,
  FORBIDDEN_NOT_YET_ENFORCED_ITEMS,
  ReleaseCriteriaStatusError,
  extract_section,
  filter_tracked_items,
  main,
  parse_numbered_items,
  read_text,
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
  def test_extract_section(self) -> None:
    section = extract_section(
      make_doc(),
      "Partially enforced:",
      "Not yet enforced:",
    )
    self.assertIn(EXPECTED_PARTIALLY_ENFORCED_ITEMS[0], section)

  def test_parse_numbered_items(self) -> None:
    self.assertEqual(parse_numbered_items("1. alpha\n2. beta\n"), ["alpha", "beta"])

  def test_parse_numbered_items_allows_wrapped_continuations(self) -> None:
    self.assertEqual(
      parse_numbered_items(
        "1. alpha\n"
        "   beta\n"
        "2. gamma\n"
        "   delta\n"
      ),
      ["alpha beta", "gamma delta"],
    )

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

  def test_validate_doc_status_rejects_stale_upstream_bridge_wording(self) -> None:
    stale_items = EXPECTED_PARTIALLY_ENFORCED_ITEMS.copy()
    stale_items[3] = (
      "Machine-tracked `semantic-bridge-obligations` status is enforced in CI "
      "(`scripts/check_semantic_bridge_obligations.py`); until all 18 obligations "
      "are discharged via the upstream verity hybrid migration path (verity#1060 / "
      "verity#1065), Solidity equivalence status remains \"Conditional\" rather "
      "than \"Proved\"."
    )
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing expected partially-enforced status items",
    ):
      validate_doc_status(make_doc(partially_enforced_items=stale_items))

  def test_validate_doc_status_allows_wrapped_tracked_partial_item(self) -> None:
    wrapped_items = EXPECTED_PARTIALLY_ENFORCED_ITEMS.copy()
    wrapped_items[3] = (
      "Machine-tracked `semantic-bridge-obligations` status is enforced in CI "
      "(`scripts/check_semantic_bridge_obligations.py`); Links 2+3 are already "
      "provided upstream for the supported fragment,\n"
      "   and Solidity equivalence status remains \"Conditional\" rather than "
      "\"Proved\" until all 18 obligations are discharged across the remaining "
      "repo-local Link 1 proofs and macro/frontend blockers."
    )
    validate_doc_status(make_doc(partially_enforced_items=wrapped_items))

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

  def test_validate_doc_status_rejects_drift_hidden_behind_inline_section_markers(self) -> None:
    drifted_doc = make_doc(partially_enforced_items=EXPECTED_PARTIALLY_ENFORCED_ITEMS[:-1])
    fake_section = "\n".join([
      "# Notes",
      "",
      "Historical notes about Partially enforced:",
      *[
        f"{index}. {item}"
        for index, item in enumerate(EXPECTED_PARTIALLY_ENFORCED_ITEMS, start=1)
      ],
      "",
      "Historical notes about Not yet enforced:",
      "1. strict `yul-identity-check` (zero structural AST mismatch for supported fragment).",
      "",
    ])
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing expected partially-enforced status items",
    ):
      validate_doc_status(f"{fake_section}{drifted_doc}")

  def test_validate_doc_status_rejects_fake_status_block_masking_missing_real_labels(self) -> None:
    drifted_doc = make_doc().replace("Partially enforced:\n", "", 1).replace(
      "\nNot yet enforced:\n",
      "\n",
      1,
    )
    fake_status = "\n".join([
      "## Status",
      "",
      "Partially enforced:",
      *[
        f"{index}. {item}"
        for index, item in enumerate(EXPECTED_PARTIALLY_ENFORCED_ITEMS, start=1)
      ],
      "",
      "Not yet enforced:",
      "1. strict `yul-identity-check` (zero structural AST mismatch for supported fragment).",
      "",
    ])
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "missing `## Status` section|missing `Partially enforced:` section",
    ):
      validate_doc_status(f"{fake_status}{drifted_doc}")

  def test_validate_doc_status_accepts_crlf_line_endings(self) -> None:
    validate_doc_status(make_doc().replace("\n", "\r\n"))

  def test_validate_workflow_passes(self) -> None:
    validate_workflow(make_workflow())

  def test_validate_workflow_accepts_trailing_comments_on_step_names(self) -> None:
    validate_workflow("\n".join([
      "jobs:",
      "  parity-target:",
      "    steps:",
      *[
        "\n".join([
          f"      - name: {step} # tracked workflow step",
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
        ])
        for step in EXPECTED_WORKFLOW_STEPS
      ],
      "",
    ]))

  def test_validate_workflow_accepts_tagged_step_names(self) -> None:
    validate_workflow("\n".join([
      "jobs:",
      "  parity-target:",
      "    steps:",
      *[
        "\n".join([
          f'      - name: !!str "{step}"',
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
        ])
        for step in EXPECTED_WORKFLOW_STEPS
      ],
      "",
    ]))

  def test_validate_workflow_accepts_tagged_and_anchored_run_scalars(self) -> None:
    validate_workflow("\n".join([
      "jobs:",
      "  parity-target:",
      "    steps:",
      f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
      f'        run: !!str {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[0]]}',
      f"      - name: {EXPECTED_WORKFLOW_STEPS[1]}",
      f"        run: &shared_run {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[1]]}",
      *[
        "\n".join([
          f"      - name: {step}",
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
        ])
        for step in EXPECTED_WORKFLOW_STEPS[2:]
      ],
      "",
    ]))

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

  def test_validate_workflow_rejects_duplicate_step_without_second_run(self) -> None:
    with self.assertRaisesRegex(
      ReleaseCriteriaStatusError,
      "duplicates workflow steps",
    ):
      validate_workflow("\n".join([
        "jobs:",
        "  parity-target:",
        "    steps:",
        f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
        f"        run: {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[0]]}",
        f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
        "        uses: actions/checkout@v5",
        *[
          "\n".join([
            f"      - name: {step}",
            f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
          ])
          for step in EXPECTED_WORKFLOW_STEPS[1:]
        ],
        "",
      ]))

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

  def test_validate_workflow_wraps_parser_errors(self) -> None:
    with mock.patch(
      "check_release_criteria_status.extract_named_step_runs",
      side_effect=ValueError("bad workflow"),
    ):
      with self.assertRaisesRegex(
        ReleaseCriteriaStatusError,
        "failed to parse verify workflow: bad workflow",
      ):
        validate_workflow(make_workflow())

  def test_validate_workflow_ignores_unnamed_run_step_after_tracked_step(self) -> None:
    validate_workflow("\n".join([
      "jobs:",
      "  parity-target:",
      "    steps:",
      f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
      f"        run: {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[0]]}",
      "      - run: echo helper step",
      *[
        "\n".join([
          f"      - name: {step}",
          f"        run: {EXPECTED_WORKFLOW_RUN_LINES[step]}",
        ])
        for step in EXPECTED_WORKFLOW_STEPS[1:]
      ],
      "",
    ]))

  def test_validate_workflow_ignores_nested_run_mapping_under_tracked_step(self) -> None:
    validate_workflow("\n".join([
      "jobs:",
      "  parity-target:",
      "    steps:",
      f"      - name: {EXPECTED_WORKFLOW_STEPS[0]}",
      "        with:",
      "          run: echo helper metadata",
      f"        run: {EXPECTED_WORKFLOW_RUN_LINES[EXPECTED_WORKFLOW_STEPS[0]]}",
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

  def test_read_text_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "RELEASE_CRITERIA.md"
      path.write_bytes(b"\xff")

      with self.assertRaisesRegex(
        ReleaseCriteriaStatusError,
        "failed to decode release criteria document",
      ):
        read_text(path, context="release criteria document")

  def test_main_rejects_invalid_utf8_doc_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      doc_path = root / "RELEASE_CRITERIA.md"
      workflow_path = root / "verify.yml"
      doc_path.write_bytes(b"\xff")
      workflow_path.write_text(make_workflow(), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_release_criteria_status.py"),
          "--doc",
          str(doc_path),
          "--workflow",
          str(workflow_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("release-criteria-status check failed:", proc.stderr)
    self.assertIn("failed to decode release criteria document", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_main_rejects_invalid_utf8_workflow_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      doc_path = root / "RELEASE_CRITERIA.md"
      workflow_path = root / "verify.yml"
      doc_path.write_text(make_doc(), encoding="utf-8")
      workflow_path.write_bytes(b"\xff")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_release_criteria_status.py"),
          "--doc",
          str(doc_path),
          "--workflow",
          str(workflow_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("release-criteria-status check failed:", proc.stderr)
    self.assertIn("failed to decode workflow", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)

  def test_main_rejects_missing_workflow_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      doc_path = root / "RELEASE_CRITERIA.md"
      workflow_path = root / "verify.yml"
      doc_path.write_text(make_doc(), encoding="utf-8")

      proc = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_release_criteria_status.py"),
          "--doc",
          str(doc_path),
          "--workflow",
          str(workflow_path),
        ],
        capture_output=True,
        text=True,
        check=False,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("release-criteria-status check failed:", proc.stderr)
    self.assertIn("failed to read workflow", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
