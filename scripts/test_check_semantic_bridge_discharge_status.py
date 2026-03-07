#!/usr/bin/env python3
"""Unit tests for SemanticBridgeDischarge upstream-status sync guard."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import textwrap
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_semantic_bridge_discharge_status import (  # noqa: E402
  ARCHITECTURE_SECTION_HEADER,
  DISCHARGE_SECTION_HEADER,
  EXPECTED_ARCHITECTURE_SUMMARY,
  EXPECTED_DISCHARGE_STATUS,
  EXPECTED_FLASHLOAN_ROW,
  FORBIDDEN_SNIPPETS,
  PROOF_STRATEGY_SECTION_HEADER,
  SemanticBridgeDischargeStatusError,
  extract_architecture_section,
  extract_discharge_section,
  main,
  normalize_text,
  validate_status,
)


def make_text() -> str:
  return textwrap.dedent(
    f"""\
    /-!
    # Semantic Bridge Discharge: Link 1 (Wrapper API ↔ EDSL)

    {ARCHITECTURE_SECTION_HEADER}

    {EXPECTED_ARCHITECTURE_SUMMARY}

    {PROOF_STRATEGY_SECTION_HEADER}

    {DISCHARGE_SECTION_HEADER}

    {EXPECTED_DISCHARGE_STATUS}

    | Phase | Operations | Link 1 | Links 2+3 |
    |-------|-----------|--------|-----------|
    | 1 | setOwner, setFeeRecipient | **proven** | typed-IR bridge available at pin `ad03fc64` |
    | 2 | enableIrm, enableLltv, setAuthorization | **proven** | typed-IR bridge available at pin `ad03fc64` |
    {EXPECTED_FLASHLOAN_ROW}
    | 4 | createMarket | provable | needs MappingWord bridge |
    | 5 | 11 remaining ops | blocked on macro | blocked |
    -/
    """
  )


class SemanticBridgeDischargeStatusTests(unittest.TestCase):
  def test_normalize_text_collapses_whitespace(self) -> None:
    self.assertEqual(normalize_text("alpha\n\n beta\tgamma"), "alpha beta gamma")

  def test_extract_architecture_section_returns_target_block(self) -> None:
    self.assertIn(EXPECTED_ARCHITECTURE_SUMMARY, extract_architecture_section(make_text()))

  def test_extract_architecture_section_rejects_missing_header(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing `## Architecture` section",
    ):
      extract_architecture_section(make_text().replace(ARCHITECTURE_SECTION_HEADER, "## Overview"))

  def test_extract_architecture_section_rejects_missing_boundary(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing `## Proof Strategy` boundary",
    ):
      extract_architecture_section(make_text().replace(PROOF_STRATEGY_SECTION_HEADER, "## Strategy", 1))

  def test_extract_architecture_section_rejects_inline_boundary_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing `## Proof Strategy` boundary",
    ):
      extract_architecture_section(
        make_text().replace(
          PROOF_STRATEGY_SECTION_HEADER,
          "## Strategy\n\nThe roadmap still references `## Proof Strategy` in prose.",
          1,
        )
      )

  def test_extract_discharge_section_returns_target_block(self) -> None:
    self.assertIn(EXPECTED_FLASHLOAN_ROW, extract_discharge_section(make_text()))

  def test_extract_discharge_section_rejects_missing_header(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing `## Discharge Status` section",
    ):
      extract_discharge_section(make_text().replace(DISCHARGE_SECTION_HEADER, "/-! ## Other"))

  def test_extract_discharge_section_rejects_missing_closing_delimiter(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing closing `-/`",
    ):
      extract_discharge_section(make_text().replace("-/\n", "", 1))

  def test_validate_status_accepts_matching_text(self) -> None:
    validate_status(make_text())

  def test_validate_status_accepts_wrapped_status_text(self) -> None:
    validate_status(
      make_text()
      .replace(
        "Links 2+3 are already provided upstream for the supported fragment "
        "(verity#1060 / #1065).",
        "Links 2+3 are already provided upstream for the supported fragment\n"
        "(verity#1060 / #1065).",
      )
      .replace(
        "For the supported fragment, Links 2+3 are already\nprovided upstream",
        "For the supported fragment, Links 2+3 are already\n\nprovided upstream",
      )
      .replace(
        "The\nremaining repo-local gaps are",
        "The\nremaining repo-local gaps are",
      )
    )

  def test_validate_status_rejects_missing_expected_status(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(make_text().replace(EXPECTED_DISCHARGE_STATUS, "old status"))

  def test_validate_status_rejects_missing_architecture_summary(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(make_text().replace(EXPECTED_ARCHITECTURE_SUMMARY, "old architecture"))

  def test_validate_status_rejects_missing_flashloan_row(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(make_text().replace(EXPECTED_FLASHLOAN_ROW, "| 3 | flashLoan | stale | stale |"))

  def test_validate_status_rejects_stale_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "stale bridge status text",
    ):
      validate_status(
        make_text().replace(
          EXPECTED_DISCHARGE_STATUS,
          EXPECTED_DISCHARGE_STATUS + "\n" + FORBIDDEN_SNIPPETS[0],
        )
      )

  def test_validate_status_rejects_wrapped_stale_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "stale bridge status text",
    ):
      validate_status(
        make_text().replace(
          EXPECTED_DISCHARGE_STATUS,
          EXPECTED_DISCHARGE_STATUS
          + "\nThe remaining gap (Links 2+3) connects the EDSL\n\n"
          + "execution to the compiled IR and then to EVMYulLean.",
        )
      )

  def test_validate_status_rejects_stale_architecture_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(
        make_text().replace(
          "Links 2+3 are already provided upstream for the supported fragment "
          "(verity#1060 / #1065).",
          "Links 2+3 are provided upstream for the supported fragment "
          "(verity#1060 / #1065).",
        )
      )

  def test_validate_status_rejects_drift_hidden_by_duplicate_elsewhere(self) -> None:
    stale_section = make_text().replace(EXPECTED_DISCHARGE_STATUS, "old status")
    masked_text = stale_section + "\n" + EXPECTED_DISCHARGE_STATUS + "\n" + EXPECTED_FLASHLOAN_ROW
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(masked_text)

  def test_validate_status_rejects_architecture_drift_hidden_by_duplicate_elsewhere(self) -> None:
    stale_section = make_text().replace(EXPECTED_ARCHITECTURE_SUMMARY, "old architecture")
    masked_text = stale_section + "\n" + EXPECTED_ARCHITECTURE_SUMMARY
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "missing expected text",
    ):
      validate_status(masked_text)

  def test_main_passes_for_synced_file(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      discharge_path = root / "SemanticBridgeDischarge.lean"
      discharge_path.write_text(make_text(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_semantic_bridge_discharge_status.py",
          "--discharge",
          str(discharge_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
