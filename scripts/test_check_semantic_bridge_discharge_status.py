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
  EXPECTED_DISCHARGE_STATUS,
  EXPECTED_FLASHLOAN_ROW,
  FORBIDDEN_SNIPPETS,
  SemanticBridgeDischargeStatusError,
  main,
  normalize_text,
  validate_status,
)


def make_text() -> str:
  return textwrap.dedent(
    f"""\
    /-!
    # Semantic Bridge Discharge: Link 1 (Wrapper API ↔ EDSL)

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

  def test_validate_status_accepts_matching_text(self) -> None:
    validate_status(make_text())

  def test_validate_status_accepts_wrapped_status_text(self) -> None:
    validate_status(
      make_text()
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
      validate_status(make_text() + "\n" + FORBIDDEN_SNIPPETS[0])

  def test_validate_status_rejects_wrapped_stale_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeDischargeStatusError,
      "stale bridge status text",
    ):
      validate_status(
        make_text()
        + "\nThe remaining gap (Links 2+3) connects the EDSL\n\n"
        + "execution to the compiled IR and then to EVMYulLean."
      )

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
