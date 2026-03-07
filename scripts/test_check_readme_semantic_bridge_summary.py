#!/usr/bin/env python3
"""Unit tests for the README semantic-bridge summary sync guard."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import textwrap
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_readme_semantic_bridge_summary import (  # noqa: E402
  ReadmeSemanticBridgeSummaryError,
  extract_link1_operations,
  main,
  validate_summary,
)
from check_semantic_bridge_readiness_summary import derive_summary  # noqa: E402


def make_config() -> dict:
  return {
    "obligations": [
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "supplySemEq",
        "operation": "supply",
        "status": "assumed",
        "macroMigrated": False,
      },
      {
        "id": "OBL-SET-OWNER-SEM-EQ",
        "hypothesis": "setOwnerSemEq",
        "operation": "setOwner",
        "status": "in_progress",
        "macroMigrated": True,
      },
      {
        "id": "OBL-FLASH-LOAN-SEM-EQ",
        "hypothesis": "flashLoanSemEq",
        "operation": "flashLoan",
        "status": "discharged",
        "macroMigrated": True,
      },
    ]
  }


def make_readme(
  proven_count: int = 2,
  total: int = 3,
  assumed_count: int = 1,
  operations: str = "`setOwner`, `flashLoan`",
) -> str:
  return textwrap.dedent(
    f"""\
    # Morpho Verity

    **Link 1 proofs (stable `Morpho.*` wrapper API ↔ EDSL) are now proven for {proven_count}/{total} operations:**
    {operations}
    The remaining {assumed_count}/{total} operations still have assumed Link 1 status in
    `config/semantic-bridge-obligations.json`.
    See `Morpho/Proofs/SemanticBridgeDischarge.lean`.
    """
  )


class ReadmeSemanticBridgeSummaryTests(unittest.TestCase):
  def test_extract_link1_operations(self) -> None:
    self.assertEqual(extract_link1_operations(make_readme()), ["setOwner", "flashLoan"])

  def test_validate_summary_accepts_matching_readme(self) -> None:
    validate_summary(make_readme(), derive_summary(make_config()))

  def test_validate_summary_rejects_proven_count_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 proof summary drift",
    ):
      validate_summary(make_readme(proven_count=1), derive_summary(make_config()))

  def test_validate_summary_rejects_assumed_count_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README assumed Link 1 summary drift",
    ):
      validate_summary(make_readme(assumed_count=2), derive_summary(make_config()))

  def test_validate_summary_rejects_operation_list_drift(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "README Link 1 operation list drift",
    ):
      validate_summary(make_readme(operations="`setOwner`, `enableIrm`"), derive_summary(make_config()))

  def test_validate_summary_rejects_duplicate_operation_list_entries(self) -> None:
    with self.assertRaisesRegex(
      ReadmeSemanticBridgeSummaryError,
      "duplicate operations",
    ):
      validate_summary(make_readme(operations="`setOwner`, `setOwner`"), derive_summary(make_config()))

  def test_main_passes_for_synced_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readme_path = root / "README.md"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readme_path.write_text(make_readme(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_readme_semantic_bridge_summary.py",
          "--config",
          str(config_path),
          "--readme",
          str(readme_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
