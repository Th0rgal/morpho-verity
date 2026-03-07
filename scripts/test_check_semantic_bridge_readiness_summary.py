#!/usr/bin/env python3
"""Unit tests for the SemanticBridgeReadiness proof-summary sync guard."""

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

from check_semantic_bridge_readiness_summary import (  # noqa: E402
  DISCHARGE_PATH_PREFIX,
  SemanticBridgeReadinessSummaryError,
  derive_summary,
  extract_discharge_path_section,
  extract_intro_section,
  extract_namespace_body,
  main,
  parse_operation_list,
  validate_summary,
)


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


def make_readiness_text(
  total: int = 3,
  link1_count: int = 2,
  assumed_count: int = 1,
  macro_migrated_count: int = 2,
  macro_pending_count: int = 1,
  link1_ops: str = "setOwner, flashLoan",
) -> str:
  return textwrap.dedent(
    f"""\
    /-!
    # Semantic Bridge Readiness

    This module tracks the semantic equivalence obligations that must be discharged
    to connect morpho-verity's invariant proofs to formally verified EVM semantics.

    ## Discharge Path (verity#1060 / #1065)

    {DISCHARGE_PATH_PREFIX}

    ## Obligation Registry
    -/

    namespace Morpho.Proofs.SemanticBridgeReadiness

    /-- All {total} semantic equivalence obligations from SolidityBridge.lean.

        Each corresponds to one Morpho operation whose Solidity equivalence
        is hypothesized in bridge proofs and must eventually be discharged
        against EVMYulLean via the verity semantic bridge. -/
    def obligations : List SemanticBridgeObligation := []

    /-- {link1_count} of {total} operations have Link 1 proven.
        Link 1 (wrapper API ↔ EDSL) is proven in `SemanticBridgeDischarge.lean`.
        These {link1_count} Link 1 operations are: {link1_ops}. -/
    theorem link1_proven_count :
        (obligations.filter (fun o => o.status != .assumed)).length = {link1_count} := by
      native_decide

    /-- There are exactly {total} semantic equivalence obligations. -/
    theorem obligation_count : obligations.length = {total} := by
      native_decide

    /-- {assumed_count} operations still have assumed status (Link 1 not yet proven). -/
    theorem assumed_count :
        (obligations.filter (fun o => o.status == .assumed)).length = {assumed_count} := by
      native_decide

    /-- {macro_migrated_count} of {total} operations have full (non-stub) macro implementations. -/
    theorem macro_migrated_count :
        (obligations.filter (fun o => o.macroMigrated)).length = {macro_migrated_count} := by
      native_decide

    /-- {macro_pending_count} operations still need macro migration before discharge. -/
    theorem macro_pending_count :
        (obligations.filter (fun o => !o.macroMigrated)).length = {macro_pending_count} := by
      native_decide
    """
  )


class SemanticBridgeReadinessSummaryTests(unittest.TestCase):
  def test_derive_summary_counts_discharged_as_link1_proven(self) -> None:
    self.assertEqual(
      derive_summary(make_config()),
      {
        "total": 3,
        "link1_count": 2,
        "link1_operations": ["setOwner", "flashLoan"],
        "assumed_count": 1,
        "macro_migrated_count": 2,
        "macro_pending_count": 1,
      },
    )

  def test_parse_operation_list_allows_none_sentinel(self) -> None:
    self.assertEqual(parse_operation_list("none"), [])

  def test_parse_operation_list_rejects_empty_text(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "operation list is empty",
    ):
      parse_operation_list("   ")

  def test_parse_operation_list_rejects_mixed_none_sentinel(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "mixes the none sentinel",
    ):
      parse_operation_list("none, setOwner")

  def test_validate_summary_accepts_matching_text(self) -> None:
    validate_summary(make_readiness_text(), derive_summary(make_config()))

  def test_extract_intro_section_returns_text_before_namespace(self) -> None:
    self.assertIn(DISCHARGE_PATH_PREFIX, extract_intro_section(make_readiness_text()))

  def test_extract_namespace_body_returns_text_after_namespace(self) -> None:
    self.assertIn("theorem obligation_count", extract_namespace_body(make_readiness_text()))

  def test_extract_discharge_path_section_returns_text_between_headers(self) -> None:
    self.assertEqual(
      extract_discharge_path_section(extract_intro_section(make_readiness_text())).strip(),
      DISCHARGE_PATH_PREFIX,
    )

  def test_validate_summary_accepts_zero_link1_operations(self) -> None:
    summary = {
      "total": 3,
      "link1_count": 0,
      "link1_operations": [],
      "assumed_count": 3,
      "macro_migrated_count": 0,
      "macro_pending_count": 3,
    }
    validate_summary(
      make_readiness_text(
        link1_count=0,
        assumed_count=3,
        macro_migrated_count=0,
        macro_pending_count=3,
        link1_ops="none",
      ),
      summary,
    )

  def test_validate_summary_rejects_link1_operation_list_drift(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "Link 1 operation list drift",
    ):
      validate_summary(
        make_readiness_text(link1_ops="setOwner, enableIrm"),
        derive_summary(make_config()),
      )

  def test_validate_summary_rejects_theorem_count_drift(self) -> None:
    readiness_text = make_readiness_text()
    readiness_text = readiness_text.replace(
      "(obligations.filter (fun o => o.status != .assumed)).length = 2",
      "(obligations.filter (fun o => o.status != .assumed)).length = 1",
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "link1_proven_count theorem drift",
    ):
      validate_summary(readiness_text, derive_summary(make_config()))

  def test_validate_summary_rejects_discharge_path_status_drift(self) -> None:
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "discharge-path upstream status drift",
    ):
      validate_summary(
        make_readiness_text().replace(
          DISCHARGE_PATH_PREFIX,
          "The Verity semantic bridge will provide, for each function `f` in a\n"
          "`verity_contract`:",
        ),
        derive_summary(make_config()),
      )

  def test_validate_summary_rejects_discharge_path_drift_hidden_after_namespace(self) -> None:
    readiness_text = make_readiness_text().replace(DISCHARGE_PATH_PREFIX, "old prefix", 1)
    readiness_text = readiness_text.replace(
      "/-- All 3 semantic equivalence obligations from SolidityBridge.lean.",
      DISCHARGE_PATH_PREFIX + "\n\n/-- All 3 semantic equivalence obligations from SolidityBridge.lean.",
      1,
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "discharge-path upstream status drift",
    ):
      validate_summary(readiness_text, derive_summary(make_config()))

  def test_validate_summary_rejects_discharge_path_drift_hidden_elsewhere_in_intro(self) -> None:
    readiness_text = make_readiness_text().replace(DISCHARGE_PATH_PREFIX, "old prefix", 1)
    readiness_text = readiness_text.replace(
      "This module tracks the semantic equivalence obligations that must be discharged\n"
      "to connect morpho-verity's invariant proofs to formally verified EVM semantics.\n",
      "This module tracks the semantic equivalence obligations that must be discharged\n"
      "to connect morpho-verity's invariant proofs to formally verified EVM semantics.\n\n"
      f"{DISCHARGE_PATH_PREFIX}\n",
      1,
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "discharge-path upstream status drift",
    ):
      validate_summary(readiness_text, derive_summary(make_config()))

  def test_validate_summary_rejects_theorem_drift_hidden_by_duplicate_match(self) -> None:
    readiness_text = make_readiness_text().replace(
      "(obligations.filter (fun o => o.status != .assumed)).length = 2",
      "(obligations.filter (fun o => o.status != .assumed)).length = 1",
      1,
    )
    readiness_text += (
      "\n/-- copied theorem text outside the real section -/\n"
      "theorem link1_proven_count :\n"
      "    (obligations.filter (fun o => o.status != .assumed)).length = 2 := by\n"
      "  native_decide\n"
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "found multiple link1_proven_count theorem matches",
    ):
      validate_summary(readiness_text, derive_summary(make_config()))

  def test_validate_summary_rejects_missing_intro_closure(self) -> None:
    readiness_text = make_readiness_text().replace(
      "    -/\n\n    namespace Morpho.Proofs.SemanticBridgeReadiness",
      "\n\n    namespace Morpho.Proofs.SemanticBridgeReadiness",
      1,
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSummaryError,
      "intro section is missing its closing `-/`",
    ):
      validate_summary(readiness_text, derive_summary(make_config()))

  def test_main_passes_for_synced_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readiness_path = root / "SemanticBridgeReadiness.lean"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readiness_path.write_text(make_readiness_text(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_semantic_bridge_readiness_summary.py",
          "--config",
          str(config_path),
          "--readiness",
          str(readiness_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
