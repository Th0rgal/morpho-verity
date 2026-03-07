#!/usr/bin/env python3
"""Unit tests for EQUIVALENCE_OBLIGATIONS.md sync guard."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_equivalence_obligations_doc import (  # noqa: E402
  MACRO_STATUS_PREFIX,
  UPSTREAM_BRIDGE_PREFIX,
  EquivalenceObligationsDocError,
  expected_table_lines,
  extract_issue_summary_table,
  main,
  parse_operation_list,
  validate_status_summary,
  validate_issue_summary_table,
)
from check_issue_blocker_clusters import validate_issue_clusters  # noqa: E402
from check_semantic_bridge_readiness_summary import derive_summary  # noqa: E402


def make_config() -> dict:
  return {
    "obligations": [
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "supplySemEq",
        "issue": 123,
        "operation": "supply",
        "status": "assumed",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20", "internalCall"],
      },
      {
        "id": "OBL-WITHDRAW-SEM-EQ",
        "hypothesis": "withdrawSemEq",
        "issue": 123,
        "operation": "withdraw",
        "status": "assumed",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["erc20", "internalCall"],
      },
      {
        "id": "OBL-LIQUIDATE-SEM-EQ",
        "hypothesis": "liquidateSemEq",
        "issue": 124,
        "operation": "liquidate",
        "status": "in_progress",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20", "externalWithReturn"],
      },
      {
        "id": "OBL-SET-OWNER-SEM-EQ",
        "hypothesis": "setOwnerSemEq",
        "operation": "setOwner",
        "status": "in_progress",
        "macroMigrated": True,
      },
    ],
    "issueClusters": [
      {"issue": 123, "title": "Core flows"},
      {"issue": 124, "title": "Collateral flows"},
    ],
  }


def make_doc(table_lines: list[str], *, summary: dict[str, object] | None = None) -> str:
  summary = derive_summary(make_config()) if summary is None else summary
  link1_operations = ", ".join(summary["link1_operations"]) or "none"
  return "\n".join([
    "# Equivalence",
    "",
    "Some intro.",
    "",
    "## Status",
    "",
    f"{UPSTREAM_BRIDGE_PREFIX} This eliminates the hand-rolled `interpretSpec` from the TCB where the macro frontend can lower the contract successfully.",
    "",
    f"{summary['link1_count']}/{summary['total']} obligations have Link 1 (stable `Morpho.*` wrapper API ↔ EDSL) proven: {link1_operations}. The proofs are in",
    "`Morpho/Proofs/SemanticBridgeDischarge.lean`.",
    "",
    f"{MACRO_STATUS_PREFIX} {summary['macro_migrated_count']}/{summary['total']} operations are macro-migrated; the remaining {summary['macro_pending_count']} are blocked on upstream macro primitive support (internal calls, ERC20 module, callbacks, oracle calls, 2D struct access).",
    "",
    "### Blocker cluster summary",
    "",
    *table_lines,
    "",
    "Next section.",
    "",
  ])


class EquivalenceObligationsDocTests(unittest.TestCase):
  def test_parse_operation_list(self) -> None:
    self.assertEqual(parse_operation_list("setOwner, liquidate"), ["setOwner", "liquidate"])

  def test_parse_operation_list_allows_none_sentinel(self) -> None:
    self.assertEqual(parse_operation_list("none"), [])

  def test_validate_status_summary_passes(self) -> None:
    config = make_config()
    validate_status_summary(make_doc([], summary=derive_summary(config)), derive_summary(config))

  def test_validate_status_summary_passes_with_zero_link1_operations(self) -> None:
    summary = {
      "link1_count": 0,
      "total": 3,
      "link1_operations": [],
      "macro_migrated_count": 0,
      "macro_pending_count": 3,
    }
    validate_status_summary(make_doc([], summary=summary), summary)

  def test_validate_status_summary_allows_wrapped_macro_summary(self) -> None:
    summary = derive_summary(make_config())
    wrapped_doc = make_doc([], summary=summary).replace(
      "operations are macro-migrated; the remaining",
      "operations are\nmacro-migrated; the remaining",
    )
    validate_status_summary(wrapped_doc, summary)

  def test_validate_status_summary_rejects_link1_count_drift(self) -> None:
    config = make_config()
    drifted_doc = make_doc(
      [],
      summary={"link1_count": 1, "total": 4, "link1_operations": ["setOwner"], "macro_migrated_count": 1, "macro_pending_count": 3},
    )
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "Link 1 status summary drift",
    ):
      validate_status_summary(drifted_doc, derive_summary(config))

  def test_validate_status_summary_rejects_link1_operation_drift(self) -> None:
    config = make_config()
    drifted_doc = make_doc(
      [],
      summary={"link1_count": 2, "total": 4, "link1_operations": ["setOwner", "enableIrm"], "macro_migrated_count": 1, "macro_pending_count": 3},
    )
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "Link 1 operation list drift",
    ):
      validate_status_summary(drifted_doc, derive_summary(config))

  def test_validate_status_summary_rejects_macro_migration_drift(self) -> None:
    config = make_config()
    drifted_doc = make_doc(
      [],
      summary={"link1_count": 2, "total": 4, "link1_operations": ["liquidate", "setOwner"], "macro_migrated_count": 2, "macro_pending_count": 2},
    )
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "macro migration summary drift",
    ):
      validate_status_summary(drifted_doc, derive_summary(config))

  def test_validate_status_summary_rejects_macro_intro_drift(self) -> None:
    config = make_config()
    drifted_doc = make_doc([], summary=derive_summary(config)).replace(
      MACRO_STATUS_PREFIX,
      "**Macro migrated** = operation has a full (non-stub) `verity_contract` implementation in\n"
      "`MacroSlice.lean` and is ready for end-to-end semantic bridge composition once verity#1065\n"
      "lands.",
    )
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "macro migration intro drift",
    ):
      validate_status_summary(drifted_doc, derive_summary(config))

  def test_validate_status_summary_rejects_upstream_bridge_status_drift(self) -> None:
    config = make_config()
    drifted_doc = make_doc([], summary=derive_summary(config)).replace(
      UPSTREAM_BRIDGE_PREFIX,
      "- **Link 2** (EDSL ↔ EVMYulLean): provided by the verity semantic bridge once\n"
      "  Layers 2+3 are composed into per-function theorems.",
    )
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "upstream bridge status drift",
    ):
      validate_status_summary(drifted_doc, derive_summary(config))

  def test_extract_issue_summary_table(self) -> None:
    table_lines = [
      "| Cluster | Operations | Blocker families | Coverage counts |",
      "|-------|------------|------------------|-----------------|",
      "| `#123` | `supply` | `erc20` | erc20\u00d71 |",
    ]
    self.assertEqual(extract_issue_summary_table(make_doc(table_lines)), table_lines)

  def test_validate_issue_summary_table_passes(self) -> None:
    clusters = validate_issue_clusters(make_config())
    validate_issue_summary_table(make_doc(expected_table_lines(clusters)), clusters)

  def test_validate_issue_summary_table_rejects_drift(self) -> None:
    clusters = validate_issue_clusters(make_config())
    drifted_doc = make_doc([
      "| Cluster | Operations | Blocker families | Coverage counts |",
      "|-------|------------|------------------|-----------------|",
      "| `#123` | `supply` | `erc20` | erc20\u00d71 |",
      "| `#124` | `liquidate` | `erc20` | erc20\u00d71 |",
    ])
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "does not match derived issue-cluster report",
    ):
      validate_issue_summary_table(drifted_doc, clusters)

  def test_validate_issue_summary_table_allows_empty_cluster_table(self) -> None:
    empty_config = {"obligations": [], "issueClusters": []}
    clusters = validate_issue_clusters(empty_config)
    self.assertEqual(clusters, [])
    validate_issue_summary_table(
      make_doc([
        "| Cluster | Operations | Blocker families | Coverage counts |",
        "|-------|------------|------------------|-----------------|",
      ]),
      clusters,
    )

  def test_main_passes_on_synced_files(self) -> None:
    config = make_config()
    clusters = validate_issue_clusters(config)
    summary = derive_summary(config)
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text(make_doc(expected_table_lines(clusters), summary=summary), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_equivalence_obligations_doc.py",
          "--config",
          str(config_path),
          "--doc",
          str(doc_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_fails_when_section_missing(self) -> None:
    config = make_config()
    summary = derive_summary(config)
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text(make_doc([], summary=summary).replace("### Blocker cluster summary\n", ""), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_equivalence_obligations_doc.py",
          "--config",
          str(config_path),
          "--doc",
          str(doc_path),
        ]
        with self.assertRaisesRegex(
          EquivalenceObligationsDocError,
          "missing `### Blocker cluster summary` section",
        ):
          main()
      finally:
        sys.argv = old_argv

  def test_extract_issue_summary_table_rejects_missing_separator(self) -> None:
    with self.assertRaisesRegex(
      EquivalenceObligationsDocError,
      "must start with the expected markdown header and separator",
    ):
      extract_issue_summary_table(
        make_doc([
          "| Cluster | Operations | Blocker families | Coverage counts |",
          "| `#123` | `supply` | `erc20` | erc20×1 |",
        ])
      )


if __name__ == "__main__":
  unittest.main()
