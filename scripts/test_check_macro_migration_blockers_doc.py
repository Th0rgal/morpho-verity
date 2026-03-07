#!/usr/bin/env python3
"""Unit tests for the remaining macro blocker doc sync guard."""

from __future__ import annotations

import json
import pathlib
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_macro_migration_blockers_doc import (  # noqa: E402
  MacroMigrationBlockersDocError,
  build_blocker_report,
  expected_table_lines,
  extract_table,
  main,
  validate_doc_table,
)


def make_config() -> dict:
  return {
    "obligations": [
      {
        "operation": "supply",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["callbacks", "erc20", "internalCall", "memoryOps"],
      },
      {
        "operation": "withdrawCollateral",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["erc20", "externalWithReturn", "internalCall", "memoryOps", "structMember2"],
      },
      {
        "operation": "setAuthorizationWithSig",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["memoryOps", "precompileAccess", "tupleDestructuring"],
      },
      {
        "operation": "createMarket",
        "macroMigrated": False,
        "macroSurfaceBlockers": ["blockTimestampValue", "externalCall", "tupleDestructuring"],
      },
      {
        "operation": "flashLoan",
        "macroMigrated": True,
      },
    ]
  }


def make_doc(table_lines: list[str]) -> str:
  return "\n".join([
    "# Equivalence",
    "",
    "Preamble.",
    "",
    "**Remaining blockers**:",
    "",
    *table_lines,
    "",
    "Next section.",
    "",
  ])


class MacroMigrationBlockersDocTests(unittest.TestCase):
  def test_build_blocker_report_aggregates_operations(self) -> None:
    report = build_blocker_report(make_config())
    self.assertEqual(
      [entry["blocker"] for entry in report],
      [
        "internalCall",
        "erc20",
        "structMember2",
        "callbacks",
        "externalWithReturn",
        "memoryOps",
        "precompileAccess",
        "tupleDestructuring",
        "externalCall",
        "blockTimestampValue",
      ],
    )
    self.assertEqual(report[0]["operations"], ["supply", "withdrawCollateral"])
    self.assertEqual(report[5]["count"], 3)

  def test_validate_doc_table_passes(self) -> None:
    report = build_blocker_report(make_config())
    validate_doc_table(make_doc(expected_table_lines(report)), report)

  def test_validate_doc_table_rejects_drift(self) -> None:
    report = build_blocker_report(make_config())
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "does not match derived macro blocker inventory",
    ):
      validate_doc_table(
        make_doc([
          "| Blocker | Operations affected | Count |",
          "|---------|-------------------|:-----:|",
          "| Internal function calls (`Stmt.internalCall`) | `supply` | 1 |",
        ]),
        report,
      )

  def test_extract_table_rejects_missing_separator(self) -> None:
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "must start with the expected markdown header and separator",
    ):
      extract_table(
        make_doc([
          "| Blocker | Operations affected | Count |",
          "| Internal function calls (`Stmt.internalCall`) | `supply` | 1 |",
        ])
      )

  def test_main_passes_on_synced_files(self) -> None:
    config = make_config()
    report = build_blocker_report(config)
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text(make_doc(expected_table_lines(report)), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_macro_migration_blockers_doc.py",
          "--config",
          str(config_path),
          "--doc",
          str(doc_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_build_blocker_report_rejects_unknown_blocker(self) -> None:
    config = make_config()
    config["obligations"][0]["macroSurfaceBlockers"] = ["unknownBlocker"]
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "references unknown blocker",
    ):
      build_blocker_report(config)


if __name__ == "__main__":
  unittest.main()
