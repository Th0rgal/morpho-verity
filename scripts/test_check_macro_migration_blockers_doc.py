#!/usr/bin/env python3
"""Unit tests for the remaining macro blocker doc sync guard."""

from __future__ import annotations

import json
import os
import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT_DIR = pathlib.Path(__file__).resolve().parent
if str(SCRIPT_DIR) not in sys.path:
  sys.path.insert(0, str(SCRIPT_DIR))

from check_macro_migration_blockers_doc import (  # noqa: E402
  NEXT_SECTION_HEADING,
  SECTION_HEADING,
  SECTION_MARKER,
  MacroMigrationBlockersDocError,
  build_blocker_report,
  expected_table_lines,
  extract_macro_migration_section,
  extract_table,
  load_config,
  main,
  read_text,
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
    SECTION_HEADING,
    "",
    "Macro blockers prelude.",
    "",
    SECTION_MARKER,
    "",
    *table_lines,
    "",
    NEXT_SECTION_HEADING,
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

  def test_extract_macro_migration_section_returns_target_block(self) -> None:
    section = extract_macro_migration_section(make_doc(expected_table_lines(build_blocker_report(make_config()))))
    self.assertIn("**Remaining blockers**:", section)

  def test_extract_macro_migration_section_rejects_fake_heading_outside_section(self) -> None:
    doc = "\n".join([
      SECTION_HEADING,
      "",
      "Fake prelude.",
      "",
      NEXT_SECTION_HEADING,
      "",
      make_doc(expected_table_lines(build_blocker_report(make_config()))),
    ])
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "found multiple `## Macro Migration Blockers` section markers",
    ):
      extract_macro_migration_section(doc)

  def test_validate_doc_table_rejects_drift_hidden_behind_fake_marker_outside_section(self) -> None:
    report = build_blocker_report(make_config())
    wrong_table = [
      "| Blocker | Operations affected | Count |",
      "|---------|-------------------|:-----:|",
      "| Internal function calls (`Stmt.internalCall`) | `supply`, `withdrawCollateral` | 2 |",
    ]
    drifted_doc = make_doc(expected_table_lines(report)).replace(
      "| Internal function calls (`Stmt.internalCall`) | `supply`, `withdrawCollateral` | 2 |",
      "| Internal function calls (`Stmt.internalCall`) | `supply` | 1 |",
      1,
    )
    masked_doc = "\n".join([
      "# Notes",
      "",
      SECTION_MARKER,
      "",
      *wrong_table,
      "",
      drifted_doc,
    ])
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "does not match derived macro blocker inventory",
    ):
      validate_doc_table(masked_doc, report)

  def test_extract_macro_migration_section_accepts_crlf_heading_lines(self) -> None:
    report = build_blocker_report(make_config())
    doc = make_doc(expected_table_lines(report)).replace("\n", "\r\n")
    section = extract_macro_migration_section(doc)
    self.assertIn(SECTION_MARKER, section)

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

  def test_build_blocker_report_rejects_duplicate_operation(self) -> None:
    config = make_config()
    config["obligations"].append({
      "operation": "supply",
      "macroMigrated": False,
      "macroSurfaceBlockers": ["callbacks"],
    })
    with self.assertRaisesRegex(
      MacroMigrationBlockersDocError,
      "duplicate obligation operation 'supply'",
    ):
      build_blocker_report(config)

  def test_load_config_rejects_malformed_json(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_text("{\n", encoding="utf-8")
      with self.assertRaisesRegex(
        MacroMigrationBlockersDocError,
        "failed to parse JSON",
      ):
        load_config(path)

  def test_load_config_rejects_non_object_root(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_text("[]", encoding="utf-8")
      with self.assertRaisesRegex(
        MacroMigrationBlockersDocError,
        "config root must be a JSON object",
      ):
        load_config(path)

  def test_read_text_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "EQUIVALENCE_OBLIGATIONS.md"
      path.write_bytes(b"\xff")
      with self.assertRaisesRegex(
        MacroMigrationBlockersDocError,
        "failed to decode UTF-8 in document file",
      ):
        read_text(path, context="document file")

  def test_load_config_rejects_invalid_utf8(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      path = pathlib.Path(d) / "config.json"
      path.write_bytes(b"\xff")
      with self.assertRaisesRegex(
        MacroMigrationBlockersDocError,
        "failed to decode UTF-8 in config file",
      ):
        load_config(path)

  def test_main_reports_invalid_utf8_config_without_traceback(self) -> None:
    report = build_blocker_report(make_config())
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_bytes(b"\xff")
      doc_path.write_text(make_doc(expected_table_lines(report)), encoding="utf-8")

      result = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_macro_migration_blockers_doc.py"),
         "--config", os.fspath(config_path),
         "--doc", os.fspath(doc_path)],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(result.returncode, 1)
      self.assertIn("macro-migration-blockers-doc check failed:", result.stderr)
      self.assertIn("failed to decode UTF-8 in config file", result.stderr)
      self.assertNotIn("Traceback", result.stderr)

  def test_main_reports_invalid_utf8_doc_without_traceback(self) -> None:
    config = make_config()
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "config.json"
      doc_path = root / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_bytes(b"\xff")

      result = subprocess.run(
        [sys.executable, str(SCRIPT_DIR / "check_macro_migration_blockers_doc.py"),
         "--config", os.fspath(config_path),
         "--doc", os.fspath(doc_path)],
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(result.returncode, 1)
      self.assertIn("macro-migration-blockers-doc check failed:", result.stderr)
      self.assertIn("failed to decode UTF-8 in document file", result.stderr)
      self.assertNotIn("Traceback", result.stderr)

  def test_main_resolves_relative_external_paths_from_other_cwd(self) -> None:
    config = make_config()
    report = build_blocker_report(config)
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      inputs = root / "inputs"
      inputs.mkdir()
      config_path = inputs / "config.json"
      doc_path = inputs / "EQUIVALENCE_OBLIGATIONS.md"
      config_path.write_text(json.dumps(config), encoding="utf-8")
      doc_path.write_text(make_doc(expected_table_lines(report)), encoding="utf-8")

      runner = root / "runner"
      runner.mkdir()
      result = subprocess.run(
        [
          sys.executable,
          str(SCRIPT_DIR / "check_macro_migration_blockers_doc.py"),
          "--config",
          str(pathlib.Path("..") / "inputs" / "config.json"),
          "--doc",
          str(pathlib.Path("..") / "inputs" / "EQUIVALENCE_OBLIGATIONS.md"),
        ],
        cwd=runner,
        capture_output=True,
        text=True,
        check=False,
      )

      self.assertEqual(result.returncode, 0, result.stderr)
      self.assertIn("macro-migration-blockers-doc check: OK", result.stdout)


if __name__ == "__main__":
  unittest.main()
