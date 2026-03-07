#!/usr/bin/env python3
"""Unit tests for the SemanticBridgeReadiness/config sync guard."""

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

from check_semantic_bridge_readiness_sync import (  # noqa: E402
  SemanticBridgeReadinessSyncError,
  build_config_projection,
  compare_entries,
  main,
  parse_readiness_entries,
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
    ]
  }


def make_readiness_text(
  supply_status: str = "assumed",
  set_owner_macro_migrated: str = "true",
) -> str:
  return textwrap.dedent(
    f"""\
    namespace Morpho.Proofs.SemanticBridgeReadiness

    def obligations : List SemanticBridgeObligation := [
      {{ id := "OBL-SUPPLY-SEM-EQ"
        hypothesis := "supplySemEq"
        operation := "supply"
        status := .{supply_status}
        macroMigrated := false }},
      {{ id := "OBL-SET-OWNER-SEM-EQ"
        hypothesis := "setOwnerSemEq"
        operation := "setOwner"
        status := .inProgress  -- Link 1 proven upstream
        macroMigrated := {set_owner_macro_migrated} }}
    ]

    end Morpho.Proofs.SemanticBridgeReadiness
    """
  )


class SemanticBridgeReadinessSyncTests(unittest.TestCase):
  def test_parse_readiness_entries_extracts_projection(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(make_readiness_text(), encoding="utf-8")
      self.assertEqual(
        parse_readiness_entries(readiness_path),
        [
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
        ],
      )

  def test_compare_entries_rejects_field_drift(self) -> None:
    config_entries = build_config_projection(make_config())
    readiness_entries = [
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "supplySemEq",
        "operation": "supply",
        "status": "discharged",
        "macroMigrated": False,
      },
      {
        "id": "OBL-SET-OWNER-SEM-EQ",
        "hypothesis": "setOwnerSemEq",
        "operation": "setOwner",
        "status": "in_progress",
        "macroMigrated": True,
      },
    ]
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "field 'status' drift",
    ):
      compare_entries(config_entries, readiness_entries)

  def test_compare_entries_rejects_missing_id(self) -> None:
    config_entries = build_config_projection(make_config())
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "missing config obligations",
    ):
      compare_entries(config_entries, config_entries[:-1])

  def test_compare_entries_rejects_duplicate_readiness_id(self) -> None:
    config_entries = build_config_projection(make_config())
    readiness_entries = [
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "supplySemEq",
        "operation": "supply",
        "status": "assumed",
        "macroMigrated": False,
      },
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "setOwnerSemEq",
        "operation": "setOwner",
        "status": "in_progress",
        "macroMigrated": True,
      },
    ]
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "contains duplicate obligation ids",
    ):
      compare_entries(config_entries, readiness_entries)

  def test_parse_readiness_entries_rejects_prefixed_fake_namespace_with_tracked_obligations(self) -> None:
    fake_namespace = textwrap.dedent(
      """\
      namespace Morpho.Proofs.SemanticBridgeReadiness

      def obligations : List SemanticBridgeObligation := [
        { id := "OBL-SUPPLY-SEM-EQ"
          hypothesis := "supplySemEq"
          operation := "supply"
          status := .assumed
          macroMigrated := false }
      ]

      end Morpho.Proofs.SemanticBridgeReadiness
      """
    )
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(
        fake_namespace + "\n" + make_readiness_text(),
        encoding="utf-8",
      )
      with self.assertRaisesRegex(
        SemanticBridgeReadinessSyncError,
        "found multiple SemanticBridgeReadiness namespace blocks with tracked obligations",
      ):
        parse_readiness_entries(readiness_path)

  def test_parse_readiness_entries_rejects_reopened_namespace_with_tracked_obligations(self) -> None:
    reopened_namespace = textwrap.dedent(
      """\
      namespace Morpho.Proofs.SemanticBridgeReadiness

      def obligations : List SemanticBridgeObligation := [
        { id := "OBL-SUPPLY-SEM-EQ"
          hypothesis := "supplySemEq"
          operation := "supply"
          status := .assumed
          macroMigrated := false }
      ]

      end Morpho.Proofs.SemanticBridgeReadiness
      """
    )
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(
        make_readiness_text() + "\n" + reopened_namespace,
        encoding="utf-8",
      )
      with self.assertRaisesRegex(
        SemanticBridgeReadinessSyncError,
        "found multiple SemanticBridgeReadiness namespace blocks with tracked obligations",
      ):
        parse_readiness_entries(readiness_path)

  def test_parse_readiness_entries_rejects_unmatched_footer_before_namespace(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(
        "end Morpho.Proofs.SemanticBridgeReadiness\n\n" + make_readiness_text(),
        encoding="utf-8",
      )
      with self.assertRaisesRegex(
        SemanticBridgeReadinessSyncError,
        "unmatched SemanticBridgeReadiness namespace footer before first namespace block",
      ):
        parse_readiness_entries(readiness_path)

  def test_parse_readiness_entries_rejects_unmatched_footer_after_namespace(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(
        make_readiness_text() + "\nend Morpho.Proofs.SemanticBridgeReadiness\n",
        encoding="utf-8",
      )
      with self.assertRaisesRegex(
        SemanticBridgeReadinessSyncError,
        "unmatched SemanticBridgeReadiness namespace footer after namespace blocks",
      ):
        parse_readiness_entries(readiness_path)

  def test_build_config_projection_rejects_duplicate_id(self) -> None:
    config = make_config()
    config["obligations"].append(
      {
        "id": "OBL-SUPPLY-SEM-EQ",
        "hypothesis": "borrowSemEq",
        "operation": "borrow",
        "status": "assumed",
        "macroMigrated": False,
      }
    )
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "config contains duplicate obligation id",
    ):
      build_config_projection(config)

  def test_parse_readiness_entries_rejects_unknown_status(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      readiness_path = pathlib.Path(d) / "SemanticBridgeReadiness.lean"
      readiness_path.write_text(
        make_readiness_text().replace("status := .inProgress", "status := .unsupported", 1),
        encoding="utf-8",
      )
      with self.assertRaisesRegex(
        SemanticBridgeReadinessSyncError,
        "supported status",
      ):
        parse_readiness_entries(readiness_path)

  def test_build_config_projection_rejects_unknown_status(self) -> None:
    config = make_config()
    config["obligations"][0]["status"] = "unsupported"
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "unsupported status",
    ):
      build_config_projection(config)

  def test_build_config_projection_rejects_non_string_hypothesis(self) -> None:
    config = make_config()
    config["obligations"][0]["hypothesis"] = 7
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "missing non-empty string field 'hypothesis'",
    ):
      build_config_projection(config)

  def test_build_config_projection_rejects_non_boolean_macro_migrated(self) -> None:
    config = make_config()
    config["obligations"][0]["macroMigrated"] = "false"
    with self.assertRaisesRegex(
      SemanticBridgeReadinessSyncError,
      "missing boolean field 'macroMigrated'",
    ):
      build_config_projection(config)

  def test_main_passes_on_synced_files(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readiness_path = root / "SemanticBridgeReadiness.lean"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readiness_path.write_text(make_readiness_text(), encoding="utf-8")

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_semantic_bridge_readiness_sync.py",
          "--config",
          str(config_path),
          "--readiness",
          str(readiness_path),
        ]
        self.assertEqual(main(), 0)
      finally:
        sys.argv = old_argv

  def test_main_rejects_macro_migrated_drift(self) -> None:
    with tempfile.TemporaryDirectory() as d:
      root = pathlib.Path(d)
      config_path = root / "semantic-bridge-obligations.json"
      readiness_path = root / "SemanticBridgeReadiness.lean"
      config_path.write_text(json.dumps(make_config()), encoding="utf-8")
      readiness_path.write_text(
        make_readiness_text(set_owner_macro_migrated="false"),
        encoding="utf-8",
      )

      old_argv = sys.argv
      try:
        sys.argv = [
          "check_semantic_bridge_readiness_sync.py",
          "--config",
          str(config_path),
          "--readiness",
          str(readiness_path),
        ]
        with self.assertRaisesRegex(
          SemanticBridgeReadinessSyncError,
          "field 'macroMigrated' drift",
        ):
          main()
      finally:
        sys.argv = old_argv


if __name__ == "__main__":
  unittest.main()
