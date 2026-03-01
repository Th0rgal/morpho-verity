#!/usr/bin/env python3
"""Unit tests for macro migration selector-slice checker."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_slice import (  # noqa: E402
  MigrationSliceError,
  ROOT,
  extract_macro_signatures,
  run_check,
  validate_against_baseline,
)


class ParseMacroSliceTests(unittest.TestCase):
  def test_extract_macro_signatures(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function owner () : Address := do
    return 0

  function nonce (authorizer : Address) : Uint256 := do
    return 0
"""
    self.assertEqual(
      extract_macro_signatures(text),
      {"owner()", "nonce(address)"},
    )

  def test_extract_macro_signatures_unsupported_type_fails(self) -> None:
    text = """
verity_contract MorphoViewSlice where
  storage
    owner : Address := slot 0

  function bad (id : Id) : Uint256 := do
    return 0
"""
    with self.assertRaises(MigrationSliceError):
      extract_macro_signatures(text)


class BaselineValidationTests(unittest.TestCase):
  def test_validate_against_baseline_matches(self) -> None:
    validate_against_baseline(
      {"owner()"},
      {"expectedMigrated": ["owner()"]},
    )

  def test_validate_against_baseline_detects_drift(self) -> None:
    with self.assertRaises(MigrationSliceError):
      validate_against_baseline(
        {"owner()", "nonce(address)"},
        {"expectedMigrated": ["owner()"]},
      )


class RepoCheckTests(unittest.TestCase):
  def test_current_repo_slice_matches(self) -> None:
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["contract"], "MorphoViewSlice")
    self.assertTrue((ROOT / report["macroPath"]).exists())
    self.assertGreaterEqual(report["migratedCount"], 1)


if __name__ == "__main__":
  unittest.main()
