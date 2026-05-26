#!/usr/bin/env python3
"""Unit tests for Solidity correspondence doc checker."""

from __future__ import annotations

import pathlib
import subprocess
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))

from check_solidity_correspondence_doc import extract_table_rows  # noqa: E402


ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "check_solidity_correspondence_doc.py"
DOC_PATH = ROOT / "docs" / "SOLIDITY_CORRESPONDENCE.md"


class CheckSolidityCorrespondenceDocTests(unittest.TestCase):
  def run_script(self) -> subprocess.CompletedProcess[str]:
    return subprocess.run(
      ["python3", str(SCRIPT)],
      cwd=ROOT,
      capture_output=True,
      text=True,
      check=False,
    )

  def test_repo_correspondence_doc_is_clean(self) -> None:
    proc = self.run_script()
    self.assertEqual(proc.returncode, 0, msg=proc.stderr)
    self.assertIn("check: OK", proc.stdout)

  def test_extracts_table_rows_by_function_name(self) -> None:
    rows = extract_table_rows(DOC_PATH.read_text(encoding="utf-8"))
    self.assertEqual(rows["setOwner"]["macro_status"], "Translated")
    self.assertEqual(rows["supply"]["proof_status"], "Assumed boundary")

  def test_detects_missing_operation_row(self) -> None:
    original = DOC_PATH.read_text(encoding="utf-8")
    try:
      DOC_PATH.write_text(
        original.replace(
          "| `supply(MarketParams,uint256,uint256,address,bytes)`",
          "| `supply_removed(MarketParams,uint256,uint256,address,bytes)`",
          1,
        ),
        encoding="utf-8",
      )
      proc = self.run_script()
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("missing row", proc.stderr)
      self.assertIn("supply", proc.stderr)
    finally:
      DOC_PATH.write_text(original, encoding="utf-8")

  def test_detects_status_drift(self) -> None:
    original = DOC_PATH.read_text(encoding="utf-8")
    try:
      DOC_PATH.write_text(
        original.replace(
          "| `setOwner(address)` | Translated | Link 1 proven |",
          "| `setOwner(address)` | Translated | Assumed boundary |",
          1,
        ),
        encoding="utf-8",
      )
      proc = self.run_script()
      self.assertNotEqual(proc.returncode, 0)
      self.assertIn("setOwner", proc.stderr)
      self.assertIn("proof status drift", proc.stderr)
    finally:
      DOC_PATH.write_text(original, encoding="utf-8")

  def test_reports_invalid_utf8_without_traceback(self) -> None:
    original = DOC_PATH.read_bytes()
    try:
      DOC_PATH.write_bytes(b"\xff")
      proc = self.run_script()
      self.assertEqual(proc.returncode, 1)
      self.assertIn("solidity-correspondence-doc check failed:", proc.stderr)
      self.assertIn("not valid UTF-8", proc.stderr)
      self.assertNotIn("Traceback", proc.stderr)
    finally:
      DOC_PATH.write_bytes(original)


if __name__ == "__main__":
  unittest.main()
