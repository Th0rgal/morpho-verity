#!/usr/bin/env python3
"""Unit tests for morphoSelectors sync checker."""

from __future__ import annotations

import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_morpho_selectors_sync import (  # noqa: E402
  MorphoSelectorsSyncError,
  find_selector_block_bounds,
  parse_selector_signatures,
  rewrite_selector_block,
)
from check_macro_migration_surface import ROOT, run_check  # noqa: E402


class CheckMorphoSelectorsSyncTests(unittest.TestCase):
  def test_find_selector_block_bounds(self) -> None:
    lines = [
      "def foo : Nat := 1",
      "def morphoSelectors : List Nat := [",
      "  0xaaaaaaaa, -- a()",
      "]",
      "end X",
    ]
    start, end = find_selector_block_bounds(lines)
    self.assertEqual((start, end), (2, 3))

  def test_find_selector_block_bounds_requires_definition(self) -> None:
    with self.assertRaisesRegex(MorphoSelectorsSyncError, "unable to find morphoSelectors definition"):
      find_selector_block_bounds(["def foo : Nat := 1"])

  def test_find_selector_block_bounds_requires_closing_bracket(self) -> None:
    with self.assertRaisesRegex(MorphoSelectorsSyncError, "unable to find morphoSelectors closing bracket"):
      find_selector_block_bounds([
        "def morphoSelectors : List Nat := [",
        "  0xaaaaaaaa, -- a()",
      ])

  def test_parse_selector_signatures(self) -> None:
    spec = """
def morphoSelectors : List Nat := [
  0xaaaaaaaa, -- a()
  0xbbbbbbbb  -- b(uint256)
]
"""
    self.assertEqual(parse_selector_signatures(spec), ["a()", "b(uint256)"])

  def test_rewrite_selector_block_updates_values(self) -> None:
    spec = (
      "def morphoSelectors : List Nat := [\n"
      "  0xaaaaaaaa, -- a()\n"
      "  0xbbbbbbbb  -- b(uint256)\n"
      "]\n"
    )
    selector_map = {"a()": 0x11111111, "b(uint256)": 0x22222222}
    rewritten, unchanged = rewrite_selector_block(spec, selector_map)
    self.assertFalse(unchanged)
    self.assertIn("0x11111111, -- a()", rewritten)
    self.assertIn("0x22222222 -- b(uint256)", rewritten)

  def test_rewrite_selector_block_noop_when_up_to_date(self) -> None:
    original = (
      "def morphoSelectors : List Nat := [\n"
      "  0x11111111, -- a()\n"
      "  0x22222222 -- b(uint256)\n"
      "]\n"
    )
    selector_map = {"a()": 0x11111111, "b(uint256)": 0x22222222}
    first, first_unchanged = rewrite_selector_block(original, selector_map)
    self.assertTrue(first_unchanged)
    rewritten, unchanged = rewrite_selector_block(first, selector_map)
    self.assertTrue(unchanged)
    self.assertEqual(rewritten, first)

  def test_rewrite_selector_block_requires_selector_mapping(self) -> None:
    spec = (
      "def morphoSelectors : List Nat := [\n"
      "  0xaaaaaaaa -- a()\n"
      "]\n"
    )
    with self.assertRaisesRegex(MorphoSelectorsSyncError, "missing selector mapping for signature: a\\(\\)"):
      rewrite_selector_block(spec, {})

  @unittest.skipUnless((pathlib.Path(__file__).resolve().parent.parent / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol").exists(),
                       "requires initialized morpho-blue submodule (IMorpho.sol)")
  @unittest.skipUnless(shutil.which("solc"), "requires solc on PATH")
  def test_repo_selector_migration_surface_still_ok(self) -> None:
    # Integration smoke check tied to repository sources.
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["selectorMismatchCount"], 0)

  def test_cli_reports_checker_error_without_traceback(self) -> None:
    with tempfile.TemporaryDirectory() as tmpdir:
      bad_repo = pathlib.Path(tmpdir)
      (bad_repo / "scripts").mkdir()
      (bad_repo / "Morpho" / "Compiler").mkdir(parents=True)
      (bad_repo / "morpho-blue" / "src" / "interfaces").mkdir(parents=True)
      (bad_repo / "scripts" / "check_morpho_selectors_sync.py").write_text(
        (ROOT / "scripts" / "check_morpho_selectors_sync.py").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      (bad_repo / "scripts" / "check_macro_migration_surface.py").write_text(
        (ROOT / "scripts" / "check_macro_migration_surface.py").read_text(encoding="utf-8"),
        encoding="utf-8",
      )
      (bad_repo / "Morpho" / "Compiler" / "Spec.lean").write_text(
        "def notMorphoSelectors : List Nat := []\n",
        encoding="utf-8",
      )
      (bad_repo / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol").write_text(
        "interface IMorpho {}\n",
        encoding="utf-8",
      )

      proc = subprocess.run(
        [sys.executable, str(bad_repo / "scripts" / "check_morpho_selectors_sync.py")],
        cwd=bad_repo,
        check=False,
        capture_output=True,
        text=True,
      )

    self.assertEqual(proc.returncode, 1)
    self.assertIn("morpho-selectors-sync check failed:", proc.stderr)
    self.assertIn("unable to find morphoSelectors", proc.stderr)
    self.assertNotIn("Traceback", proc.stderr)


if __name__ == "__main__":
  unittest.main()
