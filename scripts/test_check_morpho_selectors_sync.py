#!/usr/bin/env python3
"""Unit tests for morphoSelectors sync checker."""

from __future__ import annotations

import pathlib
import sys
import unittest

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_morpho_selectors_sync import (  # noqa: E402
  find_selector_block_bounds,
  parse_selector_signatures,
  rewrite_selector_block,
)
from check_macro_migration_surface import run_check  # noqa: E402


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

  def test_repo_selector_migration_surface_still_ok(self) -> None:
    # Integration smoke check tied to repository sources.
    report = run_check()
    self.assertEqual(report["status"], "ok")
    self.assertEqual(report["selectorMismatchCount"], 0)


if __name__ == "__main__":
  unittest.main()
