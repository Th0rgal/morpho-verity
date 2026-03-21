#!/usr/bin/env python3
"""Unit tests for fix_market_params_hash.py post-processing script."""

from __future__ import annotations

import importlib.util
import pathlib
import sys
import unittest

ROOT = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = ROOT / "scripts" / "fix_market_params_hash.py"

# Load the module dynamically so we can call fix_yul directly.
spec = importlib.util.spec_from_file_location("fix_market_params_hash", SCRIPT)
mod = importlib.util.module_from_spec(spec)  # type: ignore[arg-type]
spec.loader.exec_module(mod)  # type: ignore[union-attr]
fix_yul = mod.fix_yul


class FixMarketParamsHashTests(unittest.TestCase):
    def test_replaces_inline_pattern(self) -> None:
        src = (
            "mstore(0, marketParams_0)\n"
            "                mstore(32, marketParams_1)\n"
            "                mstore(64, marketParams_2)\n"
            "                mstore(96, marketParams_3)\n"
            "                mstore(128, marketParams_4)\n"
            "                let id := keccak256(0, 160)"
        )
        result = fix_yul(src)
        self.assertIn("keccakMarketParams(marketParams_0", result)
        self.assertNotIn("keccak256(0, 160)", result)

    def test_no_match_leaves_content_unchanged(self) -> None:
        src = "let x := keccak256(0, 32)\n"
        result = fix_yul(src)
        self.assertEqual(src, result)

    def test_inserts_helper_function_when_switch_present(self) -> None:
        src = (
            "mstore(0, marketParams_0)\n"
            "                mstore(32, marketParams_1)\n"
            "                mstore(64, marketParams_2)\n"
            "                mstore(96, marketParams_3)\n"
            "                mstore(128, marketParams_4)\n"
            "                let id := keccak256(0, 160)\n"
            "            switch shr(224, calldataload(0))\n"
        )
        result = fix_yul(src)
        self.assertIn("function keccakMarketParams(", result)

    def test_does_not_duplicate_helper(self) -> None:
        src = (
            "function keccakMarketParams(a,b,c,d,e) -> id { }\n"
            "mstore(0, marketParams_0)\n"
            "                mstore(32, marketParams_1)\n"
            "                mstore(64, marketParams_2)\n"
            "                mstore(96, marketParams_3)\n"
            "                mstore(128, marketParams_4)\n"
            "                let id := keccak256(0, 160)\n"
        )
        result = fix_yul(src)
        self.assertEqual(result.count("function keccakMarketParams("), 1)


if __name__ == "__main__":
    unittest.main()
