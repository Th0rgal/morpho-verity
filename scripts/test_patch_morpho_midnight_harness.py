#!/usr/bin/env python3
"""Regression tests for patch_morpho_midnight_harness.py."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

from patch_morpho_midnight_harness import patch_repo, validate_repo  # noqa: E402


BASE_TEST = """// fake BaseTest
contract BaseTest {
    Midnight internal midnight;

    function setUp() public virtual {
        midnight = new Midnight();
    }
}
"""

BASE_TEST_DRIFTED = """// fake BaseTest
contract BaseTest {
    Midnight internal midnight;

    function setUp() public virtual {
        midnight = Midnight(address(new Midnight()));
    }
}
"""

FOUNDRY_TOML = """[profile.default]
via_ir = true
optimizer = true
optimizer_runs = 800
bytecode_hash = "none"
evm_version = "osaka"
fs_permissions = [{ access = "read", path = "test/ticks_exact.json" }]
"""

SETTLEMENT_FEE_TEST_WITH_DIRECT_TOUCH = """// fake SettlementFeeTest
contract SettlementFeeTest is BaseTest {
    function testDirectTouch() external {
        midnight.touchMarket(market);
        id = midnight.touchMarket(market);
    }
}
"""


class PatchMorphoMidnightHarnessTests(unittest.TestCase):
    def test_patch_repo_updates_harness(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
            foundry_toml = root / "morpho-midnight" / "foundry.toml"
            base_test.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST, encoding="utf-8")
            foundry_toml.write_text(FOUNDRY_TOML, encoding="utf-8")

            changes = patch_repo(root)

            self.assertEqual(
                changes,
                [
                    "morpho-midnight/test/BaseTest.sol",
                    "morpho-midnight/foundry.toml",
                ],
            )
            base_text = base_test.read_text(encoding="utf-8")
            foundry_text = foundry_toml.read_text(encoding="utf-8")
            self.assertIn('vm.envOr("MIDNIGHT_IMPL", string("solidity"))', base_text)
            self.assertIn("midnight = _deployMidnight();", base_text)
            self.assertIn('vm.readFileBinary("../artifacts/midnight/Midnight.bin.raw")', base_text)
            self.assertIn("[profile.difftest]", foundry_text)
            self.assertIn("../artifacts/midnight", foundry_text)
            self.assertEqual(validate_repo(root), [])

    def test_patch_repo_is_idempotent(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
            foundry_toml = root / "morpho-midnight" / "foundry.toml"
            base_test.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST, encoding="utf-8")
            foundry_toml.write_text(FOUNDRY_TOML, encoding="utf-8")

            self.assertTrue(patch_repo(root))
            self.assertEqual(patch_repo(root), [])

    def test_patch_repo_leaves_direct_touch_market_untouched(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
            settlement_test = root / "morpho-midnight" / "test" / "SettlementFeeTest.sol"
            foundry_toml = root / "morpho-midnight" / "foundry.toml"
            base_test.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST, encoding="utf-8")
            settlement_test.write_text(SETTLEMENT_FEE_TEST_WITH_DIRECT_TOUCH, encoding="utf-8")
            foundry_toml.write_text(FOUNDRY_TOML, encoding="utf-8")

            changes = patch_repo(root)

            self.assertNotIn("morpho-midnight/test/SettlementFeeTest.sol", changes)
            patched = settlement_test.read_text(encoding="utf-8")
            self.assertIn("midnight.touchMarket(market)", patched)
            self.assertIn("id = midnight.touchMarket(market)", patched)
            self.assertEqual(validate_repo(root), [])

    def test_validate_repo_reports_unpatched_constructor_drift(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
            foundry_toml = root / "morpho-midnight" / "foundry.toml"
            base_test.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST_DRIFTED, encoding="utf-8")
            foundry_toml.write_text(FOUNDRY_TOML, encoding="utf-8")

            patch_repo(root)
            errors = validate_repo(root)

            self.assertIn("BaseTest.sol does not route setUp() through _deployMidnight().", errors)


if __name__ == "__main__":
    unittest.main()
