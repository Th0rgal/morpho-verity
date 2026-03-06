#!/usr/bin/env python3
"""Regression tests for patch_morpho_blue_harness.py."""

from __future__ import annotations

import pathlib
import sys
import tempfile
import unittest

ROOT = pathlib.Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / "scripts"))

from patch_morpho_blue_harness import patch_repo  # noqa: E402


BASE_TEST = """// fake BaseTest
contract BaseTest {
    IMorpho internal morpho;
    address internal OWNER;

    function setUp() public virtual {
        morpho = IMorpho(address(new Morpho(OWNER)));
    }
}
"""

ONLY_OWNER = """// fake OnlyOwnerIntegrationTest
contract OnlyOwnerIntegrationTest is BaseTest {
    function testDeployWithAddressZero() public {
        new Morpho(address(0));
    }

    function testDeployEmitOwner() public {
        new Morpho(OWNER);
    }
}
"""


class PatchMorphoBlueHarnessTests(unittest.TestCase):
    def test_patch_repo_updates_harness_and_constructor_tests(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-blue" / "test" / "BaseTest.sol"
            only_owner = root / "morpho-blue" / "test" / "integration" / "OnlyOwnerIntegrationTest.sol"
            only_owner.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST, encoding="utf-8")
            only_owner.write_text(ONLY_OWNER, encoding="utf-8")

            changes = patch_repo(root)

            self.assertEqual(
                changes,
                [
                    "morpho-blue/test/BaseTest.sol",
                    "morpho-blue/test/integration/OnlyOwnerIntegrationTest.sol",
                ],
            )
            base_text = base_test.read_text(encoding="utf-8")
            owner_text = only_owner.read_text(encoding="utf-8")
            self.assertIn('vm.envOr("MORPHO_IMPL", string("solidity"))', base_text)
            self.assertIn("morpho = _deployMorpho(OWNER);", base_text)
            self.assertIn('vm.getCode("../artifacts/yul/Morpho.bin")', base_text)
            self.assertNotIn("new Morpho(address(0));", owner_text)
            self.assertNotIn("new Morpho(OWNER);", owner_text)
            self.assertIn("_deployMorpho(address(0));", owner_text)
            self.assertIn("_deployMorpho(OWNER);", owner_text)

    def test_patch_repo_is_idempotent(self) -> None:
        with tempfile.TemporaryDirectory() as tmpdir:
            root = pathlib.Path(tmpdir)
            base_test = root / "morpho-blue" / "test" / "BaseTest.sol"
            only_owner = root / "morpho-blue" / "test" / "integration" / "OnlyOwnerIntegrationTest.sol"
            only_owner.parent.mkdir(parents=True)
            base_test.write_text(BASE_TEST, encoding="utf-8")
            only_owner.write_text(ONLY_OWNER, encoding="utf-8")

            self.assertTrue(patch_repo(root))
            self.assertEqual(patch_repo(root), [])


if __name__ == "__main__":
    unittest.main()
