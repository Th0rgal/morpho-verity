#!/usr/bin/env python3

import unittest
from pathlib import Path
import sys

sys.path.insert(0, str(Path(__file__).resolve().parent))
import check_morpho_midnight_mapping as check


class CheckMorphoMidnightMappingTest(unittest.TestCase):
    def test_interface_functions(self) -> None:
        text = """
interface I {
    function totalUnits(bytes32 id) external view returns (uint128);
    function liquidate() external returns (uint256, uint256);
}
"""
        self.assertEqual(check.interface_functions(text), ["totalUnits", "liquidate"])

    def test_manifest_functions(self) -> None:
        text = """
| Interface function | Coverage | Evidence |
|--------------------|----------|----------|
| `totalUnits()` | Focused. | `UnitsAccounting.lean` |
| `liquidate()` | Focused. | `RCF.lean` |
"""
        self.assertEqual(check.manifest_functions(text), {"totalUnits", "liquidate"})

    def test_artifact_status(self) -> None:
        text = "Complete artifact status: missing\nFocused artifact status: present\n"
        self.assertEqual(check.artifact_status(text), "missing")
        self.assertEqual(check.artifact_status(text, "Focused"), "present")


if __name__ == "__main__":
    unittest.main()
