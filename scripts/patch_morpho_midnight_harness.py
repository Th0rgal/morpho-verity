#!/usr/bin/env python3
"""Patch the Morpho Midnight test harness to honor MIDNIGHT_IMPL."""

from __future__ import annotations

import pathlib
import sys
from typing import Callable

ROOT = pathlib.Path(__file__).resolve().parents[1]

BASE_TEST_DEPLOY_OLD = "        midnight = new Midnight();\n"
BASE_TEST_DEPLOY_NEW = "        midnight = _deployMidnight();\n"
BASE_TEST_INSERT_BEFORE = "    function setUp() public virtual {\n"
BASE_TEST_HELPER = """    function _deployMidnight() internal returns (Midnight) {
        string memory impl = vm.envOr("MIDNIGHT_IMPL", string("solidity"));
        bytes32 implHash = keccak256(bytes(impl));

        if (implHash == keccak256(bytes("solidity"))) {
            return new Midnight();
        }
        if (implHash == keccak256(bytes("verity"))) {
            bytes memory initCode = vm.readFileBinary("../artifacts/midnight/Midnight.bin.raw");
            address deployed;
            assembly {
                deployed := create(0, add(initCode, 0x20), mload(initCode))
                if iszero(deployed) {
                    returndatacopy(0, 0, returndatasize())
                    revert(0, returndatasize())
                }
            }
            vm.label(deployed, "Midnight");
            return Midnight(deployed);
        }

        revert(string(abi.encodePacked("Unsupported MIDNIGHT_IMPL: ", impl)));
    }

"""
BASE_TEST_DEPLOY_STILL_HARDCODED = "midnight = new Midnight();"
BASE_TEST_OLD_VERITY_MARKET_STATE_SLOT = (
    "        bytes32 packedSlot = bytes32(uint256(keccak256(abi.encode(id, uint256(10)))) + 2);\n"
)
BASE_TEST_NEW_VERITY_MARKET_STATE_SLOT = (
    "        bytes32 packedSlot = bytes32(uint256(keccak256(abi.encode(id, uint256(1)))) + 2);\n"
)
BASE_TEST_SCAFFOLD_TO_ID = """    function toId(Market memory market) internal view returns (bytes32) {
        if (_verityImpl() && (market.enterGate != address(0) || market.liquidatorGate != address(0))) {
            return IdLib.toId(market, block.chainid, address(midnight));
        }
        if (_verityImpl()) return bytes32(market.maturity);
        return IdLib.toId(market, block.chainid, address(midnight));
    }
"""
BASE_TEST_EXACT_TO_ID = """    function toId(Market memory market) internal view returns (bytes32) {
        return IdLib.toId(market, block.chainid, address(midnight));
    }
"""
LIQUIDATION_OLD_VERITY_POSITION_SLOT = "        uint256 mappingSlot = _verityImpl() ? 11 : 0;\n"
LIQUIDATION_NEW_VERITY_POSITION_SLOT = "        uint256 mappingSlot = 0;\n"
DIRECT_TOUCH_MARKET_REPLACEMENTS = (
    ("test/SettlementFeeTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
    ("test/TakeAmountsTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
    ("test/TakeTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
    ("test/MidnightBundlesTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
    ("test/LiquidationTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
    ("test/TickGatingTest.sol", "midnight.touchMarket(market)", "touchMarket(market)"),
)
FOUNDRY_PROFILE = """[profile.difftest]
fs_permissions = [
  { access = "read", path = "test/ticks_exact.json" },
  { access = "read", path = "../artifacts/midnight" },
]
evm_version = "osaka"
"""


def _patch_base_test(text: str) -> tuple[str, bool]:
    updated = text
    changed = False

    if BASE_TEST_HELPER not in updated and BASE_TEST_INSERT_BEFORE in updated:
        updated = updated.replace(BASE_TEST_INSERT_BEFORE, BASE_TEST_HELPER + BASE_TEST_INSERT_BEFORE, 1)
        changed = True

    if BASE_TEST_DEPLOY_OLD in updated:
        updated = updated.replace(BASE_TEST_DEPLOY_OLD, BASE_TEST_DEPLOY_NEW, 1)
        changed = True

    legacy_console = """        console.log("Midnight harness implementation:");
        console.log(impl);

"""
    if legacy_console in updated:
        updated = updated.replace(legacy_console, "", 1)
        changed = True

    if BASE_TEST_OLD_VERITY_MARKET_STATE_SLOT in updated:
        updated = updated.replace(
            BASE_TEST_OLD_VERITY_MARKET_STATE_SLOT,
            BASE_TEST_NEW_VERITY_MARKET_STATE_SLOT,
            1,
        )
        changed = True

    if BASE_TEST_SCAFFOLD_TO_ID in updated:
        updated = updated.replace(BASE_TEST_SCAFFOLD_TO_ID, BASE_TEST_EXACT_TO_ID, 1)
        changed = True

    while updated.count(BASE_TEST_HELPER) > 1:
        first = updated.find(BASE_TEST_HELPER)
        second = updated.find(BASE_TEST_HELPER, first + len(BASE_TEST_HELPER))
        updated = updated[:second] + updated[second + len(BASE_TEST_HELPER):]
        changed = True

    return updated, changed


def _patch_liquidation_test(text: str) -> tuple[str, bool]:
    if LIQUIDATION_OLD_VERITY_POSITION_SLOT not in text:
        return text, False
    return text.replace(
        LIQUIDATION_OLD_VERITY_POSITION_SLOT,
        LIQUIDATION_NEW_VERITY_POSITION_SLOT,
        1,
    ), True


def _patch_direct_touch_market(text: str, old: str, new: str) -> tuple[str, bool]:
    if old not in text:
        return text, False
    return text.replace(old, new), True


def _patch_foundry_toml(text: str) -> tuple[str, bool]:
    if "[profile.difftest]" in text:
        return text, False

    suffix = "" if text.endswith("\n") else "\n"
    return text + suffix + "\n" + FOUNDRY_PROFILE, True


def _patch_file(path: pathlib.Path, patcher: Callable[[str], tuple[str, bool]]) -> bool:
    original = path.read_text(encoding="utf-8")
    updated, changed = patcher(original)
    if changed:
        path.write_text(updated, encoding="utf-8")
    return changed


def _base_test_errors(text: str) -> list[str]:
    errors: list[str] = []
    if BASE_TEST_HELPER not in text:
        errors.append("BaseTest.sol is missing the _deployMidnight helper.")
    if BASE_TEST_DEPLOY_NEW not in text:
        errors.append("BaseTest.sol does not route setUp() through _deployMidnight().")
    if BASE_TEST_DEPLOY_STILL_HARDCODED in text:
        errors.append("BaseTest.sol still hardcodes Midnight deployment in setUp().")
    if BASE_TEST_OLD_VERITY_MARKET_STATE_SLOT in text:
        errors.append("BaseTest.sol still seeds Verity marketState at the legacy slot 10.")
    if BASE_TEST_SCAFFOLD_TO_ID in text:
        errors.append("BaseTest.sol still returns the legacy Verity scaffold market id.")
    return errors


def _liquidation_test_errors(text: str) -> list[str]:
    errors: list[str] = []
    if LIQUIDATION_OLD_VERITY_POSITION_SLOT in text:
        errors.append("LiquidationTest.sol still writes Verity position debt at the legacy slot 11.")
    return errors


def _foundry_toml_errors(text: str) -> list[str]:
    errors: list[str] = []
    if "[profile.difftest]" not in text:
        errors.append("foundry.toml is missing the difftest profile.")
    if "../artifacts/midnight" not in text:
        errors.append("foundry.toml does not allow parity tests to read ../artifacts/midnight.")
    if 'evm_version = "osaka"' not in text:
        errors.append("foundry.toml difftest profile does not preserve Midnight's Osaka EVM target.")
    return errors


def validate_repo(root: pathlib.Path) -> list[str]:
    errors: list[str] = []

    base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
    if not base_test.is_file():
        errors.append(f"Missing harness file: {base_test.relative_to(root)}")
    else:
        errors.extend(_base_test_errors(base_test.read_text(encoding="utf-8")))

    liquidation_test = root / "morpho-midnight" / "test" / "LiquidationTest.sol"
    if liquidation_test.is_file():
        errors.extend(_liquidation_test_errors(liquidation_test.read_text(encoding="utf-8")))

    for rel_path, old, _new in DIRECT_TOUCH_MARKET_REPLACEMENTS:
        test_file = root / "morpho-midnight" / rel_path
        if test_file.is_file() and old in test_file.read_text(encoding="utf-8"):
            errors.append(f"{rel_path} still calls midnight.touchMarket directly instead of the harness helper.")

    foundry_toml = root / "morpho-midnight" / "foundry.toml"
    if not foundry_toml.is_file():
        errors.append(f"Missing Foundry config: {foundry_toml.relative_to(root)}")
    else:
        errors.extend(_foundry_toml_errors(foundry_toml.read_text(encoding="utf-8")))

    return errors


def patch_repo(root: pathlib.Path) -> list[str]:
    changes: list[str] = []

    base_test = root / "morpho-midnight" / "test" / "BaseTest.sol"
    if base_test.is_file() and _patch_file(base_test, _patch_base_test):
        changes.append(str(base_test.relative_to(root)))

    liquidation_test = root / "morpho-midnight" / "test" / "LiquidationTest.sol"
    if liquidation_test.is_file() and _patch_file(liquidation_test, _patch_liquidation_test):
        changes.append(str(liquidation_test.relative_to(root)))

    for rel_path, old, new in DIRECT_TOUCH_MARKET_REPLACEMENTS:
        test_file = root / "morpho-midnight" / rel_path
        if test_file.is_file() and _patch_file(
            test_file,
            lambda text, old=old, new=new: _patch_direct_touch_market(text, old, new),
        ):
            changes.append(str(test_file.relative_to(root)))

    foundry_toml = root / "morpho-midnight" / "foundry.toml"
    if foundry_toml.is_file() and _patch_file(foundry_toml, _patch_foundry_toml):
        changes.append(str(foundry_toml.relative_to(root)))

    return changes


def main() -> int:
    changes = patch_repo(ROOT)
    errors = validate_repo(ROOT)
    if errors:
        print("Morpho Midnight harness patch failed validation:", file=sys.stderr)
        for error in errors:
            print(f"- {error}", file=sys.stderr)
        return 2
    if changes:
        print("Patched Morpho Midnight harness for MIDNIGHT_IMPL parity:")
        for path in changes:
            print(f"- {path}")
    else:
        print("Morpho Midnight harness already matches MIDNIGHT_IMPL parity expectations.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
