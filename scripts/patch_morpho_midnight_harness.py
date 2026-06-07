#!/usr/bin/env python3
"""Patch the Morpho Midnight deployment harness to honor MIDNIGHT_IMPL."""

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

    while updated.count(BASE_TEST_HELPER) > 1:
        first = updated.find(BASE_TEST_HELPER)
        second = updated.find(BASE_TEST_HELPER, first + len(BASE_TEST_HELPER))
        updated = updated[:second] + updated[second + len(BASE_TEST_HELPER):]
        changed = True

    return updated, changed


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
        print("Patched Morpho Midnight deployment harness for MIDNIGHT_IMPL parity:")
        for path in changes:
            print(f"- {path}")
    else:
        print("Morpho Midnight harness already matches MIDNIGHT_IMPL parity expectations.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
