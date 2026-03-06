#!/usr/bin/env python3
"""Patch the Morpho Blue test harness to honor MORPHO_IMPL for parity runs."""

from __future__ import annotations

import pathlib
import sys
from typing import Callable

ROOT = pathlib.Path(__file__).resolve().parents[1]

BASE_TEST_DEPLOY_OLD = "        morpho = IMorpho(address(new Morpho(OWNER)));\n"
BASE_TEST_DEPLOY_NEW = "        morpho = _deployMorpho(OWNER);\n"
BASE_TEST_INSERT_BEFORE = "    function setUp() public virtual {\n"
BASE_TEST_HELPER = """    function _deployMorpho(address owner) internal returns (IMorpho) {
        string memory impl = vm.envOr("MORPHO_IMPL", string("solidity"));
        bytes32 implHash = keccak256(bytes(impl));

        console.log("Morpho harness implementation:");
        console.log(impl);

        if (implHash == keccak256(bytes("solidity"))) {
            return IMorpho(address(new Morpho(owner)));
        }
        if (implHash == keccak256(bytes("verity"))) {
            bytes memory initCode = bytes.concat(vm.getCode("../artifacts/yul/Morpho.bin"), abi.encode(owner));
            address deployed;
            assembly {
                deployed := create(0, add(initCode, 0x20), mload(initCode))
                if iszero(deployed) {
                    returndatacopy(0, 0, returndatasize())
                    revert(0, returndatasize())
                }
            }
            vm.label(deployed, "Morpho");
            return IMorpho(deployed);
        }

        revert(string(abi.encodePacked("Unsupported MORPHO_IMPL: ", impl)));
    }

"""

ONLY_OWNER_ZERO_OLD = "        new Morpho(address(0));\n"
ONLY_OWNER_ZERO_NEW = "        _deployMorpho(address(0));\n"
ONLY_OWNER_OWNER_OLD = "        new Morpho(OWNER);\n"
ONLY_OWNER_OWNER_NEW = "        _deployMorpho(OWNER);\n"


def _patch_base_test(text: str) -> tuple[str, bool]:
    updated = text
    changed = False

    if BASE_TEST_HELPER not in updated and BASE_TEST_INSERT_BEFORE in updated:
        updated = updated.replace(BASE_TEST_INSERT_BEFORE, BASE_TEST_HELPER + BASE_TEST_INSERT_BEFORE, 1)
        changed = True

    if BASE_TEST_DEPLOY_OLD in updated:
        updated = updated.replace(BASE_TEST_DEPLOY_OLD, BASE_TEST_DEPLOY_NEW, 1)
        changed = True

    return updated, changed


def _patch_only_owner_test(text: str) -> tuple[str, bool]:
    updated = text
    changed = False

    if ONLY_OWNER_ZERO_OLD in updated:
        updated = updated.replace(ONLY_OWNER_ZERO_OLD, ONLY_OWNER_ZERO_NEW, 1)
        changed = True
    if ONLY_OWNER_OWNER_OLD in updated:
        updated = updated.replace(ONLY_OWNER_OWNER_OLD, ONLY_OWNER_OWNER_NEW, 1)
        changed = True

    return updated, changed


def _patch_file(path: pathlib.Path, patcher: Callable[[str], tuple[str, bool]]) -> bool:
    original = path.read_text(encoding="utf-8")
    updated, changed = patcher(original)
    if changed:
        path.write_text(updated, encoding="utf-8")
    return changed


def patch_repo(root: pathlib.Path) -> list[str]:
    changes: list[str] = []

    base_test = root / "morpho-blue" / "test" / "BaseTest.sol"
    if base_test.is_file() and _patch_file(base_test, _patch_base_test):
        changes.append(str(base_test.relative_to(root)))

    only_owner = root / "morpho-blue" / "test" / "integration" / "OnlyOwnerIntegrationTest.sol"
    if only_owner.is_file() and _patch_file(only_owner, _patch_only_owner_test):
        changes.append(str(only_owner.relative_to(root)))

    return changes


def main() -> int:
    changes = patch_repo(ROOT)
    if changes:
        print("Patched Morpho Blue harness for MORPHO_IMPL parity:")
        for path in changes:
            print(f"- {path}")
    else:
        print("Morpho Blue harness already matches MORPHO_IMPL parity expectations.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
