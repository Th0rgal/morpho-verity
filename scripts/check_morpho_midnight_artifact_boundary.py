#!/usr/bin/env python3
"""Validate focused-vs-complete Morpho Midnight artifact boundaries."""

from __future__ import annotations

import hashlib
import pathlib
import sys

from focused_midnight_digest import (
    FOCUSED_INPUTS,
    compute_focused_input_digest as compute_canonical_focused_input_digest,
)


ROOT = pathlib.Path(__file__).resolve().parents[1]
FOCUSED_DIR = ROOT / "artifacts" / "midnight-focused"
FOCUSED_BIN = FOCUSED_DIR / "MidnightRCF.bin.raw"
FOCUSED_YUL = FOCUSED_DIR / "MidnightRCF.yul"
FOCUSED_ABI = FOCUSED_DIR / "MidnightRCF.abi.json"
FOCUSED_MANIFEST = FOCUSED_DIR / "MidnightRCF.artifact-manifest.env"
FOCUSED_README = FOCUSED_DIR / "README.md"
FORBIDDEN_FOCUSED_FULL_NAME = FOCUSED_DIR / "Midnight.bin.raw"
FULL_BIN = ROOT / "artifacts" / "midnight" / "Midnight.bin.raw"
FULL_YUL = ROOT / "artifacts" / "midnight" / "Midnight.yul"
FULL_MANIFEST = ROOT / "artifacts" / "midnight" / "Midnight.artifact-manifest.env"
PARITY_SCRIPT = ROOT / "scripts" / "run_morpho_midnight_parity.sh"
HARNESS = ROOT / "morpho-midnight" / "test" / "BaseTest.sol"
MIDNIGHT_CONTRACT = ROOT / "morpho-midnight-verity" / "Midnight" / "Contract.lean"
MIDNIGHT_TRUST_BOUNDARIES = (
    ROOT / "morpho-midnight-verity" / "Midnight" / "Proofs" / "TRUST_BOUNDARIES.md"
)


class MidnightArtifactBoundaryError(RuntimeError):
    pass


def display_path(path: pathlib.Path) -> str:
    try:
        return str(path.relative_to(ROOT))
    except ValueError:
        return str(path)


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def require_nonempty(path: pathlib.Path, label: str) -> None:
    if not path.is_file() or path.stat().st_size == 0:
        raise MidnightArtifactBoundaryError(f"missing non-empty {label}: {display_path(path)}")


def read_text(path: pathlib.Path) -> str:
    try:
        return path.read_text(encoding="utf-8")
    except OSError as exc:
        raise MidnightArtifactBoundaryError(f"failed to read {path.relative_to(ROOT)}: {exc}") from exc


def compute_focused_input_digest() -> str:
    for rel in FOCUSED_INPUTS:
        path = ROOT / rel
        require_nonempty(path, "focused artifact input")
    return compute_canonical_focused_input_digest(ROOT)


def compute_complete_input_digest() -> str:
    files = [
        "lean-toolchain",
        "lake-manifest.json",
        "lakefile.lean",
        "morpho-midnight-verity/Midnight.lean",
        "morpho-midnight-verity/Midnight/Contract.lean",
        "morpho-midnight-verity/Midnight/Compiler/ArtifactConfig.lean",
        "morpho-midnight-verity/Midnight/Compiler/Main.lean",
        "morpho-midnight-verity/MidnightCompiler.lean",
        "scripts/prepare_midnight_artifact.sh",
        "scripts/uniquify_yul_shadows.py",
    ]
    h = hashlib.sha256()
    for rel in files:
        path = ROOT / rel
        require_nonempty(path, "complete artifact input")
        h.update(f"{sha256(path)}  {rel}\n".encode("utf-8"))
    return h.hexdigest()


def parse_manifest(text: str) -> dict[str, str]:
    values: dict[str, str] = {}
    for line in text.splitlines():
        if not line or line.startswith("#"):
            continue
        if "=" not in line:
            raise MidnightArtifactBoundaryError(f"invalid manifest line: {line}")
        key, value = line.split("=", 1)
        if key in values:
            raise MidnightArtifactBoundaryError(f"duplicate manifest key: {key}")
        values[key] = value
    return values


def validate_focused_artifact() -> None:
    require_nonempty(FOCUSED_BIN, "focused MidnightRCF bytecode")
    require_nonempty(FOCUSED_YUL, "focused MidnightRCF Yul")
    require_nonempty(FOCUSED_ABI, "focused MidnightRCF ABI")
    require_nonempty(FOCUSED_MANIFEST, "focused MidnightRCF artifact manifest")
    require_nonempty(FOCUSED_README, "focused Midnight artifact README")

    if FORBIDDEN_FOCUSED_FULL_NAME.exists():
        raise MidnightArtifactBoundaryError(
            f"{FORBIDDEN_FOCUSED_FULL_NAME.relative_to(ROOT)} must not exist; "
            "focused artifacts must not use the full Midnight artifact name"
        )

    readme = read_text(FOCUSED_README)
    if "not a complete IMidnight implementation" not in readme:
        raise MidnightArtifactBoundaryError(
            "focused artifact README must state that MidnightRCF is not a complete IMidnight implementation"
        )

    manifest = parse_manifest(read_text(FOCUSED_MANIFEST))
    if manifest.get("artifact_scope") != "focused-midnight-rcf":
        raise MidnightArtifactBoundaryError("focused artifact manifest has wrong artifact_scope")
    if manifest.get("contract_name") != "MidnightRCF":
        raise MidnightArtifactBoundaryError("focused artifact manifest has wrong contract_name")
    if manifest.get("complete_imidnight_artifact") != "0":
        raise MidnightArtifactBoundaryError("focused artifact manifest must mark complete_imidnight_artifact=0")
    expected_digest = compute_focused_input_digest()
    if manifest.get("input_digest") != expected_digest:
        raise MidnightArtifactBoundaryError(
            "focused artifact manifest input_digest is stale; rerun "
            "./scripts/prepare_focused_midnight_artifact.sh"
        )


def validate_complete_artifact_boundary() -> None:
    if not FULL_BIN.exists():
        return
    require_nonempty(FULL_BIN, "complete Midnight bytecode")
    require_nonempty(FULL_YUL, "complete Midnight Yul")
    require_nonempty(FULL_MANIFEST, "complete Midnight artifact manifest")
    manifest = parse_manifest(read_text(FULL_MANIFEST))
    if manifest.get("artifact_scope") != "midnight-full-imidnight":
        raise MidnightArtifactBoundaryError("complete artifact manifest has wrong artifact_scope")
    if manifest.get("contract_name") != "Midnight":
        raise MidnightArtifactBoundaryError("complete artifact manifest has wrong contract_name")
    if manifest.get("complete_imidnight_artifact") != "1":
        raise MidnightArtifactBoundaryError("complete artifact manifest must mark complete_imidnight_artifact=1")
    if manifest.get("parity_ready") != "1":
        raise MidnightArtifactBoundaryError("complete artifact manifest must mark parity_ready=1")
    expected_digest = compute_complete_input_digest()
    if manifest.get("input_digest") != expected_digest:
        raise MidnightArtifactBoundaryError(
            "complete artifact manifest input_digest is stale; rerun "
            "./scripts/prepare_midnight_artifact.sh"
        )
    if sha256(FULL_BIN) == sha256(FOCUSED_BIN):
        raise MidnightArtifactBoundaryError(
            "complete Midnight.bin.raw must not be a copy of focused MidnightRCF.bin.raw"
        )
    validate_store_market_create2_result(read_text(FULL_YUL))


def validate_store_market_create2_result(yul: str) -> None:
    function_start = yul.find("function internal_internal_codeDataStoreMarket")
    if function_start < 0:
        raise MidnightArtifactBoundaryError(
            "complete Midnight Yul is missing internal_internal_codeDataStoreMarket"
        )
    next_function = yul.find("\n        function ", function_start + 1)
    function_body = (
        yul[function_start:] if next_function < 0 else yul[function_start:next_function]
    )
    create2_call = (
        "create2(0, __midnight_store_ptr, __midnight_store_initcode_length, salt)"
    )
    if create2_call not in function_body:
        raise MidnightArtifactBoundaryError(
            "complete Midnight Yul storeMarket body is missing the CREATE2 deployment"
        )
    if "__ret0 := pointer" not in function_body:
        raise MidnightArtifactBoundaryError(
            "complete Midnight Yul storeMarket body is missing the pointer return"
        )
    create2_idx = function_body.find("create2(")
    return_idx = function_body.rfind("__ret0 := pointer")
    assignment_idx = function_body.find(
        "pointer := __midnight_store_create2_result", create2_idx
    )
    if assignment_idx < 0 or assignment_idx > return_idx:
        raise MidnightArtifactBoundaryError(
            "complete Midnight Yul storeMarket must assign the CREATE2 result to pointer before returning"
        )


def validate_midnight_trust_boundaries() -> None:
    contract = read_text(MIDNIGHT_CONTRACT)
    doc = read_text(MIDNIGHT_TRUST_BOUNDARIES)
    expected = [
        "midnight_idlib_market_initcode_abi_hash",
        "midnight_sstore2_market_initcode_layout",
        "midnight_sstore2_market_runtime_abi_decode",
        "midnight_sstore2_market_runtime_abi_return",
    ]
    stale = ["midnight_idlib_toid_market_abi_create2_preimage"]
    for name in expected:
        if name in contract and name not in doc:
            raise MidnightArtifactBoundaryError(
                f"Midnight trust boundary doc is missing current ECM axiom: {name}"
            )
    for name in stale:
        if name in doc and name not in contract:
            raise MidnightArtifactBoundaryError(
                f"Midnight trust boundary doc mentions stale ECM axiom: {name}"
            )


def validate_parity_paths() -> None:
    script = read_text(PARITY_SCRIPT)
    harness = read_text(HARNESS)
    required = "../artifacts/midnight/Midnight.bin.raw"
    forbidden = "../artifacts/midnight-focused/MidnightRCF.bin.raw"
    if required not in script and "artifacts/midnight/Midnight.bin.raw" not in script:
        raise MidnightArtifactBoundaryError(
            "run_morpho_midnight_parity.sh must require artifacts/midnight/Midnight.bin.raw"
        )
    if "MIDNIGHT_IMPL" in harness and required not in harness:
        raise MidnightArtifactBoundaryError(
            "Morpho Midnight harness must read ../artifacts/midnight/Midnight.bin.raw for MIDNIGHT_IMPL=verity"
        )
    if forbidden in script or forbidden in harness:
        raise MidnightArtifactBoundaryError(
            "full Midnight parity path must not consume artifacts/midnight-focused/MidnightRCF.bin.raw"
        )


def main() -> int:
    try:
        validate_focused_artifact()
        validate_complete_artifact_boundary()
        validate_midnight_trust_boundaries()
        validate_parity_paths()
    except MidnightArtifactBoundaryError as exc:
        print(f"morpho-midnight-artifact-boundary check failed: {exc}", file=sys.stderr)
        return 1

    full_status = "present" if FULL_BIN.exists() else "missing"
    print(
        "morpho-midnight-artifact-boundary check: OK "
        f"(focused=present, complete={full_status})"
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
