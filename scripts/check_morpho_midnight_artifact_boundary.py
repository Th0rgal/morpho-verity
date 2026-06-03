#!/usr/bin/env python3
"""Validate focused-vs-complete Morpho Midnight artifact boundaries."""

from __future__ import annotations

import hashlib
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parents[1]
FOCUSED_DIR = ROOT / "artifacts" / "midnight-focused"
FOCUSED_BIN = FOCUSED_DIR / "MidnightRCF.bin.raw"
FOCUSED_YUL = FOCUSED_DIR / "MidnightRCF.yul"
FOCUSED_ABI = FOCUSED_DIR / "MidnightRCF.abi.json"
FOCUSED_MANIFEST = FOCUSED_DIR / "MidnightRCF.artifact-manifest.env"
FOCUSED_README = FOCUSED_DIR / "README.md"
FORBIDDEN_FOCUSED_FULL_NAME = FOCUSED_DIR / "Midnight.bin.raw"
FULL_BIN = ROOT / "artifacts" / "midnight" / "Midnight.bin.raw"
PARITY_SCRIPT = ROOT / "scripts" / "run_morpho_midnight_parity.sh"
HARNESS = ROOT / "morpho-midnight" / "test" / "BaseTest.sol"


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
    files = [
        ROOT / "lean-toolchain",
        ROOT / "lake-manifest.json",
        ROOT / "lakefile.lean",
        ROOT / "morpho-midnight-verity" / "Midnight.lean",
        ROOT / "morpho-midnight-verity" / "Midnight" / "Contract.lean",
        ROOT / "morpho-midnight-verity" / "Midnight" / "Compiler" / "ArtifactConfig.lean",
        ROOT / "morpho-midnight-verity" / "Midnight" / "Compiler" / "Main.lean",
        ROOT / "morpho-midnight-verity" / "MidnightCompiler.lean",
        ROOT / "scripts" / "prepare_focused_midnight_artifact.sh",
        ROOT / "scripts" / "uniquify_yul_shadows.py",
    ]
    h = hashlib.sha256()
    for path in files:
        require_nonempty(path, "focused artifact input")
        h.update(hashlib.sha256(path.read_bytes()).hexdigest().encode("utf-8"))
        h.update(b"  ")
        h.update(str(path).encode("utf-8"))
        h.update(b"\n")
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
    if sha256(FULL_BIN) == sha256(FOCUSED_BIN):
        raise MidnightArtifactBoundaryError(
            "complete Midnight.bin.raw must not be a copy of focused MidnightRCF.bin.raw"
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
    if required not in harness:
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
