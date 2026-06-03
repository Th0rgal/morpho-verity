#!/usr/bin/env python3
"""Validate the Morpho Midnight mapping manifest."""

from __future__ import annotations

import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
INTERFACE = ROOT / "morpho-midnight" / "src" / "interfaces" / "IMidnight.sol"
MANIFEST = ROOT / "MORPHO_MIDNIGHT_MAPPING.md"
ARTIFACT = ROOT / "artifacts" / "midnight" / "Midnight.bin.raw"
FOCUSED_ARTIFACT = ROOT / "artifacts" / "midnight-focused" / "MidnightRCF.bin.raw"


def interface_functions(text: str) -> list[str]:
    return re.findall(r"\bfunction\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(", text)


def manifest_functions(text: str) -> set[str]:
    names: set[str] = set()
    for line in text.splitlines():
        match = re.match(r"\|\s*`([A-Za-z_][A-Za-z0-9_]*)\(\)`\s*\|", line)
        if match:
            names.add(match.group(1))
    return names


def artifact_status(text: str, label: str = "Complete") -> str | None:
    match = re.search(rf"^{label} artifact status:\s*(\w+)\s*$", text, re.MULTILINE)
    return match.group(1) if match else None


def main() -> int:
    errors: list[str] = []

    if not INTERFACE.is_file():
        errors.append(f"missing interface file: {INTERFACE.relative_to(ROOT)}")
    if not MANIFEST.is_file():
        errors.append(f"missing manifest file: {MANIFEST.relative_to(ROOT)}")
    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return 2

    interface_text = INTERFACE.read_text(encoding="utf-8")
    manifest_text = MANIFEST.read_text(encoding="utf-8")

    expected = interface_functions(interface_text)
    mapped = manifest_functions(manifest_text)
    missing = sorted(set(expected) - mapped)
    extra = sorted(mapped - set(expected))

    if missing:
        errors.append("manifest is missing IMidnight functions: " + ", ".join(missing))
    if extra:
        errors.append("manifest names functions not in IMidnight: " + ", ".join(extra))

    status = artifact_status(manifest_text)
    expected_status = "present" if ARTIFACT.is_file() and ARTIFACT.stat().st_size > 0 else "missing"
    if status is None:
        errors.append("manifest is missing `Complete artifact status:` line")
    elif status != expected_status:
        errors.append(
            "manifest artifact status is "
            f"`{status}`, expected `{expected_status}` for {ARTIFACT.relative_to(ROOT)}"
        )

    focused_status = artifact_status(manifest_text, "Focused")
    expected_focused_status = (
        "present" if FOCUSED_ARTIFACT.is_file() and FOCUSED_ARTIFACT.stat().st_size > 0 else "missing"
    )
    if focused_status is None:
        errors.append("manifest is missing `Focused artifact status:` line")
    elif focused_status != expected_focused_status:
        errors.append(
            "manifest focused artifact status is "
            f"`{focused_status}`, expected `{expected_focused_status}` for "
            f"{FOCUSED_ARTIFACT.relative_to(ROOT)}"
        )

    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return 1

    print(
        "Morpho Midnight mapping covers "
        f"{len(expected)} IMidnight function entries; complete artifact status is "
        f"{expected_status}; focused artifact status is {expected_focused_status}."
    )
    return 0


if __name__ == "__main__":
    sys.exit(main())
