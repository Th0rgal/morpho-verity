#!/usr/bin/env python3
"""Canonical input digest for the focused MidnightRCF artifact."""

from __future__ import annotations

import hashlib
import pathlib
import sys


ROOT = pathlib.Path(__file__).resolve().parents[1]


FOCUSED_INPUTS = (
    "lean-toolchain",
    "lake-manifest.json",
    "lakefile.lean",
    "morpho-midnight-verity/Midnight.lean",
    "morpho-midnight-verity/Midnight/Contract.lean",
    "morpho-midnight-verity/Midnight/Compiler/ArtifactConfig.lean",
    "morpho-midnight-verity/Midnight/Compiler/Main.lean",
    "morpho-midnight-verity/MidnightCompiler.lean",
    "scripts/prepare_focused_midnight_artifact.sh",
    "scripts/uniquify_yul_shadows.py",
)


def sha256(path: pathlib.Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def compute_focused_input_digest(root: pathlib.Path = ROOT) -> str:
    digest_input = "".join(
        f"{sha256(root / rel)}  {rel}\n" for rel in FOCUSED_INPUTS
    )
    return hashlib.sha256(digest_input.encode("utf-8")).hexdigest()


def main_with_root(root: pathlib.Path) -> int:
    for rel in FOCUSED_INPUTS:
        path = root / rel
        if not path.is_file():
            print(f"ERROR: missing focused Midnight artifact input: {path}", file=sys.stderr)
            return 2
    print(compute_focused_input_digest(root))
    return 0


def main() -> int:
    root = pathlib.Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else ROOT
    return main_with_root(root)


if __name__ == "__main__":
    sys.exit(main())
