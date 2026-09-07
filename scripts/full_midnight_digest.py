#!/usr/bin/env python3
"""Canonical inputs for the complete, Solidity-generated Midnight artifact.

The handwritten implementation and admin hybrid are deliberately not inputs.
The separately packaged focused proof artifact keeps its own digest definition.
"""
from __future__ import annotations
import hashlib
import pathlib
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
FULL_INPUTS = (
    'lean-toolchain', 'lake-manifest.json', 'lakefile.lean',
    'morpho-midnight-verity/Midnight/Generated/FullModel.lean',
    'morpho-midnight-verity/Midnight/Generated/FullModel.manifest.json',
    'morpho-midnight-verity/Midnight/Generated/FullModel.abi.json',
    'morpho-midnight-verity/Midnight/Compiler/ArtifactConfig.lean',
    'morpho-midnight-verity/Midnight/Compiler/Main.lean',
    'morpho-midnight-verity/MidnightCompiler.lean',
    'scripts/prepare_midnight_artifact.sh',
    'scripts/full_midnight_digest.py',
    'scripts/compile_midnight_yul.py',
    'scripts/audit_midnight_memory.mjs',
    'scripts/lib/midnight-cei.mjs',
    'scripts/import_midnight_full.mjs',
    'scripts/lib/midnight-source.mjs',
    'scripts/lib/midnight-lowering.mjs',
    'scripts/lib/midnight-abi.mjs',
    'scripts/lib/midnight-builtins.mjs',
    'scripts/lib/keccak256.mjs',
    'config/midnight-full-import.json',
    'scripts/uniquify_yul_shadows.py',
)

def compute_full_input_digest(root: pathlib.Path = ROOT) -> str:
    records = ''.join(
        f'{hashlib.sha256((root / rel).read_bytes()).hexdigest()}  {rel}\n'
        for rel in FULL_INPUTS
    )
    return hashlib.sha256(records.encode()).hexdigest()

def main() -> int:
    root = pathlib.Path(sys.argv[1]).resolve() if len(sys.argv) > 1 else ROOT
    for rel in FULL_INPUTS:
        path = root / rel
        if not path.is_file() or not path.stat().st_size:
            print(f'ERROR: missing full Midnight artifact input: {path}', file=sys.stderr)
            return 2
    print(compute_full_input_digest(root))
    return 0

if __name__ == '__main__':
    sys.exit(main())
