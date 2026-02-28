#!/usr/bin/env python3
"""Fail-closed guard for Morpho artifact/input path separation."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
TARGETS = [
  ROOT / "Morpho" / "Compiler" / "Main.lean",
  ROOT / "Morpho" / "Compiler" / "MainTest.lean",
  ROOT / "scripts" / "prepare_verity_morpho_artifact.sh",
  ROOT / "scripts" / "run_morpho_blue_parity.sh",
  ROOT / "scripts" / "report_yul_identity_gap.py",
  ROOT / "scripts" / "test_prepare_verity_morpho_artifact.sh",
  ROOT / "scripts" / "test_run_morpho_blue_parity.sh",
  ROOT / "verity-foundry" / "foundry.toml",
  ROOT / "verity-foundry" / "test" / "VerityMorphoSmoke.t.sol",
  ROOT / "README.md",
  ROOT / "docs" / "PARITY_TARGET.md",
  ROOT / "docs" / "RELEASE_CRITERIA.md",
  ROOT / ".github" / "workflows" / "verify.yml",
]
FORBIDDEN = [
  (re.compile(r"compiler/yul"), "legacy generated-output path 'compiler/yul'"),
  (re.compile(r"compiler/external-libs"), "legacy tracked-input path 'compiler/external-libs'"),
]
REQUIRED_FILES = [
  ROOT / "artifacts" / "inputs" / "MarketParamsHash.yul",
]
FORBIDDEN_FILES = [
  ROOT / "compiler" / "external-libs" / "MarketParamsHash.yul",
]


def main() -> None:
  offenders: list[tuple[str, int, str]] = []

  for path in TARGETS:
    text = path.read_text(encoding="utf-8")
    for lineno, line in enumerate(text.splitlines(), start=1):
      for pattern, reason in FORBIDDEN:
        if pattern.search(line):
          offenders.append((str(path.relative_to(ROOT)), lineno, reason))

  missing_required = [
    str(path.relative_to(ROOT)) for path in REQUIRED_FILES if not path.is_file()
  ]
  present_forbidden = [
    str(path.relative_to(ROOT)) for path in FORBIDDEN_FILES if path.exists()
  ]

  if offenders or missing_required or present_forbidden:
    print("artifact-layout-boundary check failed:", file=sys.stderr)
    for rel, lineno, reason in offenders:
      print(f"  {rel}:{lineno}: {reason}", file=sys.stderr)
    for rel in missing_required:
      print(f"  missing required file: {rel}", file=sys.stderr)
    for rel in present_forbidden:
      print(f"  forbidden legacy file present: {rel}", file=sys.stderr)
    print(
      "Use artifacts/yul for generated outputs and artifacts/inputs for tracked Yul inputs.",
      file=sys.stderr,
    )
    sys.exit(1)

  print("artifact-layout-boundary check: OK")


if __name__ == "__main__":
  main()
