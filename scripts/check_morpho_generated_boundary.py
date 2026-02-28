#!/usr/bin/env python3
"""Fail-closed check: keep direct manual compiler surfaces behind Generated.lean."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
ALLOWED_FILES = {
  ROOT / "Morpho" / "Compiler" / "Generated.lean",
  ROOT / "Morpho" / "Compiler" / "Spec.lean",
}
TARGET_GLOB = ROOT / "Morpho" / "Compiler"
SYMBOL_RE = re.compile(r"\b(morphoSpec|morphoSelectors)\b")


def main() -> None:
  offenders: list[str] = []

  for path in sorted(TARGET_GLOB.glob("*.lean")):
    if path in ALLOWED_FILES:
      continue
    text = path.read_text(encoding="utf-8")
    if SYMBOL_RE.search(text):
      offenders.append(str(path.relative_to(ROOT)))

  if offenders:
    print("morpho-generated-boundary check failed:", file=sys.stderr)
    for rel in offenders:
      print(f"  direct morphoSpec/morphoSelectors usage in {rel}", file=sys.stderr)
    print(
      "Use Morpho.Compiler.Generated.{morphoGeneratedSpec,morphoGeneratedSelectors} as the canonical compiler boundary.",
      file=sys.stderr,
    )
    sys.exit(1)

  print("morpho-generated-boundary check: OK")


if __name__ == "__main__":
  main()
