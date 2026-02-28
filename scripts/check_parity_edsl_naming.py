#!/usr/bin/env python3
"""Fail-closed guard for EDSL-only parity naming."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
TARGETS = [
  ROOT / "scripts" / "prepare_verity_morpho_artifact.sh",
  ROOT / "scripts" / "check_input_mode_parity.sh",
  ROOT / "scripts" / "run_morpho_blue_parity.sh",
  ROOT / "README.md",
  ROOT / "docs" / "PARITY_TARGET.md",
  ROOT / "docs" / "RELEASE_CRITERIA.md",
  ROOT / ".github" / "workflows" / "verify.yml",
]
FORBIDDEN = [
  (re.compile(r"/model/"), "legacy parity fixture path '/model/'"),
  (re.compile(r"\binput mode\b", re.IGNORECASE), "legacy wording 'input mode'"),
]


def main() -> None:
  offenders: list[tuple[str, int, str]] = []

  for path in TARGETS:
    text = path.read_text(encoding="utf-8")
    for lineno, line in enumerate(text.splitlines(), start=1):
      for pattern, reason in FORBIDDEN:
        if pattern.search(line):
          offenders.append((str(path.relative_to(ROOT)), lineno, reason))

  if offenders:
    print("parity-edsl-naming check failed:", file=sys.stderr)
    for rel, lineno, reason in offenders:
      print(f"  {rel}:{lineno}: {reason}", file=sys.stderr)
    print("Use EDSL-only parity naming in scripts/docs/workflows.", file=sys.stderr)
    sys.exit(1)

  print("parity-edsl-naming check: OK")


if __name__ == "__main__":
  main()
