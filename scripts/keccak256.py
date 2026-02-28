#!/usr/bin/env python3
"""Forwarder to Verity's canonical keccak256 selector helper."""

from __future__ import annotations

import runpy
import sys
from pathlib import Path


def main() -> int:
  root = Path(__file__).resolve().parent.parent
  target = root / ".lake" / "packages" / "verity" / "scripts" / "keccak256.py"
  if not target.exists():
    print(
      f"ERROR: missing delegated selector helper: {target}",
      file=sys.stderr,
    )
    return 1

  sys.argv[0] = str(target)
  runpy.run_path(str(target), run_name="__main__")
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
