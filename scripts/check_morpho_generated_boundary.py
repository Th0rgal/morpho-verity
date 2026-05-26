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
GENERATED_PATH = ROOT / "Morpho" / "Compiler" / "Generated.lean"
REQUIRED_EXTERNAL_AXIOMS = {
  "keccakMarketParams": "market_id_deterministic",
  "borrowRate": "irm_borrow_rate_boundary",
  "collateralPrice": "oracle_collateral_price_boundary",
  "oraclePrice": "oracle_price_boundary",
  "flashLoanCallback": "flash_loan_callback_boundary",
}


class MorphoGeneratedBoundaryError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise MorphoGeneratedBoundaryError(f"failed to read {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise MorphoGeneratedBoundaryError(f"{path} is not valid UTF-8: {exc}") from exc


def validate_generated_external_axioms(text: str) -> None:
  for external_name, axiom_name in REQUIRED_EXTERNAL_AXIOMS.items():
    block_match = re.search(
      r'\{\s*name\s*:=\s*"'
      + re.escape(external_name)
      + r'".*?axiomNames\s*:=\s*\[(.*?)\]\s*\}',
      text,
      flags=re.DOTALL,
    )
    if not block_match:
      raise MorphoGeneratedBoundaryError(
        f"generated external {external_name!r} is missing or has no axiomNames block"
      )
    if f'"{axiom_name}"' not in block_match.group(1):
      raise MorphoGeneratedBoundaryError(
        f"generated external {external_name!r} must include axiom name {axiom_name!r}"
      )


def main() -> None:
  offenders: list[str] = []

  for path in sorted(TARGET_GLOB.glob("*.lean")):
    if path in ALLOWED_FILES:
      continue
    text = read_text(path)
    if SYMBOL_RE.search(text):
      offenders.append(str(path.relative_to(ROOT)))

  validate_generated_external_axioms(read_text(GENERATED_PATH))

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
  try:
    main()
  except MorphoGeneratedBoundaryError as exc:
    print(f"morpho-generated-boundary check failed: {exc}", file=sys.stderr)
    raise SystemExit(1)
