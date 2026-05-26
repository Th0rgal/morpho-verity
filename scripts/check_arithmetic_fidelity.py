#!/usr/bin/env python3
"""Fail-closed sync check for Morpho arithmetic-fidelity assumptions."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
DOC_PATH = ROOT / "docs" / "ARITHMETIC_FIDELITY.md"
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"
MATH_PATH = ROOT / "Morpho" / "Libraries" / "MathLib.lean"
PROOF_ASSUMPTIONS = {
  ROOT / "Morpho" / "Proofs" / "Invariants.lean": {
    "h_no_overflow",
    "h_supply_no_overflow",
    "h_shares_no_overflow",
    "h_denom_no_overflow",
  },
  ROOT / "Morpho" / "Proofs" / "ShareConsistency.lean": {
    "h_pos_no_overflow",
    "h_total_no_overflow",
  },
  ROOT / "Morpho" / "Proofs" / "SolidityBridge.lean": {
    "h_no_overflow",
  },
}
UINT128_MAX = "340282366920938463463374607431768211455"
UINT128_GUARD_RE = re.compile(
  rf'require\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*<=\s*{UINT128_MAX}\s*\)\s*"uint128 overflow"'
)


class ArithmeticFidelityError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise ArithmeticFidelityError(f"failed to read {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise ArithmeticFidelityError(f"{path} is not valid UTF-8: {exc}") from exc


def extract_uint128_guard_names(macro_text: str) -> set[str]:
  return set(UINT128_GUARD_RE.findall(macro_text))


def validate_uint128_guard_docs(macro_text: str, doc_text: str) -> None:
  guarded_names = extract_uint128_guard_names(macro_text)
  missing = sorted(name for name in guarded_names if f"`{name}`" not in doc_text)
  if missing:
    raise ArithmeticFidelityError(
      "ARITHMETIC_FIDELITY.md is missing uint128 guard docs for: "
      + ", ".join(missing)
    )
  if not guarded_names:
    raise ArithmeticFidelityError("MacroSlice.lean has no documented uint128 overflow guards")


def validate_proof_assumption_docs(doc_text: str) -> None:
  for path, assumptions in PROOF_ASSUMPTIONS.items():
    proof_text = read_text(path)
    rel = path.relative_to(ROOT).as_posix()
    if f"`{rel}`" not in doc_text:
      raise ArithmeticFidelityError(
        f"ARITHMETIC_FIDELITY.md is missing proof file `{rel}`"
      )
    for assumption in sorted(assumptions):
      if assumption not in proof_text:
        raise ArithmeticFidelityError(
          f"expected arithmetic assumption `{assumption}` in `{rel}`"
        )
      if f"`{assumption}`" not in doc_text:
        raise ArithmeticFidelityError(
          f"ARITHMETIC_FIDELITY.md is missing assumption `{assumption}` from `{rel}`"
        )


def validate_mathlib_boundary(math_text: str, doc_text: str) -> None:
  required_math_terms = ("mulDivDown", "mulDivUp", "checked arithmetic")
  missing_math_terms = [
    term for term in required_math_terms if term not in math_text
  ]
  if missing_math_terms:
    raise ArithmeticFidelityError(
      "MathLib.lean is missing arithmetic boundary terms: "
      + ", ".join(missing_math_terms)
    )
  required_doc_terms = ("mulDivDown", "mulDivUp", "mulDiv512Down/Up")
  missing_doc_terms = [term for term in required_doc_terms if term not in doc_text]
  if missing_doc_terms:
    raise ArithmeticFidelityError(
      "ARITHMETIC_FIDELITY.md is missing MathLib boundary terms: "
      + ", ".join(missing_doc_terms)
    )


def main() -> int:
  doc_text = read_text(DOC_PATH)
  validate_uint128_guard_docs(read_text(MACRO_PATH), doc_text)
  validate_proof_assumption_docs(doc_text)
  validate_mathlib_boundary(read_text(MATH_PATH), doc_text)
  print("arithmetic-fidelity check: OK")
  return 0


if __name__ == "__main__":
  try:
    raise SystemExit(main())
  except ArithmeticFidelityError as exc:
    print(f"arithmetic-fidelity check failed: {exc}", file=sys.stderr)
    raise SystemExit(1)
