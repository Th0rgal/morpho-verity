#!/usr/bin/env python3
"""Fail-closed sync check for Morpho arithmetic-fidelity assumptions."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
DOC_PATH = ROOT / "docs" / "ARITHMETIC_FIDELITY.md"
MACRO_PATH = ROOT / "Morpho" / "Contract.lean"
MATH_PATH = ROOT / "Morpho" / "Libraries" / "MathLib.lean"
PROOF_PATHS = (
  ROOT / "Morpho" / "Proofs" / "Invariants.lean",
  ROOT / "Morpho" / "Proofs" / "ShareConsistency.lean",
  ROOT / "Morpho" / "Proofs" / "SolidityBridge.lean",
)
UINT128_MAX = "340282366920938463463374607431768211455"
UINT128_GUARD_RE = re.compile(
  rf'require\s*\(\s*([A-Za-z_][A-Za-z0-9_]*)\s*<=\s*{UINT128_MAX}\s*\)\s*"uint128 overflow"'
)
PROOF_OVERFLOW_RE = re.compile(r"\bh_[A-Za-z0-9_]*overflow[A-Za-z0-9_]*\b")
CHECKED_HELPERS = ("addPanic", "subPanic", "mulPanic")


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


def extract_proof_overflow_assumptions(proof_text: str) -> set[str]:
  return set(PROOF_OVERFLOW_RE.findall(proof_text))


def validate_uint128_guard_docs(macro_text: str, doc_text: str) -> None:
  guarded_names = extract_uint128_guard_names(macro_text)
  missing = sorted(name for name in guarded_names if f"`{name}`" not in doc_text)
  if missing:
    raise ArithmeticFidelityError(
      "ARITHMETIC_FIDELITY.md is missing uint128 guard docs for: "
      + ", ".join(missing)
    )
  if not guarded_names:
    raise ArithmeticFidelityError("Contract.lean has no documented uint128 overflow guards")


def validate_checked_helper_docs(macro_text: str, doc_text: str) -> None:
  missing_macro = [helper for helper in CHECKED_HELPERS if helper not in macro_text]
  if missing_macro:
    raise ArithmeticFidelityError(
      "Contract.lean is missing checked arithmetic helper(s): "
      + ", ".join(missing_macro)
    )
  missing_doc = [helper for helper in CHECKED_HELPERS if f"`{helper}`" not in doc_text]
  if missing_doc:
    raise ArithmeticFidelityError(
      "ARITHMETIC_FIDELITY.md is missing checked helper docs for: "
      + ", ".join(missing_doc)
    )


def validate_proof_assumption_docs(doc_text: str) -> None:
  for path in PROOF_PATHS:
    proof_text = read_text(path)
    assumptions = extract_proof_overflow_assumptions(proof_text)
    rel = path.relative_to(ROOT).as_posix()
    if f"`{rel}`" not in doc_text:
      raise ArithmeticFidelityError(
        f"ARITHMETIC_FIDELITY.md is missing proof file `{rel}`"
      )
    if not assumptions:
      raise ArithmeticFidelityError(f"`{rel}` has no tracked overflow assumptions")
    for assumption in sorted(assumptions):
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
  macro_text = read_text(MACRO_PATH)
  validate_uint128_guard_docs(macro_text, doc_text)
  validate_checked_helper_docs(macro_text, doc_text)
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
