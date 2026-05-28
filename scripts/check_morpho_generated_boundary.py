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
MACRO_PATH = ROOT / "Morpho" / "Contract.lean"
TRUST_DOC_PATH = ROOT / "docs" / "TRUST_BOUNDARIES.md"
REQUIRED_EXTERNAL_AXIOMS: dict[str, str] = {}
LOCAL_OBLIGATION_RE = re.compile(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*:=\s*assumed\b")


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
  if not REQUIRED_EXTERNAL_AXIOMS:
    if "axiomNames :=" in text:
      raise MorphoGeneratedBoundaryError(
        "Generated.lean must not declare linked externals at the current Verity pin"
      )
    return
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


def validate_generated_uses_contract_boundary(text: str) -> None:
  if "import Morpho.Contract" not in text:
    raise MorphoGeneratedBoundaryError(
      "Generated.lean must import Morpho.Contract as the canonical contract boundary"
    )
  if "import Morpho.Compiler.MacroSlice" in text:
    raise MorphoGeneratedBoundaryError(
      "Generated.lean must not import Morpho.Compiler.MacroSlice directly"
    )
  if "Morpho.Contract.spec" not in text:
    raise MorphoGeneratedBoundaryError(
      "morphoGeneratedSpec must wrap Morpho.Contract.spec"
    )


def extract_local_obligation_names(macro_text: str) -> set[str]:
  return set(LOCAL_OBLIGATION_RE.findall(macro_text))


def validate_trust_boundaries_doc(generated_text: str, macro_text: str, doc_text: str) -> None:
  missing_axioms = [
    axiom_name
    for axiom_name in REQUIRED_EXTERNAL_AXIOMS.values()
    if f"`{axiom_name}`" not in doc_text
  ]
  local_obligations = sorted(extract_local_obligation_names(macro_text))
  missing_obligations = [
    name for name in local_obligations if f"`{name}`" not in doc_text
  ]
  if missing_axioms or missing_obligations:
    parts: list[str] = []
    if missing_axioms:
      parts.append("missing external axiom docs: " + ", ".join(missing_axioms))
    if missing_obligations:
      parts.append("missing local obligation docs: " + ", ".join(missing_obligations))
    raise MorphoGeneratedBoundaryError("TRUST_BOUNDARIES.md drift: " + "; ".join(parts))


def main() -> None:
  offenders: list[str] = []

  for path in sorted(TARGET_GLOB.glob("*.lean")):
    if path in ALLOWED_FILES:
      continue
    text = read_text(path)
    if SYMBOL_RE.search(text):
      offenders.append(str(path.relative_to(ROOT)))

  generated_text = read_text(GENERATED_PATH)
  macro_text = read_text(MACRO_PATH)
  validate_generated_uses_contract_boundary(generated_text)
  validate_generated_external_axioms(generated_text)
  validate_trust_boundaries_doc(generated_text, macro_text, read_text(TRUST_DOC_PATH))

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
