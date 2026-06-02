#!/usr/bin/env python3
"""Fail-closed check: keep Morpho compiler packaging behind ArtifactConfig.lean."""

from __future__ import annotations

import pathlib
import re
import sys


ROOT = pathlib.Path(__file__).resolve().parent.parent
ALLOWED_FILES = {
  ROOT / "morpho-blue-verity" / "Morpho" / "Compiler" / "ArtifactConfig.lean",
}
FORBIDDEN_LEGACY_FILES = {
  ROOT / "morpho-blue-verity" / "Morpho" / "Types.lean": "legacy hand-written protocol projection model",
  ROOT / "morpho-blue-verity" / "Morpho" / "Compiler" / "Spec.lean": "legacy hand-written CompilationModel",
  ROOT / "morpho-blue-verity" / "Morpho" / "Compiler" / "MacroSlice.lean": "migration-era macro slice alias",
  ROOT / "morpho-blue-verity" / "Morpho" / "Compiler" / "Generated.lean": "misleading compiler adapter name",
}
TARGET_GLOB = ROOT / "morpho-blue-verity" / "Morpho" / "Compiler"
SYMBOL_RE = re.compile(r"\b(morphoSpec|morphoSelectors)\b")
SECOND_MODEL_RE = re.compile(r"\b(MorphoState|protocolBorrow|protocolSupply|marketId)\b")
ARTIFACT_CONFIG_PATH = ROOT / "morpho-blue-verity" / "Morpho" / "Compiler" / "ArtifactConfig.lean"
MACRO_PATH = ROOT / "morpho-blue-verity" / "Morpho" / "Contract.lean"
TRUST_DOC_PATH = ROOT / "docs" / "TRUST_BOUNDARIES.md"
REQUIRED_EXTERNAL_AXIOMS: dict[str, str] = {}
LOCAL_OBLIGATION_RE = re.compile(r"\b([A-Za-z_][A-Za-z0-9_]*)\s*:=\s*assumed\b")
ARTIFACT_CONFIG_ALLOWED_IMPORTS = {
  "Compiler.CompilationModel",
  "Compiler.Selector",
  "Morpho.Contract",
}
ARTIFACT_CONFIG_ALLOWED_DEFS = {
  "internalHelperExternalNames",
  "artifactSpec",
  "artifactSelectors",
}
IMPORT_RE = re.compile(r"^import\s+([A-Za-z0-9_.'-]+)\s*$", re.MULTILINE)
TOP_LEVEL_DECL_RE = re.compile(
  r"^(?:private\s+)?(?:def|theorem|axiom|structure|inductive|class|instance)\s+([A-Za-z_][A-Za-z0-9_']*)\b",
  re.MULTILINE,
)


class MorphoArtifactBoundaryError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  try:
    return path.read_text(encoding="utf-8")
  except OSError as exc:
    raise MorphoArtifactBoundaryError(f"failed to read {path}: {exc}") from exc
  except UnicodeDecodeError as exc:
    raise MorphoArtifactBoundaryError(f"{path} is not valid UTF-8: {exc}") from exc


def validate_artifact_config_external_axioms(text: str) -> None:
  if not REQUIRED_EXTERNAL_AXIOMS:
    if "axiomNames :=" in text:
      raise MorphoArtifactBoundaryError(
        "ArtifactConfig.lean must not declare linked externals at the current Verity pin"
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
      raise MorphoArtifactBoundaryError(
        f"generated external {external_name!r} is missing or has no axiomNames block"
      )
    if f'"{axiom_name}"' not in block_match.group(1):
      raise MorphoArtifactBoundaryError(
        f"generated external {external_name!r} must include axiom name {axiom_name!r}"
      )


def validate_artifact_config_uses_contract_boundary(text: str) -> None:
  imports = set(IMPORT_RE.findall(text))
  unexpected_imports = sorted(imports - ARTIFACT_CONFIG_ALLOWED_IMPORTS)
  missing_imports = sorted(ARTIFACT_CONFIG_ALLOWED_IMPORTS - imports)
  if unexpected_imports:
    raise MorphoArtifactBoundaryError(
      "ArtifactConfig.lean must stay packaging-only; unexpected imports: "
      + ", ".join(unexpected_imports)
    )
  if missing_imports:
    raise MorphoArtifactBoundaryError(
      "ArtifactConfig.lean is missing required imports: " + ", ".join(missing_imports)
    )

  unexpected_decls: list[str] = []
  for match in TOP_LEVEL_DECL_RE.finditer(text):
    decl_name = match.group(1)
    if decl_name not in ARTIFACT_CONFIG_ALLOWED_DEFS:
      unexpected_decls.append(decl_name)
  if unexpected_decls:
    raise MorphoArtifactBoundaryError(
      "ArtifactConfig.lean must not define protocol/proof surfaces; unexpected declarations: "
      + ", ".join(sorted(unexpected_decls))
    )

  if "import Morpho.Contract" not in text:
    raise MorphoArtifactBoundaryError(
      "ArtifactConfig.lean must import Morpho.Contract as the canonical contract boundary"
    )
  if "import Morpho.Compiler.MacroSlice" in text:
    raise MorphoArtifactBoundaryError(
      "ArtifactConfig.lean must not import Morpho.Compiler.MacroSlice directly"
    )
  if "_root_.Morpho.Contract.Morpho.spec" not in text:
    raise MorphoArtifactBoundaryError(
      "artifactSpec must wrap _root_.Morpho.Contract.Morpho.spec"
    )


def extract_local_obligation_names(macro_text: str) -> set[str]:
  return set(LOCAL_OBLIGATION_RE.findall(macro_text))


def validate_trust_boundaries_doc(artifact_config_text: str, macro_text: str, doc_text: str) -> None:
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
    raise MorphoArtifactBoundaryError("TRUST_BOUNDARIES.md drift: " + "; ".join(parts))


def main() -> None:
  offenders: list[str] = []
  legacy_files = [
    f"{path.relative_to(ROOT)} ({reason})"
    for path, reason in sorted(FORBIDDEN_LEGACY_FILES.items())
    if path.exists()
  ]
  if legacy_files:
    raise MorphoArtifactBoundaryError(
      "legacy Morpho compiler model files must not exist: " + ", ".join(legacy_files)
    )

  for path in sorted(TARGET_GLOB.glob("*.lean")):
    if path in ALLOWED_FILES:
      continue
    text = read_text(path)
    if SYMBOL_RE.search(text):
      offenders.append(str(path.relative_to(ROOT)))

  second_model_offenders: list[str] = []
  for path in sorted((ROOT / "morpho-blue-verity" / "Morpho").rglob("*.lean")):
    if path == MACRO_PATH:
      continue
    text = read_text(path)
    if SECOND_MODEL_RE.search(text):
      second_model_offenders.append(str(path.relative_to(ROOT)))

  artifact_config_text = read_text(ARTIFACT_CONFIG_PATH)
  macro_text = read_text(MACRO_PATH)
  validate_artifact_config_uses_contract_boundary(artifact_config_text)
  validate_artifact_config_external_axioms(artifact_config_text)
  validate_trust_boundaries_doc(artifact_config_text, macro_text, read_text(TRUST_DOC_PATH))

  if offenders:
    print("morpho-artifact-boundary check failed:", file=sys.stderr)
    for rel in offenders:
      print(f"  direct morphoSpec/morphoSelectors usage in {rel}", file=sys.stderr)
    print(
      "Use Morpho.Compiler.ArtifactConfig.{artifactSpec,artifactSelectors} as artifact packaging over the canonical contract.",
      file=sys.stderr,
    )
    sys.exit(1)

  if second_model_offenders:
    print("morpho-artifact-boundary check failed:", file=sys.stderr)
    for rel in second_model_offenders:
      print(f"  second protocol model marker in {rel}", file=sys.stderr)
    print(
      "Protocol-level reasoning must be extracted from Morpho.Contract.Morpho.spec or live in clearly separated proof infrastructure.",
      file=sys.stderr,
    )
    sys.exit(1)

  print("morpho-artifact-boundary check: OK")


if __name__ == "__main__":
  try:
    main()
  except MorphoArtifactBoundaryError as exc:
    print(f"morpho-artifact-boundary check failed: {exc}", file=sys.stderr)
    raise SystemExit(1)
