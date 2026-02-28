#!/usr/bin/env python3
"""Fail-closed check for Morpho macro-migration selector/signature surface."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
SPEC_PATH = ROOT / "Morpho" / "Compiler" / "Spec.lean"
INTERFACE_PATH = ROOT / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol"

TYPE_ALIASES = {
  "Id": "bytes32",
}
QUALIFIERS = {"memory", "calldata", "storage", "payable"}
EXPECTED_ONLY_IN_SPEC = {
  "fee(bytes32)",
  "lastUpdate(bytes32)",
  "totalBorrowAssets(bytes32)",
  "totalBorrowShares(bytes32)",
  "totalSupplyAssets(bytes32)",
  "totalSupplyShares(bytes32)",
}


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


def fail(msg: str) -> None:
  print(f"macro-migration-surface check failed: {msg}", file=sys.stderr)
  sys.exit(1)


def strip_solidity_comments(text: str) -> str:
  text = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
  text = re.sub(r"//[^\n]*", "", text)
  return text


def split_top_level_csv(text: str) -> list[str]:
  out: list[str] = []
  current: list[str] = []
  depth = 0
  for ch in text:
    if ch == "," and depth == 0:
      item = "".join(current).strip()
      if item:
        out.append(item)
      current = []
      continue
    if ch in "([":
      depth += 1
    elif ch in ")]":
      depth -= 1
      if depth < 0:
        raise RuntimeError("unbalanced delimiters while splitting parameter list")
    current.append(ch)
  tail = "".join(current).strip()
  if tail:
    out.append(tail)
  if depth != 0:
    raise RuntimeError("unbalanced delimiters while splitting parameter list")
  return out


def parse_struct_fields(solidity_text: str) -> dict[str, list[str]]:
  clean = strip_solidity_comments(solidity_text)
  structs: dict[str, list[str]] = {}
  for match in re.finditer(r"\bstruct\s+([A-Za-z_][A-Za-z0-9_]*)\s*\{(.*?)\}", clean, flags=re.DOTALL):
    name = match.group(1)
    body = match.group(2)
    fields: list[str] = []
    for decl in body.split(";"):
      line = decl.strip()
      if not line:
        continue
      parts = line.split()
      if len(parts) < 2:
        raise RuntimeError(f"unable to parse struct field declaration: {line!r}")
      fields.append(" ".join(parts[:-1]))
    structs[name] = fields
  return structs


def split_type_and_name(param_decl: str) -> str:
  param_decl = param_decl.strip()
  if not param_decl:
    return ""
  m = re.match(r"^(.*\S)\s+([A-Za-z_][A-Za-z0-9_]*)$", param_decl)
  if m:
    return m.group(1)
  return param_decl


def canonicalize_type(raw_type: str, structs: dict[str, list[str]], stack: tuple[str, ...] = ()) -> str:
  text = " ".join(raw_type.strip().split())
  if text.startswith("struct "):
    text = text[len("struct "):].strip()

  suffix = ""
  while text.endswith("]"):
    open_idx = text.rfind("[")
    if open_idx == -1:
      break
    suffix = text[open_idx:] + suffix
    text = text[:open_idx].strip()

  pieces = [p for p in text.split(" ") if p and p not in QUALIFIERS]
  base = " ".join(pieces)
  base = TYPE_ALIASES.get(base, base)

  if base in structs:
    if base in stack:
      raise RuntimeError(f"recursive struct definition detected for {base}")
    tuple_types = [canonicalize_type(ft, structs, stack + (base,)) for ft in structs[base]]
    return f"({','.join(tuple_types)}){suffix}"

  return f"{base}{suffix}"


def extract_interface_signatures(solidity_text: str) -> set[str]:
  structs = parse_struct_fields(solidity_text)
  signatures: set[str] = set()
  for match in re.finditer(
      r"\bfunction\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)\s*[^;{]*;",
      solidity_text,
      flags=re.DOTALL,
  ):
    name = match.group(1)
    param_blob = match.group(2).strip()
    if not param_blob:
      signatures.add(f"{name}()")
      continue
    param_decls = split_top_level_csv(param_blob)
    types = [canonicalize_type(split_type_and_name(decl), structs) for decl in param_decls]
    signatures.add(f"{name}({','.join(types)})")
  return signatures


def extract_spec_signatures(spec_text: str) -> set[str]:
  lines = spec_text.splitlines()
  start_idx = None
  for idx, line in enumerate(lines):
    if re.search(r"def\s+morphoSelectors\s*:\s*List\s+Nat\s*:=", line):
      start_idx = idx
      break
  if start_idx is None:
    raise RuntimeError("unable to find morphoSelectors in Morpho/Compiler/Spec.lean")
  block_lines: list[str] = []
  for line in lines[start_idx + 1:]:
    if re.match(r"^\s*\]\s*$", line):
      break
    block_lines.append(line)
  if not block_lines:
    raise RuntimeError("unable to parse morphoSelectors list body in Morpho/Compiler/Spec.lean")
  signatures: set[str] = set()
  for line in block_lines:
    m = re.search(r"--\s*(.+?)\s*$", line)
    if not m:
      continue
    candidate = m.group(1).strip()
    if re.match(r"^[A-Za-z_][A-Za-z0-9_]*\(.*\)$", candidate):
      signatures.add(candidate)
  if not signatures:
    raise RuntimeError("morphoSelectors comments did not contain any parseable signatures")
  return signatures


def build_report(spec_signatures: set[str], interface_signatures: set[str]) -> dict[str, Any]:
  only_in_spec = sorted(spec_signatures - interface_signatures)
  only_in_interface = sorted(interface_signatures - spec_signatures)
  shared = sorted(spec_signatures & interface_signatures)
  unexpected_only_in_spec = sorted(set(only_in_spec) - EXPECTED_ONLY_IN_SPEC)
  missing_expected_only_in_spec = sorted(EXPECTED_ONLY_IN_SPEC - set(only_in_spec))
  return {
    "status": "ok" if not unexpected_only_in_spec and not only_in_interface and not missing_expected_only_in_spec else "mismatch",
    "specSignatureCount": len(spec_signatures),
    "interfaceSignatureCount": len(interface_signatures),
    "matchedSignatureCount": len(shared),
    "onlyInSpec": only_in_spec,
    "onlyInInterface": only_in_interface,
    "allowedOnlyInSpec": sorted(EXPECTED_ONLY_IN_SPEC),
    "unexpectedOnlyInSpec": unexpected_only_in_spec,
    "missingExpectedOnlyInSpec": missing_expected_only_in_spec,
  }


def run_check(spec_path: pathlib.Path = SPEC_PATH, interface_path: pathlib.Path = INTERFACE_PATH) -> dict[str, Any]:
  spec_signatures = extract_spec_signatures(read_text(spec_path))
  interface_signatures = extract_interface_signatures(read_text(interface_path))
  report = build_report(spec_signatures, interface_signatures)
  report["specPath"] = str(spec_path.relative_to(ROOT))
  report["interfacePath"] = str(interface_path.relative_to(ROOT))
  return report


def main() -> None:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--json-out", type=pathlib.Path, default=None, help="Write report JSON to this path")
  args = parser.parse_args()

  report = run_check()
  if args.json_out is not None:
    args.json_out.parent.mkdir(parents=True, exist_ok=True)
    args.json_out.write_text(json.dumps(report, indent=2) + "\n", encoding="utf-8")
    print(f"wrote migration-surface report: {args.json_out}")

  print(
    "macro-migration-surface: "
    f"spec={report['specSignatureCount']} "
    f"interface={report['interfaceSignatureCount']} "
    f"matched={report['matchedSignatureCount']}"
  )
  if report["status"] != "ok":
    if report["unexpectedOnlyInSpec"]:
      print("only in Spec.lean selectors:")
      for sig in report["unexpectedOnlyInSpec"]:
        print(f"  - {sig}")
    if report["missingExpectedOnlyInSpec"]:
      print("missing expected spec-only selectors:")
      for sig in report["missingExpectedOnlyInSpec"]:
        print(f"  - {sig}")
    if report["onlyInInterface"]:
      print("only in IMorpho.sol interface declarations:")
      for sig in report["onlyInInterface"]:
        print(f"  - {sig}")
    fail("selector/signature migration surface mismatch")
  print("macro-migration-surface check: OK")


if __name__ == "__main__":
  main()
