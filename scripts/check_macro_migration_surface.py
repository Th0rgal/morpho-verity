#!/usr/bin/env python3
"""Fail-closed check for Morpho macro-migration selector/signature surface."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import subprocess
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
SPEC_PATH = ROOT / "Morpho" / "Compiler" / "Spec.lean"
INTERFACE_PATH = ROOT / "morpho-blue" / "src" / "interfaces" / "IMorpho.sol"
IMORPHO_CONTRACT = "IMorpho"

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


def extract_spec_selector_entries(spec_text: str) -> list[tuple[str, int]]:
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
  entries: list[tuple[str, int]] = []
  for line in block_lines:
    m = re.match(r"^\s*0x([0-9a-fA-F]+)\s*,?\s*--\s*(.+?)\s*$", line)
    if not m:
      continue
    selector = int(m.group(1), 16)
    candidate = m.group(2).strip()
    if re.match(r"^[A-Za-z_][A-Za-z0-9_]*\(.*\)$", candidate):
      entries.append((candidate, selector))
  if not entries:
    raise RuntimeError("morphoSelectors comments did not contain any parseable signatures")
  return entries


def extract_spec_signatures(spec_text: str) -> set[str]:
  return {signature for signature, _ in extract_spec_selector_entries(spec_text)}


def extract_solc_selector_map(interface_path: pathlib.Path, contract_name: str = IMORPHO_CONTRACT) -> dict[str, int]:
  proc = subprocess.run(
    ["solc", "--hashes", str(interface_path)],
    check=False,
    capture_output=True,
    text=True,
  )
  if proc.returncode != 0:
    raise RuntimeError(f"solc --hashes failed for {interface_path}: {proc.stderr.strip()}")

  selector_map: dict[str, int] = {}
  in_contract_block = False
  block_header = re.compile(rf"^======= .+:{re.escape(contract_name)} =======$")
  entry = re.compile(r"^([0-9a-fA-F]{8}):\s*(\S.*)$")
  for line in proc.stdout.splitlines():
    if line.startswith("======="):
      in_contract_block = bool(block_header.match(line))
      continue
    if not in_contract_block:
      continue
    m = entry.match(line.strip())
    if not m:
      continue
    selector_map[m.group(2)] = int(m.group(1), 16)

  if not selector_map:
    raise RuntimeError(f"failed to parse selector hashes for contract {contract_name} from solc output")
  return selector_map


def build_report(
  spec_signatures: set[str],
  interface_signatures: set[str],
  spec_selector_map: dict[str, int] | None = None,
  solc_selector_map: dict[str, int] | None = None,
) -> dict[str, Any]:
  only_in_spec = sorted(spec_signatures - interface_signatures)
  only_in_interface = sorted(interface_signatures - spec_signatures)
  shared = sorted(spec_signatures & interface_signatures)
  unexpected_only_in_spec = sorted(set(only_in_spec) - EXPECTED_ONLY_IN_SPEC)
  missing_expected_only_in_spec = sorted(EXPECTED_ONLY_IN_SPEC - set(only_in_spec))
  selector_mismatches: list[dict[str, str]] = []
  if spec_selector_map is not None and solc_selector_map is not None:
    for signature in shared:
      spec_selector = spec_selector_map.get(signature)
      solc_selector = solc_selector_map.get(signature)
      if spec_selector is None or solc_selector is None:
        continue
      if spec_selector != solc_selector:
        selector_mismatches.append({
          "signature": signature,
          "specSelector": f"0x{spec_selector:08x}",
          "solcSelector": f"0x{solc_selector:08x}",
        })
  status = "ok"
  if unexpected_only_in_spec or only_in_interface or missing_expected_only_in_spec or selector_mismatches:
    status = "mismatch"
  return {
    "status": status,
    "specSignatureCount": len(spec_signatures),
    "interfaceSignatureCount": len(interface_signatures),
    "matchedSignatureCount": len(shared),
    "selectorComparableCount": len(shared) - len(only_in_spec),
    "onlyInSpec": only_in_spec,
    "onlyInInterface": only_in_interface,
    "allowedOnlyInSpec": sorted(EXPECTED_ONLY_IN_SPEC),
    "unexpectedOnlyInSpec": unexpected_only_in_spec,
    "missingExpectedOnlyInSpec": missing_expected_only_in_spec,
    "selectorMismatches": selector_mismatches,
    "selectorMismatchCount": len(selector_mismatches),
    "specSelectorCount": 0 if spec_selector_map is None else len(spec_selector_map),
    "solcSelectorCount": 0 if solc_selector_map is None else len(solc_selector_map),
  }


def run_check(spec_path: pathlib.Path = SPEC_PATH, interface_path: pathlib.Path = INTERFACE_PATH) -> dict[str, Any]:
  spec_text = read_text(spec_path)
  interface_text = read_text(interface_path)
  spec_entries = extract_spec_selector_entries(spec_text)
  spec_selector_map = {signature: selector for signature, selector in spec_entries}
  spec_signatures = set(spec_selector_map)
  interface_signatures = extract_interface_signatures(interface_text)
  solc_selector_map = extract_solc_selector_map(interface_path)
  report = build_report(spec_signatures, interface_signatures, spec_selector_map, solc_selector_map)
  report["specPath"] = str(spec_path.relative_to(ROOT))
  report["interfacePath"] = str(interface_path.relative_to(ROOT))
  report["selectorSource"] = f"solc --hashes ({IMORPHO_CONTRACT})"
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
    f"matched={report['matchedSignatureCount']} "
    f"selector_mismatches={report['selectorMismatchCount']}"
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
    if report["selectorMismatches"]:
      print("selector mismatches between Spec.lean and IMorpho.sol:")
      for mismatch in report["selectorMismatches"]:
        print(
          "  - "
          f"{mismatch['signature']}: "
          f"spec={mismatch['specSelector']} "
          f"solc={mismatch['solcSelector']}"
        )
    fail("selector/signature migration surface mismatch")
  print("macro-migration-surface check: OK")


if __name__ == "__main__":
  main()
