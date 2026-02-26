#!/usr/bin/env python3
"""Generate machine-readable Yul parity report for Morpho (Solidity vs Verity)."""

from __future__ import annotations

import argparse
import difflib
import hashlib
import json
import pathlib
import re
import shutil
import subprocess
import sys
from dataclasses import dataclass
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
PARITY_TARGET = ROOT / "config" / "parity-target.json"
VERITY_YUL = ROOT / "compiler" / "yul" / "Morpho.yul"
SOLIDITY_ARTIFACT = ROOT / "morpho-blue" / "out" / "Morpho.sol" / "Morpho.json"
DEFAULT_OUT_DIR = ROOT / "out" / "parity-target"
DEFAULT_UNSUPPORTED_MANIFEST = ROOT / "config" / "yul-identity-unsupported.json"


@dataclass(frozen=True)
class FunctionBlock:
  name: str
  ordinal: int
  key: str
  text: str


def sha256_text(text: str) -> str:
  return hashlib.sha256(text.encode("utf-8")).hexdigest()


def run(cmd: list[str], *, cwd: pathlib.Path) -> None:
  subprocess.check_call(cmd, cwd=str(cwd))


def read_json(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


def write_text(path: pathlib.Path, content: str) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  with path.open("w", encoding="utf-8") as f:
    f.write(content)


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def normalize_yul(text: str) -> str:
  # Drop comments and normalize trailing whitespace for stable comparisons.
  without_block_comments = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
  without_line_comments = re.sub(r"//[^\n]*", "", without_block_comments)
  lines = [line.rstrip() for line in without_line_comments.splitlines() if line.strip()]
  return "\n".join(lines) + ("\n" if lines else "")


def is_ident_char(ch: str) -> bool:
  return ch.isalnum() or ch in {"_", "$"}


def extract_function_blocks(normalized_yul: str) -> list[FunctionBlock]:
  text = normalized_yul
  n = len(text)
  cursor = 0
  counts: dict[str, int] = {}
  blocks: list[FunctionBlock] = []

  while cursor < n:
    idx = text.find("function", cursor)
    if idx < 0:
      break
    prev_ok = idx == 0 or not is_ident_char(text[idx - 1])
    next_pos = idx + len("function")
    next_ok = next_pos >= n or not is_ident_char(text[next_pos])
    if not (prev_ok and next_ok):
      cursor = idx + len("function")
      continue

    pos = next_pos
    while pos < n and text[pos].isspace():
      pos += 1
    name_start = pos
    while pos < n and is_ident_char(text[pos]):
      pos += 1
    if pos == name_start:
      cursor = next_pos
      continue
    name = text[name_start:pos]
    while pos < n and text[pos].isspace():
      pos += 1
    if pos >= n or text[pos] != "(":
      cursor = next_pos
      continue

    paren_depth = 1
    pos += 1
    while pos < n and paren_depth > 0:
      if text[pos] == "(":
        paren_depth += 1
      elif text[pos] == ")":
        paren_depth -= 1
      pos += 1
    if paren_depth != 0:
      raise RuntimeError("Unbalanced function parameter list while parsing Yul.")

    while pos < n and text[pos].isspace():
      pos += 1
    if text.startswith("->", pos):
      pos += 2
      while pos < n and text[pos] != "{":
        pos += 1
    while pos < n and text[pos].isspace():
      pos += 1
    if pos >= n or text[pos] != "{":
      cursor = next_pos
      continue

    brace_depth = 1
    pos += 1
    while pos < n and brace_depth > 0:
      if text[pos] == "{":
        brace_depth += 1
      elif text[pos] == "}":
        brace_depth -= 1
      pos += 1
    if brace_depth != 0:
      raise RuntimeError(f"Unbalanced function body braces for function `{name}`.")

    ordinal = counts.get(name, 0)
    counts[name] = ordinal + 1
    key = f"{name}#{ordinal}"
    blocks.append(FunctionBlock(name=name, ordinal=ordinal, key=key, text=text[idx:pos]))
    cursor = pos

  return blocks


def function_hashes(normalized_yul: str) -> dict[str, str]:
  return {block.key: sha256_text(block.text) for block in extract_function_blocks(normalized_yul)}


def compare_function_hashes(
    solidity_hashes: dict[str, str], verity_hashes: dict[str, str]
) -> dict[str, list[str]]:
  only_solidity: list[str] = []
  only_verity: list[str] = []
  hash_mismatch: list[str] = []
  for key in sorted(set(solidity_hashes) | set(verity_hashes)):
    sol_hash = solidity_hashes.get(key)
    ver_hash = verity_hashes.get(key)
    if sol_hash is None:
      only_verity.append(key)
    elif ver_hash is None:
      only_solidity.append(key)
    elif sol_hash != ver_hash:
      hash_mismatch.append(key)
  return {
    "hashMismatch": hash_mismatch,
    "onlyInSolidity": only_solidity,
    "onlyInVerity": only_verity,
  }


def load_unsupported_manifest(path: pathlib.Path) -> dict[str, Any]:
  data = read_json(path)
  if not isinstance(data, dict):
    raise RuntimeError(f"Unsupported manifest must be a JSON object: {path}")
  for key in ("allowedHashMismatchKeys", "allowedOnlyInSolidityKeys", "allowedOnlyInVerityKeys"):
    value = data.get(key)
    if not isinstance(value, list) or not all(isinstance(x, str) for x in value):
      raise RuntimeError(f"Unsupported manifest key `{key}` must be a list of strings: {path}")
  return data


def evaluate_unsupported_manifest(
    deltas: dict[str, list[str]], manifest: dict[str, Any]
) -> dict[str, Any]:
  expected_hash = sorted(manifest.get("allowedHashMismatchKeys", []))
  expected_sol = sorted(manifest.get("allowedOnlyInSolidityKeys", []))
  expected_ver = sorted(manifest.get("allowedOnlyInVerityKeys", []))
  actual_hash = sorted(deltas["hashMismatch"])
  actual_sol = sorted(deltas["onlyInSolidity"])
  actual_ver = sorted(deltas["onlyInVerity"])

  unexpected_hash = sorted(set(actual_hash) - set(expected_hash))
  unexpected_sol = sorted(set(actual_sol) - set(expected_sol))
  unexpected_ver = sorted(set(actual_ver) - set(expected_ver))
  missing_hash = sorted(set(expected_hash) - set(actual_hash))
  missing_sol = sorted(set(expected_sol) - set(actual_sol))
  missing_ver = sorted(set(expected_ver) - set(actual_ver))

  return {
    "ok": not (
      unexpected_hash or unexpected_sol or unexpected_ver or missing_hash or missing_sol or missing_ver
    ),
    "expected": {
      "hashMismatch": expected_hash,
      "onlyInSolidity": expected_sol,
      "onlyInVerity": expected_ver,
    },
    "actual": {
      "hashMismatch": actual_hash,
      "onlyInSolidity": actual_sol,
      "onlyInVerity": actual_ver,
    },
    "unexpected": {
      "hashMismatch": unexpected_hash,
      "onlyInSolidity": unexpected_sol,
      "onlyInVerity": unexpected_ver,
    },
    "missingExpected": {
      "hashMismatch": missing_hash,
      "onlyInSolidity": missing_sol,
      "onlyInVerity": missing_ver,
    },
  }


def extract_solidity_ir_optimized() -> str:
  data = read_json(SOLIDITY_ARTIFACT)
  ir = data.get("irOptimized")
  if not isinstance(ir, str) or not ir.strip():
    raise RuntimeError(
      f"Missing `irOptimized` in {SOLIDITY_ARTIFACT}. "
      "Run forge with `--extra-output irOptimized`."
    )
  return ir


def compile_solidity_ir() -> None:
  run(
    [
      "forge",
      "build",
      "--force",
      "--skip",
      "test",
      "script",
      "--contracts",
      "src/Morpho.sol",
      "--extra-output",
      "irOptimized",
      "-q",
    ],
    cwd=ROOT / "morpho-blue",
  )


def compile_verity_yul() -> None:
  run(["./scripts/prepare_verity_morpho_artifact.sh"], cwd=ROOT)


def build_report(verity_yul: str, solc_ir: str, max_diff_lines: int) -> tuple[dict[str, Any], str]:
  normalized_verity = normalize_yul(verity_yul)
  normalized_solc = normalize_yul(solc_ir)
  solidity_function_hashes = function_hashes(normalized_solc)
  verity_function_hashes = function_hashes(normalized_verity)
  function_deltas = compare_function_hashes(solidity_function_hashes, verity_function_hashes)
  raw_equal = verity_yul == solc_ir
  normalized_equal = normalized_verity == normalized_solc

  diff_lines: list[str] = []
  if not normalized_equal:
    diff_lines = list(
      difflib.unified_diff(
        normalized_solc.splitlines(),
        normalized_verity.splitlines(),
        fromfile="solidity/Morpho.irOptimized.normalized.yul",
        tofile="verity/Morpho.normalized.yul",
        lineterm="",
      )
    )

  report = {
    "status": "match" if normalized_equal else "mismatch",
    "rawEqual": raw_equal,
    "normalizedEqual": normalized_equal,
    "raw": {
      "solidity": {"bytes": len(solc_ir.encode("utf-8")), "sha256": sha256_text(solc_ir)},
      "verity": {"bytes": len(verity_yul.encode("utf-8")), "sha256": sha256_text(verity_yul)},
    },
    "normalized": {
      "solidity": {"bytes": len(normalized_solc.encode("utf-8")), "sha256": sha256_text(normalized_solc)},
      "verity": {"bytes": len(normalized_verity.encode("utf-8")), "sha256": sha256_text(normalized_verity)},
    },
    "functionBlocks": {
      "solidityCount": len(solidity_function_hashes),
      "verityCount": len(verity_function_hashes),
      "hashMismatch": function_deltas["hashMismatch"],
      "onlyInSolidity": function_deltas["onlyInSolidity"],
      "onlyInVerity": function_deltas["onlyInVerity"],
    },
    "diff": {"lineCount": len(diff_lines), "truncated": len(diff_lines) > max_diff_lines},
  }

  if len(diff_lines) > max_diff_lines:
    diff_lines = diff_lines[:max_diff_lines]

  return report, "\n".join(diff_lines) + ("\n" if diff_lines else "")


def parse_args() -> argparse.Namespace:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument(
    "--out-dir",
    default=str(DEFAULT_OUT_DIR),
    help="Output directory for generated fixtures/report (default: out/parity-target).",
  )
  parser.add_argument(
    "--max-diff-lines",
    type=int,
    default=4000,
    help="Maximum number of diff lines stored in the artifact.",
  )
  parser.add_argument(
    "--strict",
    action="store_true",
    help="Exit non-zero when normalized outputs differ.",
  )
  parser.add_argument(
    "--unsupported-manifest",
    default=str(DEFAULT_UNSUPPORTED_MANIFEST),
    help=(
      "JSON manifest of currently allowed function-level identity gaps "
      "(default: config/yul-identity-unsupported.json)."
    ),
  )
  parser.add_argument(
    "--enforce-unsupported-manifest",
    action="store_true",
    help="Exit non-zero when function-level mismatches diverge from the unsupported manifest.",
  )
  parser.add_argument(
    "--skip-build",
    action="store_true",
    help="Do not rebuild Solidity/Verity artifacts before generating the report.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  out_dir = pathlib.Path(args.out_dir).resolve()
  unsupported_manifest_path = pathlib.Path(args.unsupported_manifest).resolve()
  solc_dir = out_dir / "solidity"
  verity_dir = out_dir / "verity"
  out_dir.mkdir(parents=True, exist_ok=True)
  solc_dir.mkdir(parents=True, exist_ok=True)
  verity_dir.mkdir(parents=True, exist_ok=True)

  target = read_json(PARITY_TARGET)
  if not args.skip_build:
    compile_verity_yul()
    compile_solidity_ir()

  verity_yul = read_text(VERITY_YUL)
  solc_ir = extract_solidity_ir_optimized()

  write_text(solc_dir / "Morpho.irOptimized.yul", solc_ir)
  write_text(verity_dir / "Morpho.yul", verity_yul)
  write_text(solc_dir / "Morpho.irOptimized.normalized.yul", normalize_yul(solc_ir))
  write_text(verity_dir / "Morpho.normalized.yul", normalize_yul(verity_yul))

  report, diff_text = build_report(verity_yul, solc_ir, args.max_diff_lines)
  report["parityTarget"] = target["id"]
  report["unsupportedManifest"] = {"path": display_path(unsupported_manifest_path), "found": False, "check": None}
  if unsupported_manifest_path.exists():
    manifest = load_unsupported_manifest(unsupported_manifest_path)
    report["unsupportedManifest"]["found"] = True
    report["unsupportedManifest"]["check"] = evaluate_unsupported_manifest(report["functionBlocks"], manifest)
  report["paths"] = {
    "solidityRaw": display_path(solc_dir / "Morpho.irOptimized.yul"),
    "verityRaw": display_path(verity_dir / "Morpho.yul"),
    "diff": display_path(out_dir / "normalized.diff"),
  }

  write_text(out_dir / "normalized.diff", diff_text)
  write_text(out_dir / "report.json", json.dumps(report, indent=2) + "\n")
  shutil.copy2(PARITY_TARGET, out_dir / "parity-target.json")

  print(f"parity-target: {target['id']}")
  print(f"status: {report['status']}")
  print(f"rawEqual: {report['rawEqual']}")
  print(f"normalizedEqual: {report['normalizedEqual']}")
  print(f"report: {display_path(out_dir / 'report.json')}")
  print(f"diff: {display_path(out_dir / 'normalized.diff')}")
  if report["unsupportedManifest"]["found"]:
    print(f"unsupportedManifest: {display_path(unsupported_manifest_path)}")
    print(f"unsupportedManifestOk: {report['unsupportedManifest']['check']['ok']}")
  else:
    print(f"unsupportedManifest: missing ({display_path(unsupported_manifest_path)})")

  if args.strict and not report["normalizedEqual"]:
    print("yul-identity check failed in strict mode", file=sys.stderr)
    return 1
  if args.enforce_unsupported_manifest:
    if not report["unsupportedManifest"]["found"]:
      print("yul-identity check failed: unsupported manifest file is missing", file=sys.stderr)
      return 1
    if not report["unsupportedManifest"]["check"]["ok"]:
      print("yul-identity check failed: unsupported manifest drift detected", file=sys.stderr)
      return 1
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
