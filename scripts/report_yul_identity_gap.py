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
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
PARITY_TARGET = ROOT / "config" / "parity-target.json"
VERITY_YUL = ROOT / "compiler" / "yul" / "Morpho.yul"
SOLIDITY_ARTIFACT = ROOT / "morpho-blue" / "out" / "Morpho.sol" / "Morpho.json"
DEFAULT_OUT_DIR = ROOT / "out" / "parity-target"


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


def normalize_yul(text: str) -> str:
  # Drop comments and normalize trailing whitespace for stable comparisons.
  without_block_comments = re.sub(r"/\*.*?\*/", "", text, flags=re.DOTALL)
  without_line_comments = re.sub(r"//[^\n]*", "", without_block_comments)
  lines = [line.rstrip() for line in without_line_comments.splitlines() if line.strip()]
  return "\n".join(lines) + ("\n" if lines else "")


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
    "--skip-build",
    action="store_true",
    help="Do not rebuild Solidity/Verity artifacts before generating the report.",
  )
  return parser.parse_args()


def main() -> int:
  args = parse_args()
  out_dir = pathlib.Path(args.out_dir).resolve()
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
  report["paths"] = {
    "solidityRaw": str((solc_dir / "Morpho.irOptimized.yul").relative_to(ROOT)),
    "verityRaw": str((verity_dir / "Morpho.yul").relative_to(ROOT)),
    "diff": str((out_dir / "normalized.diff").relative_to(ROOT)),
  }

  write_text(out_dir / "normalized.diff", diff_text)
  write_text(out_dir / "report.json", json.dumps(report, indent=2) + "\n")
  shutil.copy2(PARITY_TARGET, out_dir / "parity-target.json")

  print(f"parity-target: {target['id']}")
  print(f"status: {report['status']}")
  print(f"rawEqual: {report['rawEqual']}")
  print(f"normalizedEqual: {report['normalizedEqual']}")
  print(f"report: {(out_dir / 'report.json').relative_to(ROOT)}")
  print(f"diff: {(out_dir / 'normalized.diff').relative_to(ROOT)}")

  if args.strict and not report["normalizedEqual"]:
    print("yul-identity check failed in strict mode", file=sys.stderr)
    return 1
  return 0


if __name__ == "__main__":
  raise SystemExit(main())
