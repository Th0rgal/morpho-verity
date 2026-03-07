#!/usr/bin/env python3
"""Fail-closed tracker for morphoSpec -> verity macro migration blockers."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from dataclasses import dataclass
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
SPEC_PATH = ROOT / "Morpho" / "Compiler" / "Spec.lean"
BASELINE_PATH = ROOT / "config" / "macro-migration-blockers.json"
OBLIGATIONS_PATH = ROOT / "config" / "semantic-bridge-obligations.json"

TRACKED_OPERATION_BLOCKER_OPS = {
  "borrow",
  "liquidate",
  "repay",
  "supply",
  "supplyCollateral",
  "withdraw",
  "withdrawCollateral",
}

# Supported constructor surface in Verity macro translation (current upstream state).
SUPPORTED_STMT = {
  "assignVar",
  "emit",
  "internalCall",
  "internalCallAssign",
  "letVar",
  "mstore",
  "rawLog",
  "setStorage",
  "setMapping",
  "setMapping2",
  "setMappingUint",
  "setStructMember",
  "setStructMember2",
  "require",
  "return",
  "returnStorageWords",
  "returnValues",
  "ite",
  "stop",
}

SUPPORTED_EXPR = {
  "constructorArg",
  "param",
  "localVar",
  "literal",
  "blockTimestamp",
  "contractAddress",
  "chainid",
  "add",
  "sub",
  "mul",
  "div",
  "mod",
  "bitAnd",
  "bitOr",
  "bitXor",
  "bitNot",
  "shl",
  "shr",
  "eq",
  "ge",
  "gt",
  "lt",
  "le",
  "logicalAnd",
  "logicalOr",
  "logicalNot",
  "mload",
  "keccak256",
  "storage",
  "mapping",
  "mapping2",
  "mappingUint",
  "structMember",
  "structMember2",
  "caller",
  "externalCall",
  "mulDivDown",
  "mulDivUp",
  "wMulDown",
  "wDivUp",
  "min",
  "max",
  "ite",
}

CTOR_RE = re.compile(r"\b(Stmt|Expr)\.([A-Za-z0-9_]+)")
SPEC_BODY_REF_RE = re.compile(r"body\s*:=\s*(\w+)")
SPEC_BODY_DEF_RE = re.compile(
  r"private\s+def\s+(\w+)\s*:\s*List\s+Stmt\s*:=\s*(.*?)(?=^(?:private\s+def|/--|def\s+\w+|end\b)|\Z)",
  re.MULTILINE | re.DOTALL,
)
BLOCKER_PATTERNS = {
  "callbacks": re.compile(r"\bCallbacks\.callback\b"),
  "erc20": re.compile(r"\bERC20\.safeTransfer(?:From)?\b"),
  "externalWithReturn": re.compile(r"\bCalls\.withReturn\b"),
  "internalCall": re.compile(r"\b(?:Stmt\.internalCall|callAccrueInterest)\b"),
  "memoryOps": re.compile(r"\b(?:Stmt\.mstore|Expr\.mload)\b"),
  "structMember2": re.compile(r"\b(?:Expr\.structMember2|Stmt\.setStructMember2)\b"),
}


@dataclass(frozen=True)
class ConstructorUsage:
  stmt_counts: dict[str, int]
  expr_counts: dict[str, int]


class MigrationGateError(RuntimeError):
  pass


def display_path(path: pathlib.Path) -> str:
  try:
    return str(path.relative_to(ROOT))
  except ValueError:
    return str(path)


def read_text(path: pathlib.Path) -> str:
  try:
    with path.open("r", encoding="utf-8") as f:
      return f.read()
  except UnicodeDecodeError as exc:
    raise MigrationGateError(f"failed to decode {path} as UTF-8: {exc}") from exc
  except OSError as exc:
    raise MigrationGateError(f"failed to read {path}: {exc}") from exc


def parse_constructor_usage(text: str) -> ConstructorUsage:
  stmt_counts: dict[str, int] = {}
  expr_counts: dict[str, int] = {}
  for kind, name in CTOR_RE.findall(text):
    counts = stmt_counts if kind == "Stmt" else expr_counts
    counts[name] = counts.get(name, 0) + 1
  return ConstructorUsage(stmt_counts=stmt_counts, expr_counts=expr_counts)


def sorted_unique(items: set[str]) -> list[str]:
  return sorted(items)


def total_occurrences(counts: dict[str, int], keys: set[str]) -> int:
  return sum(v for k, v in counts.items() if k in keys)


def load_baseline(path: pathlib.Path) -> dict[str, Any]:
  try:
    data = json.loads(read_text(path))
  except json.JSONDecodeError as exc:
    raise MigrationGateError(f"invalid JSON in {path}: {exc}") from exc
  if not isinstance(data, dict):
    raise MigrationGateError(f"expected top-level object in {path}")
  return data


def extract_spec_function_blocks(text: str) -> list[str]:
  match = re.search(r"functions\s*:=\s*\[", text)
  if not match:
    return []
  start = match.end()
  blocks = []
  depth = 0
  current_block = ""
  in_block = False
  for i in range(start, len(text)):
    ch = text[i]
    if ch == "{":
      if depth == 0:
        in_block = True
        current_block = ""
      depth += 1
    if in_block:
      current_block += ch
    if ch == "}":
      depth -= 1
      if depth == 0 and in_block:
        in_block = False
        blocks.append(current_block)
    if ch == "]" and depth == 0 and not in_block:
      break
  return blocks


def extract_spec_body_defs(text: str) -> dict[str, str]:
  return {m.group(1): m.group(2) for m in SPEC_BODY_DEF_RE.finditer(text)}


def extract_operation_bodies(text: str) -> dict[str, str]:
  body_defs = extract_spec_body_defs(text)
  result: dict[str, str] = {}
  for block in extract_spec_function_blocks(text):
    name_match = re.search(r'name\s*:=\s*"(\w+)"', block)
    if not name_match:
      continue
    body_ref = SPEC_BODY_REF_RE.search(block)
    result[name_match.group(1)] = body_defs.get(body_ref.group(1), block) if body_ref else block
  return result


def detect_operation_blockers(body_text: str) -> list[str]:
  return sorted(
    blocker for blocker, pattern in BLOCKER_PATTERNS.items()
    if pattern.search(body_text) is not None
  )


def build_operation_blocker_report(spec_text: str) -> dict[str, list[str]]:
  bodies = extract_operation_bodies(spec_text)
  return {
    op: detect_operation_blockers(bodies[op])
    for op in sorted(TRACKED_OPERATION_BLOCKER_OPS)
    if op in bodies
  }


def load_obligations(path: pathlib.Path) -> dict[str, dict[str, Any]]:
  raw = load_baseline(path)
  obligations = raw.get("obligations")
  if not isinstance(obligations, list):
    raise MigrationGateError(f"missing `obligations` list in {path}")
  by_operation: dict[str, dict[str, Any]] = {}
  for i, item in enumerate(obligations):
    if not isinstance(item, dict):
      raise MigrationGateError(f"obligation[{i}] is not an object in {path}")
    operation = item.get("operation")
    if not isinstance(operation, str) or not operation:
      raise MigrationGateError(f"obligation[{i}] missing non-empty `operation` in {path}")
    if operation in by_operation:
      raise MigrationGateError(f"duplicate obligation operation `{operation}` in {path}")
    by_operation[operation] = item
  return by_operation


def write_baseline(path: pathlib.Path, data: dict[str, Any]) -> None:
  try:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8") as f:
      json.dump(data, f, indent=2, sort_keys=True)
      f.write("\n")
  except OSError as exc:
    raise MigrationGateError(f"failed to write {path}: {exc}") from exc


def build_report(
  usage: ConstructorUsage,
  *,
  source_path: pathlib.Path = SPEC_PATH,
  baseline_path: pathlib.Path = BASELINE_PATH,
  obligations_path: pathlib.Path = OBLIGATIONS_PATH,
) -> dict[str, Any]:
  used_stmt = set(usage.stmt_counts)
  used_expr = set(usage.expr_counts)

  unsupported_stmt = used_stmt - SUPPORTED_STMT
  unsupported_expr = used_expr - SUPPORTED_EXPR

  supported_stmt_used = used_stmt & SUPPORTED_STMT
  supported_expr_used = used_expr & SUPPORTED_EXPR

  return {
    "source": display_path(source_path),
    "baselinePath": display_path(baseline_path),
    "obligationsPath": display_path(obligations_path),
    "supportedSurface": {
      "stmt": sorted_unique(SUPPORTED_STMT),
      "expr": sorted_unique(SUPPORTED_EXPR),
    },
    "used": {
      "stmt": sorted_unique(used_stmt),
      "expr": sorted_unique(used_expr),
    },
    "supportedUsed": {
      "stmt": sorted_unique(supported_stmt_used),
      "expr": sorted_unique(supported_expr_used),
    },
    "unsupported": {
      "stmt": sorted_unique(unsupported_stmt),
      "expr": sorted_unique(unsupported_expr),
    },
    "coverage": {
      "unique": {
        "stmt": {
          "supported": len(supported_stmt_used),
          "total": len(used_stmt),
          "pct": round(100.0 * len(supported_stmt_used) / max(len(used_stmt), 1), 2),
        },
        "expr": {
          "supported": len(supported_expr_used),
          "total": len(used_expr),
          "pct": round(100.0 * len(supported_expr_used) / max(len(used_expr), 1), 2),
        },
      },
      "occurrences": {
        "stmt": {
          "supported": total_occurrences(usage.stmt_counts, supported_stmt_used),
          "total": sum(usage.stmt_counts.values()),
        },
        "expr": {
          "supported": total_occurrences(usage.expr_counts, supported_expr_used),
          "total": sum(usage.expr_counts.values()),
        },
      },
    },
    "counts": {
      "stmt": dict(sorted(usage.stmt_counts.items())),
      "expr": dict(sorted(usage.expr_counts.items())),
    },
  }


def validate_against_baseline(report: dict[str, Any], baseline: dict[str, Any]) -> None:
  expected_stmt = set(baseline.get("expectedUnsupported", {}).get("stmt", []))
  expected_expr = set(baseline.get("expectedUnsupported", {}).get("expr", []))

  actual_stmt = set(report["unsupported"]["stmt"])
  actual_expr = set(report["unsupported"]["expr"])

  if actual_stmt != expected_stmt:
    missing = sorted(expected_stmt - actual_stmt)
    extra = sorted(actual_stmt - expected_stmt)
    raise MigrationGateError(
      "unsupported Stmt set drift detected: "
      f"missing={missing} extra={extra}"
    )

  if actual_expr != expected_expr:
    missing = sorted(expected_expr - actual_expr)
    extra = sorted(actual_expr - expected_expr)
    raise MigrationGateError(
      "unsupported Expr set drift detected: "
      f"missing={missing} extra={extra}"
    )


def validate_operation_blockers(
  operation_blockers: dict[str, list[str]],
  obligations: dict[str, dict[str, Any]],
) -> None:
  missing_ops = sorted(TRACKED_OPERATION_BLOCKER_OPS - set(operation_blockers))
  if missing_ops:
    raise MigrationGateError(
      "tracked operation blocker coverage missing spec bodies: "
      f"{missing_ops}"
    )

  for operation in sorted(TRACKED_OPERATION_BLOCKER_OPS):
    obligation = obligations.get(operation)
    if obligation is None:
      raise MigrationGateError(
        f"tracked operation blocker coverage missing obligation for `{operation}`"
      )
    if obligation.get("macroMigrated") is not False:
      raise MigrationGateError(
        f"tracked operation `{operation}` must remain macroMigrated=false while blocker-tracked"
      )
    expected = obligation.get("macroSurfaceBlockers")
    if not isinstance(expected, list) or not expected or not all(isinstance(x, str) for x in expected):
      raise MigrationGateError(
        f"obligation `{operation}` missing non-empty string list `macroSurfaceBlockers`"
      )
    if len(expected) != len(set(expected)):
      raise MigrationGateError(
        f"obligation `{operation}` has duplicate entries in `macroSurfaceBlockers`"
      )
    expected_set = set(expected)
    actual_set = set(operation_blockers[operation])
    if expected_set != actual_set:
      missing = sorted(actual_set - expected_set)
      extra = sorted(expected_set - actual_set)
      raise MigrationGateError(
        f"obligation `{operation}` macroSurfaceBlockers drift detected: "
        f"missing={missing} extra={extra}"
      )


def parser() -> argparse.ArgumentParser:
  p = argparse.ArgumentParser(
    description="Check morphoSpec constructor usage against current verity macro support"
  )
  p.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH)
  p.add_argument("--baseline", type=pathlib.Path, default=BASELINE_PATH)
  p.add_argument("--obligations", type=pathlib.Path, default=OBLIGATIONS_PATH)
  p.add_argument("--json-out", type=pathlib.Path)
  p.add_argument(
    "--write",
    action="store_true",
    help="Update baseline expectedUnsupported sets from current spec usage",
  )
  return p


def main() -> None:
  args = parser().parse_args()
  spec_text = read_text(args.spec)
  usage = parse_constructor_usage(spec_text)
  report = build_report(
    usage,
    source_path=args.spec,
    baseline_path=args.baseline,
    obligations_path=args.obligations,
  )
  operation_blockers = build_operation_blocker_report(spec_text)

  if args.write:
    baseline = {
      "source": report["source"],
      "expectedUnsupported": {
        "stmt": report["unsupported"]["stmt"],
        "expr": report["unsupported"]["expr"],
      },
      "notes": (
        "Fail-closed migration tracker for macro-native cutover. "
        "As verity macro support grows and morphoSpec shrinks, these sets should only move via explicit reviewed updates."
      ),
    }
    write_baseline(args.baseline, baseline)

  baseline = load_baseline(args.baseline)
  obligations = load_obligations(args.obligations)
  validate_against_baseline(report, baseline)
  validate_operation_blockers(operation_blockers, obligations)

  report["operationBlockers"] = operation_blockers

  if args.json_out:
    try:
      args.json_out.parent.mkdir(parents=True, exist_ok=True)
      with args.json_out.open("w", encoding="utf-8") as f:
        json.dump(report, f, indent=2, sort_keys=True)
        f.write("\n")
    except OSError as exc:
      raise MigrationGateError(f"failed to write {args.json_out}: {exc}") from exc

  stmt_cov = report["coverage"]["unique"]["stmt"]
  expr_cov = report["coverage"]["unique"]["expr"]
  print("macro-migration blockers check: OK")
  print(f"source: {report['source']}")
  print(
    "unique coverage: "
    f"Stmt {stmt_cov['supported']}/{stmt_cov['total']} ({stmt_cov['pct']}%), "
    f"Expr {expr_cov['supported']}/{expr_cov['total']} ({expr_cov['pct']}%)"
  )
  print(f"unsupported Stmt: {', '.join(report['unsupported']['stmt']) or '(none)'}")
  print(f"unsupported Expr: {', '.join(report['unsupported']['expr']) or '(none)'}")


if __name__ == "__main__":
  try:
    main()
  except MigrationGateError as e:
    print(f"macro-migration blockers check failed: {e}", file=sys.stderr)
    sys.exit(1)
