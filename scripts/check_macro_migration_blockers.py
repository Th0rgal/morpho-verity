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

# Supported constructor surface in Verity macro translation (current upstream state).
SUPPORTED_STMT = {
  "assignVar",
  "letVar",
  "setStorage",
  "setMapping",
  "setMapping2",
  "setMappingUint",
  "require",
  "return",
  "ite",
  "stop",
}

SUPPORTED_EXPR = {
  "param",
  "localVar",
  "literal",
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
  "logicalNot",
  "storage",
  "mapping",
  "mapping2",
  "mappingUint",
  "caller",
  "mulDivDown",
  "mulDivUp",
  "wMulDown",
  "wDivUp",
  "min",
  "max",
  "ite",
}

CTOR_RE = re.compile(r"\b(Stmt|Expr)\.([A-Za-z0-9_]+)")


@dataclass(frozen=True)
class ConstructorUsage:
  stmt_counts: dict[str, int]
  expr_counts: dict[str, int]


class MigrationGateError(RuntimeError):
  pass


def read_text(path: pathlib.Path) -> str:
  with path.open("r", encoding="utf-8") as f:
    return f.read()


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
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def write_baseline(path: pathlib.Path, data: dict[str, Any]) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  with path.open("w", encoding="utf-8") as f:
    json.dump(data, f, indent=2, sort_keys=True)
    f.write("\n")


def build_report(usage: ConstructorUsage) -> dict[str, Any]:
  used_stmt = set(usage.stmt_counts)
  used_expr = set(usage.expr_counts)

  unsupported_stmt = used_stmt - SUPPORTED_STMT
  unsupported_expr = used_expr - SUPPORTED_EXPR

  supported_stmt_used = used_stmt & SUPPORTED_STMT
  supported_expr_used = used_expr & SUPPORTED_EXPR

  return {
    "source": str(SPEC_PATH.relative_to(ROOT)),
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


def parser() -> argparse.ArgumentParser:
  p = argparse.ArgumentParser(
    description="Check morphoSpec constructor usage against current verity macro support"
  )
  p.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH)
  p.add_argument("--baseline", type=pathlib.Path, default=BASELINE_PATH)
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
  report = build_report(usage)

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
  validate_against_baseline(report, baseline)

  if args.json_out:
    args.json_out.parent.mkdir(parents=True, exist_ok=True)
    with args.json_out.open("w", encoding="utf-8") as f:
      json.dump(report, f, indent=2, sort_keys=True)
      f.write("\n")

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
  except FileNotFoundError as e:
    print(f"macro-migration blockers check failed: {e}", file=sys.stderr)
    sys.exit(1)
