#!/usr/bin/env python3
"""Primitive coverage analysis for semantic bridge discharge readiness.

For each macro-migrated operation in MacroSlice.lean, extracts the set of
EDSL primitives used and maps them to the proven lemmas in verity's
PrimitiveBridge.lean. Reports which operations are fully covered by
proven primitives and which have gaps requiring upstream work.

This is a forward-looking readiness check: once the Layer 3 contract-level
sorry in verity's EndToEnd.lean is discharged, operations whose primitives
are fully covered can be discharged immediately.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"

# ---------------------------------------------------------------------------
# EDSL primitive detection patterns in MacroSlice.lean function bodies
# ---------------------------------------------------------------------------

# Each entry: (primitive_name, regex_pattern)
# These match the EDSL operations used in verity_contract function bodies.
PRIMITIVE_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("msgSender", re.compile(r"\bmsgSender\b")),
    ("getStorageAddr", re.compile(r"\bgetStorageAddr\b")),
    ("setStorageAddr", re.compile(r"\bsetStorageAddr\b")),
    ("getMapping", re.compile(r"\bgetMapping\s+\w+Slot\s+\w+(?!\w)")),
    ("setMapping", re.compile(r"\bsetMapping\s+\w+Slot\s+\w+\s+\d+")),
    ("getMappingUint", re.compile(r"\bgetMappingUint\b")),
    ("setMappingUint", re.compile(r"\bsetMappingUint\b")),
    ("getMapping2", re.compile(r"\bgetMapping2\b")),
    ("setMapping2", re.compile(r"\bsetMapping2\b")),
    ("getMappingWord", re.compile(r"\bgetMappingWord\b")),
    ("require_eq", re.compile(r"\brequire\s*\(\s*\w+\s*==\s*\w+\s*\)")),
    ("require_neq", re.compile(r"\brequire\s*\(\s*\w+\s*!=\s*\w+\s*\)")),
    ("require_lt", re.compile(r"\brequire\s*\(\s*\w+\s*<\s*\w+\s*\)")),
    ("require_gt", re.compile(r"\brequire\s*\(\s*\w+\s*>\s*\w+\s*\)")),
    ("if_then_else", re.compile(r"\bif\s+\w+\s+then\b")),
    ("return", re.compile(r"\breturn\s+\w+")),
    ("returnValues", re.compile(r"\breturnValues\b")),
    ("mstore", re.compile(r"\bmstore\b")),
    ("keccak256", re.compile(r"\bkeccak256\b")),
    ("chainid", re.compile(r"\bchainid\b")),
    ("contractAddress", re.compile(r"\bcontractAddress\b")),
    ("returnStorageWords", re.compile(r"\breturnStorageWords\b")),
    ("add", re.compile(r"\badd\s+\w+\s+\w+")),
    ("and", re.compile(r"\band\s+\w+\b")),
    ("shr", re.compile(r"\bshr\s+\w+\s+\w+")),
]

# ---------------------------------------------------------------------------
# PrimitiveBridge coverage: which EDSL primitives have proven lemmas
# ---------------------------------------------------------------------------

# Maps EDSL primitive names to their bridge-level lemma status.
# Status values:
#   "proven" = bridge lemma exists (EDSL ↔ compiled Yul) with no sorry
#   "edsl_proven" = EDSL-level automation lemma proven (MappingAutomation.lean),
#                   but EDSL-to-Yul bridge lemma not yet in PrimitiveBridge
#   "partial" = lemma exists but requires additional composition
#   "missing" = no lemma yet (upstream work needed)
#
# Based on analysis of verity semantic-bridge branch at 10f6da3 (2026-03-01).
# PrimitiveBridge.lean: EDSL ↔ compiled Yul bridge lemmas
# MappingAutomation.lean: EDSL-level read/write/non-interference lemmas (zero sorry)
# MappingSlot.lean: keccak-based slot encoding infrastructure
PRIMITIVE_BRIDGE_STATUS: dict[str, str] = {
    # Fully proven in PrimitiveBridge.lean (EDSL ↔ Yul bridge)
    "msgSender": "proven",           # msgSender_matches_caller
    "getStorageAddr": "proven",      # getStorage_matches_sload
    "setStorageAddr": "proven",      # setStorage_matches_sstore
    "require_eq": "proven",          # require_matches_iszero_revert
    "require_neq": "proven",         # require_matches_iszero_revert (negated)
    "require_lt": "proven",          # require_matches_iszero_revert (lt)
    "require_gt": "proven",          # require_matches_iszero_revert (gt)
    "if_then_else": "proven",        # if_else_matches
    "add": "proven",                 # uint256_add_matches_builtin
    "and": "partial",                # no direct lemma, but bitwise ops follow same pattern
    "shr": "partial",                # no direct lemma yet
    "return": "partial",             # implicit in monad composition (bind_unfold/pure_unfold)

    # Mapping operations -- EDSL-level lemmas fully proven in MappingAutomation.lean
    # (read-after-write, non-interference, cross-slot preservation)
    # Bridge-level (EDSL ↔ Yul via keccak slot) not yet in PrimitiveBridge.
    # MappingSlot.lean provides solidityMappingSlot infrastructure.
    "getMapping": "edsl_proven",     # MappingAutomation: getMapping_runState/runValue
    "setMapping": "edsl_proven",     # MappingAutomation: setMapping_runState, same/diff
    "getMappingUint": "edsl_proven", # MappingAutomation: getMappingUint_runState/runValue
    "setMappingUint": "edsl_proven", # MappingAutomation: setMappingUint_runState, same/diff
    "getMapping2": "edsl_proven",    # MappingAutomation: getMapping2_runState/runValue
    "setMapping2": "edsl_proven",    # MappingAutomation: setMapping2_runState, same/diff
    "getMappingWord": "missing",     # struct member access via slot offset

    # Other missing primitives
    "returnValues": "missing",       # tuple return encoding
    "mstore": "missing",            # memory management
    "keccak256": "missing",         # hash computation
    "chainid": "missing",           # environment access
    "contractAddress": "missing",   # environment access
    "returnStorageWords": "missing",  # batch storage read
}


class PrimitiveCoverageError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


# ---------------------------------------------------------------------------
# MacroSlice function splitting (reused from check_spec_correspondence.py)
# ---------------------------------------------------------------------------

MACRO_FUNC_RE = re.compile(
    r"^\s+function\s+(\w+)\s*\((.*?)\)\s*:", re.MULTILINE
)

STUB_RE = re.compile(r'require\s*\(sender\s*==\s*sender\)\s*"\w+\s+noop"')
HARDCODED_RETURN_RE = re.compile(r"returnValues\s*\[\s*0(?:\s*,\s*0)*\s*\]")


def split_macro_functions(text: str) -> dict[str, str]:
    """Split MacroSlice.lean into per-function text blocks."""
    result: dict[str, str] = {}
    matches = list(MACRO_FUNC_RE.finditer(text))
    for i, m in enumerate(matches):
        fn_name = m.group(1)
        start = m.start()
        end = matches[i + 1].start() if i + 1 < len(matches) else len(text)
        result[fn_name] = text[start:end]
    return result


def is_stub(block: str) -> bool:
    return bool(STUB_RE.search(block)) or bool(HARDCODED_RETURN_RE.search(block))


# ---------------------------------------------------------------------------
# Primitive extraction and coverage analysis
# ---------------------------------------------------------------------------


def extract_primitives(fn_body: str) -> set[str]:
    """Extract the set of EDSL primitives used in a function body."""
    primitives: set[str] = set()
    for name, pattern in PRIMITIVE_PATTERNS:
        if pattern.search(fn_body):
            primitives.add(name)
    return primitives


def analyze_coverage(
    macro_text: str,
    migrated_ops: set[str],
) -> dict[str, dict[str, Any]]:
    """Analyze primitive coverage for each migrated operation."""
    fn_blocks = split_macro_functions(macro_text)
    result: dict[str, dict[str, Any]] = {}

    for op in sorted(migrated_ops):
        if op not in fn_blocks:
            result[op] = {"error": "not found in MacroSlice.lean"}
            continue

        block = fn_blocks[op]
        if is_stub(block):
            result[op] = {"error": "function is a stub"}
            continue

        primitives = extract_primitives(block)
        proven = set()
        edsl_proven = set()
        partial = set()
        missing = set()

        for prim in primitives:
            status = PRIMITIVE_BRIDGE_STATUS.get(prim, "missing")
            if status == "proven":
                proven.add(prim)
            elif status == "edsl_proven":
                edsl_proven.add(prim)
            elif status == "partial":
                partial.add(prim)
            else:
                missing.add(prim)

        fully_covered = len(missing) == 0 and len(partial) == 0 and len(edsl_proven) == 0
        # edsl_ready: all gaps are edsl_proven (bridge lemma away from discharge)
        edsl_ready = len(missing) == 0 and len(partial) == 0 and len(edsl_proven) > 0
        result[op] = {
            "primitives": sorted(primitives),
            "proven": sorted(proven),
            "edsl_proven": sorted(edsl_proven),
            "partial": sorted(partial),
            "missing": sorted(missing),
            "fully_covered": fully_covered,
            "edsl_ready": edsl_ready,
        }

    return result


def build_report(
    coverage: dict[str, dict[str, Any]],
) -> dict[str, Any]:
    fully_covered = sum(
        1 for v in coverage.values()
        if v.get("fully_covered", False)
    )
    edsl_ready = sum(
        1 for v in coverage.values()
        if v.get("edsl_ready", False)
    )
    total = len(coverage)
    return {
        "total": total,
        "fully_covered": fully_covered,
        "edsl_ready": edsl_ready,
        "gaps_remaining": total - fully_covered - edsl_ready,
        "operations": coverage,
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Analyze primitive coverage for semantic bridge discharge"
    )
    p.add_argument("--macro-slice", type=pathlib.Path, default=MACRO_PATH)
    p.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
    p.add_argument("--json-out", type=pathlib.Path)
    return p


def main() -> None:
    args = parser().parse_args()

    macro_text = read_text(args.macro_slice)

    with args.config.open("r", encoding="utf-8") as f:
        config = json.load(f)
    migrated_ops = {
        o["operation"]
        for o in config["obligations"]
        if o.get("macroMigrated")
    }

    coverage = analyze_coverage(macro_text, migrated_ops)
    report = build_report(coverage)

    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        with args.json_out.open("w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, sort_keys=True)
            f.write("\n")

    print("primitive coverage analysis: OK")
    print(f"migrated operations: {report['total']}")
    print(f"fully covered by proven bridge lemmas: {report['fully_covered']}/{report['total']}")
    print(f"EDSL-ready (need bridge lemmas only): {report['edsl_ready']}/{report['total']}")
    print(f"gaps remaining: {report['gaps_remaining']}/{report['total']}")
    print()
    for op, data in sorted(report["operations"].items()):
        if "error" in data:
            print(f"  {op}: ERROR - {data['error']}")
            continue
        if data["fully_covered"]:
            status = "READY"
        elif data.get("edsl_ready"):
            status = "EDSL-READY"
        else:
            status = "GAPS"
        print(f"  {op}: [{status}]")
        print(f"    primitives: {', '.join(data['primitives'])}")
        if data["proven"]:
            print(f"    proven:     {', '.join(data['proven'])}")
        if data.get("edsl_proven"):
            print(f"    edsl_proven: {', '.join(data['edsl_proven'])}")
        if data["partial"]:
            print(f"    partial:    {', '.join(data['partial'])}")
        if data["missing"]:
            print(f"    missing:    {', '.join(data['missing'])}")


if __name__ == "__main__":
    try:
        main()
    except PrimitiveCoverageError as e:
        print(f"primitive coverage analysis failed: {e}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"primitive coverage analysis failed: {e}", file=sys.stderr)
        sys.exit(1)
