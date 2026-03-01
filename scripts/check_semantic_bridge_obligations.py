#!/usr/bin/env python3
"""Fail-closed tracker for semantic bridge equivalence obligations.

Validates that config/semantic-bridge-obligations.json matches the set of
*SemEq definitions in Morpho/Proofs/SolidityBridge.lean, cross-checks
macroMigrated flags against MacroSlice.lean stub detection, and reports
assumed-vs-discharged status.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
BRIDGE_PATH = ROOT / "Morpho" / "Proofs" / "SolidityBridge.lean"
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"
MACRO_SLICE_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"

VALID_STATUSES = {"assumed", "in_progress", "discharged"}

SEM_EQ_RE = re.compile(r"^def\s+(\w+SemEq)\b", re.MULTILINE)

# Match function declarations in verity_contract macro syntax.
# Captures function name from lines like: "  function setOwner (..."
MACRO_FN_RE = re.compile(r"^\s+function\s+(\w+)\s*\(", re.MULTILINE)

# Detect stub functions by the noop tautology pattern: require (sender == sender)
STUB_RE = re.compile(r'require\s*\(sender\s*==\s*sender\)\s*"(\w+)\s+noop"')


class ObligationError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


def extract_sem_eq_definitions(bridge_text: str) -> list[str]:
    """Extract all *SemEq definition names from SolidityBridge.lean."""
    return [m.group(1) for m in SEM_EQ_RE.finditer(bridge_text)]


def extract_macro_functions(macro_text: str) -> dict[str, bool]:
    """Extract function names and stub status from MacroSlice.lean.

    Returns a dict mapping function name -> True if fully implemented,
    False if it's a stub (contains the noop tautology pattern).
    """
    # Split by function declarations to analyze each function body
    parts = MACRO_FN_RE.split(macro_text)
    result: dict[str, bool] = {}

    # parts alternates: [preamble, name1, body1, name2, body2, ...]
    for i in range(1, len(parts), 2):
        fn_name = parts[i]
        fn_body = parts[i + 1] if i + 1 < len(parts) else ""
        # Look for the noop stub pattern in the body (up to next function)
        is_stub = bool(STUB_RE.search(fn_body))
        result[fn_name] = not is_stub

    return result


def load_config(path: pathlib.Path) -> dict[str, Any]:
    with path.open("r", encoding="utf-8") as f:
        return json.load(f)


def validate_config(
    config: dict[str, Any],
    bridge_hypotheses: list[str],
    macro_functions: dict[str, bool] | None = None,
) -> None:
    """Validate config structure and consistency with source files."""
    obligations = config.get("obligations")
    if not isinstance(obligations, list):
        raise ObligationError("config missing 'obligations' array")

    # Check each obligation has required fields
    for i, obl in enumerate(obligations):
        for field in ("id", "hypothesis", "operation", "status"):
            if field not in obl:
                raise ObligationError(
                    f"obligation[{i}] missing required field '{field}'"
                )
        if obl["status"] not in VALID_STATUSES:
            raise ObligationError(
                f"obligation[{i}] has invalid status '{obl['status']}' "
                f"(valid: {sorted(VALID_STATUSES)})"
            )

    # Check no duplicate IDs
    ids = [o["id"] for o in obligations]
    if len(ids) != len(set(ids)):
        dupes = sorted(x for x in ids if ids.count(x) > 1)
        raise ObligationError(f"duplicate obligation IDs: {dupes}")

    # Check no duplicate hypotheses
    hyps = [o["hypothesis"] for o in obligations]
    if len(hyps) != len(set(hyps)):
        dupes = sorted(x for x in hyps if hyps.count(x) > 1)
        raise ObligationError(f"duplicate hypotheses: {dupes}")

    # Cross-reference against SolidityBridge.lean
    config_hyps = set(hyps)
    bridge_hyps = set(bridge_hypotheses)

    missing = sorted(bridge_hyps - config_hyps)
    extra = sorted(config_hyps - bridge_hyps)

    if missing:
        raise ObligationError(
            f"hypotheses in SolidityBridge.lean but not in config: {missing}"
        )
    if extra:
        raise ObligationError(
            f"hypotheses in config but not in SolidityBridge.lean: {extra}"
        )

    # Cross-reference macroMigrated against MacroSlice.lean
    if macro_functions is not None:
        for i, obl in enumerate(obligations):
            op = obl["operation"]
            claimed = obl.get("macroMigrated")
            if claimed is None:
                continue  # macroMigrated field is optional for backwards compat
            if not isinstance(claimed, bool):
                raise ObligationError(
                    f"obligation[{i}] ({op}): macroMigrated must be boolean"
                )
            actual = macro_functions.get(op)
            if actual is None:
                # Operation not found in MacroSlice — only valid if claimed false
                if claimed:
                    raise ObligationError(
                        f"obligation[{i}] ({op}): macroMigrated=true but "
                        f"function not found in MacroSlice.lean"
                    )
            elif claimed and not actual:
                raise ObligationError(
                    f"obligation[{i}] ({op}): macroMigrated=true but "
                    f"function is a stub in MacroSlice.lean"
                )
            elif not claimed and actual:
                raise ObligationError(
                    f"obligation[{i}] ({op}): macroMigrated=false but "
                    f"function has a full implementation in MacroSlice.lean"
                )


def build_report(config: dict[str, Any]) -> dict[str, Any]:
    obligations = config["obligations"]
    by_status: dict[str, int] = {}
    for obl in obligations:
        s = obl["status"]
        by_status[s] = by_status.get(s, 0) + 1

    migrated = sum(1 for o in obligations if o.get("macroMigrated"))
    pending_migration = sum(1 for o in obligations if not o.get("macroMigrated"))

    return {
        "source": str(BRIDGE_PATH.relative_to(ROOT)),
        "config": str(CONFIG_PATH.relative_to(ROOT)),
        "total": len(obligations),
        "byStatus": dict(sorted(by_status.items())),
        "macroMigrated": migrated,
        "macroPending": pending_migration,
        "obligations": [
            {
                "id": o["id"],
                "hypothesis": o["hypothesis"],
                "operation": o["operation"],
                "status": o["status"],
                "macroMigrated": o.get("macroMigrated", False),
            }
            for o in obligations
        ],
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Validate semantic bridge obligations against SolidityBridge.lean"
    )
    p.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
    p.add_argument("--bridge", type=pathlib.Path, default=BRIDGE_PATH)
    p.add_argument("--macro-slice", type=pathlib.Path, default=MACRO_SLICE_PATH)
    p.add_argument("--json-out", type=pathlib.Path)
    return p


def main() -> None:
    args = parser().parse_args()

    bridge_text = read_text(args.bridge)
    bridge_hypotheses = extract_sem_eq_definitions(bridge_text)

    macro_functions: dict[str, bool] | None = None
    if args.macro_slice.exists():
        macro_text = read_text(args.macro_slice)
        macro_functions = extract_macro_functions(macro_text)

    config = load_config(args.config)
    validate_config(config, bridge_hypotheses, macro_functions)

    report = build_report(config)

    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        with args.json_out.open("w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, sort_keys=True)
            f.write("\n")

    print("semantic bridge obligations check: OK")
    print(f"source: {report['source']}")
    print(f"total obligations: {report['total']}")
    for status, count in sorted(report["byStatus"].items()):
        print(f"  {status}: {count}")
    print(f"macro migrated: {report['macroMigrated']}/{report['total']}")
    print(f"macro pending: {report['macroPending']}/{report['total']}")


if __name__ == "__main__":
    try:
        main()
    except ObligationError as e:
        print(f"semantic bridge obligations check failed: {e}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"semantic bridge obligations check failed: {e}", file=sys.stderr)
        sys.exit(1)
