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

# Detect hardcoded-return stubs like: returnValues [0, 0, 0]
HARDCODED_RETURN_RE = re.compile(r"returnValues\s*\[\s*0(?:\s*,\s*0)*\s*\]")

# Detect hard stubs like: require (0 == 1) "createMarket stub"
HARD_STUB_RE = re.compile(r'require\s*\(0\s*==\s*1\)\s*"(\w+)\s+stub"')


class ObligationError(RuntimeError):
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
    except OSError as exc:
        raise ObligationError(f"failed to read text file {path}: {exc}") from exc
    except UnicodeDecodeError as exc:
        raise ObligationError(f"failed to decode text file {path}: {exc}") from exc


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
        # Look for stub patterns in the body (up to next function):
        # 1. noop tautology: require (sender == sender) "X noop"
        # 2. hardcoded-return: returnValues [0, 0, 0] (pending upstream support)
        # 3. hard stub: require (0 == 1) "X stub"
        is_stub = (
            bool(STUB_RE.search(fn_body))
            or bool(HARDCODED_RETURN_RE.search(fn_body))
            or bool(HARD_STUB_RE.search(fn_body))
        )
        result[fn_name] = not is_stub

    return result


def load_config(path: pathlib.Path) -> dict[str, Any]:
    try:
        with path.open("r", encoding="utf-8") as f:
            raw = json.load(f)
    except OSError as exc:
        raise ObligationError(f"failed to read JSON config {path}: {exc}") from exc
    except json.JSONDecodeError as exc:
        raise ObligationError(
            f"{path} contains invalid JSON: {exc.msg} at line {exc.lineno} column {exc.colno}"
        ) from exc
    except UnicodeDecodeError as exc:
        raise ObligationError(f"failed to decode JSON config {path}: {exc}") from exc
    if not isinstance(raw, dict):
        raise ObligationError(f"{path} must contain a JSON object at the top level")
    return raw


def write_json_report(path: pathlib.Path, report: dict[str, Any]) -> None:
    try:
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, sort_keys=True)
            f.write("\n")
    except OSError as exc:
        raise ObligationError(f"failed to write JSON report {path}: {exc}") from exc


def validate_config(
    config: dict[str, Any],
    bridge_hypotheses: list[str],
    macro_functions: dict[str, bool] | None = None,
    *,
    bridge_path: pathlib.Path = BRIDGE_PATH,
) -> None:
    """Validate config structure and consistency with source files."""
    if "source" in config:
        raise ObligationError("config uses legacy 'source' field; use 'sourcePath'")

    source_path = config.get("sourcePath")
    if source_path is not None:
        if not isinstance(source_path, str) or not source_path:
            raise ObligationError("config field 'sourcePath' must be a non-empty string")
        expected_source_path = display_path(bridge_path.resolve())
        if source_path != expected_source_path:
            raise ObligationError(
                "config field 'sourcePath' does not match bridge input: "
                f"expected {expected_source_path!r}, got {source_path!r}"
            )

    obligations = config.get("obligations")
    if not isinstance(obligations, list):
        raise ObligationError("config missing 'obligations' array")

    # Check each obligation has required fields
    for i, obl in enumerate(obligations):
        if not isinstance(obl, dict):
            raise ObligationError(f"obligation[{i}] is not an object")
        for field in ("id", "hypothesis", "operation", "status"):
            if field not in obl:
                raise ObligationError(
                    f"obligation[{i}] missing required field '{field}'"
                )
        for field in ("id", "hypothesis", "operation"):
            value = obl[field]
            if not isinstance(value, str) or not value:
                raise ObligationError(
                    f"obligation[{i}] field '{field}' must be a non-empty string"
                )
        status = obl["status"]
        if not isinstance(status, str) or status not in VALID_STATUSES:
            raise ObligationError(
                f"obligation[{i}] has invalid status '{status}' "
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
            alias = obl.get("macroAlias", op)
            actual = macro_functions.get(alias)
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


def build_report(
    config: dict[str, Any],
    *,
    bridge_path: pathlib.Path = BRIDGE_PATH,
    config_path: pathlib.Path = CONFIG_PATH,
    macro_slice_path: pathlib.Path = MACRO_SLICE_PATH,
) -> dict[str, Any]:
    bridge_path = bridge_path.resolve()
    config_path = config_path.resolve()
    macro_slice_path = macro_slice_path.resolve()
    obligations = config["obligations"]
    by_status: dict[str, int] = {}
    for obl in obligations:
        s = obl["status"]
        by_status[s] = by_status.get(s, 0) + 1

    migrated = sum(1 for o in obligations if o.get("macroMigrated"))
    pending_migration = sum(1 for o in obligations if not o.get("macroMigrated"))

    return {
        "sourcePath": display_path(bridge_path),
        "configPath": display_path(config_path),
        "macroSlicePath": display_path(macro_slice_path),
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
    bridge_path = args.bridge.resolve()
    config_path = args.config.resolve()
    macro_slice_path = args.macro_slice.resolve()

    bridge_text = read_text(bridge_path)
    bridge_hypotheses = extract_sem_eq_definitions(bridge_text)

    macro_text = read_text(macro_slice_path)
    macro_functions = extract_macro_functions(macro_text)

    config = load_config(config_path)
    validate_config(
        config,
        bridge_hypotheses,
        macro_functions,
        bridge_path=bridge_path,
    )

    report = build_report(
        config,
        bridge_path=bridge_path,
        config_path=config_path,
        macro_slice_path=macro_slice_path,
    )

    if args.json_out:
        write_json_report(args.json_out, report)

    print("semantic bridge obligations check: OK")
    print(f"source: {report['sourcePath']}")
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
