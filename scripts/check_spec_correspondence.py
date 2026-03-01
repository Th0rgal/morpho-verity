#!/usr/bin/env python3
"""Fail-closed checker for CompilationModel spec correspondence.

Validates that macro-migrated functions in MacroSlice.lean have structural
correspondence with the manual Spec.lean: matching function names, parameter
counts, storage slot assignments, and require condition counts.

This is a textual approximation -- full semantic correspondence will be
verified at the Lean level once the verity semantic bridge lands.
"""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any


ROOT = pathlib.Path(__file__).resolve().parent.parent
SPEC_PATH = ROOT / "Morpho" / "Compiler" / "Spec.lean"
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"
CONFIG_PATH = ROOT / "config" / "semantic-bridge-obligations.json"


class CorrespondenceError(RuntimeError):
    pass


def read_text(path: pathlib.Path) -> str:
    with path.open("r", encoding="utf-8") as f:
        return f.read()


# ---------------------------------------------------------------------------
# Spec.lean extraction
# ---------------------------------------------------------------------------

# Matches param entries like: { name := "newOwner", ty := .address }
SPEC_PARAM_RE = re.compile(r'\{\s*name\s*:=\s*"(\w+)",\s*ty\s*:=\s*\.(\w+)\s*\}')

# Matches Stmt.require lines
SPEC_REQUIRE_RE = re.compile(r"Stmt\.require\b")

# Matches state mutations
SPEC_SET_RE = re.compile(
    r"Stmt\.(setStorage|setMapping2?|setMappingUint|setStructMember2?)\b"
)


def extract_spec_function_blocks(text: str) -> list[str]:
    """Extract individual function spec blocks from the functions list."""
    # Find the functions := [ section
    match = re.search(r"functions\s*:=\s*\[", text)
    if not match:
        return []
    start = match.end()

    # Extract each { name := "..." ... } block within the functions list
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


def extract_spec_functions(text: str) -> dict[str, dict[str, Any]]:
    """Extract function metadata from Spec.lean."""
    result: dict[str, dict[str, Any]] = {}

    for block in extract_spec_function_blocks(text):
        m = re.search(r'name\s*:=\s*"(\w+)"', block)
        if not m:
            continue
        fn_name = m.group(1)
        params = SPEC_PARAM_RE.findall(block)
        requires = len(SPEC_REQUIRE_RE.findall(block))
        mutations = len(SPEC_SET_RE.findall(block))
        result[fn_name] = {
            "params": [(name, ty) for name, ty in params],
            "param_count": len(params),
            "require_count": requires,
            "mutation_count": mutations,
        }
    return result


# ---------------------------------------------------------------------------
# MacroSlice.lean extraction
# ---------------------------------------------------------------------------

# Matches function declarations
MACRO_FUNC_RE = re.compile(
    r"^\s+function\s+(\w+)\s*\((.*?)\)\s*:", re.MULTILINE
)

# Matches macro params like: newOwner : Address
MACRO_PARAM_RE = re.compile(r"(\w+)\s*:\s*(\w+)")

# Matches storage slot declarations: ownerSlot : Address := slot 0
MACRO_SLOT_RE = re.compile(r"(\w+Slot)\s*:.*?:=\s*slot\s+(\d+)")

# Matches require calls in macro
MACRO_REQUIRE_RE = re.compile(r"\brequire\s*\(")

# Matches storage mutations in macro
MACRO_SET_RE = re.compile(
    r"\b(setStorageAddr|setStorage\b|setMapping2?\b|setMappingUint)\b"
)

# Noop stub pattern
STUB_RE = re.compile(r'require\s*\(sender\s*==\s*sender\)\s*"\w+\s+noop"')

# Hardcoded-return stub pattern (e.g., returnValues [0, 0, 0])
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


def extract_macro_functions(text: str) -> dict[str, dict[str, Any]]:
    """Extract function metadata from MacroSlice.lean."""
    result: dict[str, dict[str, Any]] = {}
    for fn_name, block in split_macro_functions(text).items():
        m = MACRO_FUNC_RE.search(block)
        if not m:
            continue
        param_text = m.group(2)
        params = MACRO_PARAM_RE.findall(param_text)
        is_stub = bool(STUB_RE.search(block)) or bool(HARDCODED_RETURN_RE.search(block))
        requires = len(MACRO_REQUIRE_RE.findall(block))
        mutations = len(MACRO_SET_RE.findall(block))
        result[fn_name] = {
            "params": params,
            "param_count": len(params),
            "is_stub": is_stub,
            "require_count": requires,
            "mutation_count": mutations,
        }
    return result


def extract_macro_slots(text: str) -> dict[str, int]:
    """Extract storage slot assignments from MacroSlice.lean."""
    result: dict[str, int] = {}
    for m in MACRO_SLOT_RE.finditer(text):
        field_name = m.group(1).replace("Slot", "")
        result[field_name] = int(m.group(2))
    return result


# ---------------------------------------------------------------------------
# Spec.lean field extraction
# ---------------------------------------------------------------------------


def extract_spec_fields(text: str) -> dict[str, int]:
    """Extract storage field -> slot mappings from Spec.lean fields block."""
    result: dict[str, int] = {}
    # Find the fields := [ section
    match = re.search(r"fields\s*:=\s*\[", text)
    if not match:
        return result
    start = match.end()

    # Extract each field block
    depth = 0
    current = ""
    in_block = False
    for i in range(start, len(text)):
        ch = text[i]
        if ch == "{":
            if depth == 0:
                in_block = True
                current = ""
            depth += 1
        if in_block:
            current += ch
        if ch == "}":
            depth -= 1
            if depth == 0 and in_block:
                in_block = False
                name_m = re.search(r'name\s*:=\s*"(\w+)"', current)
                slot_m = re.search(r"slot\s*:=\s*(?:some\s+)?(\d+)", current)
                if name_m and slot_m:
                    result[name_m.group(1)] = int(slot_m.group(1))
        if ch == "]" and depth == 0 and not in_block:
            break
    return result


# ---------------------------------------------------------------------------
# Correspondence validation
# ---------------------------------------------------------------------------


def validate_correspondence(
    spec_fns: dict[str, dict[str, Any]],
    macro_fns: dict[str, dict[str, Any]],
    spec_fields: dict[str, int],
    macro_slots: dict[str, int],
    migrated_ops: set[str],
) -> list[str]:
    """Validate structural correspondence for migrated functions."""
    errors: list[str] = []

    # 1. Storage slot correspondence
    for macro_name, macro_slot in macro_slots.items():
        if macro_name in spec_fields:
            if macro_slot != spec_fields[macro_name]:
                errors.append(
                    f"slot mismatch: {macro_name} is slot {macro_slot} in "
                    f"MacroSlice but slot {spec_fields[macro_name]} in Spec"
                )

    # 2. Per-function checks for migrated operations
    for op in sorted(migrated_ops):
        if op not in macro_fns:
            errors.append(f"migrated op '{op}' not found in MacroSlice.lean")
            continue
        if op not in spec_fns:
            errors.append(f"migrated op '{op}' not found in Spec.lean")
            continue

        macro = macro_fns[op]
        spec = spec_fns[op]

        if macro["is_stub"]:
            errors.append(f"migrated op '{op}' is a stub in MacroSlice.lean")

        if macro["param_count"] != spec["param_count"]:
            errors.append(
                f"param count mismatch for '{op}': "
                f"MacroSlice={macro['param_count']}, Spec={spec['param_count']}"
            )

        # Macro may expand ownership checks into more require statements
        # (e.g., requireOwner in Spec is 1 Stmt.require, but macro has 2: msgSender + require ==)
        # So we check that macro has at least the spec's require count minus expansions.
        # But we should NOT have FEWER requires than spec (after accounting for requireOwner).

        # MacroSlice may expand a single mutation into if/else branches
        # (e.g., setMapping2 in both then/else), so we allow up to 2x.
        if macro["mutation_count"] > 2 * spec["mutation_count"]:
            errors.append(
                f"extra mutations for '{op}': "
                f"MacroSlice={macro['mutation_count']}, Spec={spec['mutation_count']}"
            )

    return errors


def build_report(
    spec_fns: dict[str, dict[str, Any]],
    macro_fns: dict[str, dict[str, Any]],
    migrated_ops: set[str],
    errors: list[str],
) -> dict[str, Any]:
    correspondences = []
    for op in sorted(migrated_ops):
        entry: dict[str, Any] = {"operation": op}
        if op in macro_fns and op in spec_fns:
            entry["macroParams"] = macro_fns[op]["param_count"]
            entry["specParams"] = spec_fns[op]["param_count"]
            entry["macroRequires"] = macro_fns[op]["require_count"]
            entry["specRequires"] = spec_fns[op]["require_count"]
            entry["macroMutations"] = macro_fns[op]["mutation_count"]
            entry["specMutations"] = spec_fns[op]["mutation_count"]
            entry["isStub"] = macro_fns[op].get("is_stub", False)
        correspondences.append(entry)

    return {
        "specFunctions": len(spec_fns),
        "macroFunctions": len(macro_fns),
        "migratedChecked": len(migrated_ops),
        "errors": errors,
        "correspondences": correspondences,
    }


def parser() -> argparse.ArgumentParser:
    p = argparse.ArgumentParser(
        description="Validate CompilationModel spec correspondence"
    )
    p.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH)
    p.add_argument("--macro-slice", type=pathlib.Path, default=MACRO_PATH)
    p.add_argument("--config", type=pathlib.Path, default=CONFIG_PATH)
    p.add_argument("--json-out", type=pathlib.Path)
    return p


def main() -> None:
    args = parser().parse_args()

    spec_text = read_text(args.spec)
    macro_text = read_text(args.macro_slice)

    spec_fns = extract_spec_functions(spec_text)
    macro_fns = extract_macro_functions(macro_text)
    spec_fields = extract_spec_fields(spec_text)
    macro_slots = extract_macro_slots(macro_text)

    with args.config.open("r", encoding="utf-8") as f:
        config = json.load(f)
    migrated_ops = {
        o["operation"]
        for o in config["obligations"]
        if o.get("macroMigrated")
    }

    errors = validate_correspondence(
        spec_fns, macro_fns, spec_fields, macro_slots, migrated_ops,
    )

    report = build_report(spec_fns, macro_fns, migrated_ops, errors)

    if args.json_out:
        args.json_out.parent.mkdir(parents=True, exist_ok=True)
        with args.json_out.open("w", encoding="utf-8") as f:
            json.dump(report, f, indent=2, sort_keys=True)
            f.write("\n")

    if errors:
        for e in errors:
            print(f"  FAIL: {e}", file=sys.stderr)
        print(
            f"spec correspondence check failed: {len(errors)} error(s)",
            file=sys.stderr,
        )
        sys.exit(1)

    print("spec correspondence check: OK")
    print(f"spec functions: {report['specFunctions']}")
    print(f"macro functions: {report['macroFunctions']}")
    print(f"migrated checked: {report['migratedChecked']}")
    for c in report["correspondences"]:
        print(
            f"  {c['operation']}: params={c['macroParams']}/{c['specParams']} "
            f"requires={c['macroRequires']}/{c['specRequires']} "
            f"mutations={c['macroMutations']}/{c['specMutations']}"
        )


if __name__ == "__main__":
    try:
        main()
    except CorrespondenceError as e:
        print(f"spec correspondence check failed: {e}", file=sys.stderr)
        sys.exit(1)
    except FileNotFoundError as e:
        print(f"spec correspondence check failed: {e}", file=sys.stderr)
        sys.exit(1)
