#!/usr/bin/env python3
"""Uniquify shadowed Yul variable names to satisfy solc --strict-assembly.

The Verity compiler may emit nested blocks that reuse the same `let` variable
name (e.g. `__ite_cond`).  Yul semantics allow inner-block shadowing, but
`solc --strict-assembly` rejects it.  This script gives every declaration after
the first a globally unique suffix and renames all references within the
declaring block scope.

Usage:
    python3 scripts/uniquify_yul_shadows.py --input Morpho.yul --output Morpho.yul
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys


def remove_dead_ignored_lets(source: str) -> tuple[str, int]:
    """Remove `let _ignored* := undeclaredVar` lines emitted by the compiler."""
    ignored_re = re.compile(r"^\s*let\s+_ignored\w+\s*:=\s*\w+\s*$")
    lines = source.split("\n")
    kept: list[str] = []
    removed = 0
    for line in lines:
        if ignored_re.match(line):
            removed += 1
        else:
            kept.append(line)
    return "\n".join(kept), removed


def uniquify(source: str) -> tuple[str, int]:
    """Return (rewritten_source, rename_count).

    Strategy: scan line by line, track brace depth. For every `let X :=`,
    if X was already declared in any enclosing scope, rename this declaration
    and all references in its block to a globally unique name.
    """
    lines = source.split("\n")
    # scope_stack[i] = set of names declared at brace depth i
    scope_stack: list[set[str]] = [set()]
    global_counter: dict[str, int] = {}
    rename_count = 0

    let_re = re.compile(r"(\blet\s+)(\w+)(\s*:=)")

    i = 0
    while i < len(lines):
        line = lines[i]

        # Process braces BEFORE checking let (opening braces create new scopes)
        # But we need to handle the case where { and let are on the same line,
        # and } reduces scope. Process character by character up to the let.

        # First, handle any scope changes from braces on this line
        # We need to process { } that appear BEFORE the let declaration
        m = let_re.search(line)
        let_col = m.start() if m else len(line)

        for ci in range(len(line)):
            ch = line[ci]
            if ch == "{":
                scope_stack.append(set())
            elif ch == "}":
                if len(scope_stack) > 1:
                    scope_stack.pop()

        if m:
            var_name = m.group(2)
            # Check if already declared in any scope
            already_in_scope = any(var_name in s for s in scope_stack)
            if already_in_scope:
                global_counter[var_name] = global_counter.get(var_name, 0) + 1
                new_name = f"{var_name}_{global_counter[var_name]}"
                rename_count += 1
                # Rename declaration
                lines[i] = line[:m.start(2)] + new_name + line[m.end(2):]
                # Rename refs in subsequent lines until block closes
                depth = 0
                for j in range(i + 1, len(lines)):
                    for ch in lines[j]:
                        if ch == "{":
                            depth += 1
                        elif ch == "}":
                            depth -= 1
                    if depth < 0:
                        break
                    lines[j] = re.sub(
                        rf"\b{re.escape(var_name)}\b", new_name, lines[j]
                    )
                # Add new_name to current scope
                scope_stack[-1].add(new_name)
            else:
                scope_stack[-1].add(var_name)

        i += 1

    return "\n".join(lines), rename_count


def main() -> int:
    parser = argparse.ArgumentParser(description="Uniquify shadowed Yul variable names")
    parser.add_argument("--input", type=pathlib.Path, required=True)
    parser.add_argument("--output", type=pathlib.Path, required=True)
    args = parser.parse_args()

    source = args.input.read_text(encoding="utf-8")
    result, shadow_count = uniquify(source)
    result, ignored_count = remove_dead_ignored_lets(result)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(result, encoding="utf-8")

    print(f"uniquify_yul_shadows: renamed {shadow_count} shadowed, removed {ignored_count} dead _ignored lets")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
