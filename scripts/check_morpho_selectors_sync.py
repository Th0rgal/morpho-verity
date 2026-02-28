#!/usr/bin/env python3
"""Fail-closed check (or auto-fix) for morphoSelectors selector values."""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
from check_macro_migration_surface import (  # noqa: E402
  EXPECTED_ONLY_IN_SPEC,
  IMORPHO_CONTRACT,
  INTERFACE_PATH,
  SPEC_PATH,
  extract_solc_selector_map,
  extract_spec_selector_entries,
  read_text,
)


ENTRY_RE = re.compile(r"^(\s*)0x[0-9a-fA-F]+,?\s*--\s*(.+?)\s*$")


def fail(msg: str) -> None:
  print(f"morpho-selectors-sync check failed: {msg}", file=sys.stderr)
  sys.exit(1)


def find_selector_block_bounds(lines: list[str]) -> tuple[int, int]:
  start_idx = None
  for idx, line in enumerate(lines):
    if re.search(r"def\s+morphoSelectors\s*:\s*List\s+Nat\s*:=", line):
      start_idx = idx + 1
      break
  if start_idx is None:
    raise RuntimeError("unable to find morphoSelectors definition in Spec.lean")
  for idx in range(start_idx, len(lines)):
    if re.match(r"^\s*\]\s*$", lines[idx]):
      return start_idx, idx
  raise RuntimeError("unable to find morphoSelectors closing bracket in Spec.lean")


def infer_indent(body_lines: list[str]) -> str:
  for line in body_lines:
    m = ENTRY_RE.match(line)
    if m:
      return m.group(1)
  return "  "


def parse_selector_signatures(spec_text: str) -> list[str]:
  return [signature for signature, _ in extract_spec_selector_entries(spec_text)]


def make_extra_selector_map(signatures: set[str]) -> dict[str, int]:
  if not signatures:
    return {}
  temp_dir = pathlib.Path("/tmp")
  temp_path = temp_dir / "morpho_extra_selectors.sol"
  sorted_sigs = sorted(signatures)
  declarations = "\n".join(f"  function {sig} external view;" for sig in sorted_sigs)
  source = (
    "pragma solidity ^0.8.0;\n\n"
    "interface IExtraMorphoSelectors {\n"
    f"{declarations}\n"
    "}\n"
  )
  temp_path.write_text(source, encoding="utf-8")
  try:
    return extract_solc_selector_map(temp_path, "IExtraMorphoSelectors")
  finally:
    temp_path.unlink(missing_ok=True)


def expected_selector_map(interface_path: pathlib.Path, spec_signatures: list[str]) -> dict[str, int]:
  interface_map = extract_solc_selector_map(interface_path, IMORPHO_CONTRACT)
  missing = set(spec_signatures) - set(interface_map)
  unexpected_missing = missing - EXPECTED_ONLY_IN_SPEC
  if unexpected_missing:
    raise RuntimeError(
      "signatures present in morphoSelectors but missing from IMorpho and getter allowlist: "
      f"{sorted(unexpected_missing)}"
    )

  extra_map = make_extra_selector_map(set(spec_signatures) & EXPECTED_ONLY_IN_SPEC)
  selector_map = dict(interface_map)
  selector_map.update(extra_map)
  return selector_map


def render_selector_block(spec_text: str, selector_map: dict[str, int]) -> list[str]:
  lines = spec_text.splitlines()
  start_idx, end_idx = find_selector_block_bounds(lines)
  body_lines = lines[start_idx:end_idx]
  indent = infer_indent(body_lines)
  signatures = parse_selector_signatures(spec_text)

  rendered: list[str] = []
  for idx, signature in enumerate(signatures):
    if signature not in selector_map:
      raise RuntimeError(f"missing selector mapping for signature: {signature}")
    comma = "," if idx < len(signatures) - 1 else ""
    rendered.append(f"{indent}0x{selector_map[signature]:08x}{comma} -- {signature}")
  return rendered


def rewrite_selector_block(spec_text: str, selector_map: dict[str, int]) -> tuple[str, bool]:
  lines = spec_text.splitlines()
  start_idx, end_idx = find_selector_block_bounds(lines)
  rendered = render_selector_block(spec_text, selector_map)
  unchanged = lines[start_idx:end_idx] == rendered
  if unchanged:
    return spec_text, True
  updated = lines[:start_idx] + rendered + lines[end_idx:]
  return "\n".join(updated) + "\n", False


def main() -> None:
  parser = argparse.ArgumentParser(description=__doc__)
  parser.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH, help="Path to Spec.lean")
  parser.add_argument("--interface", type=pathlib.Path, default=INTERFACE_PATH, help="Path to IMorpho.sol")
  parser.add_argument("--write", action="store_true", help="Rewrite morphoSelectors block in-place")
  args = parser.parse_args()

  spec_path = args.spec.resolve()
  interface_path = args.interface.resolve()
  spec_text = read_text(spec_path)
  signatures = parse_selector_signatures(spec_text)
  selector_map = expected_selector_map(interface_path, signatures)
  rewritten, unchanged = rewrite_selector_block(spec_text, selector_map)

  if unchanged:
    print("morpho-selectors-sync check: OK (up to date)")
    return

  if args.write:
    spec_path.write_text(rewritten, encoding="utf-8")
    print(f"morpho-selectors-sync: updated {spec_path}")
    return

  fail(
    "morphoSelectors block is stale; run "
    "`python3 scripts/check_morpho_selectors_sync.py --write` and commit the update"
  )


if __name__ == "__main__":
  main()
