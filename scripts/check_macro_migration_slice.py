#!/usr/bin/env python3
"""Fail-closed tracker for macro-native migrated Morpho selector slice."""

from __future__ import annotations

import argparse
import json
import pathlib
import re
import sys
from typing import Any

from check_macro_migration_surface import (  # reuse canonical spec-signature parser
  SPEC_PATH,
  extract_spec_signatures,
  read_text,
)


ROOT = pathlib.Path(__file__).resolve().parent.parent
MACRO_PATH = ROOT / "Morpho" / "Compiler" / "MacroSlice.lean"
BASELINE_PATH = ROOT / "config" / "macro-migration-slice.json"
DEFAULT_CONTRACT = "MorphoViewSlice"

CONTRACT_RE = re.compile(r"^\s*verity_contract\s+([A-Za-z_][A-Za-z0-9_]*)\s+where\s*$")
FUNCTION_RE = re.compile(
  r"^\s*function\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*:\s*(.+?)\s*:=\s*"
)
PARAM_RE = re.compile(r"^\s*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*(.+?)\s*$")

TYPE_MAP = {
  "Address": "address",
  "Uint256": "uint256",
  "Bool": "bool",
  "Bytes32": "bytes32",
  "Bytes": "bytes",
}


class MigrationSliceError(RuntimeError):
  pass


def _normalize_type(type_src: str) -> str:
  return " ".join(type_src.strip().split())


def _sol_type(lean_ty: str) -> str:
  ty = _normalize_type(lean_ty)
  if ty in TYPE_MAP:
    return TYPE_MAP[ty]
  if ty.startswith("Array "):
    elem = ty[len("Array "):].strip()
    return f"{_sol_type(elem)}[]"
  raise MigrationSliceError(f"unsupported parameter type in macro slice: {ty!r}")


def _split_params(params_src: str) -> list[str]:
  params_src = params_src.strip()
  if not params_src:
    return []
  out: list[str] = []
  for raw in params_src.split(","):
    part = raw.strip()
    if not part:
      continue
    m = PARAM_RE.match(part)
    if not m:
      raise MigrationSliceError(f"invalid parameter declaration: {part!r}")
    out.append(_sol_type(m.group(2)))
  return out


def extract_macro_signatures(text: str, contract_name: str = DEFAULT_CONTRACT) -> set[str]:
  in_contract = False
  signatures: set[str] = set()

  for line in text.splitlines():
    cm = CONTRACT_RE.match(line)
    if cm:
      in_contract = cm.group(1) == contract_name
      continue
    if not in_contract:
      continue

    fm = FUNCTION_RE.match(line)
    if not fm:
      continue
    name = fm.group(1)
    params = ",".join(_split_params(fm.group(2)))
    signatures.add(f"{name}({params})")

  if not signatures:
    raise MigrationSliceError(
      f"no function signatures found for verity_contract {contract_name} in macro slice source"
    )
  return signatures


def load_baseline(path: pathlib.Path) -> dict[str, Any]:
  with path.open("r", encoding="utf-8") as f:
    return json.load(f)


def write_baseline(path: pathlib.Path, data: dict[str, Any]) -> None:
  path.parent.mkdir(parents=True, exist_ok=True)
  with path.open("w", encoding="utf-8") as f:
    json.dump(data, f, indent=2, sort_keys=True)
    f.write("\n")


def validate_against_baseline(actual: set[str], baseline: dict[str, Any]) -> None:
  expected = set(baseline.get("expectedMigrated", []))
  if actual != expected:
    missing = sorted(expected - actual)
    extra = sorted(actual - expected)
    raise MigrationSliceError(
      "macro migrated-signature set drift detected: "
      f"missing={missing} extra={extra}"
    )


def validate_blocked_against_baseline(
  spec_signatures: set[str],
  migrated_signatures: set[str],
  baseline: dict[str, Any],
) -> dict[str, str]:
  blocked = baseline.get("expectedBlocked")
  if not isinstance(blocked, dict):
    raise MigrationSliceError("baseline expectedBlocked must be a map of signature -> reason")

  invalid_reason = [sig for sig, reason in blocked.items() if not isinstance(reason, str) or not reason.strip()]
  if invalid_reason:
    raise MigrationSliceError(
      "baseline expectedBlocked contains empty/invalid reason(s): "
      f"{sorted(invalid_reason)}"
    )

  expected_blocked = set(blocked)
  actual_blocked = spec_signatures - migrated_signatures
  overlap = expected_blocked & migrated_signatures
  if overlap:
    raise MigrationSliceError(
      "baseline expectedBlocked overlaps migrated signatures: "
      f"{sorted(overlap)}"
    )

  if expected_blocked != actual_blocked:
    missing = sorted(actual_blocked - expected_blocked)
    extra = sorted(expected_blocked - actual_blocked)
    raise MigrationSliceError(
      "macro blocked-signature set drift detected: "
      f"missing={missing} extra={extra}"
    )

  return {sig: str(blocked[sig]).strip() for sig in sorted(expected_blocked)}


def run_check(
  spec_path: pathlib.Path = SPEC_PATH,
  macro_path: pathlib.Path = MACRO_PATH,
  baseline_path: pathlib.Path = BASELINE_PATH,
  contract_name: str = DEFAULT_CONTRACT,
) -> dict[str, Any]:
  spec_signatures = extract_spec_signatures(read_text(spec_path))
  migrated_signatures = extract_macro_signatures(read_text(macro_path), contract_name=contract_name)
  unknown_in_spec = sorted(migrated_signatures - spec_signatures)
  if unknown_in_spec:
    raise MigrationSliceError(
      "migrated macro signatures are not present in morphoSpec selector surface: "
      f"{unknown_in_spec}"
    )

  baseline = load_baseline(baseline_path)
  validate_against_baseline(migrated_signatures, baseline)
  blocked_signatures = validate_blocked_against_baseline(
    spec_signatures=spec_signatures,
    migrated_signatures=migrated_signatures,
    baseline=baseline,
  )

  coverage = round(100.0 * len(migrated_signatures) / max(len(spec_signatures), 1), 2)
  return {
    "status": "ok",
    "specPath": str(spec_path.relative_to(ROOT)),
    "macroPath": str(macro_path.relative_to(ROOT)),
    "contract": contract_name,
    "migratedCount": len(migrated_signatures),
    "specSignatureCount": len(spec_signatures),
    "coveragePct": coverage,
    "migratedSignatures": sorted(migrated_signatures),
    "blockedCount": len(blocked_signatures),
    "blockedSignatures": blocked_signatures,
  }


def parser() -> argparse.ArgumentParser:
  p = argparse.ArgumentParser(
    description="Check macro-native migrated Morpho selector slice against spec and baseline."
  )
  p.add_argument("--spec", type=pathlib.Path, default=SPEC_PATH)
  p.add_argument("--macro", type=pathlib.Path, default=MACRO_PATH)
  p.add_argument("--baseline", type=pathlib.Path, default=BASELINE_PATH)
  p.add_argument("--contract", default=DEFAULT_CONTRACT)
  p.add_argument("--json-out", type=pathlib.Path)
  p.add_argument(
    "--write",
    action="store_true",
    help="Update baseline expectedMigrated set from current macro slice source",
  )
  return p


def main() -> None:
  args = parser().parse_args()

  macro_signatures = extract_macro_signatures(read_text(args.macro), contract_name=args.contract)
  if args.write:
    spec_signatures = extract_spec_signatures(read_text(args.spec))
    blocked = sorted(spec_signatures - macro_signatures)
    baseline = {
      "source": str(args.macro.relative_to(ROOT)),
      "contract": args.contract,
      "expectedMigrated": sorted(macro_signatures),
      "expectedBlocked": {sig: "pending macro migration (tracked blocker)" for sig in blocked},
      "notes": (
        "Fail-closed macro-native migration slice tracker. "
        "Changes require explicit reviewed baseline updates; "
        "every non-migrated signature must stay explicitly classified with a blocker reason."
      ),
    }
    write_baseline(args.baseline, baseline)

  report = run_check(
    spec_path=args.spec,
    macro_path=args.macro,
    baseline_path=args.baseline,
    contract_name=args.contract,
  )

  if args.json_out:
    args.json_out.parent.mkdir(parents=True, exist_ok=True)
    with args.json_out.open("w", encoding="utf-8") as f:
      json.dump(report, f, indent=2, sort_keys=True)
      f.write("\n")

  print("macro-migration-slice check: OK")
  print(f"contract: {report['contract']}")
  print(
    "selector coverage: "
    f"{report['migratedCount']}/{report['specSignatureCount']} ({report['coveragePct']}%)"
  )
  print(f"migrated signatures: {', '.join(report['migratedSignatures'])}")


if __name__ == "__main__":
  try:
    main()
  except MigrationSliceError as e:
    print(f"macro-migration-slice check failed: {e}", file=sys.stderr)
    sys.exit(1)
  except FileNotFoundError as e:
    print(f"macro-migration-slice check failed: {e}", file=sys.stderr)
    sys.exit(1)
